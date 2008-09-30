/**************************************************************************
 *
 * Copyright (C) 2008, Johns Hopkins University.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or
 * without modification, are permitted provided that the following
 * conditions are met:
 *
 *   - Redistributions of source code must contain the above
 *     copyright notice, this list of conditions, and the following
 *     disclaimer.
 *
 *   - Redistributions in binary form must reproduce the above
 *     copyright notice, this list of conditions, and the following
 *     disclaimer in the documentation and/or other materials
 *     provided with the distribution.
 *
 *   - Neither the names of the copyright holders nor the names of any
 *     of any contributors may be used to endorse or promote products
 *     derived from this software without specific prior written
 *     permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 **************************************************************************/

/// @file Requirements checking pass for DEFREPR declarations.
///
///
/// Consider the defrepr:
///
/// <pre>
/// (defrepr name
///   (Ctr1 f11:type f21:type ... fn1:type
///     (where fp1=val11 fq1=val21 ... fm1=valm1))
///   (Ctr2 f12:type f22:type ... fn2:type
///     (where fp2=val12 fq2=val22 ... fm2=valm2))
///  ... )
/// </ped>
///
/// The following restrictions apply:
///
/// For all constructors Ctrx, Ctry, Ctrz ...:
///
/// - All fields fpx, fqx...fmx appearing in the `when' clause of a
///   constructor form Ctrx must be described within the body of
///   Ctrx. That is, {fpx, fqx...fmx} <= {f1x ... fnx}
///
/// - If the same field name appears in multiple legs of a DEFREPR, that
///   field must appear at the same bit offset in all legs where it
///   appears.  That is, fpx = fpy implies
///   bitOffset(fpx) = bitOffset(fpy).
///
/// - The fields within the when clauses of all constructor forms must
///   uniquely distinguish all constructible values of the union. The
///   compiler will not introduce any more tag bits for any defrepr
///   value.
///
/// - The defrepr form will not accept type arguments over
///   which it can be instantiated.
///
/// This pass tests that all of these requirements are satisfied.


#include <stdint.h>
#include <stdlib.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <string>
#include <map>
#include <sstream>
#include <errno.h>
#include "Version.hxx"
#include "UocInfo.hxx"
#include "AST.hxx"
#include "Environment.hxx"
#include "inter-pass.hxx"
#include "backend.hxx"

using namespace std;
using namespace boost;
using namespace sherpa;

/// @brief Given a sub-form describing a field, return the AST
/// describing its type.
static shared_ptr<AST>
getTypeAst(shared_ptr<AST> fld)
{
  switch(fld->astType) {
  case at_field:
    return fld->child(1);

  case at_fill:
  case at_reserved:
    return fld->child(0);

  default:
    assert(false);
    return GC_NULL;
  }
}

/// @brief Compute the bit offset of a field within its containing union.
///
/// Note that this computation is target dependent if the Word type is
/// being used.
static size_t
bitOffset(shared_ptr<AST> leg, size_t n)
{
  size_t off = 0;
  for (size_t c=1; c < n; c++) {
    shared_ptr<AST> fld = leg->child(c);
    shared_ptr<AST> fldType = getTypeAst(fld);
    if (fldType->astType == at_bitfield)
      off += fldType->field_bits;
    else
      off += fldType->symType->size();
  }

  return off;
}

/// @brief Compare types of two fields
///
/// If a field appears in multiple legs of a DEFREPR, it must have the
/// same type in all legs. If it is of bitfield type, it must also
/// have the same number of bits in all occurrences.
static bool
TypesAgree(shared_ptr<AST> fld1, shared_ptr<AST> fld2)
{
  shared_ptr<AST> fT1 = getTypeAst(fld1);
  shared_ptr<AST> fT2 = getTypeAst(fld2);

  if ((fT1->symType->strictlyEquals(fT2->symType)) &&
     (fT1->field_bits == fT2->field_bits))
    return true;
  else
    return false;
}

/// @brief Enforce the sanity requirements on REPR declarations.
bool
reprCheck(std::ostream& errStream, shared_ptr<AST> ast)
{
  bool errFree = true;
  switch(ast->astType) {
  case at_defrepr:
  case at_reprctrs:
  case at_reprctr:
  case at_declrepr:
    assert(false);
    break;

  case at_defunion:
    {
      if ((ast->flags & UNION_IS_REPR) == 0)
	break;

      shared_ptr<AST> ctrs = ast->child(4);

      /* The fields in the when cluses must be of integer-type
         This may be relaxed later. */
      for (size_t c=0; c < ctrs->children.size(); c++) {
	shared_ptr<AST> ctrc = ctrs->child(c);
	
	for (size_t i=1; i < ctrc->children.size(); i++) {
	  shared_ptr<AST> fldi = ctrc->child(i);

	  if (fldi->astType == at_field)
	    if (fldi->flags & FLD_IS_DISCM)
	      if (!fldi->symType->isInteger()) {
		errStream << fldi->loc << ": "
			  << " The discriminating field "
			  << fldi->child(0)->s
			  << " has a non-integer/bitfield type."
			  << std::endl;
		errFree = false;
	      }	  	
	}
      }

      /* Ascertain that common fields are at the same bit-offset
         and have the same type */
      for (size_t c=0; c < ctrs->children.size(); c++) {
	shared_ptr<AST> ctrc = ctrs->child(c);

	for (size_t i=1; i < ctrc->children.size(); i++) {
	  shared_ptr<AST> fldi = ctrc->child(i);
	  if (fldi->astType != at_field)
	    continue;
	
 	  size_t offc = bitOffset(ctrc, i);	  	
	  for (size_t d=c+1; d < ctrs->children.size(); d++) {
	    shared_ptr<AST> ctrd = ctrs->child(d);
	
	    for (size_t j=1; j < ctrd->children.size(); j++) {
	      shared_ptr<AST> fldj = ctrd->child(j);
	      if (fldj->astType != at_field)
		continue;
	
	      if (fldi->child(0)->s == fldj->child(0)->s) {
		if (offc != bitOffset(ctrd, j)) {
		  errStream << ctrd->loc << ": "
			    << " The constructors "
			    << ctrc->child(0)->s << " and "
			    << ctrd->child(0)->s
			    << " don't agree on the bit-offset "
			    << " for field " << fldi->child(0)->s
			    << std::endl;
		  errFree = false;
		}
		
		if (!TypesAgree(fldi, fldj)) {
		  errStream << ctrd->loc << ": "
			    << " The constructors "
			    << ctrc->child(0)->s << " and "
			    << ctrd->child(0)->s
			    << " don't agree on types "
			    << " for field " << fldi->child(0)->s
			    << std::endl;
		  errFree = false;
		}
	      }
	    }
	  }
	}
      }

      /* Ascertain that ``where'' fields are decisive */
      for (size_t c=0; c < ctrs->children.size(); c++) {
	shared_ptr<AST> ctrc = ctrs->child(c);
	
	typedef map<string, size_t> WhenMap;
	WhenMap when;
	
	for (size_t i=1; i < ctrc->children.size(); i++) {
	  shared_ptr<AST> fldi = ctrc->child(i);
	  if (fldi->flags & FLD_IS_DISCM) {
	    assert(fldi->astType == at_field);

	    when[fldi->child(0)->s] = fldi->unin_discm;
	  }
	}
	
	for (size_t d=c+1; d < ctrs->children.size(); d++) {
	  shared_ptr<AST> ctrd = ctrs->child(d);
	  bool differ=false;

	  for (size_t j=1; (!differ) && (j < ctrd->children.size()); j++) {
	    shared_ptr<AST> fldj = ctrd->child(j);

	    if (fldj->flags & FLD_IS_DISCM) {
	      assert(fldj->astType == at_field);
	
	      WhenMap::iterator itr_k = when.find(fldj->child(0)->s);
	      if (itr_k == when.end())
		continue;

	      if (itr_k->second != fldj->unin_discm) {
		differ=true;
		break;
	      }
	    }
	  }
	
	  if (!differ) {
	    errStream << ctrc->loc << ": "
		      << "Ambiguous defrepr constructors: "
		      << ctrc->child(0)->s << " and "
		      << ctrd->child(0)->s << ". Ambiguous case is: "
		      << std::endl;
	
	    for (WhenMap::iterator itr_k = when.begin();
		itr_k != when.end(); ++itr_k) {
	      if (itr_k != when.begin())
		errStream << ", ";	
	      errStream << itr_k->first << " = " << itr_k->second;
	    }
	
	    errStream << "." << std::endl;	
	    errFree = false;
	  }
	}
      }
    }

  default:
    {
      // only defunion clause has meaning
      for (size_t c=0; c < ast->children.size(); c++)
	CHKERR(errFree, reprCheck(errStream, ast->child(c)));
      break;
    }
  }

  return errFree;
}

bool
UocInfo::fe_reprCheck(std::ostream& errStream,
		      bool init, unsigned long flags)
{
  bool errFree = reprCheck(errStream, uocAst);
  return errFree;
}

