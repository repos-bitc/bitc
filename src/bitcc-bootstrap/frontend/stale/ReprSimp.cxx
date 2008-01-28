/**************************************************************************
 *
 * Copyright (C) 2006, Johns Hopkins University.
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

#include <stdint.h>
#include <stdlib.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>
#include <libsherpa/UExcept.hxx>
#include <libsherpa/CVector.hxx>
#include <libsherpa/avl.hxx>
#include <assert.h>
#include "AST.hxx"
#include "Type.hxx"
#include "inter-pass.hxx"

using namespace sherpa;

void
CollectBodyTags(GCPtr<AST> body, GCPtr<CVector<std::string> > tags, std::string tagPrefix)
{
  GCPtr<CVector<GCPtr<AST> > > subCases = new CVector<GCPtr<AST> >;

  // Keep track of where this pass started adding tags.
  size_t startTagNdx = tags->size();

  // First, collect all tags and sub-bodies that appear at the current 
  // level.
  for (size_t i = 0; i < body->children->size(); i++) {
    GCPtr<AST> bform = body->child(i);

    if (bform->astType == at_reprtag) {
      for(size_t tag = 0; tag < bform->children->size(); tag++) {
	// Remember that at this point we have not done symbol
	// resolution yet, so we must manipulate the ASTs using the
	// 's' field!
	GCPtr<AST> tagID = bform->child(tag);
	std::string accumTag = tagPrefix + tagID->s + "#";

	// If this tag is not already present in the tag collection, add it:
	bool found = false;
	for (size_t etg = 0; !found && etg < tags->size(); etg++) {
	  if (tags->elem(etg) == accumTag) 
	    found = true;
	}

	if (!found) {
	  REPR_SIMP_DEBUG std::cerr << "Appending tag " 
				    << accumTag << std::endl;
	  tags->append(accumTag);
	}
      }
    }
    else if (bform->astType == at_reprcase) {
      for (size_t leg = 0; leg < bform->children->size(); leg++) {
	GCPtr<AST> theLeg = bform->child(leg);
	subCases->append(theLeg);
      }
    }
  }

  size_t endTagNdx = tags->size();

  if (subCases->size()) {
    REPR_SIMP_DEBUG std::cerr << "There are subcases to process." 
			      << std::endl;

    for (size_t i = startTagNdx; i < endTagNdx; i++) {
      std::string prefix = tags->elem(i);

      for (size_t leg = 0; leg < subCases->size(); leg++) {
	GCPtr<AST> theLeg = subCases->elem(leg);
	GCPtr<AST> body = theLeg->child(0);

	for (size_t tag = 1; tag < theLeg->children->size(); tag++) {
	  GCPtr<AST> theTagID = theLeg->child(tag);
	  std::string matchThis = std::string("#") + theTagID->s + "#";

	  // We now have the tag on this leg in a form that we can
	  // pattern match against the accumulated supertag. If it is
	  // present in the supertag, then this body is active and we
	  // need to process it for this case.
	  if (prefix.find(matchThis, 0) != std::string::npos)
	    CollectBodyTags(body, tags, prefix);
	}
      }
    }
  }
}

void
CollectReprLeg(GCPtr<AST> body, GCPtr<AST> ctor, std::string theTag)
{
  //GCPtr<CVector<GCPtr<AST> > > subCases = new CVector<GCPtr<AST> >;

  for (size_t i = 0; i < body->children->size(); i++) {
    GCPtr<AST> bform = body->child(i);

    if (bform->astType == at_field) {
      GCPtr<AST> ident = bform->child(0);
      REPR_SIMP_DEBUG std::cerr << "  Appending field " 
				<< ident->s << std::endl;

      ctor->children->append(bform);
    }
    else if (bform->astType == at_reprcase) {
      for (size_t leg = 0; leg < bform->children->size(); leg++) {
	GCPtr<AST> theLeg = bform->child(leg);
	GCPtr<AST> subBody = theLeg->child(0);

	for (size_t tag = 1; tag < theLeg->children->size(); tag++) {
	  GCPtr<AST> theTagID = theLeg->child(tag);
	  std::string matchThis = std::string("#") + theTagID->s + "#";

	  // std::cerr << "Checking " << matchThis << std::endl;

	  if (theTag.find(matchThis, 0) != std::string::npos)
	    CollectReprLeg(subBody, ctor, theTag);
	}
      }
    }
  }
}

// ReprSimp -- simplify defrepr forms to unions.
//
// This version simply wipes out the defrepr forms altogether and 
// replaces them with unions.
//
static GCPtr<AST> 
ReprSimp(GCPtr<AST> ast)
{
  switch (ast->astType) {
  case at_defrepr:
    {
      GCPtr<AST> body = ast->child(4);
      GCPtr<AST> ctors = new AST(at_constructors, ast->loc);

      REPR_SIMP_DEBUG std::cerr << "Processing defrepr " 
				<< ast->fqn << std::endl;
      GCPtr< CVector<std::string> > tagVec = new CVector<std::string>;
      CollectBodyTags(body, tagVec, "#");

      for (size_t tag = 0; tag < tagVec->size(); tag++) {
	std::string theTag = tagVec->elem(tag);
	GCPtr<AST> ident = new AST(at_ident, LToken(ast->loc, theTag));
	GCPtr<AST> ctor = new AST(at_constructor, ast->loc, ident);

	REPR_SIMP_DEBUG std::cerr << "Processing leg for " 
				  << theTag << std::endl; 

	CollectReprLeg(body, ctor, theTag);
	ctors->children->append(ctor);
      }

      // ast->child(4) = FlattenBody(body, ctors, "");
      ast->child(4) = ctors;

      ast->astType = at_defunion;

      return ast;
    }

  case at_declrepr:
    {
      ast->astType = at_declunion;
      return ast;
    }

  default:
    break;
  }

  GCPtr< CVector<GCPtr<AST> > > newChildren = new CVector<GCPtr<AST> >;
  for (size_t i = 0; i < ast->children->size(); i++) {
    GCPtr<AST> newAst = ReprSimp(ast->child(i));
    if (newAst)
      newChildren->append(newAst);
  }
  ast->children->erase();

  for (size_t i = 0; i < newChildren->size(); i++)
    ast->children->append(newChildren->elem(i));

  return ast;
}

bool
UocInfo::fe_reprSimp(std::ostream& errStream, 
		     bool init, unsigned long flags)
{
  ReprSimp(ast);
  return true;
}

