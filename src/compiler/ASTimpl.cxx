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

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>

#include "UocInfo.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "TypeInfer.hxx"
#include "inter-pass.hxx"
#include "FQName.hxx"
#include "inter-pass.hxx"

using namespace sherpa;

GCPtr<AST> 
AST::makeBoolLit(const sherpa::LToken &tok)
{
  GCPtr<AST> ast = new AST(at_boolLiteral, tok);
  if(tok.str == "#t")
    ast->litValue.b = true;
  else
    ast->litValue.b = false;
  
  return ast;
}

GCPtr<AST> 
AST::makeCharLit(const sherpa::LToken &tok)
{
  // FIX: (shap) This needs to convert to ordinal representation
  // and use a more appropriate element type.

  GCPtr<AST> ast = new AST(at_charLiteral, tok);
  //  mpz_init_set_str(ast->litValue.i, tok.is.c_str(), 0);

  ast->litValue.c = LitValue::DecodeCharacter(tok.str);
  return ast;
}

GCPtr<AST>  
AST::makeIntLit(const sherpa::LToken &tok) 
{
  GCPtr<AST> ast = new AST(at_intLiteral, tok);
  std::string num = "";
  bool negative = false;

  /* Forgetting the sign, base information in the lexer, and
     rediscovering it here is a little stupid. */
  
  if (tok.str[0] == '-') {
    num = tok.str.substr(1, tok.str.size());
    negative = true;
  }
  else {
    num = tok.str.substr(0, tok.str.size());
  }

  std::string::size_type pos = num.find ('r');
  if(pos != std::string::npos) {
    std::string rad = num.substr(0, pos);
    num = num.substr(pos+1, num.size());
    char *end;
    ast->litBase = strtoul(rad.c_str(), &end, 10); //&OK
  }
  else {
    ast->litBase = 10;
  }
  
  ast->litValue.i = BigNum(num, ast->litBase);  
  
  // Sign is not being considered by bignum implementation
  if(negative)
    ast->litValue.i = -ast->litValue.i;
  
  return ast;
}

/// @bug This is not doing the correct conversion. It completely
/// ignores the radix encoding and the exponent encoding.
GCPtr<AST> 
AST::makeFloatLit(const sherpa::LToken &tok) 
{ 
  GCPtr<AST> ast = new AST(at_floatLiteral, tok);
  ast->litBase = 10;  
  ast->litValue.d = strtod(tok.str.c_str(), 0);
#if 0
  std::string litString = tok.str;
  std::string expString;
  std::string mantissaString;
 
  std::string::size_type epos = litString.find ('^');

  if(epos != std::string::npos) {
    expString = litString.substr(epos+1, litString.size());
    mantissaString = litString.substr(0, epos);
  }
  else {
    expString = "";
    mantissaString = litString;
  }
   
  /* Handle the mantissa */
  
  std::string::size_type pos = mantissaString.find ('r');
  if(pos != std::string::npos) {
    std::string rad;
    if(mantissaString[0] == '-') {
      rad = mantissaString.substr(1, pos); 
      mantissaString =
        "-" + mantissaString.substr(pos+1, mantissaString.size());
    }
    else {
      rad = mantissaString.substr(0, pos);
      mantissaString = mantissaString.substr(pos+1, mantissaString.size());
    }
    
    char *end;
    ast->litBase = strtoul(rad.c_str(), &end, 10); // &OK
  }
   
  /* Handle the exponent part */
  std::string exponent = "";//ss.str();
  if(epos != std::string::npos) {
    size_t expBase = 10;
    
    std::string::size_type pos = expString.find ('r');
    if(pos != std::string::npos) {
      std::string rad;
      if(expString[0] == '-') {
	rad = expString.substr(1, pos); 
	expString = "-" + expString.substr(pos+1, expString.size());
      }
      else {
	rad = expString.substr(0, pos);
	expString = expString.substr(pos+1, expString.size());
      }
      
      char *end;
      expBase = strtoul(rad.c_str(), &end, 10); //&OK
    }
    mpz_t expmpz;
    mpz_init_set_str(expmpz, expString.c_str(), expBase);
    exponent = "@" + std::string(mpz_get_str (NULL, ast->litBase, expmpz));
  }
  else 
    exponent = "@0";
  
  //std::cout << " Mantissa = " << mantissaString << " Exponent = " 
  //	    << exponent 
  //	    << " Base = " << ast->litBase;
  
  /* Finish off */
  mpf_init_set_str(ast->litValue.d, (mantissaString + exponent).c_str(),
		   ast->litBase);
  //gmp_printf(" %Ff\n", ast->litValue.d);
  
#endif
  return ast;
}

GCPtr<AST> 
AST::makeStringLit(const sherpa::LToken &tok)
{
  GCPtr<AST> ast = new AST(at_stringLiteral, tok);
  ast->litValue.s = tok.str;
  
  return ast;
}

std::string
AST::atKwd() const
{
  switch(astType) {
  case at_Null:
    return "NULL";

  case at_docString:
    return "<doccomment>";

  case at_ident:
    return "<ident>";

  case at_ifident:
    return "<ifident>";

  case at_boolLiteral:
    return "<boolLiteral>";

  case at_charLiteral:
    return "<charLiteral>";

  case at_intLiteral:
    return "<intLiteral>";

  case at_floatLiteral:
    return "<floatLiteral>";

  case at_stringLiteral:
    return "<stringLiteral>";

  case at_module:
    return "module";

  case at_interface:
    return "interface";

  case at_defunion:
  case at_declunion:
    return "defunion";

  case at_declrepr:
  case at_defrepr:
    return "defrepr";

    //   case at_reprbody:
    //     return "<reprbody>";
    //   case at_reprcase:
    //     return "<reprcase>";
    //   case at_reprcaselegR:
    //     return "<reprcaseleg>";
    //   case at_reprtag:
    //     return "<reprtag>";
    

  case at_defstruct:
  case at_declstruct:
    return "defstruct";

  case at_define:
  case at_recdef:
    return "define";

  case at_declares:
    return "declares";

  case at_declare:
    return "declare";

  case at_tvlist:
    return "<tvlist>";

  case at_constructors:
    return "<constructors>";

  case at_constructor:
    return "<constructor>";

  case at_reprctrs:
    return "<reprctrs>";

  case at_reprctr:
    return "<reprctr>";

  case at_reprrepr:
    return "<reprrepr>";

  case at_fields:
    return "<fields>";

  case at_field:
    return "field";

  case at_fill:
    return "fill";

  case at_reserved:
    return "reserved";

  case at_bitfield:
    return "bitfield";

  case at_byrefType:
    return "by-ref";

  case at_refType:
    return "ref";

  case at_valType:
    return "val";

  case at_fn: 
    return "fn";

//   case at_closureType: 
//     return "closure";

  case at_primaryType:
    return "<primaryType>";

  case at_argVec:
  case at_fnargVec:
    return "";

  case at_arrayType:
    return "array";

  case at_vectorType:
    return "vector";

  case at_typeapp:
    return "";

  case at_mutableType:
    return " mutable";

  case at_identPattern:
    return "<identPattern>";

  case at_tqexpr:
    return "the";

  case at_suspend:
    return "suspend";

  case at_unit:
    return "unit";

  case at_letGather:
    return "<letgather>";

  case at_allocREF:
    return "ALLOC-REF";

  case at_mkClosure:
    return "MAKE-CLOSURE";

  case at_copyREF:
    return "COPY-REF";

  case at_setClosure:
    return "SET!-REF";

  case at_dup:
    return "dup";

  case at_makevectorL:
    return "make-vectorL";

  case at_vector:
    return "vector";

  case at_array:
    return "array";

  case at_vector_length:
    return "vector-length";

  case at_array_length:
    return "array-length";

  case at_begin:
    return "begin";

  case at_do:
    return "do";

  case at_dotest:
    return "<dotest>";

  case at_select:
    return "<select>";

  case at_fqCtr:
    return "<fqCtr>";

  case at_sel_ctr:
    return "<sel_ctr>";

  case at_usesel:
    return "<usesel>";

  case at_lambda:
    return "lambda";

  case at_apply:
    return "<apply>";

  case at_if:
    return "if";

  case at_when:
    return "when";

  case at_and:
    return "and";

  case at_not:
    return "not";

  case at_or:
    return "or";

  case at_cond:
    return "cond";

  case at_cond_legs:
    return "<cond_legs>";

  case at_cond_leg:
    return "<cond_leg>";

  case at_otherwise:
    return "otherwise";

  case at_switch:
    return "switch";
    
  case at_sw_legs:
    return "<sw_legs>";

  case at_sw_leg:
    return "<sw_leg>";

  case at_setbang:
    return "set!";

  case at_deref:
    return "deref";

  case at_inner_ref:
    return "inner_ref";

  case at_try:
    return "try";

  case at_throw:
    return "throw";

  case at_let:
    return "let";

  case at_letStar:
    return "let*";

  case at_letbindings:
    return "<letbindings>";

  case at_letbinding:
    return "letbinding";

  case at_dobindings:
    return "<dobindings>";

  case at_dobinding:
    return "dobinding";

  case at_letrec:
    return "letrec";

  case at_use:
    return "use";

  case at_use_case:
    return "<use_case>";

  case at_deftypeclass:
    return "deftypeclass";

  case at_definstance:
    return "definstance";

  case at_defexception:
    return "defexception";

  case at_exceptionType:
    return "exception";

  case at_dummyType:
    return "#dummy#";

  case at_importAs:
    return "import";

  case at_provide:
    return "provide";

  case at_from:
    return "from";

  case at_ifsel:
    return "<ifsel>";

  case at_proclaim:
    return "proclaim";

  case at_array_nth:
    return "array-nth";

  case at_vector_nth:
    return "vector-nth";

  case at_tcdecls:
    return "<tc_decls>";

  case at_method_decls:
    return "<method_decls>";

  case at_method_decl:
    return "<method_decl>";

  case at_methods:
    return "<methods>";

  case at_tyfn:
    return "tyfn";

  case at_tcapp:
    return "<tcapp>";

  case at_refCat:
    return "ref";    

  case at_valCat:
    return "val";

  case at_opaqueCat:
    return "opaque";

  case at_qualType:
    return "forall";

  case at_constraints:
    return "<constraints>";

  case at_struct_apply:
    return "<struct_apply>";

  case at_ucon_apply:
    return "<ucon_apply>";

  case at_localFrame:
    return "<local_frame>";

  case at_frameBindings:
    return "<frame_bindings>";

  case at_identList:
    return "<identList>";

  case at_container:
    return "<container>";

  case at_AnyGroup:
  case agt_var:
  case agt_literal:
  case agt_tvar:
  case agt_CompilationUnit:
  case agt_definition:
  case agt_type_definition:
  case agt_tc_definition:
  case agt_value_definition:
  case agt_if_definition:
  case agt_category:
  case agt_qtype:
  case agt_fielditem:
  case agt_type:
  case agt_expr:
  case agt_expr_or_define:
  case agt_eform:
  case agt_ow:
    //case agt_reprbodyitem:
  case agt_ucon:
    return "<GROUP>";
  }

  // Impossible to reach here.
  assert(false);
  // Satisfy the compiler
  return "<IMPOSSIBLE>";
}

std::string 
identTypeToString(IdentType id) 
{
  switch (id) {
  case id_unresolved:
    return "Unresolved";
  case id_value:
    return "Value";
  case id_type:
    return "Type";
  case id_constructor:
    return "Constructor";
  case id_field:
    return "Field";
  case id_exn:
    return "Exception";
  case id_typeclass:
    return "Type-class";
    //  case id_module:
    //    return "module";
  case id_interface:
    return "Interface";
  default:
    return "IMPOSSIBLE";
  }
}

void
AST::disown(size_t s)
{
  children = eliminate<GCPtr<AST> >(children, s);
}

bool 
AST::isTopLevelForm()
{
  switch(astType) {
  case at_define:
  case at_recdef:
  case at_defstruct:
  case at_defunion:
  case at_declunion:
  case at_declstruct:
  case at_proclaim:
  case at_declare:
  case at_importAs:
  case at_provide:
  case at_defexception:    
  case at_use:
    return true;
    
  default:
    return false;
  }
}

bool 
AST::leadsToTopLevelForm()
{
  switch(astType) {
  case at_module:
  case at_interface:
  case at_define:
  case at_recdef:
  case at_defstruct:
  case at_defunion:
  case at_declunion:
  case at_declstruct:
  case at_proclaim:
  case at_declare:
  case at_importAs:
  case at_provide:
  case at_defexception:    
  case at_use:
    return true;
    
  default:
    return false;
  }
}

void
AST::clearTypes() {
  symType = NULL;
  scheme = NULL;
  for(size_t i=0; i<children->size(); i++)
    child(i)->clearTypes();
}


void 
AST::getIds(std::ostream &errStream,
	    GCPtr< CVector<GCPtr<AST> > > ids,
	    bool getPattern)
{
  switch(astType) {
  case at_identPattern:
    if(getPattern)
      ids->append(this);
    else
      ids->append(child(0));
    break;
    
  default:
    errStream << loc << ": Internal Compiler Error,"
	      << " getIds routine obtained the wrong "
	      << "AST TYPE " << astTypeName()
	      << std::endl;
  }
}

GCPtr<AST> 
AST::getID()
{
  switch(astType) {
  case at_define:
  case at_recdef:
  case at_letbinding:
  case at_dobinding:
    return child(0)->child(0);
    
  case at_defstruct:
  case at_defunion:
  case at_proclaim:
  case at_declstruct:
  case at_declunion:
  case at_defexception:
  case at_deftypeclass:
    return child(0);
    
  default:
    return NULL;
  }
}

bool 
AST::isUnionLeg()
{
  assert(astType == at_ident);
  if(((identType == id_constructor) || 
      (identType == id_value && (Flags & ID_IS_CTOR))) &&
     symType->isUType())
    return true;
  else
    return false;     
}

bool
AST::isMethod()
{  
  if((astType == at_ident) && (Flags & ID_IS_METHOD))
    return true;
  
  return false;
}

GCPtr<AST> 
AST::getCtr()
{
  if(astType == at_ident)
    return this;
  
  if (astType == at_fqCtr)
    return child(1);

  assert(false);
  return NULL;
}

/* Rename identifier `from' to `to' in `ast' */ 
void
AST::rename(GCPtr<AST> from, std::string newName)
{ 
  GCPtr<AST> me = this;
  switch(astType) {
  case at_ident:    
    if(me == from || symbolDef == from)
      s = newName;
    break;
  
    // switched identifier also gets renamed in 
    // the encoded at_switch and at_try positions here
    
  default:
    for(size_t c = 0; c < children->size(); c++)
      child(c)->rename(from, newName);
    break;
  }
}

bool
AST::isLocation()
{
  switch (astType) {
  case at_ident:
      return true;
    
  case at_vector_nth:
  case at_deref:
      return true;

  case at_array_nth:
      return child(0)->isLocation();

  case at_select:
    return child(0)->isLocation();
        
  default:
      return false;
  }
}

bool
AST::isLiteral()
{
  switch (astType) {
  case at_boolLiteral:
  case at_charLiteral:
  case at_intLiteral:
  case at_floatLiteral:
  case at_stringLiteral:    
    return true;    
    
  default:
    return false;
  }
}
