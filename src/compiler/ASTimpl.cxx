/**************************************************************************
 *
 * Copyright (C) 2010, Jonathan S. Shapiro
 * Portions Copyright (C) 2008, Johns Hopkins University.
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

using namespace boost;
using namespace sherpa;

shared_ptr<AST>
AST::makeBoolLit(const sherpa::LToken &tok)
{
  shared_ptr<AST> ast = AST::make(at_boolLiteral, tok);
  if (tok.str == "#t" || tok.str == "true")
    ast->litValue.b = true;
  else
    ast->litValue.b = false;
  ast->litValue.litType = lt_bool;

  return ast;
}

shared_ptr<AST>
AST::makeCharLit(const sherpa::LToken &tok)
{
  // FIX: (shap) This needs to convert to ordinal representation
  // and use a more appropriate element type.

  shared_ptr<AST> ast = AST::make(at_charLiteral, tok);
  //  mpz_init_set_str(ast->litValue.i, tok.is.c_str(), 0);

  ast->litValue.c = LitValue::DecodeCharacter(tok.str);
  ast->litValue.litType = lt_char;

  return ast;
}

/// This needs to use bignum representation, because host integer
/// and target integer sizes may not agree.
shared_ptr<AST>
AST::makeIntLit(const sherpa::LToken &tok)
{
  shared_ptr<AST> ast = AST::make(at_intLiteral, tok);
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

  ast->litBase = 10;

  if (num.size() > 1) {
    if (num[0] == '0' && num[1] == 'x') {
      ast->litBase = 16;
      num = num.substr(2, num.size());
    }
    else if (num[0] == '0' && num[1] == 'o') {
      ast->litBase = 8;
      num = num.substr(2, num.size());
    }
    else if (num[0] == '0' && num[1] == 'b') {
      ast->litBase = 2;
      num = num.substr(2, num.size());
    }
    else if (num[0] == '0') {
      ast->litBase = 8;
      // No need to adjust num in this case.
    }
  }

  ast->litValue.i = BigNum(num, ast->litBase);

  // Sign is not being considered by bignum implementation
  if (negative)
    ast->litValue.i = -ast->litValue.i;

  ast->litValue.litType = lt_int;

  return ast;
}

/// @bug This works only for single- and double-precision floats, and
/// it doesn't work at all for non-ANSI floating point implementations
/// on either host or target.
shared_ptr<AST>
AST::makeFloatLit(const sherpa::LToken &tok)
{
  shared_ptr<AST> ast = AST::make(at_floatLiteral, tok);
  ast->litBase = 10;
  ast->litValue.d = strtod(tok.str.c_str(), 0);

  ast->litValue.litType = lt_float;
  return ast;
}

shared_ptr<AST>
AST::makeStringLit(const sherpa::LToken &tok)
{
  shared_ptr<AST> ast = AST::make(at_stringLiteral, tok);
  ast->litValue.s = tok.str;
  ast->litValue.litType = lt_string;

  return ast;
}

std::string
AST::atKwd() const
{
  std::string s = old_atKwd();
  if (s != printName()) {
    std::cerr << s << " != " << printName() << std::endl;
    assert(old_atKwd() == printName());
  }

  return printName();
}

std::string
AST::old_atKwd() const
{
  switch(astType) {
  case at_Null:
    return "NULL";

  case at_mixExpr:
    return "mixExpr";

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
    return "union";

  case at_declrepr:
  case at_defrepr:
    return "repr";

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
    return "struct";

  case at_defobject:
    return "object";

  case at_define:
  case at_recdef:
    return "def";

  case at_declares:
    return "<declares>";

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
    return "<field>";

  case at_methdecl:
    return "<methdecl>";

  case at_fill:
    return "fill";

  case at_bitfieldType:
    return "bitfield";

  case at_byRefType:
    return "ByRef";

  case at_arrayRefType:
    return "ArrayRef";

  case at_boxedType:
    return "boxed";

  case at_unboxedType:
    return "unboxed";

  case at_fn:
    return "fn";

  case at_methType:
    return "method";

  case at_fieldType:
    return "<field>";
    
//   case at_closureType:
//     return "closure";

  case at_primaryType:
    return "<primaryType>";

  case at_argVec:
    return "<argVec>";

  case at_fnargVec:
    return "<fnargVec>";

  case at_arrayType:
    return "array";

  case at_vectorType:
    return "vector";

  case at_typeapp:
    return "<typeapp>";

  case at_mutableType:
    return "mutable";

  case at_constType:
    return "const";

  case at_identPattern:
    return "<identPattern>";

  case at_tqexpr:
    return "<tqexpr>";

  case at_suspend:
    return "suspend";

  case at_unit:
    return "()";

  case at_letGather:
    return "<letgather>";

  case at_allocREF:
    return "<allocREF>";

  case at_mkClosure:
    return "<mkClosure>";

  case at_mkArrayRef:
    return "<mkArrayRef>";

  case at_copyREF:
    return "<copyREF>";

  case at_setClosure:
    return "<setClosure>";

  case at_dup:
    return "dup";

  case at_MakeVector:
    return "MakeVector";

  case at_vector:
    return "vector";

  case at_array:
    return "array";

#ifdef HAVE_INDEXABLE_LENGTH_OPS
  case at_array_length:
    return "array-length";

  case at_array_ref_length:
    return "array-ref-length";

  case at_vector_length:
    return "vector-length";
#endif

  case at_begin:
    return "begin";

  case at_labeledBlock:
    return "label";

  case at_return_from:
    return "<return_from>";

  case at_loop:
    return "loop";

  case at_looptest:
    return "<looptest>";

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

  case at_condelse:
    return "otherwise";

  case at_uswitch:
    return "switch";

  case at_usw_legs:
    return "<sw_legs>";

  case at_usw_leg:
    return "case";

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
    return "<letStar>";

  case at_letbindings:
    return "<letbindings>";

  case at_letbinding:
    return "letbinding";

  case at_loopbindings:
    return "<loopbindings>";

  case at_loopbinding:
    return "loopbinding";

  case at_letrec:
    return "letrec";

  case at_deftypeclass:
    return "trait";

  case at_definstance:
    return "instance";

  case at_defexception:
    return "exception";

  case at_exceptionType:
    return "exception";

  case at_dummyType:
    return "#dummy#";

  case at_importAs:
    return "import";

  case at_provide:
    return "provide";

  case at_import:
    return "import";

  case at_ifsel:
    return "<ifsel>";

  case at_proclaim:
    return "def";

  case at_array_nth:
    return "<array_nth>";

  case at_array_ref_nth:
    return "<array_ref_nth>";

  case at_vector_nth:
    return "<vector_nth>";

  case at_tcdecls:
    return "<tc_decls>";

  case at_method_decls:
    return "<method_decls>";

  case at_method_decl:
    return "<method_decl>";

  case at_tcmethods:
    return "<methods>";

  case at_tcmethod_binding:
    return "<method_bindings>";

  case at_tyfn:
    return "tyfn";

  case at_tcapp:
    return "<tcapp>";

  case at_oc_closed:
    return "closed";
    
  case at_oc_open:
    return "open";
    
  case at_unboxedCat:
    return "unboxed";

  case at_boxedCat:
    return "boxed";

  case at_opaqueCat:
    return "opaque";

  case at_qualType:
    return "forall";

  case at_sizeof:
    return "sizeof";

  case at_bitsizeof:
    return "bitsizeof";

  case at_constraints:
    return "<constraints>";

  case at_struct_apply:
    return "<struct_apply>";

  case at_object_apply:
    return "<object_apply>";

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
  case agt_uselhs:
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
  case agt_openclosed:
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
  case id_block:
    return "block";
  case id_unresolved:
    return "unresolved";
  case id_tvar:
    return "type-variable";
  case id_union:
    return "union";
  case id_struct:
    return "struct";
  case id_object:
    return "object";
  case id_typeclass:
    return "typeclass";
  case id_tcmethod:
    return "method";
  case id_method:
    return "method";
  case id_field:
    return "field";
  case id_interface:
    return "interface";
  case id_value:
    return "value";
  case id_ucon:
    return "union-ctr";
  case id_ucon0:
    return "union-ctr0";
  case idc_type:
    return "Type";
  case idc_value:
    return "Value";
  case idc_ctor:
    return "Constructor";
  case idc_uctor:
    return "Union-ctor";
  case idc_apply:
    return "Applicable-value";
  case idc_usesel_lhs:
    return "Usesel-lhs";
  }
  return "IMPOSSIBLE";
}

void
AST::disown(size_t s)
{
  children.erase(children.begin() + s);
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
    return true;

  default:
    return false;
  }
}

void
AST::clearTypes() {
  symType = GC_NULL;
  scheme = GC_NULL;
  for (size_t i=0; i<children.size(); i++)
    child(i)->clearTypes();
}


void
AST::getIds(std::ostream &errStream,
            std::vector<shared_ptr<AST> >& ids,
            bool getPattern)
{
  switch(astType) {
  case at_identPattern:
    if (getPattern)
      ids.push_back(shared_from_this());
    else
      ids.push_back(child(0));
    break;

  default:
    errStream << loc << ": Internal Compiler Error,"
              << " getIds routine obtained the wrong "
              << "AST TYPE " << tagName()
              << std::endl;
  }
}

shared_ptr<AST>
AST::getID()
{
  switch(astType) {
  case at_define:
  case at_recdef:
  case at_letbinding:
  case at_loopbinding:
    return child(0)->child(0);

  case at_defstruct:
  case at_defunion:
  case at_declstruct:
  case at_declunion:
  case at_proclaim:
  case at_defexception:
  case at_deftypeclass:
    return child(0);

  default:
    return GC_NULL;
  }
}

shared_ptr<AST>
AST::getInstanceMethod(std::string s)
{
  assert(astType == at_definstance);
  shared_ptr<AST> methods = child(1);
  
  for(size_t c=0; c < methods->children.size(); c++) {
    shared_ptr<AST> method = methods->child(c);
    shared_ptr<AST> method_name = method->child(0);
    shared_ptr<AST> method_val = method->child(1);

    if(method_name->s == s) 
      return method_val;
  }

  return GC_NULL;
}


bool
AST::isUnionLeg()
{
  assert(astType == at_ident);
  return isIdentType(idc_uctor);
}

bool
AST::isTcMethod()
{
  if ((astType == at_ident) && isIdentType(id_tcmethod))
    return true;
  else
    return false;
}

shared_ptr<AST>
AST::getCtr()
{
  if (astType == at_ident)
    return shared_from_this();

  if (astType == at_fqCtr)
    return child(1);

  assert(false);
  return GC_NULL;
}

/* Rename identifier `from' to `to' in `ast' */
void
AST::rename(shared_ptr<AST> from, std::string newName)
{
  shared_ptr<AST> me = shared_from_this();
  switch(astType) {
  case at_ident:
    if (me == from || symbolDef == from)
      s = newName;
    break;

    // switched identifier also gets renamed in
    // the encoded at_switch and at_try positions here

  default:
    for (size_t c = 0; c < children.size(); c++)
      child(c)->rename(from, newName);
    break;
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

bool
AST::isIdentType(IdentType t)
{
  return ((identType == t) ||
          ((t == idc_type) && ((identType == id_tvar) ||
                               (identType == id_union) ||
                               (identType == id_struct) ||
                               (identType == id_object))) ||
          ((t == idc_value) && ((identType == id_value) ||
                                (identType == id_ucon0) ||
                                (identType == id_tcmethod))) ||
          ((t == idc_ctor)  && ((identType == id_struct) ||
                                (identType == id_object) ||
                                (identType == id_ucon) ||
                                (identType == id_ucon0))) ||
          ((t == idc_uctor) && ((identType == id_ucon) ||
                                (identType == id_ucon0))) ||
          // The idc_apply case is written as a recursive call and not
          // as disjunction of various id_* cases since in the case
          // where we are applying a valye that was proclaimed, the
          // identType would still be idc_value, and not id_value.
          ((t == idc_apply) && (isIdentType(idc_value) ||
                                isIdentType(idc_ctor) ||
                                isIdentType(id_method))) ||
          ((t == idc_usesel_lhs) && ((identType == id_interface) ||
                                     (identType == id_struct) ||
                                     (identType == id_object))));
}

int
AST::precedence() const
{
  switch(astType) {

  case at_usesel:
  case at_ident:
  case at_select:
  case at_primaryType:
  case at_exceptionType:
    return 0;

  case at_typeapp:
    return 1;

    /* blk_permqual_type */
  case at_mutableType:
  case at_constType:
    return 2;

    /* blk_postfix_type */
  case at_boxedType:
  case at_arrayType:
  case at_vectorType:
    return 3;

  default:
    return -1;                  // no particular precedence
  }
}
