
/*
 * Copyright (C) 2008, The EROS Group, LLC.
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
 */

#include <stdlib.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <string>
#include "AST.hxx"


#include "Type.hxx"
#include "Special.hxx"
#include "UocInfo.hxx"

using namespace boost;
using namespace sherpa;
unsigned long long AST::astCount = 0;


AST::~AST()
{
}

AST::AST(const AstType at)
{
  astType = at;

  ID = ++(AST::astCount);
  identType = id_unresolved;
  flags = NO_FLAGS;
  isDecl = false;
  scheme = GC_NULL;
  symType = GC_NULL;
  symbolDef = GC_NULL;
  defn = GC_NULL;
  defForm = GC_NULL;
  defbps = GC_NULL;
  decl = GC_NULL;
  printVariant = pf_NONE;		// until otherwise stated
  tagType = GC_NULL;
  field_bits = 0;
  unin_discm = 0;
  total_fill = 0;
  tvarLB = GC_NULL;
  parentLB = GC_NULL;
}

AST::AST(const AstType at, const AST_TOKEN_TYPE& tok)
{
  astType = at;
  loc = tok.loc;
  s = tok.str;

  ID = ++(AST::astCount);
  identType = id_unresolved;
  flags = NO_FLAGS;
  isDecl = false;
  scheme = GC_NULL;
  symType = GC_NULL;
  symbolDef = GC_NULL;
  defn = GC_NULL;
  defForm = GC_NULL;
  defbps = GC_NULL;
  decl = GC_NULL;
  printVariant = pf_NONE;		// until otherwise stated
  tagType = GC_NULL;
  field_bits = 0;
  unin_discm = 0;
  total_fill = 0;
  tvarLB = GC_NULL;
  parentLB = GC_NULL;
}

AST::AST(const AstType at, const AST_LOCATION_TYPE& _loc)
{
  astType = at;
  loc = _loc;

  ID = ++(AST::astCount);
  identType = id_unresolved;
  flags = NO_FLAGS;
  isDecl = false;
  scheme = GC_NULL;
  symType = GC_NULL;
  symbolDef = GC_NULL;
  defn = GC_NULL;
  defForm = GC_NULL;
  defbps = GC_NULL;
  decl = GC_NULL;
  printVariant = pf_NONE;		// until otherwise stated
  tagType = GC_NULL;
  field_bits = 0;
  unin_discm = 0;
  total_fill = 0;
  tvarLB = GC_NULL;
  parentLB = GC_NULL;
}

AST::AST(const AstType at, const AST_LOCATION_TYPE& _loc,
         AST_SMART_PTR<AST> child1)
{
  astType = at;
  loc = _loc;
  addChild(child1);

  ID = ++(AST::astCount);
  identType = id_unresolved;
  flags = NO_FLAGS;
  isDecl = false;
  scheme = GC_NULL;
  symType = GC_NULL;
  symbolDef = GC_NULL;
  defn = GC_NULL;
  defForm = GC_NULL;
  defbps = GC_NULL;
  decl = GC_NULL;
  printVariant = pf_NONE;		// until otherwise stated
  tagType = GC_NULL;
  field_bits = 0;
  unin_discm = 0;
  total_fill = 0;
  tvarLB = GC_NULL;
  parentLB = GC_NULL;
}

AST::AST(const AstType at, const AST_LOCATION_TYPE& _loc,
         AST_SMART_PTR<AST> child1,
         AST_SMART_PTR<AST> child2)
{
  astType = at;
  loc = _loc;
  addChild(child1);
  addChild(child2);

  ID = ++(AST::astCount);
  identType = id_unresolved;
  flags = NO_FLAGS;
  isDecl = false;
  scheme = GC_NULL;
  symType = GC_NULL;
  symbolDef = GC_NULL;
  defn = GC_NULL;
  defForm = GC_NULL;
  defbps = GC_NULL;
  decl = GC_NULL;
  printVariant = pf_NONE;		// until otherwise stated
  tagType = GC_NULL;
  field_bits = 0;
  unin_discm = 0;
  total_fill = 0;
  tvarLB = GC_NULL;
  parentLB = GC_NULL;
}

AST::AST(const AstType at, const AST_LOCATION_TYPE& _loc,
         AST_SMART_PTR<AST> child1,
         AST_SMART_PTR<AST> child2,
         AST_SMART_PTR<AST> child3)
{
  astType = at;
  loc = _loc;
  addChild(child1);
  addChild(child2);
  addChild(child3);

  ID = ++(AST::astCount);
  identType = id_unresolved;
  flags = NO_FLAGS;
  isDecl = false;
  scheme = GC_NULL;
  symType = GC_NULL;
  symbolDef = GC_NULL;
  defn = GC_NULL;
  defForm = GC_NULL;
  defbps = GC_NULL;
  decl = GC_NULL;
  printVariant = pf_NONE;		// until otherwise stated
  tagType = GC_NULL;
  field_bits = 0;
  unin_discm = 0;
  total_fill = 0;
  tvarLB = GC_NULL;
  parentLB = GC_NULL;
}

AST::AST(const AstType at, const AST_LOCATION_TYPE& _loc,
         AST_SMART_PTR<AST> child1,
         AST_SMART_PTR<AST> child2,
         AST_SMART_PTR<AST> child3,
         AST_SMART_PTR<AST> child4)
{
  astType = at;
  loc = _loc;
  addChild(child1);
  addChild(child2);
  addChild(child3);
  addChild(child4);

  ID = ++(AST::astCount);
  identType = id_unresolved;
  flags = NO_FLAGS;
  isDecl = false;
  scheme = GC_NULL;
  symType = GC_NULL;
  symbolDef = GC_NULL;
  defn = GC_NULL;
  defForm = GC_NULL;
  defbps = GC_NULL;
  decl = GC_NULL;
  printVariant = pf_NONE;		// until otherwise stated
  tagType = GC_NULL;
  field_bits = 0;
  unin_discm = 0;
  total_fill = 0;
  tvarLB = GC_NULL;
  parentLB = GC_NULL;
}

AST::AST(const AstType at, const AST_LOCATION_TYPE& _loc,
         AST_SMART_PTR<AST> child1,
         AST_SMART_PTR<AST> child2,
         AST_SMART_PTR<AST> child3,
         AST_SMART_PTR<AST> child4,
         AST_SMART_PTR<AST> child5)
{
  astType = at;
  loc = _loc;
  addChild(child1);
  addChild(child2);
  addChild(child3);
  addChild(child4);
  addChild(child5);

  ID = ++(AST::astCount);
  identType = id_unresolved;
  flags = NO_FLAGS;
  isDecl = false;
  scheme = GC_NULL;
  symType = GC_NULL;
  symbolDef = GC_NULL;
  defn = GC_NULL;
  defForm = GC_NULL;
  defbps = GC_NULL;
  decl = GC_NULL;
  printVariant = pf_NONE;		// until otherwise stated
  tagType = GC_NULL;
  field_bits = 0;
  unin_discm = 0;
  total_fill = 0;
  tvarLB = GC_NULL;
  parentLB = GC_NULL;
}

::std::string
AST::getTokenString()
{
  return s;
}

void
AST::addChild(AST_SMART_PTR<AST> cld)
{
  children.push_back(cld);
}

const char *
AST::tagName(const AstType at)
{
  switch(at) {
  case at_Null:
    return "at_Null";
  case at_AnyGroup:
    return "at_AnyGroup";
  case at_ident:
    return "at_ident";
  case at_ifident:
    return "at_ifident";
  case at_usesel:
    return "at_usesel";
  case at_boolLiteral:
    return "at_boolLiteral";
  case at_charLiteral:
    return "at_charLiteral";
  case at_intLiteral:
    return "at_intLiteral";
  case at_floatLiteral:
    return "at_floatLiteral";
  case at_stringLiteral:
    return "at_stringLiteral";
  case at_module:
    return "at_module";
  case at_interface:
    return "at_interface";
  case at_defunion:
    return "at_defunion";
  case at_declunion:
    return "at_declunion";
  case at_defstruct:
    return "at_defstruct";
  case at_declstruct:
    return "at_declstruct";
  case at_defrepr:
    return "at_defrepr";
  case at_declrepr:
    return "at_declrepr";
  case at_reprctrs:
    return "at_reprctrs";
  case at_reprctr:
    return "at_reprctr";
  case at_reprrepr:
    return "at_reprrepr";
  case at_refCat:
    return "at_refCat";
  case at_valCat:
    return "at_valCat";
  case at_opaqueCat:
    return "at_opaqueCat";
  case at_closed:
    return "at_closed";
  case at_defexception:
    return "at_defexception";
  case at_deftypeclass:
    return "at_deftypeclass";
  case at_tcdecls:
    return "at_tcdecls";
  case at_tyfn:
    return "at_tyfn";
  case at_tcapp:
    return "at_tcapp";
  case at_method_decls:
    return "at_method_decls";
  case at_method_decl:
    return "at_method_decl";
  case at_qualType:
    return "at_qualType";
  case at_constraints:
    return "at_constraints";
  case at_definstance:
    return "at_definstance";
  case at_tcmethods:
    return "at_tcmethods";
  case at_tcmethod_binding:
    return "at_tcmethod_binding";
  case at_proclaim:
    return "at_proclaim";
  case at_define:
    return "at_define";
  case at_recdef:
    return "at_recdef";
  case at_importAs:
    return "at_importAs";
  case at_provide:
    return "at_provide";
  case at_import:
    return "at_import";
  case at_ifsel:
    return "at_ifsel";
  case at_declares:
    return "at_declares";
  case at_declare:
    return "at_declare";
  case at_tvlist:
    return "at_tvlist";
  case at_constructors:
    return "at_constructors";
  case at_constructor:
    return "at_constructor";
  case at_fields:
    return "at_fields";
  case at_field:
    return "at_field";
  case at_fill:
    return "at_fill";
  case at_literalType:
    return "at_literalType";
  case at_bitfield:
    return "at_bitfield";
  case at_refType:
    return "at_refType";
  case at_byrefType:
    return "at_byrefType";
  case at_valType:
    return "at_valType";
  case at_fn:
    return "at_fn";
  case at_primaryType:
    return "at_primaryType";
  case at_fnargVec:
    return "at_fnargVec";
  case at_arrayType:
    return "at_arrayType";
  case at_vectorType:
    return "at_vectorType";
  case at_mutableType:
    return "at_mutableType";
  case at_constType:
    return "at_constType";
  case at_typeapp:
    return "at_typeapp";
  case at_exceptionType:
    return "at_exceptionType";
  case at_dummyType:
    return "at_dummyType";
  case at_identPattern:
    return "at_identPattern";
  case at_tqexpr:
    return "at_tqexpr";
  case at_unit:
    return "at_unit";
  case at_suspend:
    return "at_suspend";
  case at_sizeof:
    return "at_sizeof";
  case at_bitsizeof:
    return "at_bitsizeof";
  case at_makevectorL:
    return "at_makevectorL";
  case at_vector:
    return "at_vector";
  case at_array:
    return "at_array";
  case at_begin:
    return "at_begin";
  case at_select:
    return "at_select";
  case at_fqCtr:
    return "at_fqCtr";
  case at_sel_ctr:
    return "at_sel_ctr";
  case at_array_nth:
    return "at_array_nth";
  case at_vector_nth:
    return "at_vector_nth";
  case at_array_length:
    return "at_array_length";
  case at_vector_length:
    return "at_vector_length";
  case at_lambda:
    return "at_lambda";
  case at_argVec:
    return "at_argVec";
  case at_apply:
    return "at_apply";
  case at_struct_apply:
    return "at_struct_apply";
  case at_ucon_apply:
    return "at_ucon_apply";
  case at_if:
    return "at_if";
  case at_when:
    return "at_when";
  case at_and:
    return "at_and";
  case at_or:
    return "at_or";
  case at_not:
    return "at_not";
  case at_cond:
    return "at_cond";
  case at_cond_legs:
    return "at_cond_legs";
  case at_cond_leg:
    return "at_cond_leg";
  case at_condelse:
    return "at_condelse";
  case at_setbang:
    return "at_setbang";
  case at_deref:
    return "at_deref";
  case at_dup:
    return "at_dup";
  case at_inner_ref:
    return "at_inner_ref";
  case at_allocREF:
    return "at_allocREF";
  case at_copyREF:
    return "at_copyREF";
  case at_mkClosure:
    return "at_mkClosure";
  case at_setClosure:
    return "at_setClosure";
  case at_block:
    return "at_block";
  case at_return_from:
    return "at_return_from";
  case at_switch:
    return "at_switch";
  case at_sw_legs:
    return "at_sw_legs";
  case at_sw_leg:
    return "at_sw_leg";
  case at_otherwise:
    return "at_otherwise";
  case at_try:
    return "at_try";
  case at_throw:
    return "at_throw";
  case at_let:
    return "at_let";
  case at_letbindings:
    return "at_letbindings";
  case at_letbinding:
    return "at_letbinding";
  case at_letrec:
    return "at_letrec";
  case at_do:
    return "at_do";
  case at_dobindings:
    return "at_dobindings";
  case at_dobinding:
    return "at_dobinding";
  case at_dotest:
    return "at_dotest";
  case at_localFrame:
    return "at_localFrame";
  case at_frameBindings:
    return "at_frameBindings";
  case at_letStar:
    return "at_letStar";
  case at_identList:
    return "at_identList";
  case at_container:
    return "at_container";
  case at_docString:
    return "at_docString";
  case at_letGather:
    return "at_letGather";
  case agt_var:
    return "agt_var";
  case agt_literal:
    return "agt_literal";
  case agt_tvar:
    return "agt_tvar";
  case agt_CompilationUnit:
    return "agt_CompilationUnit";
  case agt_definition:
    return "agt_definition";
  case agt_type_definition:
    return "agt_type_definition";
  case agt_tc_definition:
    return "agt_tc_definition";
  case agt_value_definition:
    return "agt_value_definition";
  case agt_if_definition:
    return "agt_if_definition";
  case agt_category:
    return "agt_category";
  case agt_openclosed:
    return "agt_openclosed";
  case agt_fielditem:
    return "agt_fielditem";
  case agt_qtype:
    return "agt_qtype";
  case agt_type:
    return "agt_type";
  case agt_expr:
    return "agt_expr";
  case agt_expr_or_define:
    return "agt_expr_or_define";
  case agt_eform:
    return "agt_eform";
  case agt_ucon:
    return "agt_ucon";
  case agt_ow:
    return "agt_ow";
  default:
    return "<unknown>";
  }
}

const char *
AST::astTypeName() const
{
  return tagName(astType);
}

const char *
AST::astName() const
{
  switch(astType) {
  case at_Null:
    return "Null";
  case at_AnyGroup:
    return "AnyGroup";
  case at_ident:
    return "ident";
  case at_ifident:
    return "ifident";
  case at_usesel:
    return "usesel";
  case at_boolLiteral:
    return "boolLiteral";
  case at_charLiteral:
    return "charLiteral";
  case at_intLiteral:
    return "intLiteral";
  case at_floatLiteral:
    return "floatLiteral";
  case at_stringLiteral:
    return "stringLiteral";
  case at_module:
    return "module";
  case at_interface:
    return "interface";
  case at_defunion:
    return "defunion";
  case at_declunion:
    return "declunion";
  case at_defstruct:
    return "defstruct";
  case at_declstruct:
    return "declstruct";
  case at_defrepr:
    return "defrepr";
  case at_declrepr:
    return "declrepr";
  case at_reprctrs:
    return "reprctrs";
  case at_reprctr:
    return "reprctr";
  case at_reprrepr:
    return "reprrepr";
  case at_refCat:
    return "refCat";
  case at_valCat:
    return "valCat";
  case at_opaqueCat:
    return "opaqueCat";
  case at_closed:
    return "closed";
  case at_defexception:
    return "defexception";
  case at_deftypeclass:
    return "deftypeclass";
  case at_tcdecls:
    return "tcdecls";
  case at_tyfn:
    return "tyfn";
  case at_tcapp:
    return "tcapp";
  case at_method_decls:
    return "method_decls";
  case at_method_decl:
    return "method_decl";
  case at_qualType:
    return "qualType";
  case at_constraints:
    return "constraints";
  case at_definstance:
    return "definstance";
  case at_tcmethods:
    return "tcmethods";
  case at_tcmethod_binding:
    return "tcmethod_binding";
  case at_proclaim:
    return "proclaim";
  case at_define:
    return "define";
  case at_recdef:
    return "recdef";
  case at_importAs:
    return "importAs";
  case at_provide:
    return "provide";
  case at_import:
    return "import";
  case at_ifsel:
    return "ifsel";
  case at_declares:
    return "declares";
  case at_declare:
    return "declare";
  case at_tvlist:
    return "tvlist";
  case at_constructors:
    return "constructors";
  case at_constructor:
    return "constructor";
  case at_fields:
    return "fields";
  case at_field:
    return "field";
  case at_fill:
    return "fill";
  case at_literalType:
    return "literalType";
  case at_bitfield:
    return "bitfield";
  case at_refType:
    return "refType";
  case at_byrefType:
    return "byrefType";
  case at_valType:
    return "valType";
  case at_fn:
    return "fn";
  case at_primaryType:
    return "primaryType";
  case at_fnargVec:
    return "fnargVec";
  case at_arrayType:
    return "arrayType";
  case at_vectorType:
    return "vectorType";
  case at_mutableType:
    return "mutableType";
  case at_constType:
    return "constType";
  case at_typeapp:
    return "typeapp";
  case at_exceptionType:
    return "exceptionType";
  case at_dummyType:
    return "dummyType";
  case at_identPattern:
    return "identPattern";
  case at_tqexpr:
    return "tqexpr";
  case at_unit:
    return "unit";
  case at_suspend:
    return "suspend";
  case at_sizeof:
    return "sizeof";
  case at_bitsizeof:
    return "bitsizeof";
  case at_makevectorL:
    return "makevectorL";
  case at_vector:
    return "vector";
  case at_array:
    return "array";
  case at_begin:
    return "begin";
  case at_select:
    return "select";
  case at_fqCtr:
    return "fqCtr";
  case at_sel_ctr:
    return "sel_ctr";
  case at_array_nth:
    return "array_nth";
  case at_vector_nth:
    return "vector_nth";
  case at_array_length:
    return "array_length";
  case at_vector_length:
    return "vector_length";
  case at_lambda:
    return "lambda";
  case at_argVec:
    return "argVec";
  case at_apply:
    return "apply";
  case at_struct_apply:
    return "struct_apply";
  case at_ucon_apply:
    return "ucon_apply";
  case at_if:
    return "if";
  case at_when:
    return "when";
  case at_and:
    return "and";
  case at_or:
    return "or";
  case at_not:
    return "not";
  case at_cond:
    return "cond";
  case at_cond_legs:
    return "cond_legs";
  case at_cond_leg:
    return "cond_leg";
  case at_condelse:
    return "condelse";
  case at_setbang:
    return "setbang";
  case at_deref:
    return "deref";
  case at_dup:
    return "dup";
  case at_inner_ref:
    return "inner_ref";
  case at_allocREF:
    return "allocREF";
  case at_copyREF:
    return "copyREF";
  case at_mkClosure:
    return "mkClosure";
  case at_setClosure:
    return "setClosure";
  case at_block:
    return "block";
  case at_return_from:
    return "return_from";
  case at_switch:
    return "switch";
  case at_sw_legs:
    return "sw_legs";
  case at_sw_leg:
    return "sw_leg";
  case at_otherwise:
    return "otherwise";
  case at_try:
    return "try";
  case at_throw:
    return "throw";
  case at_let:
    return "let";
  case at_letbindings:
    return "letbindings";
  case at_letbinding:
    return "letbinding";
  case at_letrec:
    return "letrec";
  case at_do:
    return "do";
  case at_dobindings:
    return "dobindings";
  case at_dobinding:
    return "dobinding";
  case at_dotest:
    return "dotest";
  case at_localFrame:
    return "localFrame";
  case at_frameBindings:
    return "frameBindings";
  case at_letStar:
    return "letStar";
  case at_identList:
    return "identList";
  case at_container:
    return "container";
  case at_docString:
    return "docString";
  case at_letGather:
    return "letGather";
  case agt_var:
    return "{var}";
  case agt_literal:
    return "{literal}";
  case agt_tvar:
    return "{tvar}";
  case agt_CompilationUnit:
    return "{CompilationUnit}";
  case agt_definition:
    return "{definition}";
  case agt_type_definition:
    return "{type_definition}";
  case agt_tc_definition:
    return "{tc_definition}";
  case agt_value_definition:
    return "{value_definition}";
  case agt_if_definition:
    return "{if_definition}";
  case agt_category:
    return "{category}";
  case agt_openclosed:
    return "{openclosed}";
  case agt_fielditem:
    return "{fielditem}";
  case agt_qtype:
    return "{qtype}";
  case agt_type:
    return "{type}";
  case agt_expr:
    return "{expr}";
  case agt_expr_or_define:
    return "{expr_or_define}";
  case agt_eform:
    return "{eform}";
  case agt_ucon:
    return "{ucon}";
  case agt_ow:
    return "{ow}";
  default:
    return "<unknown>";
  }
}

#define ISSET(v,b) ((v)[((b)/8)] & (1u << ((b)%8)))

void
astChTypeError(const AST &myAst, const AstType exp_at,
               const AstType act_at, size_t child)
{
  ::std::cerr << myAst.loc.asString() << ": " << myAst.astTypeName();
  ::std::cerr << " has incompatible Child# " << child;
  ::std::cerr << ". Expected " << AST::tagName(exp_at) << ", "; 
  ::std::cerr << "Obtained " << AST::tagName(act_at) << "." << ::std::endl;
}

void
astChNumError(const AST &myAst, const size_t exp_ch,
               const size_t act_ch)
{
  ::std::cerr << myAst.loc.asString() << ": " << myAst.astTypeName();
  ::std::cerr << " has wrong number of children. ";
  ::std::cerr << "Expected " << exp_ch << ", ";
  ::std::cerr << "Obtained " << act_ch << "." << ::std::endl;
}

static const unsigned char *astMembers[] = {
  (unsigned char *)"\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_Null
  (unsigned char *)"\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_AnyGroup
  (unsigned char *)"\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_ident
  (unsigned char *)"\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_ifident
  (unsigned char *)"\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_usesel
  (unsigned char *)"\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_boolLiteral
  (unsigned char *)"\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_charLiteral
  (unsigned char *)"\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_intLiteral
  (unsigned char *)"\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_floatLiteral
  (unsigned char *)"\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_stringLiteral
  (unsigned char *)"\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_module
  (unsigned char *)"\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_interface
  (unsigned char *)"\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_defunion
  (unsigned char *)"\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_declunion
  (unsigned char *)"\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_defstruct
  (unsigned char *)"\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_declstruct
  (unsigned char *)"\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_defrepr
  (unsigned char *)"\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_declrepr
  (unsigned char *)"\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_reprctrs
  (unsigned char *)"\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_reprctr
  (unsigned char *)"\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_reprrepr
  (unsigned char *)"\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_refCat
  (unsigned char *)"\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_valCat
  (unsigned char *)"\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_opaqueCat
  (unsigned char *)"\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_closed
  (unsigned char *)"\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_defexception
  (unsigned char *)"\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_deftypeclass
  (unsigned char *)"\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_tcdecls
  (unsigned char *)"\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_tyfn
  (unsigned char *)"\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_tcapp
  (unsigned char *)"\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_method_decls
  (unsigned char *)"\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_method_decl
  (unsigned char *)"\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_qualType
  (unsigned char *)"\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_constraints
  (unsigned char *)"\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_definstance
  (unsigned char *)"\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_tcmethods
  (unsigned char *)"\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_tcmethod_binding
  (unsigned char *)"\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_proclaim
  (unsigned char *)"\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_define
  (unsigned char *)"\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_recdef
  (unsigned char *)"\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_importAs
  (unsigned char *)"\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_provide
  (unsigned char *)"\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_import
  (unsigned char *)"\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_ifsel
  (unsigned char *)"\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_declares
  (unsigned char *)"\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_declare
  (unsigned char *)"\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_tvlist
  (unsigned char *)"\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_constructors
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_constructor
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_fields
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_field
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_fill
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_literalType
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_bitfield
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_refType
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_byrefType
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_valType
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_fn
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_primaryType
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_fnargVec
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_arrayType
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_vectorType
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_mutableType
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_constType
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_typeapp
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_exceptionType
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_dummyType
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_identPattern
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_tqexpr
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_unit
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_suspend
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_sizeof
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_bitsizeof
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_makevectorL
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_vector
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_array
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_begin
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_select
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_fqCtr
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00", // at_sel_ctr
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00", // at_array_nth
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00", // at_vector_nth
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00", // at_array_length
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00", // at_vector_length
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00", // at_lambda
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00", // at_argVec
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00", // at_apply
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00", // at_struct_apply
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00", // at_ucon_apply
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00", // at_if
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00", // at_when
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00", // at_and
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00", // at_or
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00", // at_not
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00", // at_cond
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00", // at_cond_legs
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00", // at_cond_leg
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00", // at_condelse
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00", // at_setbang
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00", // at_deref
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00", // at_dup
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00", // at_inner_ref
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00", // at_allocREF
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00", // at_copyREF
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00", // at_mkClosure
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00", // at_setClosure
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00", // at_block
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00", // at_return_from
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00", // at_switch
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00", // at_sw_legs
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00", // at_sw_leg
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00", // at_otherwise
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00", // at_try
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00", // at_throw
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00", // at_let
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00", // at_letbindings
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00", // at_letbinding
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00", // at_letrec
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00", // at_do
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00", // at_dobindings
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00", // at_dobinding
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00", // at_dotest
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00", // at_localFrame
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00", // at_frameBindings
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00", // at_letStar
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00", // at_identList
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00", // at_container
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00", // at_docString
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00", // at_letGather
  (unsigned char *)"\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00", // agt_var
  (unsigned char *)"\xe0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00", // agt_literal
  (unsigned char *)"\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00", // agt_tvar
  (unsigned char *)"\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00", // agt_CompilationUnit
  (unsigned char *)"\x00\xf0\x03\x06\xe4\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\x03\x00", // agt_definition
  (unsigned char *)"\x00\xf0\x03\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00", // agt_type_definition
  (unsigned char *)"\x00\x00\x00\x04\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00", // agt_tc_definition
  (unsigned char *)"\x00\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00", // agt_value_definition
  (unsigned char *)"\x00\x00\x00\x00\x20\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00", // agt_if_definition
  (unsigned char *)"\x00\x00\xe0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00", // agt_category
  (unsigned char *)"\x01\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00", // agt_openclosed
  (unsigned char *)"\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00", // agt_fielditem
  (unsigned char *)"\x14\x00\x00\x00\x01\x00\xe0\xf7\x07\x00\x00\x00\x00\x00\x00\x00\x0a\x60\x00", // agt_qtype
  (unsigned char *)"\x14\x00\x00\x00\x00\x00\xe0\xf7\x07\x00\x00\x00\x00\x00\x00\x00\x0a\x40\x00", // agt_type
  (unsigned char *)"\xf4\x03\x00\x00\x00\x00\x00\x00\xf0\xff\xdf\x7f\xfc\x1f\x67\x50\x06\x80\x02", // agt_expr
  (unsigned char *)"\xf4\x03\x00\x00\xc0\x00\x00\x00\xf0\xff\xdf\x7f\xfc\x1f\x67\x50\x06\x80\x03", // agt_expr_or_define
  (unsigned char *)"\xf4\x03\x00\x00\x00\x00\x00\x00\xe0\xff\xdf\x7f\xfc\x1f\x67\x50\x06\x00\x02", // agt_eform
  (unsigned char *)"\x14\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x00\x00\x00\x02\x00\x04", // agt_ucon
  (unsigned char *)"\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x08"  // agt_ow
};

bool
AST::isMemberOfType(AstType ty) const
{
  return ISSET(astMembers[ty], astType) ? true : false;}

bool
AST::isValid() const
{
  size_t c;
  size_t specNdx;
  bool errorsPresent = false;

  for (c = 0; c < children.size(); c++) {
    if (!child(c)->isValid())
      errorsPresent = true;
  }

  c = 0;
  specNdx = 0;

  switch(astType) {
  case at_Null: // leaf AST:
    if(children.size() != 0) {
      astChNumError(*this, 0, children.size());
      errorsPresent = true;
    }
    break;

  case at_AnyGroup: // leaf AST:
    if(children.size() != 0) {
      astChNumError(*this, 0, children.size());
      errorsPresent = true;
    }
    break;

  case at_ident: // leaf AST:
    if(children.size() != 0) {
      astChNumError(*this, 0, children.size());
      errorsPresent = true;
    }
    break;

  case at_ifident: // leaf AST:
    if(children.size() != 0) {
      astChNumError(*this, 0, children.size());
      errorsPresent = true;
    }
    break;

  case at_usesel: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_boolLiteral: // leaf AST:
    if(children.size() != 0) {
      astChNumError(*this, 0, children.size());
      errorsPresent = true;
    }
    break;

  case at_charLiteral: // leaf AST:
    if(children.size() != 0) {
      astChNumError(*this, 0, children.size());
      errorsPresent = true;
    }
    break;

  case at_intLiteral: // leaf AST:
    if(children.size() != 0) {
      astChNumError(*this, 0, children.size());
      errorsPresent = true;
    }
    break;

  case at_floatLiteral: // leaf AST:
    if(children.size() != 0) {
      astChNumError(*this, 0, children.size());
      errorsPresent = true;
    }
    break;

  case at_stringLiteral: // leaf AST:
    if(children.size() != 0) {
      astChNumError(*this, 0, children.size());
      errorsPresent = true;
    }
    break;

  case at_module: // normal AST:
    // match agt_definition*
    while (c < children.size()) {
      if (!ISSET(astMembers[agt_definition], child(c)->astType))
        astChTypeError(*this, agt_definition, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_interface: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_definition*
    while (c < children.size()) {
      if (!ISSET(astMembers[agt_definition], child(c)->astType))
        astChTypeError(*this, agt_definition, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_defunion: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_tvlist
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_tvlist], child(c)->astType)) {
      astChTypeError(*this, at_tvlist, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_category
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_category], child(c)->astType)) {
      astChTypeError(*this, agt_category, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_declares
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_declares], child(c)->astType)) {
      astChTypeError(*this, at_declares, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_constructors
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_constructors], child(c)->astType)) {
      astChTypeError(*this, at_constructors, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_constraints
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_constraints], child(c)->astType)) {
      astChTypeError(*this, at_constraints, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_declunion: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_tvlist
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_tvlist], child(c)->astType)) {
      astChTypeError(*this, at_tvlist, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_category
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_category], child(c)->astType)) {
      astChTypeError(*this, agt_category, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_constraints
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_constraints], child(c)->astType)) {
      astChTypeError(*this, at_constraints, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_defstruct: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_tvlist
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_tvlist], child(c)->astType)) {
      astChTypeError(*this, at_tvlist, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_category
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_category], child(c)->astType)) {
      astChTypeError(*this, agt_category, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_declares
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_declares], child(c)->astType)) {
      astChTypeError(*this, at_declares, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_fields
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_fields], child(c)->astType)) {
      astChTypeError(*this, at_fields, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_constraints
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_constraints], child(c)->astType)) {
      astChTypeError(*this, at_constraints, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_declstruct: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_tvlist
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_tvlist], child(c)->astType)) {
      astChTypeError(*this, at_tvlist, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_category
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_category], child(c)->astType)) {
      astChTypeError(*this, agt_category, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_constraints
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_constraints], child(c)->astType)) {
      astChTypeError(*this, at_constraints, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_defrepr: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_category
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_category], child(c)->astType)) {
      astChTypeError(*this, agt_category, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_declares
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_declares], child(c)->astType)) {
      astChTypeError(*this, at_declares, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_reprctrs
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_reprctrs], child(c)->astType)) {
      astChTypeError(*this, at_reprctrs, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_declrepr: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_category
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_category], child(c)->astType)) {
      astChTypeError(*this, agt_category, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_reprctrs: // normal AST:
    // match at_reprctr+
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_reprctr], child(c)->astType)) {
      astChTypeError(*this, at_reprctr, child(c)->astType, 0);
      errorsPresent = true;
    }
    while (c < children.size()) {
      if (!ISSET(astMembers[at_reprctr], child(c)->astType))
        astChTypeError(*this, at_reprctr, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_reprctr: // normal AST:
    // match at_constructor
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_constructor], child(c)->astType)) {
      astChTypeError(*this, at_constructor, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_reprrepr+
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_reprrepr], child(c)->astType)) {
      astChTypeError(*this, at_reprrepr, child(c)->astType, 1);
      errorsPresent = true;
    }
    while (c < children.size()) {
      if (!ISSET(astMembers[at_reprrepr], child(c)->astType))
        astChTypeError(*this, at_reprrepr, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_reprrepr: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_intLiteral
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_intLiteral], child(c)->astType)) {
      astChTypeError(*this, at_intLiteral, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_refCat: // leaf AST:
    if(children.size() != 0) {
      astChNumError(*this, 0, children.size());
      errorsPresent = true;
    }
    break;

  case at_valCat: // leaf AST:
    if(children.size() != 0) {
      astChNumError(*this, 0, children.size());
      errorsPresent = true;
    }
    break;

  case at_opaqueCat: // leaf AST:
    if(children.size() != 0) {
      astChNumError(*this, 0, children.size());
      errorsPresent = true;
    }
    break;

  case at_closed: // leaf AST:
    if(children.size() != 0) {
      astChNumError(*this, 0, children.size());
      errorsPresent = true;
    }
    break;

  case at_defexception: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_field*
    while (c < children.size()) {
      if (!ISSET(astMembers[at_field], child(c)->astType))
        astChTypeError(*this, at_field, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_deftypeclass: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_tvlist
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_tvlist], child(c)->astType)) {
      astChTypeError(*this, at_tvlist, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_tcdecls
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_tcdecls], child(c)->astType)) {
      astChTypeError(*this, at_tcdecls, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_openclosed
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_openclosed], child(c)->astType)) {
      astChTypeError(*this, agt_openclosed, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_method_decls
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_method_decls], child(c)->astType)) {
      astChTypeError(*this, at_method_decls, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_constraints
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_constraints], child(c)->astType)) {
      astChTypeError(*this, at_constraints, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_tcdecls: // normal AST:
    // match at_tyfn*
    while (c < children.size()) {
      if (!ISSET(astMembers[at_tyfn], child(c)->astType))
        astChTypeError(*this, at_tyfn, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_tyfn: // normal AST:
    // match at_fnargVec
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_fnargVec], child(c)->astType)) {
      astChTypeError(*this, at_fnargVec, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_tvar
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_tvar], child(c)->astType)) {
      astChTypeError(*this, agt_tvar, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_tcapp: // normal AST:
    // match agt_var
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_var], child(c)->astType)) {
      astChTypeError(*this, agt_var, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_type+
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_type], child(c)->astType)) {
      astChTypeError(*this, agt_type, child(c)->astType, 1);
      errorsPresent = true;
    }
    while (c < children.size()) {
      if (!ISSET(astMembers[agt_type], child(c)->astType))
        astChTypeError(*this, agt_type, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_method_decls: // normal AST:
    // match at_method_decl*
    while (c < children.size()) {
      if (!ISSET(astMembers[at_method_decl], child(c)->astType))
        astChTypeError(*this, at_method_decl, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_method_decl: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_fn
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_fn], child(c)->astType)) {
      astChTypeError(*this, at_fn, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_qualType: // normal AST:
    // match at_constraints
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_constraints], child(c)->astType)) {
      astChTypeError(*this, at_constraints, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_type
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_type], child(c)->astType)) {
      astChTypeError(*this, agt_type, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_constraints: // normal AST:
    // match at_tcapp*
    while (c < children.size()) {
      if (!ISSET(astMembers[at_tcapp], child(c)->astType))
        astChTypeError(*this, at_tcapp, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_definstance: // normal AST:
    // match at_tcapp
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_tcapp], child(c)->astType)) {
      astChTypeError(*this, at_tcapp, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_tcmethods
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_tcmethods], child(c)->astType)) {
      astChTypeError(*this, at_tcmethods, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_constraints
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_constraints], child(c)->astType)) {
      astChTypeError(*this, at_constraints, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_tcmethods: // normal AST:
    // match at_tcmethod_binding*
    while (c < children.size()) {
      if (!ISSET(astMembers[at_tcmethod_binding], child(c)->astType))
        astChTypeError(*this, at_tcmethod_binding, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_tcmethod_binding: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_proclaim: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_qtype
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_qtype], child(c)->astType)) {
      astChTypeError(*this, agt_qtype, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_constraints
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_constraints], child(c)->astType)) {
      astChTypeError(*this, at_constraints, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_define: // normal AST:
    // match at_identPattern
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_identPattern], child(c)->astType)) {
      astChTypeError(*this, at_identPattern, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_constraints
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_constraints], child(c)->astType)) {
      astChTypeError(*this, at_constraints, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_recdef: // normal AST:
    // match at_identPattern
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_identPattern], child(c)->astType)) {
      astChTypeError(*this, at_identPattern, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_constraints
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_constraints], child(c)->astType)) {
      astChTypeError(*this, at_constraints, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_importAs: // normal AST:
    // match at_ifident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ifident], child(c)->astType)) {
      astChTypeError(*this, at_ifident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_provide: // normal AST:
    // match at_ifident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ifident], child(c)->astType)) {
      astChTypeError(*this, at_ifident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_ident+
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, 1);
      errorsPresent = true;
    }
    while (c < children.size()) {
      if (!ISSET(astMembers[at_ident], child(c)->astType))
        astChTypeError(*this, at_ident, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_import: // normal AST:
    // match at_ifident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ifident], child(c)->astType)) {
      astChTypeError(*this, at_ifident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_ifsel*
    while (c < children.size()) {
      if (!ISSET(astMembers[at_ifsel], child(c)->astType))
        astChTypeError(*this, at_ifsel, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_ifsel: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_declares: // normal AST:
    // match at_declare*
    while (c < children.size()) {
      if (!ISSET(astMembers[at_declare], child(c)->astType))
        astChTypeError(*this, at_declare, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_declare: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_type?
    if ((c < children.size()) && ISSET(astMembers[agt_type], child(c)->astType))
      c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_tvlist: // normal AST:
    // match agt_tvar*
    while (c < children.size()) {
      if (!ISSET(astMembers[agt_tvar], child(c)->astType))
        astChTypeError(*this, agt_tvar, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_constructors: // normal AST:
    // match at_constructor+
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_constructor], child(c)->astType)) {
      astChTypeError(*this, at_constructor, child(c)->astType, 0);
      errorsPresent = true;
    }
    while (c < children.size()) {
      if (!ISSET(astMembers[at_constructor], child(c)->astType))
        astChTypeError(*this, at_constructor, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_constructor: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_fielditem*
    while (c < children.size()) {
      if (!ISSET(astMembers[agt_fielditem], child(c)->astType))
        astChTypeError(*this, agt_fielditem, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_fields: // normal AST:
    // match agt_fielditem*
    while (c < children.size()) {
      if (!ISSET(astMembers[agt_fielditem], child(c)->astType))
        astChTypeError(*this, agt_fielditem, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_field: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_type
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_type], child(c)->astType)) {
      astChTypeError(*this, agt_type, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_fill: // normal AST:
    // match agt_type
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_type], child(c)->astType)) {
      astChTypeError(*this, agt_type, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_intLiteral?
    if ((c < children.size()) && ISSET(astMembers[at_intLiteral], child(c)->astType))
      c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_literalType: // normal AST:
    // match agt_literal
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_literal], child(c)->astType)) {
      astChTypeError(*this, agt_literal, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_primaryType?
    if ((c < children.size()) && ISSET(astMembers[at_primaryType], child(c)->astType))
      c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_bitfield: // normal AST:
    // match at_primaryType
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_primaryType], child(c)->astType)) {
      astChTypeError(*this, at_primaryType, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_intLiteral
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_intLiteral], child(c)->astType)) {
      astChTypeError(*this, at_intLiteral, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_refType: // normal AST:
    // match agt_type
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_type], child(c)->astType)) {
      astChTypeError(*this, agt_type, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_byrefType: // normal AST:
    // match agt_type
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_type], child(c)->astType)) {
      astChTypeError(*this, agt_type, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_valType: // normal AST:
    // match agt_type
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_type], child(c)->astType)) {
      astChTypeError(*this, agt_type, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_fn: // normal AST:
    // match at_fnargVec
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_fnargVec], child(c)->astType)) {
      astChTypeError(*this, at_fnargVec, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_type
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_type], child(c)->astType)) {
      astChTypeError(*this, agt_type, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_primaryType: // leaf AST:
    if(children.size() != 0) {
      astChNumError(*this, 0, children.size());
      errorsPresent = true;
    }
    break;

  case at_fnargVec: // normal AST:
    // match agt_type*
    while (c < children.size()) {
      if (!ISSET(astMembers[agt_type], child(c)->astType))
        astChTypeError(*this, agt_type, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_arrayType: // normal AST:
    // match agt_type
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_type], child(c)->astType)) {
      astChTypeError(*this, agt_type, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_intLiteral
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_intLiteral], child(c)->astType)) {
      astChTypeError(*this, at_intLiteral, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_vectorType: // normal AST:
    // match agt_type
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_type], child(c)->astType)) {
      astChTypeError(*this, agt_type, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_intLiteral?
    if ((c < children.size()) && ISSET(astMembers[at_intLiteral], child(c)->astType))
      c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_mutableType: // normal AST:
    // match agt_type
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_type], child(c)->astType)) {
      astChTypeError(*this, agt_type, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_constType: // normal AST:
    // match agt_type
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_type], child(c)->astType)) {
      astChTypeError(*this, agt_type, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_typeapp: // normal AST:
    // match agt_var
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_var], child(c)->astType)) {
      astChTypeError(*this, agt_var, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_type+
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_type], child(c)->astType)) {
      astChTypeError(*this, agt_type, child(c)->astType, 1);
      errorsPresent = true;
    }
    while (c < children.size()) {
      if (!ISSET(astMembers[agt_type], child(c)->astType))
        astChTypeError(*this, agt_type, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_exceptionType: // leaf AST:
    if(children.size() != 0) {
      astChNumError(*this, 0, children.size());
      errorsPresent = true;
    }
    break;

  case at_dummyType: // leaf AST:
    if(children.size() != 0) {
      astChNumError(*this, 0, children.size());
      errorsPresent = true;
    }
    break;

  case at_identPattern: // normal AST:
    // match agt_var
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_var], child(c)->astType)) {
      astChTypeError(*this, agt_var, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_qtype?
    if ((c < children.size()) && ISSET(astMembers[agt_qtype], child(c)->astType))
      c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_tqexpr: // normal AST:
    // match agt_eform
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_eform], child(c)->astType)) {
      astChTypeError(*this, agt_eform, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_type
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_type], child(c)->astType)) {
      astChTypeError(*this, agt_type, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_unit: // leaf AST:
    if(children.size() != 0) {
      astChNumError(*this, 0, children.size());
      errorsPresent = true;
    }
    break;

  case at_suspend: // normal AST:
    // match agt_var
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_var], child(c)->astType)) {
      astChTypeError(*this, agt_var, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_sizeof: // normal AST:
    // match agt_type
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_type], child(c)->astType)) {
      astChTypeError(*this, agt_type, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_bitsizeof: // normal AST:
    // match agt_type
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_type], child(c)->astType)) {
      astChTypeError(*this, agt_type, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_makevectorL: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_vector: // normal AST:
    // match agt_expr+
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, 0);
      errorsPresent = true;
    }
    while (c < children.size()) {
      if (!ISSET(astMembers[agt_expr], child(c)->astType))
        astChTypeError(*this, agt_expr, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_array: // normal AST:
    // match agt_expr+
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, 0);
      errorsPresent = true;
    }
    while (c < children.size()) {
      if (!ISSET(astMembers[agt_expr], child(c)->astType))
        astChTypeError(*this, agt_expr, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_begin: // normal AST:
    // match agt_expr_or_define+
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr_or_define], child(c)->astType)) {
      astChTypeError(*this, agt_expr_or_define, child(c)->astType, 0);
      errorsPresent = true;
    }
    while (c < children.size()) {
      if (!ISSET(astMembers[agt_expr_or_define], child(c)->astType))
        astChTypeError(*this, agt_expr_or_define, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_select: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_fqCtr: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_sel_ctr: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_array_nth: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_vector_nth: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_array_length: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_vector_length: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_lambda: // normal AST:
    // match at_argVec
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_argVec], child(c)->astType)) {
      astChTypeError(*this, at_argVec, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_argVec: // normal AST:
    // match at_identPattern*
    while (c < children.size()) {
      if (!ISSET(astMembers[at_identPattern], child(c)->astType))
        astChTypeError(*this, at_identPattern, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_apply: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr*
    while (c < children.size()) {
      if (!ISSET(astMembers[agt_expr], child(c)->astType))
        astChTypeError(*this, agt_expr, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_struct_apply: // normal AST:
    // match agt_var
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_var], child(c)->astType)) {
      astChTypeError(*this, agt_var, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr+
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, 1);
      errorsPresent = true;
    }
    while (c < children.size()) {
      if (!ISSET(astMembers[agt_expr], child(c)->astType))
        astChTypeError(*this, agt_expr, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_ucon_apply: // normal AST:
    // match agt_ucon
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_ucon], child(c)->astType)) {
      astChTypeError(*this, agt_ucon, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr+
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, 1);
      errorsPresent = true;
    }
    while (c < children.size()) {
      if (!ISSET(astMembers[agt_expr], child(c)->astType))
        astChTypeError(*this, agt_expr, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_if: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_when: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_and: // normal AST:
    // match agt_expr+
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, 0);
      errorsPresent = true;
    }
    while (c < children.size()) {
      if (!ISSET(astMembers[agt_expr], child(c)->astType))
        astChTypeError(*this, agt_expr, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_or: // normal AST:
    // match agt_expr+
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, 0);
      errorsPresent = true;
    }
    while (c < children.size()) {
      if (!ISSET(astMembers[agt_expr], child(c)->astType))
        astChTypeError(*this, agt_expr, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_not: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_cond: // normal AST:
    // match at_cond_legs
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_cond_legs], child(c)->astType)) {
      astChTypeError(*this, at_cond_legs, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_condelse
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_condelse], child(c)->astType)) {
      astChTypeError(*this, at_condelse, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_cond_legs: // normal AST:
    // match at_cond_leg*
    while (c < children.size()) {
      if (!ISSET(astMembers[at_cond_leg], child(c)->astType))
        astChTypeError(*this, at_cond_leg, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_cond_leg: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_condelse: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_setbang: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_deref: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_dup: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_inner_ref: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_allocREF: // normal AST:
    // match agt_type
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_type], child(c)->astType)) {
      astChTypeError(*this, agt_type, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_copyREF: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_mkClosure: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_setClosure: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr+
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, 2);
      errorsPresent = true;
    }
    while (c < children.size()) {
      if (!ISSET(astMembers[agt_expr], child(c)->astType))
        astChTypeError(*this, agt_expr, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_block: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_return_from: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_switch: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_sw_legs
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_sw_legs], child(c)->astType)) {
      astChTypeError(*this, at_sw_legs, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_ow
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_ow], child(c)->astType)) {
      astChTypeError(*this, agt_ow, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_sw_legs: // normal AST:
    // match at_sw_leg*
    while (c < children.size()) {
      if (!ISSET(astMembers[at_sw_leg], child(c)->astType))
        astChTypeError(*this, at_sw_leg, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_sw_leg: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_ucon+
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_ucon], child(c)->astType)) {
      astChTypeError(*this, agt_ucon, child(c)->astType, 2);
      errorsPresent = true;
    }
    while (c < children.size()) {
      if (!ISSET(astMembers[agt_ucon], child(c)->astType))
        astChTypeError(*this, agt_ucon, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_otherwise: // normal AST:
    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_try: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_ident
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_ident], child(c)->astType)) {
      astChTypeError(*this, at_ident, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_sw_legs
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_sw_legs], child(c)->astType)) {
      astChTypeError(*this, at_sw_legs, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_ow
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_ow], child(c)->astType)) {
      astChTypeError(*this, agt_ow, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_throw: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_let: // normal AST:
    // match at_letbindings
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_letbindings], child(c)->astType)) {
      astChTypeError(*this, at_letbindings, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_constraints
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_constraints], child(c)->astType)) {
      astChTypeError(*this, at_constraints, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_letbindings: // normal AST:
    // match at_letbinding+
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_letbinding], child(c)->astType)) {
      astChTypeError(*this, at_letbinding, child(c)->astType, 0);
      errorsPresent = true;
    }
    while (c < children.size()) {
      if (!ISSET(astMembers[at_letbinding], child(c)->astType))
        astChTypeError(*this, at_letbinding, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_letbinding: // normal AST:
    // match at_identPattern
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_identPattern], child(c)->astType)) {
      astChTypeError(*this, at_identPattern, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_letrec: // normal AST:
    // match at_letbindings
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_letbindings], child(c)->astType)) {
      astChTypeError(*this, at_letbindings, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_constraints
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_constraints], child(c)->astType)) {
      astChTypeError(*this, at_constraints, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_do: // normal AST:
    // match at_dobindings
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_dobindings], child(c)->astType)) {
      astChTypeError(*this, at_dobindings, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match at_dotest
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_dotest], child(c)->astType)) {
      astChTypeError(*this, at_dotest, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_dobindings: // normal AST:
    // match at_dobinding*
    while (c < children.size()) {
      if (!ISSET(astMembers[at_dobinding], child(c)->astType))
        astChTypeError(*this, at_dobinding, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_dobinding: // normal AST:
    // match at_identPattern
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_identPattern], child(c)->astType)) {
      astChTypeError(*this, at_identPattern, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_dotest: // normal AST:
    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_localFrame: // normal AST:
    // match at_frameBindings
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_frameBindings], child(c)->astType)) {
      astChTypeError(*this, at_frameBindings, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_frameBindings: // normal AST:
    // match at_identPattern*
    while (c < children.size()) {
      if (!ISSET(astMembers[at_identPattern], child(c)->astType))
        astChTypeError(*this, at_identPattern, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_letStar: // normal AST:
    // match at_letbindings
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_letbindings], child(c)->astType)) {
      astChTypeError(*this, at_letbindings, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_identList: // normal AST:
    // match at_ident*
    while (c < children.size()) {
      if (!ISSET(astMembers[at_ident], child(c)->astType))
        astChTypeError(*this, at_ident, child(c)->astType, c);
      c++;
    }

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_container: // normal AST:
    // match at_identList
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[at_identList], child(c)->astType)) {
      astChTypeError(*this, at_identList, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    // match agt_expr
    if(c >= children.size()) {
      astChNumError(*this, c+1, children.size());
      errorsPresent = true;
      break;
    }
    if (!ISSET(astMembers[agt_expr], child(c)->astType)) {
      astChTypeError(*this, agt_expr, child(c)->astType, c);
      errorsPresent = true;
    }
    c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_docString: // normal AST:
    // match at_stringLiteral?
    if ((c < children.size()) && ISSET(astMembers[at_stringLiteral], child(c)->astType))
      c++;

    if(c != children.size()) {
      astChNumError(*this, c, children.size());
      errorsPresent = true;
    }
    break;

  case at_letGather: // leaf AST:
    if(children.size() != 0) {
      astChNumError(*this, 0, children.size());
      errorsPresent = true;
    }
    break;

  // group ASTagt_var gets default
    break;

  // group ASTagt_literal gets default
    break;

  // group ASTagt_tvar gets default
    break;

  // group ASTagt_CompilationUnit gets default
    break;

  // group ASTagt_definition gets default
    break;

  // group ASTagt_type_definition gets default
    break;

  // group ASTagt_tc_definition gets default
    break;

  // group ASTagt_value_definition gets default
    break;

  // group ASTagt_if_definition gets default
    break;

  // group ASTagt_category gets default
    break;

  // group ASTagt_openclosed gets default
    break;

  // group ASTagt_fielditem gets default
    break;

  // group ASTagt_qtype gets default
    break;

  // group ASTagt_type gets default
    break;

  // group ASTagt_expr gets default
    break;

  // group ASTagt_expr_or_define gets default
    break;

  // group ASTagt_eform gets default
    break;

  // group ASTagt_ucon gets default
    break;

  // group ASTagt_ow gets default
    break;

  default:
    errorsPresent = true;
  }

  return !errorsPresent;
}


