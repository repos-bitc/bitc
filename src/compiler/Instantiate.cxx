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

#include <stdint.h>
#include <stdlib.h>
#include <dirent.h>
#include <errno.h>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>
#include <map>

#include "Options.hxx"
#include "UocInfo.hxx"
#include "AST.hxx"
#include "Type.hxx"
#include "inter-pass.hxx"
#include "Instantiate.hxx"

using namespace std;
using namespace boost;
using namespace sherpa;

#if #INST_DEBUG == #DEBUG_ON
#define STRICTLYEQUALS(x) strictlyEqualsA(x, DEBUG_VERBOSE)
#else
#define STRICTLYEQUALS(x) strictlyEqualsA(x)
#endif

typedef map<shared_ptr<AST>, shared_ptr<AST> > AstMap;

/*******************************************************************
                        ENVIRONMENT HANDLING
*******************************************************************/

/*
   Even though the unified-AST can be formed on demand, R&Ting the new
   definitions must be done in a fuly built environment --
   the mega-environment.

   We need the mega-environment because there is -- in general --
   no single environment where we can perform this R&T. For example,
   consider:

   (interface IF1
   (proclaim UID:(fn ('a) 'a))
   )

   (module SM1
   (provide if1 IF1)
   (defunion (un 'a) (Ctr c:'a))
   (define (UID x) (Ctr x) x)
   )

   (module SM2
   (defstruct St st:bool)
   (define p (UID (St))
   )

   => Now, if we want to instantiate p:

   1) We start with SM2's environment, need to instantiate p. So, we copy
   the definition of p as:
   (define p#St (UID (St))), and type it in SM2's environment,
   everything is OK.

   2) Now, we need to recurse over this definition, we encounter UID,
   get its definition, see that it is a proclaimation, and get its
   real definition. Then, we make a copy of this definition:

   (define UID#fn_St_St:(fn (St) St) (lambda (x) (Ctr x) x))

   Now, we must type it. Which environment should we type it in?

   Not in SM1 because we don't know `St' there, and not in SM2 because
   we don't  know `un' there. Not in the big-AST environment as there is
   nothing relevant there.

   There is no single environment which has enough information to type
   this new definition.

   Moreover, this fully built environment must also contain the output
   definitions produced by the polyinstantiator because
   further instantiations may need this definition. When we are
   recursing over the instantiated form, looking for further
   specialization, some types or functions will be specialized over
   the types that are newly instantiated. For example:
   (defstruct st1 a:int32 b:(list bool))

   (defstruct (st2 'a 'b) a:'a b:'b)
   (define (main.main argVec:(vector string))
   (cons
   (st2 (st2 #t (cons 0:int32 nil))
   (st1 0:int32 (cons #t nil)))
   nil)
   0:int32)

   In order to solve both of these problems, we build the *output* UOC
   (also called target-uoc or unified-Uoc) in
   the following form:

   Unified-UOC
   -----------
   |         |    ------------------  parent   ---------------
   | env   --|--->| unified-env    |==========>|  mega-env   |
   |         |    ------------------           ---------------
   | gamma --|--->|unified-gamma   |==========>|  mega-gamma |
   |         |    ------------------           ---------------
   |instEnv--|--->|unified-instEnv |==========>|  mega-gamma |
   |         |    ------------------           ---------------
   -----------
*/

/*
  The main environment(s) which the Unified-UOC thinks it has, is the
  UOC where the output of the polyinstantiator resides. By prior
  arrangement (see CreateUnifiedUOC) we will build wrappers around the
  main environments, which will hold the mega-environments. In this
  way, when the resolver/type-checker looks at the environment, it can
  find ALL definitions (from all input and output).

  Since the instantiator's output is mutually exclusive from the
  input, there will be no collisions. Also, the wrappers MUST NEVER be
  discarded if we want to support incremental instantiation /
  interpretation. For this reason, all further passes must call R&T
  with reinit-flag turned on.

  In the future comments, will will refer to the megaENVs and
  unifiedUOC in separate terms sometimes, but it should be understood
  that they are both reachable from the unifiedUOC and that any such
  separation is only a matter of emphasis. */


/* We have previously established that we need to build a
   mega-Env from all UOCs. How do we want to build it? Of
   course, for the first time, we  must build it by iterating over all
   the environments of all interfaces  and source modules and build a new
   environment based on their FQNs. However, we do not want to do it
   every time we enter the instantiator.  Since here is that the
   interface-list and the source-module list are append-only -- they are
   append-only in the case of an interactive interpreter; in the case of
   static compiler, these lists are frozen -- we can note the previously
   last processed indices into these lists, and  only processes additions
   each time.

   In the case of a compiler, interfaces and source module environments are
   immutable once the entire module is processed. Therefore, the index into
   the module list will suffice. However, in the case of an interactive
   interpreter, there is exactly one unit of compilation that is ever
   expanding, and the instantiator must cope with this. There are two ways
   in which we can think about the interactive loop.

   i) Some units of compilation are "dynamic" and the instantiator must
   process them every-time (as an optimization, we can remember their last
   processed environment index, and process only the new ones).

   ii) The interactive loop is not one unit of compilation, but it is an
   arrangement of nested interfaces, each of which is by itself frozen.
   That is, _every_ new definition has its own interface, and will import
   the interface introduced in the previous step, and use all the forms
   in it.

   Option (i) is probably better from an implementation standpoint.

   In the current implementation, we have two falgs on every UOC:
   UOC_IS_MUTABLE: Definitions in this UOC must be examined at every
   step.
   UOC_SEEN_BY_INST: UOC has been processed already.

   We only need to reprocess the UOC if we have never seen it before, or
   it is mutable. */


static void
importSymBindings(shared_ptr<ASTEnvironment > fromEnv,
		  shared_ptr<ASTEnvironment > toEnv)
{
  for (ASTEnvironment::iterator itr = fromEnv->begin();
      itr != fromEnv->end(); ++ itr) {
    shared_ptr<Binding<AST> > bdng = itr->second;

    if ((bdng->flags & BF_PRIVATE) == 0) {
      std::string nm = bdng->val->fqn.asString();
      toEnv->addBinding(nm, bdng->val);
      toEnv->setFlags(nm, BF_REBIND | BF_COMPLETE);

      INST_ENV_DEBUG
	cerr << "Added to env: "
	     << bdng->val->fqn.asString()
	     << endl;
    }
  }
}

static void
importTSBindings(shared_ptr<TSEnvironment > fromEnv,
		 shared_ptr<TSEnvironment > toEnv)
{
  for (TSEnvironment::iterator itr = fromEnv->begin();
      itr != fromEnv->end(); ++ itr) {
    shared_ptr<Binding<TypeScheme> > bdng = itr->second;

    if ((bdng->flags & BF_PRIVATE) == 0) {
      std::string nm = bdng->val->ast->fqn.asString();
      toEnv->addBinding(nm, bdng->val);
      toEnv->setFlags(nm, BF_REBIND | BF_COMPLETE);

      INST_ENV_DEBUG
	cerr << "Added to Gamma: "
	     << bdng->val->ast->fqn.asString()
	     << " with type "
	     << bdng->val->asString()
	     << endl;
    }
  }
}

static void
importInstBindings(shared_ptr<InstEnvironment > fromEnv,
		   shared_ptr<InstEnvironment > toEnv)
{
  for (InstEnvironment::iterator itr = fromEnv->begin();
      itr != fromEnv->end(); ++ itr) {
    shared_ptr<Binding<set<shared_ptr<Instance> > > > bdng =
      itr->second;

    if (bdng->flags & BF_PRIVATE)
      continue;

    shared_ptr<set<shared_ptr<Instance> > > fromInsts = bdng->val;
    if (fromInsts->size() == 0)
      continue;

    // We need the FQN of the typeclass. Choose an arbitrary member of
    // the instance set, reach into that instance's definition's AST,
    // the get the AST of the Typeclass being defined and then its
    // FQN.
    set<shared_ptr<Instance> >::iterator itr = fromInsts->begin();

    shared_ptr<AST> instAST = (*itr)->ast;
    shared_ptr<AST> tcAST = instAST->child(0)->child(0)->symbolDef;
    string tcFQN = tcAST->fqn.asString();

    shared_ptr<set<shared_ptr<Instance> > > toInsts = toEnv->getBinding(tcFQN);
    if (toInsts) {
      cerr << "Available non-private instance for "
	   << tcFQN << ": ";
      for (set<shared_ptr<Instance> >::iterator itr_j = toInsts->begin();
	   itr_j != toInsts->end(); ++itr_j)
	cerr << (*itr_j)->asString() << "     ";
      cerr << endl;
      assert(false);
    }

    INST_ENV_DEBUG
      cerr << "Adding Instance for " << tcFQN << endl;
    toEnv->addBinding(tcFQN, fromInsts);
  }
}

void
UpdateMegaEnvs(shared_ptr<UocInfo> uoc)
{
  // The input uoc is the unifiedUOC, So, get the megaENVs
  // from the current uoc by fetching the parents of the
  // current envs.
  shared_ptr<ASTEnvironment > megaEnv = uoc->env->parent;
  shared_ptr<TSEnvironment > megaGamma = uoc->gamma->parent;
  shared_ptr<InstEnvironment > megaInstEnv =
    uoc->instEnv->parent;

  INST_ENV_DEBUG
    cerr << "#envs = " << megaEnv->size()
	 << endl
	 << "#tss = " << megaGamma->size()
	 << endl
	 << "#Instances = " << megaInstEnv->size()
	 << endl;

  for (UocMap::iterator itr = UocInfo::ifList.begin();
      itr != UocInfo::ifList.end(); ++itr) {
    shared_ptr<UocInfo> puoci = itr->second;
    if ((puoci->flags & UOC_IS_MUTABLE) ||
       ((puoci->flags & UOC_SEEN_BY_INST) == 0)) {
      INST_ENV_DEBUG
	cerr << "Importing Symbols from Interface: "
	     << puoci->uocName << "."
	     << endl;

      importSymBindings(puoci->env, megaEnv);
      importTSBindings(puoci->gamma, megaGamma);
      importInstBindings(puoci->instEnv, megaInstEnv);
      puoci->flags |= UOC_SEEN_BY_INST;
    }
  }

  for (UocMap::iterator itr = UocInfo::srcList.begin();
      itr != UocInfo::srcList.end(); ++itr) {
    shared_ptr<UocInfo> puoci = itr->second;
    if ((puoci->flags & UOC_IS_MUTABLE) ||
       ((puoci->flags & UOC_SEEN_BY_INST) == 0)) {
      INST_ENV_DEBUG
	cerr << "Importing Symbols from Module: "
	     << puoci->uocName << "."
	     << endl;

      importSymBindings(puoci->env, megaEnv);
      importTSBindings(puoci->gamma, megaGamma);
      importInstBindings(puoci->instEnv, megaInstEnv);
      puoci->flags |= UOC_SEEN_BY_INST;
    }
  }
}


/*******************************************************************
                        NAME HANDLING
*******************************************************************/

shared_ptr<AST>
UocInfo::lookupByFqn(const FQName& fqn, shared_ptr<UocInfo> &targetUoc)
{
  targetUoc = GC_NULL;


  // Search all Interfaces and source modules serially for the AST
  // that is the definition of the FQN requested. If a definition is
  // found, we always try to return the definition. This is not just a
  // convenience, it is necessary for the instantiator. This is
  // because, we introduce the type qualifications for all
  // instantiated bindings, and if the definition and the declaration
  // differ in top level mutability at fn-arg-ret, if we start from
  // the type of the declaration, we might end up in a type
  // qualification that can never be satisfied.

  // Search all interfaces
  {
    UocMap::iterator itr = UocInfo::ifList.find(fqn.iface);

    if (itr != UocInfo::ifList.end()) {
      shared_ptr<UocInfo> uoc = itr->second;

      targetUoc = uoc;
      shared_ptr<AST> def = uoc->env->getBinding(fqn.ident);
      assert(def->fqn == fqn);

      // If this is a declaration, then try to get the definition if
      // one exists, and return that.
      if (def->defn)
	def = def->defn;

      return def->defForm;
    }
  }

  // Next all source modules
  for (UocMap::iterator itr = UocInfo::srcList.begin();
      itr != UocInfo::srcList.end(); ++itr) {
    shared_ptr<UocInfo> uoc = itr->second;

    if (uoc->uocName != fqn.iface)
      continue;

    targetUoc = uoc;
    shared_ptr<AST> def = uoc->env->getBinding(fqn.ident);
      assert(def->fqn == fqn);

    if (def->defn)
      def = def->defn;

    return def->defForm;
  }

  return GC_NULL;
}

// Get the Instantiated name for the polyinstantiated AST in the
// new UOC. Ordinarily,
// newName = _ + FQN.size() + FQN + # + typ->mangledString()
//
// However, Union constructors and vales are given type
// based on the full union
//
// We must play a little trick on the mangled string, because we
// always instantiate value-composite types to maximally mutable type.
// IT IS NOT ENOUGH only to add a type-qualifier on the LHS with the
// right mutability of the type, because the RHS being copy-compatible
// will get instantiated to a different type ...

static string
getInstName(shared_ptr<const AST> def, shared_ptr<Type> typ)
{
  stringstream ss;
  shared_ptr<Type> uAdjTyp = typ;

  if (typ->isUcon() || typ->isUval())
    uAdjTyp = typ->getUnionType();

  string fqnString = def->fqn.asString();
  ss << "_" << fqnString.size() << fqnString;
  ss << "#" << uAdjTyp->mangledString();

  INST_DEBUG
    cerr << "New Name: " << ss.str()
	 << " for " << def->s << " with type "
	 << typ->asString()
	 << endl;

  return ss.str();
}

// Namakaranam -- the ritual of giving a name
#define NAMKARAN(ast, name) do {		\
    ast->s = name;				\
    ast->flags |= IDENT_MANGLED;		\
  } while (0);

// Rename an AST with its instantiated name
void
InstMangle(shared_ptr<AST> def)
{
  NAMKARAN(def, getInstName(def, def->symType));
}

// In the case let-bindings, names (fqns) are not unique
// So, in order to maintain a worklist, we form a
// unique name based on the uniqueID of the AST, and
// the mangled_string of the obtained by getInstName()
static string
uniqName(const string name, const shared_ptr<AST> def)
{
  stringstream ss;
  ss << name;
  ss << def->ID;
  return ss.str();
}

// Fix the usage of any global definition to instead refer to its FQN
// because that is what is in megaENVs
static void
name2fqn(shared_ptr<AST> ast)
{
  switch(ast->astType) {
  case at_ident:
    {
      if (ast->flags & IDENT_MANGLED)
	break;

      if (!ast->symbolDef)
	break;

      if (!ast->symbolDef->isGlobal())
	break;

      ast->s = ast->symbolDef->fqn.asString();
      break;
    }

  case at_field:
  case at_methdecl:
    {
      name2fqn(ast->child(1));
      break;
    }

  case at_fqCtr:
  case at_sel_ctr:
  case at_select:
    {
      name2fqn(ast->child(0));

      // There is *NO* name2fqn of the field in the case of
      // at_sel_crt or at_fqCtr
      // This is because, because the original AST contains
      // unqualified names within type-records.
      // We only added fqns to the mega environment, with pointers to
      // the old unchanged ASTs.
      break;
    }

  case at_declares:
    {
      break;
    }

  case at_module:
    {
      assert(false);
      break;
    }

    // Special handling for switch unnecessary, if symbolDef is null
    // recursion stops.
  default:
    {
      for (size_t c = 0; c < ast->children.size(); c++)
	name2fqn(ast->child(c));
      break;
    }
  }
}

/*******************************************************************
                  MORE HELPER FUNCTIONS AND MACROS
*******************************************************************/

#define UNIFIED_ENVS GC_NULL

// Warning: The following macros need local errStream
#define RANDT_DROP(expr, mess, env) do {		\
    assert(RandTexpr(errStream, expr,			\
		     POLY_SYM_FLAGS, POLY_TYP_FLAGS,	\
		     mess, false, env));		\
  }while (0);

#define RANDT_COMMIT(expr, mess, env) do {		\
    assert(RandTexpr(errStream, expr,			\
		     POLY_SYM_FLAGS, POLY_TYP_FLAGS,	\
		     mess, true, env));			\
  }while (0);


// Many a time, we will have to explicitely write (or re-write)
// a type-qualification. This is a helper function to do that.
// It gets type in AST form, and prepares it in the current context.
static shared_ptr<AST>
typeAsAst(shared_ptr<Type> t, const LexLoc& loc)
{
  shared_ptr<AST> tAst = t->asAST(loc);
  name2fqn(tAst);
  return tAst;
}

// No constraints should survive after Polyinstantiation

static void
clearConstraints(shared_ptr<AST> ast)
{
  switch(ast->astType) {
  case at_define:
  case at_recdef:
  case at_proclaim:
    assert(ast->child(2)->astType == at_constraints);
    ast->child(2)->children.clear();
    break;

  case at_declstruct:
  case at_declunion:
    assert(ast->child(3)->astType == at_constraints);
    ast->child(3)->children.clear();
    break;

  case at_defstruct:
  case at_defunion:
    assert(ast->child(5)->astType == at_constraints);
    ast->child(5)->children.clear();
    break;

  case at_defexception:
    break;

  case at_let:
  case at_letrec:
    assert(ast->child(2)->astType == at_constraints);
    ast->child(2)->children.clear();
    break;

  default:
    assert(false);
    break;
  }
}

// Substitute old name for new name; however, this substitution tries
// to be a little more smart. If we see a typeapp whose first child
// (the base type) is substituted, we replace the typeapp with the
// type-name only. For example:
//   (list 'a) -> list#int32.

// This is a necessary step, and not a convenience that would
// otherwise be handled by recinstantiate. The problem is that not all
// typeapps in a definition can be removed, but the typeapps to the
// currently specialized type _must_ be removed before it can be R&Ted
// in the original environment.

static shared_ptr<AST>
substitute(shared_ptr<AST> ast, shared_ptr<AST> from, shared_ptr<AST> to)
{
  switch(ast->astType) {
  case at_ident:
    {
      if (ast->symbolDef == from) {
	assert((ast->flags & IDENT_MANGLED) == 0);
	
	NAMKARAN(ast, to->s);
	ast->symbolDef = to;
      }
      return ast;
    }

  case at_typeapp:
    {
      if (ast->child(0)->symbolDef == from)
	return substitute(ast->child(0), from, to);

      for (size_t c = 0; c < ast->children.size(); c++)
	ast->child(c) = substitute(ast->child(c), from, to);

      return ast;
    }

  default:
    {
      for (size_t c = 0; c < ast->children.size(); c++)
	ast->child(c) = substitute(ast->child(c), from, to);

      return ast;
    }
  }
}

// Replace type variable usage with concrete types in the
// instantiation
static shared_ptr<AST>
tvarSub(shared_ptr<AST> ast, shared_ptr<AST> tv, shared_ptr<Type> typ)
{
  switch(ast->astType) {
  case at_ident:
    if (ast->symbolDef == tv)
      return typeAsAst(typ, ast->loc);
    else
      return ast;

  default:
    for (size_t c = 0; c < ast->children.size(); c++)
      ast->child(c) = tvarSub(ast->child(c), tv, typ);
    return ast;

  }
}

// ``Instantiate'' type variables scoped at
//   the current letbinging to new type variables.
static shared_ptr<AST>
tvarInst(shared_ptr<AST> ast, shared_ptr<AST> scope, AstMap &newBinds)
{
  switch(ast->astType) {
  case at_ident:
    {
      // We are only concerned with type variables
      // that are scoped at ``scope''
      if (!ast->isIdentType(id_tvar))
	return ast;

      shared_ptr<AST> def = ast->symbolDef;

      if (def->tvarLB != scope)
	return ast;

      AstMap::iterator itr = newBinds.find(def);
      if (itr != newBinds.end())
	return itr->second->Use();

      shared_ptr<Type> newTV = Type::make(ty_tvar);
      shared_ptr<AST> newTvAst = newTV->asAST(ast->loc,
				   TvPrinter::make(false));
      newTvAst->symbolDef = newTvAst;
      newTvAst->flags |= TVAR_POLY_SPECIAL;

      newBinds[def] = newTvAst;
      return newTvAst;
    }


  case at_switch:
  case at_try:
    {
      for (size_t c = 0; c < ast->children.size(); c++)
	if (c != IGNORE(ast))
	  ast->child(c) = tvarInst(ast->child(c), scope, newBinds);
      return ast;
    }

  default:
    {
      for (size_t c = 0; c < ast->children.size(); c++)
	ast->child(c) = tvarInst(ast->child(c), scope, newBinds);
      return ast;
    }
  }
}

// Coerce a non-concrete type to a more concrete type
static void
coerce(ostream &errStream, shared_ptr<Type> t, 
       bool maybeOnly=false)
{
  INST_DEBUG
    errStream << "COERCING "
	      << t->asString(Options::debugTvP)
	      << " to ";
  t->adjMaybe(Trail::make(), false, false, true);
  if(!maybeOnly)
    t->SetTvarsToUnit();
  INST_DEBUG
    errStream << t->asString(Options::debugTvP)
	      << endl;
}


// Build a new proclaimation for the new instantiation.
// In the case of a value declaration, use typ to produce the type to
// declare. In the case of struct/union declaration, build an empty
// declaration where tvlist is empty. No constraints are ever emitted
static shared_ptr<AST>
buildNewDeclaration(shared_ptr<AST> def, shared_ptr<Type> typ)
{
  shared_ptr<AST> ident = def->getID()->getDeepCopy();

  if (ident->externalName.size())
    ident->flags |= DEF_IS_EXTERNAL;

  // We must only proclaim globals
  assert(ident->isGlobal());

  NAMKARAN(ident, getInstName(ident, typ));
  shared_ptr<AST> decl = GC_NULL;

  switch(def->astType) {
  case at_define:
  case at_recdef:
  case at_proclaim:
  case at_defexception:
    decl = AST::make(at_proclaim, def->loc, ident,
		   typeAsAst(typ, ident->loc),
		   AST::make(at_constraints, def->loc));
    break;

  case at_declstruct:
    decl = AST::make(at_declstruct, def->loc, ident,
		   AST::make(at_tvlist, ident->loc),
		   AST::make(at_constraints, def->loc));
    break;

  case at_declunion:
    decl = AST::make(at_declunion, def->loc, ident,
		   AST::make(at_tvlist, ident->loc),
		   AST::make(at_constraints, def->loc));
    break;

  default:
    assert(false);
    break;
  }

  INST_DEBUG
    cerr << "Built new Declaration "
	 << " for " << def->s << " with type "
	 << typ->asString() << " as " << endl
	 << decl->asString()
	 << endl;
  return decl;
}

static shared_ptr<AST>
getIDFromInstantiation(shared_ptr<AST> oldDefID, shared_ptr<AST> newDef)
{
  shared_ptr<AST> oldDef = oldDefID->defForm;

  // If we are looking for a constructor, find it and return
  if ((oldDef->astType == at_defunion) && oldDefID->isUnionLeg()) {
    shared_ptr<AST> oldCtrs = oldDef->child(4);
    shared_ptr<AST> newCtrs = newDef->child(4);
    shared_ptr<AST> newCtr = GC_NULL;
    for (size_t c=0; c < oldCtrs->children.size(); c++)
      if (oldCtrs->child(c)->child(0) == oldDefID)
	newCtr = newCtrs->child(c)->child(0);

    assert(newCtr);
    return newCtr->Use();
  }


  // Normal case. We don't worry about methods any more
  return newDef->getID()->Use();	
}


// Take in an identifier and return the symbol environment in which
// that identifier is defined

static shared_ptr<AST>
getOuterLet(shared_ptr<AST> ast)
{
  shared_ptr<AST> outerLet = GC_NULL;
  switch(ast->astType) {
  case at_ident:
    assert(!ast->symbolDef);
    assert(!ast->isGlobal());
    outerLet = ast->defForm->defForm->defForm;
    //     id    lb       lbs      let
    break;

  case at_letbinding:
    outerLet = ast->defForm->defForm;
    //    lb     lbs      let
    break;

  case at_letbindings:
    outerLet = ast->defForm;
    //    lbs    let
    break;

  default:
    assert(false);
    break;
  }

  assert(outerLet->astType == at_let ||
	 outerLet->astType == at_letrec);

  return outerLet;
}

static shared_ptr<AST>
getInnerLet(shared_ptr<AST> ast)
{
  shared_ptr<AST> outerLet = getOuterLet(ast);
  shared_ptr<AST> innerLet = outerLet->child(1);
  assert(innerLet->astType == at_let ||
	 innerLet->astType == at_letrec);
  return innerLet;
}


/*******************************************************************
                    The CORE POLYINSTANTIATOR
*******************************************************************/

/* Algorithm for new Polyinstantiator:
 *
 * 1) See if the desired identifier has been instantiated for the desired
 * type by searching the Unified-UOC environment. If found, just
 * return.
 *
 * 2) Examine the worklist whether we are in the process of instantiating
 * the current definition, if so emit a proclaimation, and exit.
 *
 * 3) Copy the incoming AST -- always copy the definition (rather than the
 * declaration) if one exists.
 *
 * 4) Mangle the name of the identifier being defined depending on the
 * desired type, and also change any recursive usages of the same name
 * within the current definition.
 *
 * ex: (define id#fn_bool_bool:(fn (bool) bool) (lambda (x) x))
 *
 * 5) Clamp the type of this AST to the desired type by introducing a
 * qualifier.
 *
 * ex, to instantiate id to (fn (bool) bool), we write:
 * (define id:(fn (bool) bool) (lambda (x) x))
 *
 * 6) Change the usage of any global identifiers to their FQNs since the
 * mega-Env contains the FQNs of all definitions.
 *
 * 7) R&T the new definition in the unified-UOC so that the types in
 * the body of the definition get clamped aiding further
 * instantiation. After successful R&T, throw away any changes to the
 * environment, as this definition is still in a "quasi" state.
 *
 * 8) Add the mangled name (id#fn_bool_bool) onto the worklist.
 *
 * 9) Recursively instantiate the body
 *
 * 10) After the current form has been completely instantiated, add it
 * to the target big-AST. Again R&T in the unified-UOC environment.
 *
 * 11) Remove the current definition from the worklist, exit.
 */

/////////////////////////////////////////////////////////////////////
//         Dealing with Type-classes and Constructors              //
/////////////////////////////////////////////////////////////////////

static shared_ptr<AST>
getDefToInstantiate(ostream &errStream, shared_ptr<UocInfo> unifiedUOC,
		    shared_ptr<AST> def, shared_ptr<Type> typ)
{
  // In the case of union constructors, we need to instantiate the
  // entire union type, the defForm field of all constructors point to
  // the entire defunion

  if (def->isUnionLeg())
    return def->defForm;

  // If this is a typeclass method, find an appropriate instance, get
  // the correct *identifier* representing (assumes instance-hoist)
  // the function to be used for this method-instance, and return
  // its defFrom (entire definition)
  //
  // But, the instance of a method could be satisfied by another
  // method (of the same or different type class). So, this process
  // must be repeated until we get an actual function.

  while (def->isTcMethod()) {
    // First look at the typeclass, get a copy of its type.
    shared_ptr<AST> typClass = def->defForm;
    shared_ptr<AST> tcID = typClass->child(0);
    shared_ptr<Typeclass> pred = tcID->scheme->type_instance();

    // Then find out which method we are concerned about.
    size_t nthMethod = 0;
    bool found = false;
    for (size_t i = 0; i < pred->components.size(); i++) {
      shared_ptr<Type> ithMethod = pred->components[i]->typ->getType();
      if (ithMethod->defAst == def) {
	nthMethod = i;
	found = true;
	break;
      }
    }
    assert(found);

    // Unify the method's type with the necessary type (type at
    // current use).
    assert(pred->components[nthMethod]->typ->unifyWith(typ));

    // Now pred contains the properly specialized type. This should
    // have enough information to uniquely identify the instance we
    // must now instantiate. So, search for the (most) appripriate
    // instance among all the instances we have for the
    // current typeclass.

    shared_ptr<set<shared_ptr<Instance> > > insts =
      unifiedUOC->instEnv->getBinding(tcID->fqn.asString());

    set<shared_ptr<Instance> >::iterator matchingInstance;

    for (matchingInstance = insts->begin();
	matchingInstance != insts->end(); ++matchingInstance) {
      shared_ptr<Instance> currInst = (*matchingInstance);
      if (currInst->satisfies(pred, unifiedUOC->instEnv))
	break;
    }

    found = (matchingInstance != insts->end());
    assert(found);

    shared_ptr<AST> instAST = (*matchingInstance)->ast;
    shared_ptr<AST> theMethod = instAST->getInstanceMethod(def->s);

    // If an immediate lambda was present, then InstLamHoist hoisted
    // it and left us with an ID wrapped by a THE.
    assert (theMethod->astType == at_ident || theMethod->astType == at_tqexpr);
    if (theMethod->astType == at_tqexpr)
      theMethod = theMethod->child(0);

    // Finally, we have the instance we want to instantiate,
    // Set def to its defining occurence, and loop.
    def = theMethod->symbolDef;
  }

  // The natural case, just return my containing defining form
  // This returns:
  //   Top-level definition for globals
  //   The corresponding let-binding for locals
  return def->defForm;
}

/////////////////////////////////////////////////////////////////////
//             Recursive Instantiation-propagator                  //
/////////////////////////////////////////////////////////////////////


// This function is the recursive walker to fix
// the body of instantiated forms. This function must
// *only* operate on the copied forms, and never the original

shared_ptr<AST>
UocInfo::recInstantiate(ostream &errStream,
			shared_ptr<AST> ast,
			bool &errFree,
			WorkList<string>& worklist)
{
  INST_DEBUG
    cerr << "RecInstantiate: "
	 << "[" << ast->atKwd() << "]"
	 << ast->asString() << ": "
	 << ((ast->symType) ?
	     ast->symType->asString(Options::debugTvP) :
	     "??")
	 << endl;

  switch(ast->astType) {
  case at_ident:
    {
      // If Instantiate has already fixed this identifier, do nothing
      if (ast->flags & IDENT_MANGLED)
	break;

      // We should never be dealing with tvars because:
      // i) In the case of type definitions, all type-arguemnts are
      //    cleared
      // ii) In the case of type-qualifications in the case of value
      //    definitions, rec-instantiate is never called on the
      //    type-part. We alyays emit the type-AST by hand.
      assert(!ast->isIdentType(id_tvar));

      // We should only be handling use-occurences.
      // Defining occurences are handled by their respective container
      // ASTs -- local definitions here, and global definitions in
      // doInstantiate() function.
      shared_ptr<AST> def = ast->symbolDef;
      assert(def);

      // If this is a local defined at a non-let variable, whose name
      // I just fixed, fix the name of the use case, and do nothing.
      if (def->flags & LOCAL_NOGEN_VAR) {
	assert(def->flags & IDENT_MANGLED);
	NAMKARAN(ast, def->s);
	break;
      }

      // If the type is not concrete, fix it. The observation is that
      // all self uses of a recursive definition have already been
      // marked by Instantiate, and this use must be a use outside the
      // definition if IDENT_MANGLED is not set. So, there is no loss
      // of generality in fixing the use case.
      // In case of maybe Types, we can
      //    a) Just consider the core() part
      //    b) Pick any variant of the core(), in particular the
      //       minimally mutable one.
      //
      // FIX: Need to check that the type is not constrained, so that I
      //      can use unit as the placeholder value.
      // FIX: Sometime, change unit to some placeholder non-generalizable
      //      thing so that the user can be shown a tvar, rather than the
      //      unit

      if (!ast->symType->isConcrete())
	coerce(errStream, ast->symType);

      // Now that the identifier is a concrete instantiation,
      // (poly)instantiate it, and replace the use case with the
      // use of the (possibly) new AST.
      ast = doInstantiate(errStream, def, ast->symType,
			  errFree, worklist);
      break;
    }

  case at_typeapp:
    {
      for (size_t c = 0; c < ast->children.size(); c++)
	ast->child(c) = recInstantiate(errStream,
				       ast->child(c),
				       errFree, worklist);

      // There should be no typeapps in the polyinstantiated AST.
      ast = ast->child(0);
      break;
    }

  case at_methdecl:
    // FIX: instantiating at_methdecl may not be correct here.
  case at_block:
  case at_return_from:
  case at_field:
    {
      ast->child(1) = recInstantiate(errStream,
				     ast->child(1),
				     errFree, worklist);
      break;
    }

  case at_select:
    {
      ast->child(0) = recInstantiate(errStream,
				     ast->child(0),
				     errFree, worklist);
      break;
    }

  case at_fqCtr:
  case at_sel_ctr:
    {
      ast->child(0) = recInstantiate(errStream,
				     ast->child(0),
				     errFree, worklist);
      ast->child(1) = recInstantiate(errStream,
				     ast->child(1),
				     errFree, worklist);
      break;
    }

  case at_inner_ref:
    {
      ast->child(0) = recInstantiate(errStream,
				     ast->child(0),
				     errFree, worklist);

      if (ast->flags & INNER_REF_NDX)
	ast->child(1) = recInstantiate(errStream,
				       ast->child(1),
				       errFree, worklist);
      break;
    }

  case at_declare:
    {
      if (ast->children.size() > 1)
	ast->child(1) = recInstantiate(errStream,
				       ast->child(1),
				       errFree, worklist);
      break;
    }

  case at_arrayType:
    {
      // Don't process the integer literal, the RHS is an integer
      // literal, it must not be type qualified.
      ast->child(0) = recInstantiate(errStream,
				     ast->child(0),
				     errFree, worklist);
      break;
    }

  case at_bitfield:
  case at_fill:
    {
      break;
    }

  case at_identPattern:
    {
      ast->child(0) = recInstantiate(errStream,
				     ast->child(0),
				     errFree, worklist);

      // Explicitely re-write EVERY type-qualification by hand.
      // Otherwise tvar-scoping will play havoc due to copy --
      // especially at let/letrec
      //
      // Need to be careful here about byref types: The byref
      // type goes on the function type, not on the identifier's type
      // Therefore, need to preserve by-ref AST qualifications as-is.

      if (ast->children.size() > 1) {
	shared_ptr<AST> typeAST = typeAsAst(ast->child(0)->symType,
					    ast->child(1)->loc);
	if (ast->child(1)->astType == at_byRefType)
	  ast->child(1) = AST::make(at_byRefType,
				    typeAST->loc, typeAST);
	else
	  ast->child(1) = typeAST;
 	
	RANDT_DROP(ast->child(1), "[[IP R&T]]", UNIFIED_ENVS);	
	
	ast->child(1) = recInstantiate(errStream,
				       ast->child(1),
				       errFree, worklist);	
      }
      break;
    }

  case at_tqexpr:
    {
      ast->child(0) = recInstantiate(errStream,
				     ast->child(0),
				     errFree, worklist);

      if (ast->child(0)->astType == at_tqexpr) {	
	// We already generated a qualified expression
	// See IntLit / FloatLit case
	assert(ast->child(0)->child(0)->astType == at_intLiteral ||
	       ast->child(0)->child(0)->astType == at_floatLiteral);
	
	ast = ast->child(0);
	break;
      }

      ast->child(1) = typeAsAst(ast->child(0)->symType,
				ast->child(1)->loc);

      RANDT_DROP(ast->child(1), "[[TQ R&T]]", UNIFIED_ENVS);

      ast->child(1) = recInstantiate(errStream,
				     ast->child(1),
				     errFree, worklist);
      break;
    }
    
  case at_sizeof:
  case at_bitsizeof:
    {
      shared_ptr<AST> typAst = ast->child(0);
      if (!typAst->symType->isConcrete()) 
	coerce(errStream, typAst->symType);

      ast->child(0) = typeAsAst(typAst->symType, typAst->loc);
      ast->child(0) = recInstantiate(errStream, ast->child(0),
				     errFree, worklist);
      break;
    }

  case at_floatLiteral:
  case at_intLiteral:
    {
      // Integer literals must have their types explicitely clamped
      // with a qualifier, we no longer deal with type classes after
      // this pass
      if (!ast->symType->isConcrete()) 
	coerce(errStream, ast->symType, true);
      
      assert(ast->symType->isConcrete());

      ast = AST::make(at_tqexpr, ast->loc, ast,
		      typeAsAst(ast->symType, ast->loc));
      RANDT_DROP(ast, "[[IntFloat R&T]]", UNIFIED_ENVS);
      break;	
    }

  case at_letStar:
    {
      assert(false);
      break;
    }

  case at_let:
  case at_letrec:
    {
      // Create a wrapper let expression, where the Instantiator will
      // fill in definitions. This let initially has empty bindings.
      // (an invalid AST), but we will shortly fix this.

      // (let[rec] (bindings) body constraints) is transformed into
      // (let[rec] (bindings) (let[rec] () body) constraints)
      // We will *not* carry over the constraints, these constraints
      // are automatiocally dropped when we finally declare the inner
      // let expression as "the" AST, and drop the outer wrapper.

      shared_ptr<AST> originalLbs = ast->child(0);
      shared_ptr<AST> originalExpr = ast->child(1);

      shared_ptr<AST> newLet = AST::make(ast->astType, ast->loc,
			    AST::make(at_letbindings,
				    originalLbs->loc),
			    originalExpr,
			    AST::make(at_constraints,
				    ast->child(2)->loc));

      shared_ptr<AST> newLbs = newLet->child(0);
      shared_ptr<AST> newExpr = newLet->child(1);

      // Actually rewrite the current expression
      ast->child(1) = newLet;

      // We re-built the let-expression, So, remark all defForms.
      findDefForms(ast, GC_NULL, ast->defForm);

      INST_DEBUG
	errStream << "recInstantiate: WrappedLet =  "
		  << endl
		  << ast->asString()
		  << endl;

      newLet->child(1) = recInstantiate(errStream, newExpr,
					errFree, worklist);
      newExpr = newLet->child(1);

      // We need to carry-forward any non-polymorphic expression for
      // the sake of their (possible) side-effects.
      // We must not carry-forward everything because we must remove
      // all polymorphism and qualifications/constraints here.
      // Since no non-value can be polymorphic, this check will ensure
      // that we do not drop any state change.
      for (size_t i=0; i < originalLbs->children.size(); i++) {
	shared_ptr<AST> lb = originalLbs->child(i);
	shared_ptr<AST> ident = lb->child(0)->child(0);
	if (ident->symType->isConcrete()) {
	  // Instantiate this definition, it will automatically get
	  // added to the inner let, so we just drop the use
	  // case identifier returned by doInstantiate().
	  doInstantiate(errStream, ident, ident->symType,
			errFree, worklist);
	}
      }

      // Declare the inner let-expression as "the" let-expression.
      // In case after instantiation, it so happens that no
      // let-bindings are carried forward, just drop the
      // let-expression, and return the let-body.
      if (newLbs->children.size() > 0)
	ast = newLet;
      else
	ast = newExpr;
      break;
    }

  case at_letbindings:
  case at_letbinding:
    {
      assert(false);
      break;
    }


    // We have already handled defining occurences of let-bound
    // identifiers. Now, we need to deal with other forms of local
    // definitions like:
    // - Lambda binding parameters (not generalized)
    // - Identifiers being defined at switch, case, do, (not
    //   generalized)
    //
    // These are as concrete as that fixed by the outer
    // containing form. There is no way use orrurences can have a
    // diferent type than the original, Fix their name and do
    // nothing.
    //
    // However, this must be done on a per-case basis because, if we
    // rename the defining occurence, we must **immediately** rename
    // all the use-cases before an R&T is done. Since parts of a
    // single global definition are R&Ted potentially many times due
    // to R&Ts of let-bindings, we must always keep the namespace
    // clean.
    //
    // In theory, it may be OK to not do this because we leave
    // environments intact many-a-time, even though we mangle
    // ASTs. But this method seems cleaner, and its advantages are
    // better redability of dumps, and ability to track all defining
    // occurence changes.
    //
    // Once we do this update, it is important also to update the key
    // in the corresponding environments because we are not R&Ting
    // here. It creates other problems as we are still in the process
    // of recursive-changes over the definition.

  case at_lambda:
    {
      shared_ptr<AST> args = ast->child(0);
      shared_ptr<AST> body = ast->child(1);
      for (size_t c=0; c < args->children.size(); c++) {
	shared_ptr<AST> argPat = args->child(c);
	shared_ptr<AST> arg = argPat->child(0);
	string oldName = arg->s;

	arg->flags |= LOCAL_NOGEN_VAR;
	NAMKARAN(arg, getInstName(arg, arg->symType));
	ast->envs.updateKey(oldName, arg->s);
	
	substitute(body, arg, arg); //!!
	
	shared_ptr<AST> typeAST = typeAsAst(arg->symType,
					    argPat->loc);
	
	if(args->symType->CompFlags(c) & COMP_BYREF)
	  typeAST = AST::make(at_byRefType, typeAST->loc, typeAST);
	
	if(argPat->children.size() == 2)
	  ast->child(1) = typeAST;
	else
	  argPat->addChild(typeAST);
	
	// No need to RANDT the new type AST generated here, it will
	// be handled in the identPattern case in the following
	// recInstantiate calls. 
      }

      ast->child(0) = recInstantiate(errStream, args,
				     errFree, worklist);
      ast->child(1) = recInstantiate(errStream, body,
				     errFree, worklist);
      break;
    }

  case at_dobinding:
    {
      // init and update
      for (size_t c = 1; c < ast->children.size(); c++)
	ast->child(c) = recInstantiate(errStream,
				       ast->child(c),
				       errFree, worklist);
      break;
    }

  case at_do:
    {
      shared_ptr<AST> doBds = ast->child(0);
      shared_ptr<AST> doTest = ast->child(1);
      shared_ptr<AST> doExprs = ast->child(2);

      for (size_t c = 0; c < doBds->children.size(); c++) {
	shared_ptr<AST> doBd = doBds->child(c);
	shared_ptr<AST> local = doBd->child(0)->child(0);

	string oldName = local->s;
	local->flags |= LOCAL_NOGEN_VAR;
	NAMKARAN(local, getInstName(local, local->symType));
	ast->envs.updateKey(oldName, local->s);

	for (size_t d = 0; d < doBds->children.size(); d++) {
	  shared_ptr<AST> update = doBds->child(d)->child(2);
	  substitute(update, local, local);
	}
	substitute(doTest, local, local);
	substitute(doExprs, local, local);
      }

      for (size_t c = 0; c < ast->children.size(); c++)
	ast->child(c) = recInstantiate(errStream,
				       ast->child(c),
				       errFree, worklist);

      break;
    }

  case at_sw_leg:
  case at_otherwise:
    {
      shared_ptr<AST> local = ast->child(0);
      shared_ptr<AST> expr = ast->child(1);
      string oldName = local->s;
      local->flags |= LOCAL_NOGEN_VAR;
      NAMKARAN(local, getInstName(local, local->symType));
      ast->envs.updateKey(oldName, local->s);
      substitute(expr, local, local); //!!

      // expr and all constructors
      for (size_t c = 1; c < ast->children.size(); c++)
	ast->child(c) = recInstantiate(errStream,
				       ast->child(c),
				       errFree, worklist);
      break;
    }

  case at_switch:
  case at_try:
    {
      for (size_t c = 0; c < ast->children.size(); c++)
	if (c != IGNORE(ast))
	  ast->child(c) = recInstantiate(errStream,
					 ast->child(c),
					 errFree, worklist);
      break;
    }

  default:
    {
      // Obviously,
      for (size_t c = 0; c < ast->children.size(); c++)
	ast->child(c) = recInstantiate(errStream,
				       ast->child(c),
				       errFree, worklist);
      break;
    }
  }

  INST_DEBUG
    cerr << "##" << "[" << ast->atKwd() << "]"
	 << " RecInstantiated to: " << ast->asString()
	 << " with type "
	 << ((ast->symType != NULL)?ast->symType->asString(Options::debugTvP):"??") << endl;

  return ast;
}


/////////////////////////////////////////////////////////////////////
//                        THE Instantiator                         //
/////////////////////////////////////////////////////////////////////

// Specialize the input AST /def/ according to the supplied concrete
// type /typ/ and emit the resulting, renamed AST to outUOC.
shared_ptr<AST>
UocInfo::doInstantiate(ostream &errStream,
		       shared_ptr<AST> ast,
		       shared_ptr<Type> typ,
		       bool &errFree,
		       WorkList<string>& worklist)
{
  // INPUT: /ast/ is the *defining* occurence of an identifier whise
  //        definition must be instantiated to the type /typ/
  //
  // OUTPUT: *Use* occurence of the instantiated identifier.
  //
  // SIDE-EFFECTS: The instantiated definition (defForm) is added
  //               to the output AST
  assert(ast->astType == at_ident);
  assert(!ast->symbolDef);

  // getDefToInstantiate returns the correct
  // definition to instantiate. ex: In the case of
  // constructors, entire union must be instantiated. Special
  // handling is necessary for methods, etc.
  shared_ptr<AST> def = getDefToInstantiate(errStream, shared_from_this(), ast, typ);

  assert((def->astType != at_deftypeclass) ||
	 (def->astType == at_definstance));

  INST_DEBUG
    cerr << "To Instantiate: " << def->asString()
	 << " for type " << typ->asString() << endl;

  // Chase down any unification:
  //         keep mutability, maybes don't matter
  typ = typ->getTheType(true, false);

  // In the case of value-type-definitions, increase the
  // mutability-permissibility of all type-arguments to the maximum
  // possible extent. However, we must shed the outermost mutable
  // wrapper that will be produced by mazimizeMutability.
  //
  // Consideration: Should we mazimize mutability in the case of value
  // definitions also?
  // **NO** IF AT ALL POSSIBLE. This will increase closure coversion
  // as everything is mutable, and is only useful if a polymnorphic
  // definition is used polymorphically wrt mutability beyond shallow
  // mutability (since we catch thich case in the type-system).
  if (def->astType == at_defstruct || def->astType == at_defunion) {
    typ = typ->getDCopy()->maximizeMutability();
    typ = typ->getBareType();

    INST_DEBUG
      cerr << "Type after maximizeMutability = "
	   << typ->asString() << endl;
  }

  // We always specialize to some concrete type
  assert(typ->isConcrete());

  // We can only specialize defining forms
  shared_ptr<AST> defIdent = def->getID();
  assert(defIdent);

  // Make sure we don't mangle any identifier twice. We should never
  // be called for the use of an identifier we have already processed
  assert((defIdent->flags & IDENT_MANGLED) == 0);

  // Are we instantiating a local, or a global?
  // There is ont thing we must remember about the difference between
  // instantiating a local and instantiating a global.
  //
  // GLOBAL: The input definition is the original AST. Therefore, we
  // must never touch it directly. Also, the environment attached on
  // this AST is the original environment (not a part of the
  // unifiedUOC).
  //
  // LOCAL: The input AST is a let-binding, which is a part of a copy
  // of another global AST which has already been instantiated. This
  // AST has already been (quasily) R&Ted in the unifiedUOC (see the
  // working of this function).
  bool globalInst = true;
  if (!defIdent->isGlobal())
    globalInst = false;


  // If we are trying to instantiate a declaration,
  // try instantiating the definition if one exists.
  //
  // This must be performed as the first step because the definitions
  // and declarations can differ in mutability at the fn-arg-ret
  // positions, and hence mangled string() will produce different
  // names. This is actually bad because there might be other things
  // that rely on the mangledString(), and we must revisit
  // fn-compatibility and then generate mangledString()s based on the
  // outside type of a function

  if (defIdent->defn) {
    assert(globalInst);
    defIdent = defIdent->defn;
    def = defIdent->defForm;
    INST_DEBUG
      cerr << "Definition found, will instantiate: "
	   << def->asString() << endl;
  }

  // Now, we know all we can, about ident and def wrt the
  // whole program. If def is still a declation, there is
  // *globally* no definition available for it.

  // Get the new name to which this definition will be
  // instantiated.
  string newName = getInstName(defIdent, typ);
  // Get a globally unique working-name to deal with the
  // worklist. This name is based on the uniqueID of the AST def, and
  // is globally unique even wrt local definitions.
  string wkName = uniqName(newName, def);

  // See if we have already instantiated this AST for this type
  // before. If so, that AST must be in my environment,
  // just return it.
  //
  // GLOBALS: Search in the UnifiedUOC (my) environment.
  //          DO NOT search in the AST's environment, that is the
  //          original environment.
  //
  // LOCALS:  Search the let-bindings in the inner let to see if we
  //          already have an instantiation.
  //          Don't search the environment saved as part of the
  //          corresponding let AST's environment. The let-binding is
  //          R&Ted independent of the let-wrapper. There is no
  //          environment there.

  shared_ptr<AST> alreadyInstantiated = GC_NULL;
  if (globalInst)
    alreadyInstantiated = env->getBinding(newName);
  else {
    shared_ptr<AST> innerLet = getInnerLet(defIdent);
    shared_ptr<AST> innerLbs = innerLet->child(0);
    for (size_t c=0; c < innerLbs->children.size(); c++) {
      shared_ptr<AST> id = innerLbs->child(c)->getID();
      if (id->s == newName)
	alreadyInstantiated = id;
    }
  }

  INST_DEBUG
    if (!alreadyInstantiated) {
      errStream << "No existing instantiation found for "
		<< newName;
      if (!globalInst)
	errStream << " in Local Env of "
		  << endl
		  << getInnerLet(defIdent)->asString();

      errStream << endl;
    }

  // If an instantiation is already found, return a use ast of the
  // corresponding identifier.
  if (alreadyInstantiated)
    return getIDFromInstantiation(ast, alreadyInstantiated->defForm);

  // If I am within the worklist, emit a declaration at this
  // point. This is necessary to ensure that mutually recursive
  // instantiations do not loop for ever

  if (worklist.contains(wkName)) {

    // In the case of local definitions (within a letrec), we cannot
    // emit a declaratin. Instead, make up an identifer AST with the
    // correct name and return it to the caller. This will all work
    // out when the corresponding let-binding will get instantiated
    // some day.
    if (!globalInst) {
      shared_ptr<AST> res = ast->getDeepCopy();
      res->symbolDef = GC_NULL;
      NAMKARAN(res, newName);

      INST_DEBUG
	errStream << "LOCAL definition "
		  << wkName << " is present in the workist, "
		  << " returning " << newName
		  << endl;
      return res;
    }

    // If we are instantiating a union because of a constructor, we
    // cannot be still in the process of defining it, because the
    // type-definition must be complete before constructors can be
    // used. So, make sure we are OK.
    assert(!ast->isUnionLeg());

    // Get the proclaimation / declType
    shared_ptr<AST> newDecl = buildNewDeclaration(def, typ);

    // Marking done wrt my UOC.
    findDefForms(newDecl);

    // This new Declaration can never contain the name being
    // defined. So, no immediate fixup necessary.
    // Safe to R&T in MEGA environment.
    RANDT_DROP(newDecl, "[[InstDecl: R&T-1]]", UNIFIED_ENVS);

    // In the case of a proclaimation, I need to recurse over
    // the type-part and instantiate any types if necessary.
    // It is safe to recurse as there are no self references.
    // No constraints are ever emitted.
    if (newDecl->astType == at_proclaim)
      newDecl->child(1) = recInstantiate(errStream,
					 newDecl->child(1),
					 errFree, worklist);

    // Now that the declaration is fixed up -- if necessary --
    // R&T it (in my enviromment) and commit the AST.
    RANDT_COMMIT(newDecl, "[[InstDecl: R&T-2]]", UNIFIED_ENVS);

    // Actually add the declaration AST
    addTopLevelForm(newDecl);

    // Done with the declaration, somebody, please emit the definition
    // later.
    return newDecl->getID()->Use();
  }

  // If we reach  here, we have never seen this definition before, so,
  // need to really instantiate it. So, add this definition to the
  // worklist and mark that we are on it.
  worklist.insert(wkName);


  // Make a copy of the definition, make a true-copy, we need the
  // symbolDefs to still point to the old ones so that substitute
  // works correctly.
  shared_ptr<AST> copy = def->getTrueCopy();
  shared_ptr<AST> copyIdent = copy->getID();
  NAMKARAN(copyIdent, newName);

  // Then, within the current definition, replace all references to the
  // definition, with references to the copy.
  // This step **MUST** be done before clamping the AST with the type
  // annotation.
  copy = substitute(copy, defIdent, copyIdent);

  // and adjust the defForms and constraints in the new AST.
  if (globalInst) {
    findDefForms(copy); // Marking done wrt my UOC.
    clearConstraints(copy);
  }
  else {
    // In the case of local-instantiation, it is safe to
    // traverse defForm upwards
    shared_ptr<AST> copyTopExpr = ((globalInst) ? copy :
			copy->defForm->defForm->defForm);
    //lb    lbs      let      define

    findDefForms(copy, copy->defForm, copyTopExpr);
    // constraints are dealt with in recInstantiate itself.
  }

  // Clamp the type of this definition based on typ. This must be done
  // on a per ASTtype basis
  // In the case of local instantiation, we actually will add the
  // letbinding to the AST here, before we RandT for the first time.
  switch(copy->astType) {
  case at_define:
  case at_recdef:
  case at_letbinding:
    {
      // For a definition, fix the type by emitting a concrete type
      // qualification in its identPattern. If the user has written
      // one, overwrite it. The type obtained from the type record is
      // at least as precise what the user wrote

      shared_ptr<AST> ip = copy->child(0);
      assert(ip->astType == at_identPattern);
      shared_ptr<AST> typAST = typeAsAst(typ, copy->loc);
      if (ip->children.size() > 1)
	ip->child(1) = typAST;	
      else
	ip->children.push_back(typAST);	

      if (!globalInst) {
	// In the case of local definitions, we add the copied
	// letbinding into the *inner* let-form (see recInstantiate
	// at_let/at_letrec case).
	// ex:(define top
	//      (let ((id (lambda (x) x)))
	//        (let ((id#fn_bool_bool (lambda (x) x))) ...)))
	assert(copy->astType == at_letbinding);
	shared_ptr<AST> innerLet = getInnerLet(copy);
	shared_ptr<AST> innerLbs = innerLet->child(0);
	innerLbs->children.push_back(copy);

	// We also need to fixup any references to
	// type-variables scoped at the current let-binding.
	// For example:
	// (let ((l (lambda (x:'a) #f)))
        //       (l #t)))
	// Here, we must not generate
	//   (let ((l#fn_4bool_4bool (lambda (x:'a) #f)))
	// as the inner let-binding because it will clamp the
	// generalizable type variable 'a.
	// This will result in a RIGID type variable unification
	// error. Therefore, we need to rename all variables that are
	// scoped at this let-bindings.

	AstMap newBinds;
	tvarInst(copy, getOuterLet(copy)->child(0), newBinds);
      }

      break;
    }

  case at_proclaim:
    {
      // Rewrite the type provided by the user, to the type desired.
      copy->child(1) = typeAsAst(typ, copy->child(1)->loc);
      break;
    }

  case at_defexception:
    {
      break;
    }

  case at_declstruct:
  case at_declunion:
    {
      // Even lesser work here, remove any type variables, and we are
      // done (possibly polymorphic -> concrete).
      copy->child(1)->children.clear();
      break;
    }

  case at_defstruct:
    {
      // First the new definition is concrete, remove
      // type-variables.
      copy->child(1)->children.clear();

      // We happily replaced the use of old name with new name and
      // removed type-variables. But there may be typeapps in the
      // fields, and old tvars must now refer to concrete types. So,
      // fix that before recursing over the fields.

      shared_ptr<AST> defTvList = def->child(1);
      assert(typ->typeArgs.size() == defTvList->children.size());
      shared_ptr<AST> copyFields = copy->child(4);
      for (size_t i=0; i < defTvList->children.size(); i++) {
	shared_ptr<AST> defTv = defTvList->child(i);
	shared_ptr<Type> arg = typ->TypeArg(i);
	for (size_t j=0; j < copyFields->children.size(); j++) {
	  shared_ptr<AST> copyField = copyFields->child(j);
	  if (copyField->astType == at_fill)
	    continue;
	  copyField->child(1) =  tvarSub(copyField->child(1),
					 defTv, arg);
	}
      }
      break;
    }

  case at_defunion:
    {
      copy->child(1)->children.clear();

      shared_ptr<AST> defTvList = def->child(1);
      assert(typ->typeArgs.size() == defTvList->children.size());

      shared_ptr<AST> copyCtrs = copy->child(4);
      for (size_t j=0; j < copyCtrs->children.size(); j++) {	
	shared_ptr<AST> copyCtr = copyCtrs->child(j);
	
	// Unions are a little more complicated. We must:
	// i)  Fix the name of the constructor with the new name
	//     according to the new union we created
	// ii) Fix tvars of the fields of the constructor
	shared_ptr<AST> copyCtrID = copyCtr->child(0);
	NAMKARAN(copyCtrID, getInstName(copyCtrID, typ));

	for (size_t i=0; i < defTvList->children.size(); i++) {
	  shared_ptr<AST> defTv = defTvList->child(i);
	  shared_ptr<Type> arg = typ->TypeArg(i);

	  for (size_t k=1; k < copyCtr->children.size(); k++) {
	    shared_ptr<AST> copyField = copyCtr->child(k);
	    if (copyField->astType == at_fill)
	      continue;
	
	    copyField->child(1) =  tvarSub(copyField->child(1),
					   defTv, arg);
	  }
	}
      }
      break;
    }

   default:
    assert(false);
    break;
  }

  // Now, replace use of any global identifiers with their FQNs
  // because the mega-ENV only recognizes definitions by their FQNs.
  // This step **MUST** be done after clamping the AST with the type
  // annotation.
  name2fqn(copy);

  // At this point, the entire definition must be a valid AST. So, we
  // will type the new definition. However, since this definition is
  // still in "quasi" state, call R&T with a note that changes should
  // be thrown away. The only purpose of this R&T is that the types of
  // rest of the expression is unchanged, and we can recurse over the
  // expression body an instantiate any dependencies properly

  INST_DEBUG
    cerr << "Copy after name fixup: " << copy->asString() << endl;

  shared_ptr<EnvSet> envset = (globalInst ? UNIFIED_ENVS
			       :
			       (EnvSet::make(getOuterLet(copy)->envs)));

  RANDT_DROP(copy, "[[Inst: R&T-1: ]]", envset);

  // Now that the expression is typed, recurse over the body and
  // process dependencies

  switch(copy->astType) {
  case at_define:
  case at_recdef:
  case at_letbinding:
    {
      // Instantiate any types in the qualification we just emitted
      shared_ptr<AST> ip = copy->child(0);
      ip->child(1) = recInstantiate(errStream,
				    ip->child(1),
				    errFree, worklist);

      // Attend to any instantiations necessary in the body of this
      // definition
      copy->child(1) = recInstantiate(errStream,
				      copy->child(1),
				      errFree, worklist);

      break;
    }

  case at_proclaim:
    {
      // Nothing much to do, just recurse over the type specifier
      copy->child(1) = recInstantiate(errStream,
				      copy->child(1),
				      errFree, worklist);
      break;
    }

  case at_declstruct:
  case at_declunion:
    {
      // Even lesser work here
      break;
    }

  case at_defunion:
  case at_defstruct:
    {
      shared_ptr<AST> copyFieldsCtr = copy->child(4);
      copy->child(4) = recInstantiate(errStream,
				      copyFieldsCtr,
				      errFree, worklist);
      break;
    }

  case at_defexception:
    {
      // Instantiate the fields, if any
      for (size_t c=1; c < copy->children.size(); c++)
	copy->child(c) = recInstantiate(errStream, copy->child(c),
					errFree, worklist);
      break;
    }

  default:
    assert(false);
    break;
  }

  if (globalInst) {
    // Now we have the newly instantiated AST to be added to our
    // environment. Before that, perform some cleanup in order to
    // eliminate any references to the old module.
    copy->getID()->decl = GC_NULL;
    copy->getID()->defn = GC_NULL;

    // FINALLY, add the new form to my UOC in the case
    addTopLevelForm(copy);

    // R&T in my UOC to make sure everything is OK. This will also add
    // the copy to the environment so that further calls can just use
    // this one.
    INST_DEBUG
      cerr << "Copy after all fixup: " << copy->asString() << endl;
    RANDT_COMMIT(copy, "[[Inst: R&T-COMMIT]]", UNIFIED_ENVS);
  }
  // No ned for RandT in the case of local definitions
  // (let-bindings).

  // Now that we are (almost) done, remove current entry from the
  // worklist so that we are really done
  worklist.erase(wkName);
  if(copy->astType == at_defstruct) {
    shared_ptr<AST> fields = copy->child(4);
    for (size_t i=0; i < fields->children.size(); i++) {
      shared_ptr<AST> field = fields->child(i);

      if(!field->symType->isMethod())
	continue;
      
      shared_ptr<AST> methName = field->child(0);

      std::stringstream oldMeth;
      oldMeth << defIdent->fqn << "." << methName->s;
      std::stringstream newMeth;
      newMeth << copyIdent->s << "." << methName->s;

      shared_ptr<AST> methAST = env->getBinding(oldMeth.str());
      env->addBinding(newMeth.str(), methAST);

      shared_ptr<TypeScheme> methTS =
	gamma->getBinding(oldMeth.str())->ts_instance();
      errStream << "methTS is: " << methTS->asString() << std::endl;
      shared_ptr<Type> methType = methTS->tau->getType();
      shared_ptr<Type> methArgs = methType->Args()->getType();
      shared_ptr<Type> structArg = methArgs->CompType(0)->getType();
      assert(structArg->isStruct());
      errStream << "structArg: " << structArg->asString() << std::endl;
      structArg->defAst = copyIdent;
      structArg->myContainer = copyIdent;
      errStream << "structArg: " << structArg->asString() << std::endl;
      errStream << "methType: " << methType->asString() << std::endl;
      errStream << "methTS now is: " << methTS->asString() << std::endl;
      gamma->addBinding(newMeth.str(), methTS);
      
      errStream << "For Structure: " << copyIdent->s
		<< " obtained from " << defIdent->s 
		<< ", Adding binding "
		<< newMeth.str() << " ==> "
		<< oldMeth.str() << " with type "
		<< methTS->asString()
		<< std::endl;
      
#if 0

      shared_ptr<AST> methName = field->child(0);
      shared_ptr<AST> newMethName = AST::make(at_ident,
					      methName->loc);
      newMethName->flags |= ID_IS_GLOBAL;
      newMethName->s = copyIdent->s + "." + methName->s;
      
      shared_ptr<AST> oldMethName = AST::make(at_ident,
					      methName->loc);
      oldMethName->s = defIdent->fqn.asString() + "." + methName->s;
      
      shared_ptr<AST> newMethPat =  AST::make(at_identPattern,
					      newMethName->loc,
					      newMethName); 
      shared_ptr<AST> newMethDefn = AST::make(at_define,
					      methName->loc, 
					      newMethPat,
					      oldMethName);
      
      errStream << "For Structure: " << copyIdent->s
		<< " obtained from " << defIdent->s 
		<< ", generated definition "
		<< newMethDefn->asString()
		<< std::endl;
#endif
    }
  }
  
  INST_DEBUG
    cerr << "Instantiated: " << def->asString()
	 << " for type " << typ->asString() << endl
	 << " to " << copy->asString();

  // Now that we have instantiated the definition completely, we must
  // return a *use case* of the identifier for which we instantiated.
  // getIDFromInstantiation() does just that, with special attention
  // to the constructor case, where we have to return the constructor
  // ident's use instead
  return getIDFromInstantiation(ast, copy);
}

/* Things to remember:
   1) If we see a proclaimation that has a definition,
   we will only emit the definition. Here, we are relying on the
   fact that any externalNames hve already been propagated to the
   definitions (by the resolver), and that getDCopy() preserves
   externalNames.

   2) Suppose one defined a union type as:

   (defstruct (st 'a) a:'b)
   (defunion (unin 'a) nil)

   and if we see the definition
   (define (main argv:(vector string)))
   nil:(unin (st int32)) 0:int32)

   nil and this unin must be specialized for (st int32).

   But, after this substitution, the new unin ast will look like:
   (defunion _4unin#UR_SR_3st_5int32 _3nil#UR_SR_3st_5int32)

   3) Here are some old notes when we were dealing with type-variables
   in value definitions in rec-Instantiate:
   The first time we see a type-variable in an expression, it
   arbitrarily choosen as the defining occurence. But, it must
   also be considered as a use occurence.
   if (ast == def)
   assert(def->isIdentType(id_tvar));
   Type variables could have been present at defining occuernces in
   this case.

   4) It is generaly a good idea only to R&T at specific points:
   entire top-level form, and a particular let-binding when no
   generalization is involved. */



/*******************************************************************
                    TOP_LEVEL INTERFACE FUNCTIONS
*******************************************************************/

bool
UocInfo::instantiateFQN(ostream &errStream, const FQName& epName)
{
  bool errFree = true;
  shared_ptr<UocInfo> targetUoc = GC_NULL;
  shared_ptr<AST> def = UocInfo::lookupByFqn(epName, targetUoc);

  if (!def) {
    errStream << "bitcc: Entry point \"" << epName
	      << "\" not found.\n";
    return false;
  }
  else if (!def->symType->isConcrete()) {
    errStream << "Non-concrete procedure \"" << epName
	      << "\" defined at "
	      << def->loc
	      << " cannot be used as an entry point.\n";
    return false;
  }

  shared_ptr<AST> defIdent = def->getID();

  INST_DEBUG {
    INOstream ino(cerr);
    ino << "Instantiating form " << endl;
    ino.more();
    ino << def->asString() << endl;
    ino.less();
    ino << endl;
  }

  WorkList<string> worklist;
  doInstantiate(errStream, defIdent,
		defIdent->symType, errFree, worklist);

  if (epName == FQName("bitc.main", "main"))
    UocInfo::mainIsDefined = true;

  return errFree;
}

// One Shot instantiator
bool
UocInfo::instantiate(ostream &errStream, const FQName& epName)
{
  bool errFree = true;
  UpdateMegaEnvs(shared_from_this());

  CHKERR(errFree, instantiateFQN(errStream, epName));

  INST_DEBUG
    cerr << "Unified UOC after instantiation is "
	 << uocAst->asString() << endl;
  CHKERR(errFree, RandT(errStream, true, POLY_SYM_FLAGS,
			POLY_TYP_FLAGS, "[[Post Instantiation]]"));

  return errFree;
}

// Batch mode Instantiator -- same as the one shot mode,
// except that it does not refresh the megaENVs after
// after instantiationg rach definition

bool
UocInfo::instantiateBatch(ostream &errStream,
			  set<FQName>& epNames)
{
  bool errFree = true;
  UpdateMegaEnvs(shared_from_this());

  /// The following symbols are introduced by code that is added in the
  /// SSA pass. This code is added @em after polyinstantiation, and
  /// consequently cannot be discovered by the demand-driven incremental
  /// instantiation process. Add them to the emission list by hand here
  /// to ensure that they get emitted.
  ///
  /// These need to be instantiated before any of the user-specified
  /// entry points are instantiated, because the entry point
  /// instantiations may reference these.
  CHKERR(errFree, 
	 instantiateFQN(errStream, 
			FQName("bitc.prelude","__index_lt")));
  CHKERR(errFree,
	 instantiateFQN(errStream, 
			FQName("bitc.prelude","IndexBoundsError")));

  for (set<FQName>::iterator itr = epNames.begin();
      itr != epNames.end(); ++itr)
    CHKERR(errFree, instantiateFQN(errStream, (*itr)));

  INST_DEBUG
    cerr << "Unified UOC after instantiation is "
	 << uocAst->asString() << endl;
  CHKERR(errFree, RandT(errStream, true, POLY_SYM_FLAGS,
			POLY_TYP_FLAGS, "Post Instantiation: "));

  return errFree;
}


