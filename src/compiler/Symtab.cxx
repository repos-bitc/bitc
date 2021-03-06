/**************************************************************************
 *
 * Copyright (c) 2008, Johns Hopkins University.
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

/// @brief Symbol resolver implementation.
///
/// This is the symbol resolution checker for BitC.
///
/// The resolver makes a recursive-descent pass over the AST. The
/// resolution mechanism is not particularly unusual, except in the
/// handling of import and export. A pointer to the top-level
/// environment for each unit of compilation is stored into the
/// corresponding UoC. A pointer to the environment currently in
/// effect is stored into each AST as we traverse. In some cases this
/// is later overridden (what are these cases?).
///
/// Resolution and type-checking is re-run after many passes as a
/// sanity check. In post-SSA passes, there are some unusual cases
/// that the resolver knows to ignore.
///
/// @issue those cases need to get described here.
///
/// @issue Need to talk about what environments are tracked (lamLevel)
/// and what environment pointers are recorded into ASTs.
///
/// @issue Need to talk about handling of multiple defs/decls.
///
/// When an interface is imported, 
///
/// @issue This file is misnamed. It does not actually build a symbol
/// table.

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <string>
#include <stack>

#include "Options.hxx"
#include "AST.hxx"
#include "Environment.hxx"
#include "Symtab.hxx"
#include "inter-pass.hxx"
#include "MixFix.hxx"

using namespace std;
using namespace boost;
using namespace sherpa;
 
// Shap isn't convinced that the spec required this. Having said that,
// this is similar to the unresolved forward static declaration
// warning in C, and it is similarly useful.

static bool
warnUnresRef(std::ostream& errStream, 
             shared_ptr<AST> mod,
             shared_ptr<ASTEnvironment > env)
{
  bool errorFree = true;
  
  assert(mod->astType == at_module);
  for (size_t c=0; c < mod->children.size(); c++) {
    shared_ptr<AST> ast = mod->child(c);
    switch(ast->astType) {
    case at_declunion:
    case at_declstruct:
    case at_proclaim:
      {
        // Various phases introduce proclaims internally. Don't
        // complain about those:
        if (ast->flags & PROCLAIM_IS_INTERNAL)
          break;
        
        // If the declaration is identified as externally provided,
        // don't complain about that:
        if (ast->flags & DEF_IS_EXTERNAL)
          break;

        // If we saw a declaration first, that will have been bound in
        // the environment. A subsequent definition replaces that
        // binding in-place, and following that a further declaration
        // will not change the environment. So if we look up the
        // symbol in the environment and get a declaration, we haven't
        // seen a definition:
        shared_ptr<AST> binding = env->getBinding(ast->child(0)->s);
        if (binding->isDecl) {
          errStream << ast->loc << ": WARNING: " 
                    << "Local declaration of " << ast->child(0)->s 
                    << " found here, but no definition found."
                    << std::endl;
          
          if (Options::Wall)
            errorFree = false;
          break;
        }
      }
    default:
      break;
    }
  }
  return errorFree;
}

static shared_ptr<UocInfo>
findInterface(std::ostream& errStream, shared_ptr<AST> ifAst)
{
  shared_ptr<UocInfo> iface=GC_NULL;

  UocMap::iterator itr = UocInfo::ifList.find(ifAst->s);
  if (itr != UocInfo::ifList.end()) {
    shared_ptr<UocInfo> thisIface = itr->second;
    iface = thisIface;
  }
      
  if (!iface) {
    errStream << ifAst->loc << ": "
              << "Internal Compiler Error. "
              << "Interface " << ifAst->s
              << " has NOT been processed"
              << std::endl;
    return GC_NULL;
  }
  
  if (!iface->env || !iface->gamma || !iface->instEnv) { 
    errStream << ifAst->loc << ": "
              << "Internal Compiler Error. "
              << "Interface " << ifAst->s
              << " has at least one NULL environment"
              << std::endl;
    return GC_NULL;
  }
  
  ifAst->envs.env = iface->env;
  ifAst->envs.gamma = iface->gamma;
  ifAst->envs.instEnv = iface->instEnv;

  return iface;
}

#if 0
// Debugging function used for displaying the environment
// along with status of completeness
static void
showEnv(std::ostream& errStream, 
        shared_ptr<ASTEnvironment > env)
{
  for (ASTEnvironment::iterator itr = env->begin(); 
       itr != env->end(); ++ itr) {
    std::string name = itr->first;
    shared_ptr<Binding<AST> > bdng = itr->second;
    
    errStream << name << ": "
              << ((bdng->flags & BF_COMPLETE)? "Complete" : "Incomplete")
              << std::endl;
  }
  
  if(env->parent) {
    errStream << " -- " << std::endl;
    showEnv(errStream, env->parent);
  }
}
#endif
              
static void
aliasPublicBindings(const std::string& idName,
                    shared_ptr<ASTEnvironment > aliasEnv, 
                    shared_ptr<ASTEnvironment > fromEnv, 
                    shared_ptr<ASTEnvironment > toEnv)
{
  for (ASTEnvironment::iterator itr = fromEnv->begin();
      itr != fromEnv->end(); ++itr) {
    std::string s = itr->first;
    shared_ptr<Binding<AST> > bdng = itr->second;
    
    if (bdng->flags & BF_PRIVATE)
      continue;

    shared_ptr<AST> ast = bdng->val;

    if (aliasEnv && aliasEnv->getBinding(ast->fqn.asString()))
      continue;


    if (idName.size())
      s = idName + FQName::LocalBindingSep + s;

    toEnv->addBinding(s, ast);

    // @bug There is a moderately serious bug here. If the importing
    // module is a consumer of an incomplete proclaim, then the
    // working assumption is that there are global initialization
    // constraints, and those constraints ensure that the provider has
    // already defined the proclaimed symbol, in which case it should
    // be marked complete in the importing module's context.
    //
    // However, it must NOT be marked complete in the *providing*
    // module's context until it is actually defined, and this yields
    // the following dilemma:
    //
    //  (import theModule X) ;; causing to be marked locally complete)
    //  ... use of X requiring completeness ...
    //  (provide theModule X) ;; at which point we suddenly realize
    //    that it can't be complete. We can un-mark it here, but the
    //    damage has already been done by the use.
    //
    // This is all tied in with the general problem of initialization
    // ordering.
    toEnv->setFlags(s, BF_PRIVATE|BF_COMPLETE);
  }
}

static bool
importIfBinding(std::ostream& errStream, 
                shared_ptr<ASTEnvironment > aliasEnv,
                shared_ptr<AST> ifName)
{
  if (!findInterface(errStream, ifName))
    return false;

  std::string canonicalIfName = ifName->s;

  // If we have seen this interface before, use the original import:
  shared_ptr<AST> ifAst = aliasEnv->getBinding(canonicalIfName);
  if (ifAst) {
    // Override the environments populated by findInterface with the
    // canonical duplicates.
    ifName->envs.env = ifAst->envs.env;
    ifName->envs.gamma = ifAst->envs.gamma;
    ifName->envs.instEnv = ifAst->envs.instEnv;
  }

  aliasEnv->addBinding(canonicalIfName, ifName);

  // Need to form the canonical duplicate environment in the current
  // importing UoC for this interface.
  shared_ptr<ASTEnvironment > dupEnv = 
    ASTEnvironment::make(ifName->envs.env->uocName);

  for (ASTEnvironment::iterator itr = ifName->envs.env->begin();
      itr != ifName->envs.env->end(); ++itr) {
    std::string s = itr->first;
    shared_ptr<Binding<AST> > bdng = itr->second;
    
    if (bdng->flags & BF_PRIVATE) {
      continue;
    }
      
    shared_ptr<AST> ast = bdng->val;

    dupEnv->addBinding(s, ast);
    dupEnv->setFlags(s, bdng->flags);
  }

  ifName->envs.env = dupEnv;
  return true;
}

static bool
providing(shared_ptr<ASTEnvironment > aliasEnv, const FQName& fqn)
{
  // Retrieve the thinned public environment for this fqn:
  shared_ptr<AST> ifName = aliasEnv->getBinding(fqn.iface);

  // If there is no binding for the canonical interface name, then we
  // areb processing the grand output AST, and providing has already
  // been checked.
  if (!ifName)
    return true;

  shared_ptr<ASTEnvironment > pubEnv = ifName->envs.env;

  // Retrieve the binding (if any) for fqn.ident:
  shared_ptr<Binding<AST> > bdng = pubEnv->doGetBinding(fqn.ident);

  assert(bdng);

  // Check the BF_PROVIDING flag on the binding:
  return (bdng->flags & BF_PROVIDING);
}

#if 0
bool
makeLocalAlias(shared_ptr<ASTEnvironment > fromEnv,
               std::string fromName,
               shared_ptr<ASTEnvironment > toEnv, 
               const std::string& toPfx,
               shared_ptr<AST> toIdent)
{
  shared_ptr<Binding<AST> > bndg = fromEnv->doGetBinding(fromName);

  if (bndg->flags & BF_PRIVATE)
    return false;
      
  shared_ptr<AST> ast = bndg->val;

  std::string s = toIdent->s;
  if (toPfx.size())
    s = toPfx + FQName::sep + s;

  toEnv->addBinding(s, ast);
  toEnv->setFlags(s, BF_PRIVATE|BF_COMPLETE);

  return true;
}
#endif

static void
bindIdentDef(shared_ptr<AST> ast, shared_ptr<ASTEnvironment > env, 
             IdentType identType, shared_ptr<AST> currLB,
             ResolverFlags rflags)
{  
  if (ast->isIdentType(id_tvar)) {
    env->addDefBinding(ast->s, ast);
    env->setFlags(ast->s, BF_COMPLETE | BF_NO_MERGE);

    assert(currLB);
    ast->tvarLB = currLB;   
  }
  else
    env->addBinding(ast->s, ast);

  // Type (arguments) Variables are not bound to incompleteness
  // restriction  I believe the following check is sufficient.
  // If there is problem, I will have to pass around an additional
  // bool addToIncomplete parameter, or at the caller, add them to a
  // dummy list.
  
  if (ast->identType != id_unresolved) 
    assert(ast->isIdentType(identType));
  else 
    ast->identType = identType;
  
//   cout << "Added "<< ast->s << " at " << ast->loc 
//        <<" with idType = " 
//        << identTypeToString(identType)
//        << std::endl;

}

static void
markComplete(shared_ptr<ASTEnvironment > env)
{
  for (ASTEnvironment::iterator itr = env->begin();
      itr != env->end(); ++itr)
    itr->second->flags |= BF_COMPLETE;
}

#define REWRITE_AST_IN_PLACE(_to, _from)          \
  do {                                            \
    shared_ptr<AST> to = (_to);                   \
    shared_ptr<AST> from = (_from);               \
                                                  \
    to->s = from->s;                              \
    to->astType = from->astType;                  \
    to->identType = from->identType;              \
    to->flags = from->flags;                      \
    to->children = from->children;                \
    to->fqn = from->fqn;                          \
  } while (0)

//WARNING: **REQUIRES** answer and errorFree.
// Carries aliasEnv passively throughout, as if closed over.
#define RESOLVE(ast,env,lamLevel,mode,identType,currLB,flags)         \
  do {                                                                \
    bool answer =                                                     \
      resolve(errStream, (ast), aliasEnv, (env), (lamLevel),          \
              (mode), (identType), (currLB), (flags));                \
    if (answer == false)                                              \
      errorFree = false;                                              \
  } while (0)


/// @brief The symbol resolver.
///
/// Traverses @p ast, keeping track of the current environment @p env,
/// the alias environment @p aliasEnv, the environment @p lamLevel
/// that was in effect at the most recently enclosing lambda form, and
/// a pointer to the AST of the most recently enclosing let binding
/// form.
///
/// The @p mode determines whether we are defining, declaring, or
/// using identifiers at any given moment (see ResolutionMode). In
/// DEF_MODE and DECL_MODE, identifiers will be added to the current
/// environment. In USE_MODE, identifiers will be looked up. 
/// 
/// LOCAL_MODE AND TYPE_MODE are not used by the resolver.
///
/// Various context-dependent constraints on the resolver are dictated
/// by @p flags (see ResolverFlags).
///
/// The expected/required identifier classification is carried
/// downward through @p identType. In DEF_MODE, @p identType specifies
/// the identifier class to be defined. In USE_MODE, it specifies the
/// type of identifier that may legally occur. WHAT ABOUT IN
/// DECL_MODE?
bool
resolve(std::ostream& errStream, 
        shared_ptr<AST> ast, 
        shared_ptr<ASTEnvironment > aliasEnv,
        shared_ptr<ASTEnvironment > env,
        shared_ptr<ASTEnvironment > lamLevel,
        ResolutionMode mode, 
        IdentType identType,
        shared_ptr<AST> currLB,
        ResolverFlags flags)
{
  bool errorFree = true;

  // Save the current environment in the AST.
  // If we create a new environment, we will update it later.
  ast->envs.env = env;

  //     errStream << "RES: " << ast->loc << ": " 
  //                 << ast->s << "[" << ast->astTypeName() << "]" 
  //                 << "   identType = " << identTypeToString(identType)
  //                 << " IncompleteOK = " 
  //                 << ((flags & RSLV_INCOMPLETE_OK)? "true" : "false")
  //                 << std::endl;  
  
  switch(ast->astType) {

  case at_Null:
  case at_boxedCat:
  case at_unboxedCat:
  case at_opaqueCat:
  case agt_category:
  case at_AnyGroup:
  case agt_literal:
  case agt_tvar:
  case agt_var:
  case agt_definition:
  case agt_type:
  case agt_expr:
  case agt_expr_or_define:
  case agt_eform:
  case agt_type_definition:
    //case agt_reprbodyitem:
  case agt_value_definition:
  case agt_uselhs:
  case at_letbindings:
  case at_loopbindings:
  case at_loopbinding:
  case agt_CompilationUnit:
  case agt_tc_definition:
  case agt_if_definition:
  case agt_ow:
  case at_localFrame:
  case at_frameBindings:
  case at_identList:
  case agt_qtype:
  case agt_fielditem:
  case at_defrepr:
    //case at_reprbody:
    //case at_reprcase:
    //case at_reprcaselegR:
    //case at_reprtag:
  case at_oc_closed:
  case at_oc_open:
  case agt_openclosed:
  case at_reprctrs:
  case at_reprctr:
  case at_reprrepr:
  case at_docString:
  case at_letGather:
  case agt_ucon:
    {
      errStream << ast->loc << ": Internal Compiler Error. " 
                << "Function resolve, unexpected astType: " 
                << ast->tagName()
                << std::endl;
      
      errorFree = false;
      break;
    }

  case at_boolLiteral:
  case at_charLiteral:
  case at_intLiteral:
  case at_floatLiteral:
  case at_stringLiteral:
    break;
    
  case at_ident:
    {
      // If you change the way this works, see the comment in the
      // at_usesel case first!
      if (!ast->fqn.isInitialized()) {
        if (ast->isGlobal()) {
          ast->fqn = FQName(env->uocName, ast->s);
          ast->flags |= ID_IS_PRIVATE;
        }
        else
          ast->fqn = FQName(FQ_NOIF, ast->s);
      }

      switch(mode) {
        // DEF_MODE is mainly for top level. It can be a:
        // type name
        // value
        // constructor
        // field name
        // module/interface name
        // Individial cases distinguished using identType

      case DEF_MODE:
        {
          assert(env);
          assert(identType != id_unresolved);
          
          shared_ptr<AST> sym = env->getBinding(ast->s);
        
          if (sym) {
            if (sym->isDecl) {
              if ((sym->fqn.iface != ast->fqn.iface) &&
                  !providing(aliasEnv, sym->fqn)) {
                // We are defining an ident that has an existing
                // binding that came about through import. Confirm
                // that we also marked it as providable:

                errStream << ast->loc << ": " 
                          << ast->s
                          << " aliases an interface symbol that"
                          << " is not being provided."
                          << std::endl;
                errorFree = false;
                break;
                
              }

              ast->identType = identType;
              if (!ast->isIdentType(sym->identType)) {
                errStream << sym->loc << ": " 
                          << ast->s << " is declared here as "
                          << identTypeToString(sym->identType)
                          << std::endl
                          << ast->loc
                          << " and defined here as " 
                          << identTypeToString(identType)
                          << std::endl;
                errorFree = false;
                break;
              }
              
              sym->identType = ast->identType;
              sym->defn = ast;
              ast->decl = sym;
              env->removeBinding(ast->s);
              
              if (sym->externalName.size()) {
                ast->externalName = sym->externalName;
              }
            }
            else if (ast->isGlobal()) {
              errStream << ast->loc << ": Redefinition of Symbol " 
                        << ast->s << ". Already defined at " 
                        << sym->loc << "." << std::endl;
              errorFree = false;
              break;
            }
          }
          
          assert(!ast->isDecl);
          bindIdentDef(ast, env, identType, currLB, flags);
          break;
        }
      
      case DECL_MODE:
        {
          assert(env);
          assert(identType != id_unresolved);

          ast->isDecl = true;

          shared_ptr<AST> sym = env->getBinding(ast->s);
          
          if (sym) {            
            if (!sym->isDecl) {
              // Will not be necessary in the new Polyinstantiator
              if ((flags & RSLV_NO_RESOLVE_DECL) == 0) {
                errStream << ast->loc << ": Symbol " 
                          << ast->s << " already defined at " 
                          << sym->loc << "." << std::endl;
                errorFree = false;
                break;
              }
            }
            
            if (!sym->isIdentType(identType)) {
              errStream << sym->loc << ": " 
                        << ast->s << " is declared/defined here as "
                        << identTypeToString(sym->identType)
                        << std::endl
                        << ast->loc
                        << " and here as " 
                        << identTypeToString(identType)
                        << std::endl;
              errorFree = false;
              break;
            }
            ast->identType = sym->identType;

            if (ast->externalName.size()) {
              assert(ast->flags & DEF_IS_EXTERNAL);
              if (sym->externalName.size()) {
                if (sym->externalName != ast->externalName) {
                  errStream << ast->loc << ": " 
                            << ast->s << " Identifier was previously "
                            << " declared with a different external name. "
                            << "Name here: " << ast->externalName
                            << "Previous name: " << sym->externalName
                            << std::endl;
                  errorFree = false;
                  break;
                }  
              }
              else {
                sym->externalName = ast->externalName;
              }
            }
            else {
              if (sym->externalName.size()) {
                ast->externalName = sym->externalName;
              }
            }
            
            /* This is a compatible declaration, do nothing */
          }
          else
            bindIdentDef(ast, env, identType, currLB, flags);
          break;
        }
       
      case USE_MODE:
        {
          assert(env);
          assert((flags & RSLV_NO_CHK_USE_TYPE) || 
                 (identType != id_unresolved));
          
          // When resolving a label, must find it within current
          // lambda body or no joy.
          
          ast->symbolDef = env->getBinding(ast->s);
          
          //         if (ast->symbolDef == NULL) 
          //           cout << " Symdef is NULL for " << ast->s << endl;
          
          if (!ast->symbolDef) {
            // If there is a type-variable, it must be treated as though
            // it is a defining occurrence in order to support things
            // like:
            //
            // (define x:'a 100)
            //
            // However, a further caveat is that a type-variable should
            // be considered as a defining occurance ONLY for its first
            // opccurance in a defining scope.  This is to support
            // definitions like:
            //
            //   (lambda x:'a (lambda y:'a 0))
            //
            // That is, a type-variable in this slot should be treated
            // as a defining occurance ONLY if it is NOT ALREADY bound
            // in the current scope.  Also, it should only be done in
            // some cases -- the cases where RSLV_NEW_TV_OK is set in
            // flags. Otherwise, things like: (defunion list:ref (Next
            // 'a)) will also resolve.
        
             if (ast->isIdentType(id_tvar) &&
               ((flags & RSLV_NEW_TV_OK) || 
                (ast->flags & TVAR_POLY_SPECIAL))) {
              bindIdentDef(ast, env, identType, currLB, flags);
              ast->symbolDef = ast;
              //errStream << "Created new ident for " << ast->s
              //              << " identType = " 
              //              << identTypeToString(ast->identType)
              //              << std::endl;
            }
            else {
              errStream << ast->loc << ": Identifier `"
                        << ast->s << "' used here, Undefined." 
                        << std::endl;
              
              //errStream << "Available bindings are: "
              //          << env->asString()
              //           << std::endl;
              
              errorFree = false;
              break;
            }
          }
          
          assert(ast->symbolDef);

          // If we are resolving a use occurrence of a label for
          // labeled escape, check that we did not cross a lambda
          // boundary.
          if ((identType == id_block) &&
              !env->getBinding(ast->s, lamLevel)) {
            errStream << ast->loc << ": Escape to "
                      << ast->s << "' must not cross lambda boundary."
                      << std::endl;
            errorFree = false;
            break;
          }

          shared_ptr<AST> def = ast->symbolDef;

          if ((flags & RSLV_USE_ONLY_PUBLIC) && 
             ((env->getFlags(ast->s) & BF_PRIVATE))) {
            errStream << ast->loc << ": Identifier `"
                      << ast->s << "' used here, But NO public definition "
                      << "found." << std::endl;
            errorFree = false;
            break;
          }
        
          if (((flags & RSLV_NO_CHK_USE_TYPE) == 0) && 
             (!def->isIdentType(identType))) {
            errStream << ast->loc << ": " << identTypeToString(identType) 
                      << " `" << ast->s << "' Undefined"
                      << " [But there is a " 
                      << identTypeToString(def->identType) 
                      << " defined]" << std::endl;
            errorFree = false;
          }
        
          bool ICRviolation = false;
          // The closure-conversion pass introduces some carefully 
          // crafted code using internal constructors that appear
          // incomplete the following restrictions. Since there cannot
          // be any incompleteness errors at that point,
          // incompleteness checking is disabled. otherwise,
          // incompleteness checking proceeds as usual.
          if (((flags & RSLV_INCOMPLETE_NO_CHK) == 0) &&
              ((env->getFlags(ast->s) & BF_COMPLETE) == 0)) {
            if ((flags & RSLV_INCOMPLETE_OK) == 0) {
              // If usage of an Incomplete variable is NOT OK, 
              // there is a violation, and we are done.
              ICRviolation = true;
            }
            else {                
              if (def->isIdentType(id_value)) {
                assert(lamLevel);
                  
                // This is a little subtle.
                // In cases like:
                // (define main3 (lambda () 
                //                       (letrec ((x x))
                //                          x))
                // Usage of x in the letrec should not be allowed
                // even though we are in a lambda.
                // However, letrec cannot disable RSLV_INCOMPLETE_OK 
                // because, it is still OK to use main3 inside
                // the letrec expression
                // So, here, if the RSLV_INCOMPLETE_OK is true,
                // we still check that the identifier is defined at 
                // the right LAMBDA LEVEL.            
                  
                if (!lamLevel->getBinding(ast->s))
                  ICRviolation = true;
              }
            }
          }

          if (ICRviolation) {
            errStream << ast->loc << ": Usage of Identifier `"
                      << ast->s << "' Violates Incompleteness Restriction." 
                      << std::endl;
            errorFree = false;
          }
      
          if (def->flags & ID_FOR_USWITCH) {
            if ((flags & RSLV_SWITCHED_ID_OK) == 0) {
              errStream << ast->loc << ": The identifier `"
                        << ast->s << "' can only appear to the left of a `.'" 
                        << std::endl;
              errorFree = false;
            }
            else if (flags & RSLV_WITHIN_CATCH_MC) {
              errStream << ast->loc << ": The identifier `"
                        << ast->s << "' cannot be used while"
                        << " catching multiple exceptions."
                        << std::endl;
              errorFree = false;              
            }
          }

          ast->identType = def->identType;
          ast->flags  |= def->flags;
          ast->flags |= def->flags;
          ast->flags  &= ~MASK_FLAGS_FROM_USE;
          ast->externalName = def->externalName;

          /* Make sure tvars are scoped properly */
          if (def->isIdentType(id_tvar)) {                        
            assert(currLB);
            
            shared_ptr<AST> thisLB = GC_NULL;
            if (def->tvarLB->envs.env->isAncestor(currLB->envs.env))
              thisLB = currLB;
            else
              thisLB = def->tvarLB;
            
            while (thisLB->flags & LBS_PROCESSED) {
              thisLB = thisLB->parentLB;
              assert(thisLB);
            }
            
            def->tvarLB = thisLB;                    
          }

          //         cout << "Resolved " << ast->s << " at " << ast->loc
          //              << " to " << def->loc
          //              << " addr = " << &(*def)
          //              <<endl;
          break;
        }

      default:
        {
          assert(false);
          break;
        }
      }

      break;
    }
    
  case at_usesel:
    {
      /// @paragraph hygienic-symbols Hygienic Symbol Strategy
      ///
      /// Hygienic symbols are decidedly odd. Syntactically they look
      /// like structure references, but the LHS is actually a local
      /// identifier that names an <em>environment</em>. More
      /// precisely, it names a local copy of the imported interface's
      /// environment that contains only the public symbols of that
      /// interface.
      ///
      /// We handle this through an outright kludge. At the point of
      /// hygienic import, we bind both the ifalias (for uniqueness)
      /// and all of the imported identifiers. The imported
      /// identifiers get bound in the local namespace using
      /// "ifalias.ifident" (note that '.' is not a legal identifier
      /// character, so this cannot be matched otherwise).
      ///
      /// At defining and use occurrences, we recognize the selector
      /// pattern in the parser and generate an at_usesel AST node for
      /// it. We then arrive here, where we re-write the at_usesel
      /// node IN PLACE, rewriting it into an at_ident node whose name
      /// is "ifalias.ident", where "ifalias" is our local name for
      /// the interface's public environment. We then recurse by hand
      /// **on the same node** (the one that we just re-wrote) to get
      /// the symbol resolved.
      ///
      /// The only problem with that is that the recursive resolution
      /// is going to give us an FQN whose @p iface component is the
      /// current UoC and whose @p ident component is our forged
      /// "ifalias.ident". Ultimately, the at_usesel is actually an
      /// <em>alias</em> of the other symbol, so its FQN needs to be
      /// the FQN of the external symbol. We ensure this by whacking
      /// it back to the right thing <em>after</em> the hand-recursing
      /// resolution returns.
      ///
      /// Finally, note that BeginSimp has done a rewrite of all local
      /// at_define and at_recdef forms into at_let and at_letrec
      /// forms, respectively. That pass has checked that no at_usesel
      /// can be bound by a local definition. I do wonder if perhaps
      /// we should not catch this case syntactically in the parser.

      /// @bug There is a problem here that this syntax might now be:
      ///
      ///   StructTypeName.methodname
      ///   ifname.StructTypeName.methodName
      ///
      /// neither case is currently handled, and both need to be.
      /// It may turn out to be better to handle this case specially.

      shared_ptr<AST> lhs = ast->child(0);
      {
        // at_usesel can now have at_usesel as a left-hand element, in
        // which case we have ifName.StructTypeName.methodName and we
        // need to recurse on the LHS first.

        if (lhs->astType == at_usesel) {
          RESOLVE(lhs, env, lamLevel, USE_MODE, id_interface, 
                  GC_NULL, flags);

          if (!errorFree)
            break;

          // ast->astType should now be at_ident.
          assert(lhs->astType == at_ident);
        }
      }

      RESOLVE(lhs, env, lamLevel, USE_MODE, idc_usesel_lhs, 
              GC_NULL, flags);

      if (!errorFree)
        break;

      if (lhs->identType == id_interface) {
        shared_ptr<AST> iface = lhs;
      
        stringstream lookupStream;
        lookupStream << ":" << iface->s << ":";
        std::string lookupName = lookupStream.str();
        shared_ptr<AST> ifNameAst= aliasEnv->getBinding(lookupName);
        if (!ifNameAst) {
          errStream << ast->loc << ": "
                    << "Identifier `"
                    << iface->s 
                    << "' is undefined. Are you missing the name on an import statement?"
                    << std::endl;
          errorFree = false;
          break;
        }

        std::string ifName = ifNameAst->s;
        assert(ifName != "");
      
        shared_ptr<ASTEnvironment > ifenv = iface->symbolDef->envs.env;
      
        if (!ifenv) {
          errStream << ast->loc << ": "
                    << "Internal Compiler Error. "
                    << "Interface " << ifName
                    << " needed by "<< iface->s << " has a NULL environment"
                    << std::endl;
          errorFree = false;
          break;
        }
      
        FQName importedFQN = FQName(ifName, ast->child(1)->s);
      
        ast->s = lhs->s + FQName::LocalBindingSep + ast->child(1)->s;
        ast->astType = at_ident;
        ast->identType = ast->child(1)->identType;
        ast->flags = ast->child(1)->flags;
        ast->flags |= ID_IS_GLOBAL;

        ast->children.clear();

        // SHOULD THE PUBLIC FLAG BE TAKEN OFF HERE ??
        RESOLVE(ast, env, lamLevel, mode, identType, currLB,  
                (flags & (~RSLV_BIND_PUBLIC)));

        ast->fqn = importedFQN;
      }
      else if ((lhs->identType == id_struct) ||
               (lhs->identType == id_object)) {
        ast->s = lhs->s + "." + ast->child(1)->s;
        ast->astType = at_ident;
        ast->identType = ast->child(1)->identType;
        ast->flags = ast->child(1)->flags;
        ast->flags |= ID_IS_GLOBAL;

        ast->children.clear();

        RESOLVE(ast, env, lamLevel, mode, identType, currLB, 
                flags);

        break;
      }
      else {
        errStream << ast->loc << ": "
                  << "Required struct name or interface name to"
                  << " the left of this dot."
                  << std::endl;
        errorFree = false;
        break;
      }

      break;
    }

  case at_interface:
    {
      flags |= RSLV_IS_INTERFACE;

      // match agt_definition*
      for (size_t c = 1; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, DEF_MODE, identType, 
                GC_NULL, flags);

      // In an interface unit, we do not check for unresolved
      // references, because they will (hopefully) be satisfied by
      // providing modules.

      break;
    }

  case at_module:
    {
      // match agt_definition*
      for (size_t c = 0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, DEF_MODE, identType, 
                GC_NULL, flags);

      if ((flags & RSLV_NO_RESOLVE_DECL) == 0)        
        CHKERR(errorFree, warnUnresRef(errStream, ast, env));

      break;
    }

  case at_defunion:
    {
      shared_ptr<ASTEnvironment > tmpEnv = env->newDefScope();
      ast->envs.env = tmpEnv;

      shared_ptr<AST> category = ast->child(2);

      // match at_ident
      RESOLVE(ast->child(0), tmpEnv, lamLevel, DEF_MODE, 
              id_union, ast, 
              (flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK)) | RSLV_BIND_PUBLIC);
      if (category->astType == at_boxedCat)
        tmpEnv->setFlags(ast->child(0)->s, BF_COMPLETE);

      // match at_tvlist
      RESOLVE(ast->child(1), tmpEnv, lamLevel, DEF_MODE, 
              id_tvar, ast,
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));

      // category keyword at child(2)

      // match at_declares
      RESOLVE(ast->child(3), tmpEnv, lamLevel, NULL_MODE,
              id_unresolved, ast,              
              ((flags & (~RSLV_NEW_TV_OK)) & (~RSLV_INCOMPLETE_OK)) | RSLV_WITHIN_DEFUNION);

    
      // match at_constructors
      RESOLVE(ast->child(4), tmpEnv, lamLevel, DEF_MODE,
              idc_uctor, ast,               
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));

      // match at_constraints
      RESOLVE(ast->child(5), tmpEnv, lamLevel, USE_MODE, 
              idc_type, ast, 
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));
      
      env->mergeBindingsFrom(tmpEnv);
      break;
    }

  case at_defstruct:
  case at_defobject:
  case at_defexception:
    {
      shared_ptr<ASTEnvironment > tmpEnv = env->newDefScope();
      ast->envs.env = tmpEnv;

      shared_ptr<AST> category = ast->child(2);
      shared_ptr<AST> fields = ast->child(4);

      if (ast->astType == at_defexception &&
          category->astType != at_boxedCat) {
        errStream << ast->loc << ": "
                  << "exception " << ast->child(0)->s
                  << ": Exception definitions must be boxed."
                  << std::endl;
        errorFree = false;
      }              

      IdentType identType;
      switch(ast->astType) {
      case at_defstruct:
        identType = id_struct;
        break;
      case at_defobject:
        identType = id_object;
        break;
      case at_defexception:
        identType = (fields->children.size() == 0) ? id_ucon0 : id_ucon;
        break;
      }

      // match at_ident
      RESOLVE(ast->child(0), tmpEnv, lamLevel, DEF_MODE, 
              identType, ast,
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK) | RSLV_BIND_PUBLIC);
      if (category->astType == at_boxedCat)
        tmpEnv->setFlags(ast->child(0)->s, BF_COMPLETE);
      
      // match at_tvlist
      RESOLVE(ast->child(1), tmpEnv, lamLevel, DEF_MODE, 
              id_tvar, ast, 
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));
    
      // category keyword at child(2)

      // match at_declares
      RESOLVE(ast->child(3), tmpEnv, lamLevel, NULL_MODE, 
              id_unresolved, ast, 
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));
      
      // match at_fields
      RESOLVE(ast->child(4), tmpEnv, lamLevel, DEF_MODE, 
              id_field, ast, 
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));
 
      // match at_constraints
      RESOLVE(ast->child(5), tmpEnv, lamLevel, USE_MODE, 
              idc_type, ast, 
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));
      
      env->mergeBindingsFrom(tmpEnv);
      break;
    }
    
  case at_declunion:
  case at_declstruct:
  case at_declrepr:
    {
      shared_ptr<ASTEnvironment > tmpEnv = env->newDefScope();
      ast->envs.env = tmpEnv;

      IdentType it = ((ast->astType == at_declstruct) ?
                      id_struct : id_union);
      
      // match at_ident
      RESOLVE(ast->child(0), tmpEnv, lamLevel, DECL_MODE, 
              it, ast, 
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK)
              | RSLV_BIND_PUBLIC);
    
      // match at_tvlist
      RESOLVE(ast->child(1), tmpEnv, lamLevel, DEF_MODE, 
              id_tvar, ast, 
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));

      // category keyword at child(2)
      // empty declares at child(3)
      // empty fields/ctors at child(4)
      
      // match at_constraints
      RESOLVE(ast->child(5), tmpEnv, lamLevel, USE_MODE, 
              idc_type, ast, 
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));
      
      env->mergeBindingsFrom(tmpEnv);
      break;
    }

  case at_proclaim:
    {
      shared_ptr<ASTEnvironment > tmpEnv = env->newDefScope();
      ast->envs.env = tmpEnv;

      // match at_ident
      RESOLVE(ast->child(0), tmpEnv, lamLevel, DECL_MODE, 
              idc_value, ast, 
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK)
              | RSLV_BIND_PUBLIC);
   
      // match at_type
      RESOLVE(ast->child(1), tmpEnv, lamLevel, USE_MODE, 
              idc_type, ast, 
              flags | (RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));
      
      // match at_constraints
      RESOLVE(ast->child(2), tmpEnv, lamLevel, USE_MODE, 
              idc_type, ast, 
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));
      
      env->mergeBindingsFrom(tmpEnv);
      break;
    }

  case at_recdef:
  case at_define:
    {
      /// Note that BeginSimp has re-written all local define and
      /// recdef forms into at_let and at_letrec, respectively, so if
      /// we see at_define or at_refdef here, it is a top-level form.

      shared_ptr<ASTEnvironment > tmpEnv = env->newDefScope();
      ast->envs.env = tmpEnv;

      /* Mark the present identifier "not mutable yet". If we see a
      set!, this will get fixed. This must be cleared here because, the
      marking may not hold true after a certain pass. For example,
      after refization by the Closure converter, the identifier is no
      longer mutable. It is the thing that is pointed to, that is
      mutable.  */
      assert(ast->child(0)->astType == at_identPattern);
      ast->child(0)->child(0)->flags &= ~ID_IS_MUTATED;
      
      if (ast->astType == at_recdef) {
        // Binding patterns must be in scope.
        // match agt_bindingPattern
        RESOLVE(ast->child(0), tmpEnv, lamLevel, DEF_MODE, 
                id_value, ast, 
                flags | (RSLV_NEW_TV_OK) | RSLV_BIND_PUBLIC);
      }

      // match agt_expr
      RESOLVE(ast->child(1), tmpEnv, lamLevel, USE_MODE, 
              idc_value, ast, 
              flags | (RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));
      
      if (ast->astType == at_define) {
        // Binding patterns not in scope within expr, so handle them
        // later.
        // match agt_bindingPattern
        RESOLVE(ast->child(0), tmpEnv, lamLevel, DEF_MODE, 
                id_value, ast, 
                flags | (RSLV_NEW_TV_OK) | RSLV_BIND_PUBLIC);
      }

      // match at_constraints
      RESOLVE(ast->child(2), tmpEnv, lamLevel, USE_MODE, 
              idc_type, ast, 
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));    
      
      /* Mark the present identifier closed wrt mutability */
      ast->child(0)->child(0)->flags |= ID_MUT_CLOSED;

      env->mergeBindingsFrom(tmpEnv);
      break;
    }

  case at_deftypeclass:
    {
      shared_ptr<ASTEnvironment > tmpEnv = env->newDefScope();
      ast->envs.env = tmpEnv;

      // match at_ident
      RESOLVE(ast->child(0), tmpEnv, lamLevel, DEF_MODE, 
              id_typeclass, ast,
              (flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK)
               | RSLV_BIND_PUBLIC));
      
      // match at_tvlist
      RESOLVE(ast->child(1), tmpEnv, lamLevel, DEF_MODE, 
              idc_type, ast, 
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));


      // match at_tc_decls
      RESOLVE(ast->child(2), tmpEnv, lamLevel, USE_MODE, 
              idc_type, ast, 
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));
      
      // Skip the open/closed node.

      // match at method_decls
      RESOLVE(ast->child(4), tmpEnv, lamLevel, USE_MODE, 
              idc_type, ast, 
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));      

      // match at constraints
      RESOLVE(ast->child(5), tmpEnv, lamLevel, USE_MODE, 
              idc_type, ast, 
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));      

      tmpEnv->setFlags(ast->child(0)->s, BF_COMPLETE);
      env->mergeBindingsFrom(tmpEnv);
      break;
    }

  case at_tcdecls:
    {
      // match at agt_tcdecl+ 
      for (size_t c = 0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, 
                USE_MODE, identType, currLB,  flags);
      
      break;
    }

  case at_tyfn:
    {
      // match at_tvlist
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_type, currLB, flags);
      
      // match agt_tvar
      RESOLVE(ast->child(1), env, lamLevel, USE_MODE, 
              idc_type, currLB, flags);  
      break;
    }

  case at_tcapp:
    {
      // match at agt_var
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              id_typeclass, currLB, flags);

      // match at agt_type+
      for (size_t c = 1; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, USE_MODE, 
                idc_type, currLB, flags);

      break;
    }

  case at_method_decls:
    {
      // match at at_method_decl+
      for (size_t c = 0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, 
                DEF_MODE, id_tcmethod, currLB, flags);
      
      break;
    }

  case at_method_decl:
    {
      // match at at_ident
      RESOLVE(ast->child(0), env, lamLevel, DEF_MODE, 
              id_tcmethod, currLB,
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK)
              | RSLV_BIND_PUBLIC);
      
      // match at at_fn
      RESOLVE(ast->child(1), env, lamLevel, USE_MODE, 
              idc_type, currLB, flags);
      
      break;
    }

  case at_definstance:
    {      
      shared_ptr<ASTEnvironment > tmpEnv = env->newDefScope();
      ast->envs.env = tmpEnv;

      // match at at_tcapp
      RESOLVE(ast->child(0), tmpEnv, lamLevel, 
              USE_MODE, idc_type, ast, flags | RSLV_NEW_TV_OK);

      /// If the type class is marked ":closed", then it is required
      /// that all instance definitions appear in the same interface or
      /// module as the type class definition. 
      ///
      /// The exception to this is that certain constraints that are
      /// incestuously known to the compiler are defined in the
      /// prelude as :closed but are extended at need by the
      /// implementation. Examples of this include

      if (errorFree) {
        shared_ptr<AST> tcName = ast->child(0)->child(0);
        shared_ptr<AST> tcSym = tcName->symbolDef;
        shared_ptr<AST> tcDef = tcSym->defForm;
        bool isClosed = (tcDef->child(3)->astType == at_oc_closed);

        if (isClosed && tcSym->fqn.iface != env->uocName) {
          errStream << ast->loc
                    << ": Type class " << tcSym->fqn
                    << " is closed. Instances may not be defined by other"
                    << " modules or interfaces."
                    << std::endl;
          errorFree = false;
        }
      }

      // match at at_tcmethods
      RESOLVE(ast->child(1), tmpEnv, lamLevel, 
              USE_MODE, idc_value, ast, flags);
      
      // match at at_constraints
      RESOLVE(ast->child(2), tmpEnv, lamLevel, 
              USE_MODE, idc_type, ast, flags | RSLV_NEW_TV_OK);
      break;
    }

  case at_tcmethods:
    { 
      // match at expr+
      for (size_t c = 0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, 
                USE_MODE, idc_value, currLB, flags);
      
      // Make sure that each method is defined at most once (no
      // duplicates). 
      
      //std::cerr << ast->loc << ": Processing :" 
      //            << ast->asString() << std::endl;
      
      for (size_t i = 0; i < ast->children.size(); i++) {
        shared_ptr<AST> ith_bind = ast->child(i);

        for (size_t j = i+1; j < ast->children.size(); j++) {
          shared_ptr<AST> jth_bind = ast->child(j);

          if(ith_bind->child(0)->s == jth_bind->child(0)->s) {
            errStream << jth_bind->loc 
                      << ": Duplicate method definition for "
                      << ith_bind->child(0)->s
                      << std::endl;
            errorFree = false;
            break;
          }
        }
      }
      
      // Checking that all methods are being defined, and that only
      // those methods belonging to a particular type class are
      // defined is done by the type system. We don't have access to
      // the typeclass's definition AST (the identifier's symbolDef
      // points to the typeclass identifier) here, and the type system
      // already has a readily constructed structure to handle this.

      break;
    }
    
  case at_tcmethod_binding:
    { 
      // match at expr+
      for (size_t c = 0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, 
                USE_MODE, idc_value, currLB, flags);
      break;
    }

  case at_ifident:
    break;
    
  case at_provide:
    {
      shared_ptr<AST> ifAst = ast->child(0);
      if (!importIfBinding(errStream, aliasEnv, ifAst)) {
        errStream << ast->loc << ": "
                  << "Unable to provide interface `"
                  << ifAst->s
                  << "' - interface file not found. "
                  << std::endl;
        errorFree = false;
        break;
      }

      shared_ptr<ASTEnvironment > ifEnv = ifAst->envs.env;

      for (size_t i = 1; i < ast->children.size(); i++) {
        shared_ptr<AST> provideName=ast->child(i);
        shared_ptr<Binding<AST> > bndg = ifEnv->doGetBinding(provideName->s);
        if (!bndg) {
          errStream << ast->loc << ": "
                    << provideName->s
                    << " not found in interface "
                    << ifAst->s
                    << std::endl;
          
          errorFree = false;
          break;
        }

        if (!bndg->val->isDecl) {
          errStream << ast->loc << ": Cannot provide "
                    << provideName->s
                    << " in interface "
                    << ifAst->s
                    << ", which is defined in the interface."
                    << std::endl;
          
          errorFree = false;
          break;
        }

        bndg->flags |= BF_PROVIDING;
      }

      break;
    }    

  case at_importAs:
    {
      shared_ptr<AST> ifAst = ast->child(0); 
      shared_ptr<AST> idAst = ast->child(1);

      if (!importIfBinding(errStream, aliasEnv, ifAst)) {
        errStream << ast->loc << ": "
                  << "Unable to import interface `"
                  << ifAst->s
                  << "' - interface file not found. "
                  << std::endl;
        errorFree = false;
        break;
      }

      // import ident ifname
      shared_ptr<ASTEnvironment > tmpEnv = env->newScope();
        //        new ASTEnvironment(ifAst->envs.env->uocName);

      ast->envs.env = tmpEnv;
      
      if (ifAst->s == env->uocName) {
        errStream << ast->loc << ": "
                  << "Cannot import an undefined interface. "
                  << std::endl;
        
        errorFree = false;
        break;
      }
      
      RESOLVE(idAst, tmpEnv, lamLevel, DEF_MODE,
              id_interface, GC_NULL, flags);

      // For every (importAs local global) add to the alias
      // environment, the mapping :local: -> ifAst which 
      // holds the interface name. This is consulted from the
      // at_usesel case in order to obtain the real name of the
      // interface for purposes of error diagnostics, and setting the
      // FQN correctly.
      // This handling is in liu of using the now retired ifName field
      // on the idAst AST.
      stringstream ifNameMap;
      ifNameMap << ":" << idAst->s << ":";
      std::string ifNameMapStr = ifNameMap.str();

      aliasEnv->addBinding(ifNameMapStr, ifAst);
      aliasEnv->setFlags(ifNameMapStr, BF_PRIVATE);

      // The interface name must not be exported
      env->setFlags(idAst->s,
                    ((env->getFlags(idAst->s)) |
                     BF_PRIVATE)); 
   
      idAst->envs.env = ifAst->envs.env;
      idAst->envs.gamma = ifAst->envs.gamma;
      idAst->envs.instEnv = ifAst->envs.instEnv;

      aliasPublicBindings(idAst->s, GC_NULL, idAst->envs.env, tmpEnv);
      env->mergeBindingsFrom(tmpEnv);
      break;
    }

  case at_import:
    {
      // from ifName alias+
      shared_ptr<ASTEnvironment > tmpEnv = env->newScope();
      ast->envs.env = tmpEnv;
     
      shared_ptr<AST> ifAst = ast->child(0);

      if (ifAst->s == env->uocName) {
        errStream << ast->loc << ": "
                  << "Cannot import an undefined interface. "
                  << std::endl;
        
        errorFree = false;
        break;
      }

      if (!importIfBinding(errStream, aliasEnv, ifAst)) {
        errStream << ast->loc << ": "
                  << "Unable to import interface `"
                  << ifAst->s
                  << "' - interface file not found. "
                  << std::endl;
        errorFree = false;
        break;
      }

      if (ast->children.size() == 1) {
        // This is an import-all form
        aliasPublicBindings(std::string(), aliasEnv, ifAst->envs.env, tmpEnv);
      }
      else {
        // Need to import only certain bindings
        for (size_t c = 1; c < ast->children.size(); c++) {
          shared_ptr<AST> alias = ast->child(c);
          shared_ptr<AST> localName = alias->child(0);
          shared_ptr<AST> pubName = alias->child(1);
        
          RESOLVE(pubName, ifAst->envs.env, lamLevel, USE_MODE,
                  id_unresolved, currLB, 
                  ((flags & (~RSLV_NEW_TV_OK))) | RSLV_NO_CHK_USE_TYPE);
        
          if (!errorFree)
            break;

          // Enforce the "one alias" rule.
          shared_ptr<Binding<AST> > bndg = 
            ifAst->envs.env->doGetBinding(pubName->s);

          std::string pubFQN = bndg->val->fqn.asString();

          shared_ptr<AST> oldAlias = aliasEnv->getBinding(pubFQN);
          if (oldAlias) {
            errStream << alias->loc << ": The public identifier "
                      << pubFQN
                      << ", being aliased here to " 
                      << pubName->s
                      << ", was previously aliased to "
                      << oldAlias->s
                      << " at "
                      << oldAlias->loc
                      << std::endl;
            errorFree = false;
            break;
          }
            
          aliasEnv->addBinding(pubFQN, localName);

          shared_ptr<AST> oldDef = env->getBinding(localName->s);
          if (oldDef) {
            errStream << alias->loc << ": Conflict for alias definition"
                      << localName->s
                      << ". Previously defined at "
                      << oldDef->loc
                      << std::endl;
            errorFree = false;
            break;
          }
        
          tmpEnv->addBinding(localName->s, pubName->symbolDef);
          tmpEnv->setFlags(localName->s, BF_PRIVATE);
        }
      }

      env->mergeBindingsFrom(tmpEnv);
      break;
    }
    
  case at_ifsel:
    {
      assert(false);
      break;
    }

  case at_declares:
    {
      // match at_declare*
      for (size_t c = 0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, NULL_MODE, identType,  
                currLB,  flags);

      break;
    }

  case at_declare:
    {
      // match at_ident
      // The first identifier has special meaning, and must be 
      // dealt with by hand.

      std::string declID = ast->child(0)->s;

      if (declID != "stateful"
          && declID != "tag") {
        errStream << ast->child(0)->loc 
                  << ": " 
                  << declID 
                  << " is not a recognized declare identifier"
                  << std::endl;
        errorFree = false;
      }

      if (declID == "stateful") {
        if (!(flags & RSLV_IS_INTERFACE)) {
          errStream << ast->child(0)->loc 
                    << ": Only Interfaces can be declared to be stateful"
                    << std::endl;
          errorFree = false;
        }
      }

      if (declID == "tag") {
        if (!(flags & RSLV_WITHIN_DEFUNION)) {
          errStream << ast->child(0)->loc 
                    << ": tag declaration can only occur within a defunion"
                    << std::endl;
          errorFree = false;
        } 
      }        
            
      // match agt_type?
      if (ast->children.size() > 1) {
        RESOLVE(ast->child(1), env, lamLevel, USE_MODE, 
                idc_type, currLB, flags);
      }

      break;
    }

  case at_tvlist:
    {
      // match agt_tvar*
      for (size_t c = 0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, mode, identType, 
                currLB, flags);

      break;
    }

  case at_constructors:
    // match at_constructor+
    { 
      std::string ifName="";
      bool varConst = false;
      
      // No problem with usesel here. select cannot appear in
      // this context.
      if (ast->child(0)->child(0)->astType == at_usesel) {
        ifName = ast->child(0)->child(0)->child(0)->s;
        varConst = true;
      }
      
      // FIX: Isn't this stale now? Aren't constructor names
      // now constrained to unqualified idents?
      for (size_t c = 0; c < ast->children.size(); c++) {
        if (varConst) {
          if (ast->child(c)->child(0)->astType != at_usesel ||
             ast->child(c)->child(0)->child(0)->s != ifName) {
            errStream << ast->child(c)->child(0)->loc << ": "
                      << " All Constructors of a Union declaration must "
                      << "belong to the same interface "
                      << std::endl;
            errorFree = false;
          }
        }
        else {
          if (ast->child(c)->child(0)->astType == at_usesel) {
            errStream << ast->child(c)->child(0)->loc << ": "
                      << " All Constructors of a Union declaration must "
                      << "belong to the same compilation Unit "
                      << std::endl;
            errorFree = false;
          }
        }
        
        RESOLVE(ast->child(c), env, lamLevel, NULL_MODE, identType, 
                currLB, flags);
      }
      break;
    }

  case at_constructor:
    {
      // match at_ident
      IdentType it = ((ast->children.size() > 1) ? 
                      id_ucon : id_ucon0);
      
      RESOLVE(ast->child(0), env, lamLevel, DEF_MODE,
              it, currLB, flags | RSLV_BIND_PUBLIC);
            
      pair< set<string>::iterator, bool > pr;
      set<string> names;

      // Resolve each field name, test for collisions:

      for (size_t c = 1; c < ast->children.size(); c++) {
        shared_ptr<AST> fld = ast->child(c);
        RESOLVE(fld, env, lamLevel, USE_MODE, identType,
                currLB, flags); 

        if (fld->astType != at_field)
          continue;
        
        pr = names.insert(fld->child(0)->s);
        if (!pr.second) {
          errStream << ast->loc << ": "
                    << "Duplicate field label: "
                    << fld->child(0)->s
                    << std::endl;
          errorFree = false;
        }
      }
      break;
    }

  case at_fields:
    {
      // match at_field*

      // Resolve each field and method name, test for collisions:
      pair< set<string>::iterator, bool > pr;
      set<string> names;

      for (size_t c = 0; c < ast->children.size(); c++) {
        shared_ptr<AST> fld = ast->child(c);
        RESOLVE(fld, env, lamLevel, USE_MODE, identType,
                currLB, flags); 

        if (fld->astType == at_fill)
          // This entry has no name.
          continue;
        
        pr = names.insert(fld->child(0)->s);
        if (!pr.second) {
          errStream << ast->loc << ": "
                    << "Duplicate field/method name: "
                    << fld->child(0)->s
                    << std::endl;
          errorFree = false;
        }
      }
      break;
    }

  case at_field:
  case at_methdecl:
    {
      // match at_ident
      ast->child(0)->fqn = FQName(FQ_NOIF, ast->child(0)->s);

      // match agt_type:
      RESOLVE(ast->child(1), env, lamLevel, USE_MODE, 
              idc_type, currLB, 
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));

      break;
    }

  case at_fill:
    {
      // match agt_type?
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_type, currLB, 
              flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));

      if(ast->children.size() == 2)
        RESOLVE(ast->child(1), env, lamLevel, USE_MODE, 
                idc_type, currLB, 
                flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));
      
      break;
    }

  case at_bitfieldType:
    {
      // match at_type
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_type, currLB, flags);

      // match at_intLiteral
      RESOLVE(ast->child(1), env, lamLevel, NULL_MODE, 
              identType, currLB,  
              flags & (~RSLV_INCOMPLETE_OK));
      break;
    }

  case at_arrayRefType:
  case at_byRefType:
    {
      // match agt_type
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_type, currLB, 
              flags | (RSLV_INCOMPLETE_OK));
      break;
    }

  case at_boxedType:
    {
      // match agt_type
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_type, currLB, 
              flags | (RSLV_INCOMPLETE_OK));
      break;
    }

  case at_unboxedType:
    {
      // match agt_type
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_type, currLB, 
              flags & (~RSLV_INCOMPLETE_OK));

      break;
    }

  case at_methType:
  case at_fn:
    {
      // match agt_type
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_type, currLB, flags);

      // match agt_type
      RESOLVE(ast->child(1), env, lamLevel, USE_MODE, 
              idc_type, currLB, flags);
    
      break;
    }

  case at_fnargVec:
    {
      // match agt_type*
      for (size_t c = 0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, USE_MODE, 
                idc_type, currLB, flags);

      break;
    }


  case at_primaryType:
  case at_fieldType:
    {
      break;
    }

  case at_arrayType:
    {
      // match agt_type
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_type, currLB, flags);

      // match at_intLiteral
      RESOLVE(ast->child(1), env, lamLevel, NULL_MODE, identType,
              currLB, flags); 
      if (ast->child(1)->litValue.i < 0) {
        errStream << ast->child(1)->loc << ": "
                  << "Array index cannot be negative." 
                  << std::endl;
        errorFree = false;
      }

      break;
    }

  case at_vectorType:
    {
      // match agt_type
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_type, currLB, flags);

      // match at_intLiteral?
      if (ast->children.size() > 1) {
        RESOLVE(ast->child(1), env, lamLevel, NULL_MODE, identType,
                currLB, flags); 
      }

      break;
    }

  case at_exceptionType:
    {
      break;
    }

  case at_dummyType:
    {
      break;
    }

  case at_mutableType:
  case at_constType:
    {
      // match agt_type
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_type, currLB, flags);

      break;
    }

  case at_typeapp:
    {
      // match agt_var agt_tvar+
      for (size_t c = 0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, USE_MODE, 
                idc_type, currLB, flags);

      break;
    }

  case at_qualType:
    {
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_type, currLB, flags);      
      RESOLVE(ast->child(1), env, lamLevel, USE_MODE, 
              idc_type, currLB, flags);      
      break;
    }

  case at_constraints:
    {
      for (size_t c=0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, USE_MODE, 
                idc_type, currLB, flags);
      break;
    }    
    
  case at_identPattern:
    {
      // match agt_var
      RESOLVE(ast->child(0), env, lamLevel, mode, identType,
              currLB, flags); 
      
      // Type Qualifications ONLY in Binding Patterns
      // match agt_type?
      if (ast->children.size() > 1) {
        RESOLVE(ast->child(1), env, lamLevel, USE_MODE, idc_type,
                currLB, flags); 
      }
    
      break;
    }

  case at_mixfix:
    {
      // First process the elements:
      for (size_t c = 0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, USE_MODE, idc_value,
                currLB, flags);


      // Now do the mixfix conversion:
      if (errorFree) {
        shared_ptr<AST> tree = ProcessMixFix(errStream, ast);

        if (tree) {
          // If we got back an at_apply node, we need to run the
          // resolver on the leading child, because that was inserted
          // by the mixfix converter and has not been resolved.
          //
          // Since it's harmless, we do this by re-running the
          // resolver on the resulting node so that the fixups for
          // builtins get run as well.
          if (tree->astType == at_apply)
            RESOLVE(tree, env, lamLevel, USE_MODE, 
                    idc_apply, currLB, flags);        

          // Finally, clobber the at_mixfix node in-place with the
          // processed result:
          REWRITE_AST_IN_PLACE(ast, tree);
        }
        else {
          // Mixfix layer parse error
          errorFree = false;
        }
      }

      break;
    }

  case at_typeAnnotation:
    {
      // match agt_eform
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_value, currLB, flags);

      RESOLVE(ast->child(1), env, lamLevel, USE_MODE, 
              idc_type, currLB, flags);
    
      break;
    }

  case at_suspend:
    {
      // match agt_var
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_value, currLB, flags);
      // match agt_eform
      RESOLVE(ast->child(1), env, lamLevel, USE_MODE, 
              idc_value, currLB, flags);
      break;
    }

  case at_unit:
    {
      break;
    }

  case at_allocREF:
    {
      // match at_type, at_fn+
      for (size_t c = 0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, USE_MODE, 
                idc_type, currLB, flags);
      break;
    }

  case at_mkClosure:
    {
      // match at_type, at_fn+
      for (size_t c = 0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, USE_MODE, 
                idc_value, currLB, 
                flags | RSLV_INCOMPLETE_OK);
      break;
    }

  case at_mkArrayRef:
    {
      // match at_expr
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_value, currLB, flags);
      break;
    }

  case at_copyREF:
  case at_setClosure:
    {
      // match at_type, at_fn+
      for (size_t c = 0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, USE_MODE, 
                id_value, currLB, flags);
      break;
    }
    

  case at_MakeVector:
    {
      // match agt_expr
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_value, currLB, flags);

      // match agt_expr
      RESOLVE(ast->child(1), env, lamLevel, USE_MODE, 
              idc_value, currLB, flags);

      break;
    }

  case at_array:
  case at_vector:
    {
      // match agt_expr+
      for (size_t c = 0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, USE_MODE, idc_value,
                currLB, flags); 

      break;
    }

  case at_begin:
    {
      // match agt_expr*
      for (size_t c = 0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, USE_MODE, idc_value,
                currLB, flags);

      break;
    }
    
  case at_labeledBlock:
    {
      RESOLVE(ast->child(0), env, lamLevel, DEF_MODE,
              id_block, currLB, flags);
      // I suspect that LABEL needs to be treated as a let boundary,
      // in the sense that definitions within label must not float 
      // outside it. Given this, is currLB correct?
      RESOLVE(ast->child(1), env, lamLevel, USE_MODE,
              idc_value, currLB, flags);
      break;
    }

  case at_return_from:
    {
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE,
              id_block, currLB, flags);
      RESOLVE(ast->child(1), env, lamLevel, USE_MODE,
              idc_value, currLB, flags);
      break;
    }

  case at_fqCtr:
    {
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_type, currLB, flags);
     
      // Second Component handled by the type checker
      break;
    }
    
  case at_sel_ctr:
    {
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_value, currLB, flags );

      // Second Component handled by the type checker
      break;
    }

  case at_select:
    {
      // match agt_expr
      shared_ptr<AST> lhs = ast->child(0);
      
      // CAREFUL: this might be a usesel
      if (lhs->astType == at_ident || lhs->astType == at_usesel) {
        // Strictly speaking, RSLV_SWITCHED_ID_OK should only be
        // passed if the lhs->astType is at_ident. It is harmless here
        // because if lhs->astType is at_usesel, we know by
        // construction that the LHS cannot possibly be a switched ID,
        // so the resolver cannot return a boogered answer in that
        // case.
        
        RESOLVE(lhs, env, lamLevel, USE_MODE, identType, currLB, 
                flags | RSLV_NO_CHK_USE_TYPE | RSLV_SWITCHED_ID_OK);
        
        // If ast->child(0) was an at_usesel, now it would have
        // turned into at_ident.
        if (lhs->isIdentType(id_interface)) {
          ast->astType = at_usesel;
          // Recursively call RESOLVE on ourselves to process the
          // at_usesel
          RESOLVE(ast, env, lamLevel, USE_MODE, identType, currLB, flags);
        } 
        else if (lhs->isIdentType(id_union)) {
          // This is UNIONNAME.LEGNAME.
          ast->astType = at_fqCtr;
          RESOLVE(ast, env, lamLevel, USE_MODE, idc_type, currLB, flags);
        }
        else if (!lhs->isIdentType(id_value)) {
          errStream << lhs->loc 
                    << ": Identifier "
                    << " `" << lhs->s << "'" 
                    << " is undefined." << std::endl;
          errorFree = false;
          break;
        }
      }
      else {
        RESOLVE(lhs, env, lamLevel, USE_MODE, idc_value, currLB, flags);
      }
      
      // match at_ident
      // This cannot be resolved. Defer to typing
      // RESOLVE(ast->child(1), ast->child(0)->subEnv, lamLevel, 
      // USE_MODE, id_field, flags);
    
      break;
    }

#ifdef HAVE_INDEXABLE_LENGTH_OPS
  case at_array_length:
  case at_array_ref_length:
  case at_vector_length:
    {
      // match agt_expr
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              id_value, currLB, flags);

      break;
    }
#endif

  case at_nth:
  case at_array_nth:
  case at_array_ref_nth:
  case at_vector_nth:
    {
      // match agt_expr
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              id_value, currLB, flags);

      // match agt_expr
      RESOLVE(ast->child(1), env, lamLevel, USE_MODE, 
              id_value, currLB, flags);

      break;
    }

  case at_lambda:
    {
      shared_ptr<ASTEnvironment > lamEnv = env->newScope();
      ast->envs.env = lamEnv;

      // match agt_bindingPatterns
      shared_ptr<AST> argVec = ast->child(0);
      for (size_t c = 0; c < argVec->children.size(); c++) {
        RESOLVE(argVec->child(c), lamEnv, lamEnv, DEF_MODE, 
                id_value, currLB, flags);
        shared_ptr<AST> id = argVec->child(c)->child(0);
        lamEnv->setFlags(id->s, BF_COMPLETE);
      }
      
      // match agt_expr
      RESOLVE(ast->child(1), lamEnv, lamEnv, USE_MODE, 
              idc_value, currLB, 
              flags | (RSLV_INCOMPLETE_OK));
      break;
    }

  case at_argVec:
    {
      assert(false);
      break;
    }
    
  case at_struct_apply:
  case at_object_apply:
  case at_ucon_apply: 
  case at_apply:
    {
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_apply, currLB, flags);        
      
      for (size_t c = 1; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, USE_MODE, 
                idc_value, currLB, flags);

      // Special-case fixup for AND and OR:
      if (ast->child(0)->s == "and") {
        shared_ptr<AST> node = 
          AST::make(at_and, ast->loc, ast->child(1), ast->child(2));

        REWRITE_AST_IN_PLACE(ast, node);
      }
      else if (ast->child(0)->s == "or") {
        shared_ptr<AST> node = 
          AST::make(at_and, ast->loc, ast->child(1), ast->child(2));

        REWRITE_AST_IN_PLACE(ast, node);
      }

      break;
    }

  case at_if:
    {
      // match agt_expr
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_value, currLB, flags);

      // match agt_expr
      RESOLVE(ast->child(1), env, lamLevel, USE_MODE, 
              idc_value, currLB, flags);

      // match agt_expr
      RESOLVE(ast->child(2), env, lamLevel, USE_MODE,
              idc_value, currLB, flags);

      break;
    }

  case at_when:
  case at_unless:
  case at_and:
  case at_or:
    {
      // match agt_expr+
      for (size_t c = 0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, USE_MODE, 
                idc_value, currLB, flags);

      break;
    }

  case at_cond:
    {
      // match at_cond_legs
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_value, currLB, flags);

      //match at_condelse
      RESOLVE(ast->child(1), env, lamLevel, USE_MODE, 
              idc_value, currLB, flags);
      break;
    }

  case at_cond_legs:
    {
      // match at_cond_leg+
      for (size_t c = 0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, USE_MODE, 
                idc_value, currLB, flags);

      break;
    }

  case at_cond_leg:
    {
      // match agt_expr
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_value, currLB, flags);
  
      // match agt_expr
      RESOLVE(ast->child(1), env, lamLevel, USE_MODE, 
              idc_value, currLB, flags);

      break;
    }

  case at_condelse:
    {
      // match agt_expr
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_value, currLB, flags);
      break;
    }

  case at_setbang:
    {
      shared_ptr<AST> lhs = ast->child(0);
      shared_ptr<AST> rhs = ast->child(1);

      // match agt_expr
      RESOLVE(lhs, env, lamLevel, USE_MODE, id_value, currLB, flags); 

      // match agt_expr
      RESOLVE(rhs, env, lamLevel, USE_MODE, idc_value, currLB, flags);

      if (lhs->astType == at_ident)
        if ((lhs->symbolDef->flags & ID_MUT_CLOSED) == 0)
          lhs->symbolDef->flags |= ID_IS_MUTATED;

      break;
    }

  case at_sizeof:
  case at_bitsizeof:
    {
      // match agt_type

      // The type must be complete, because otherwise we can get
      // cyclically self-referencing types.
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_type, currLB, 
              flags | (~RSLV_INCOMPLETE_OK));
      break;      
    }    

  case at_dup:
    {
      // match agt_expr
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_value, currLB, flags);
      break;      
    }    

  case at_deref:
    {
      // match agt_expr
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              id_value, currLB, flags);

      break;
    }

  case at_inner_ref:
    {
      // match agt_expr
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_value, currLB, flags);

      if (((ast->flags & INNER_REF_NDX) == 0) &&
         ast->child(1)->astType == at_ident) {
        // Could be a field-select
      }
      else {
        // match agt_expr
        RESOLVE(ast->child(1), env, lamLevel, USE_MODE, 
                id_value, currLB, flags);
      }
      break;
    }

  case at_uswitch:
    {
      // match at at_ident: ignore

      // match at agt_expr
      RESOLVE(ast->child(1), env, lamLevel, USE_MODE, 
              idc_value, currLB, flags);    
      
      // match at_case_legs or at_usw_legs
      RESOLVE(ast->child(2), env, lamLevel, USE_MODE, 
              idc_value, currLB, flags);

      // match at_otherwise (agt_ow)
      if (ast->child(3)->astType != at_Null) 
        RESOLVE(ast->child(3), env, lamLevel, USE_MODE, 
                idc_value, currLB, flags);

      break;
    }

  case at_usw_legs:
    {
      // match at_case_leg+ or at_usw_leg+
      for (size_t c = 0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, USE_MODE, 
                idc_value, currLB, flags);

      break;
    }

  case at_usw_leg:
  case at_otherwise:
    {
      shared_ptr<ASTEnvironment > legEnv = env->newScope();
      ast->envs.env = legEnv;

      /* match at_ident -- the contents after cracking the constructor */
      RESOLVE(ast->child(0), legEnv, lamLevel, DEF_MODE, 
              id_value, currLB, flags);
      ASTEnvironment::iterator itr = legEnv->find(ast->child(0)->s);
      assert(itr != legEnv->end());
      itr->second->flags |= BF_COMPLETE;

      if (ast->astType == at_usw_leg)
        ast->child(0)->flags |= ID_FOR_USWITCH;

      /* match at_expr */
      if ((flags & RSLV_WITHIN_CATCH) && (ast->children.size() > 3))
        flags |= RSLV_WITHIN_CATCH_MC;
        
      RESOLVE(ast->child(1), legEnv, lamLevel, USE_MODE, 
              idc_value, currLB, flags);

      /* match at_ident -- the constructors (if any) */
      for (size_t c=2; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), legEnv, lamLevel, USE_MODE, 
                idc_uctor, currLB, flags);      
      
      break;
    }

  case at_try:
    {
      // match agt_expr
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              id_value, currLB, flags);
    
      // match_at_ident: ignore

      // match at_case_legs
      RESOLVE(ast->child(2), env, lamLevel, USE_MODE, 
              idc_value, currLB, flags | RSLV_WITHIN_CATCH);

      // match at_otherwise
      if (ast->child(3)->astType != at_Null) 
        RESOLVE(ast->child(3), env, lamLevel, USE_MODE, 
                idc_value, currLB, flags | RSLV_WITHIN_CATCH);
      break;
    }

  case at_throw:
    {
      ResolverFlags rf = flags;

      if (ast->child(0)->astType == at_ident)
        rf |= RSLV_SWITCHED_ID_OK;

      // match agt_expr
      RESOLVE(ast->child(0), env, lamLevel, USE_MODE, 
              idc_value, currLB, 
              rf);
      break;
    }

  case at_container:
    {
      RESOLVE(ast->child(1), env, lamLevel, USE_MODE, 
              idc_value, currLB, 
              flags);      
      break;
    }

  case at_loop:
    {
      // NOTE: Do is re-written in the parser
      shared_ptr<ASTEnvironment > loopEnv = env->newScope();
      ast->envs.env = loopEnv;

      shared_ptr<AST> lbs = ast->child(0);
      
      // match at_loopbindings
      // First process the initializers.
      for (size_t c = 0; c < lbs->children.size(); c++) {
        shared_ptr<AST> lb = lbs->child(c);        
        shared_ptr<AST> init = lb->child(1);
        RESOLVE(init, loopEnv, lamLevel, USE_MODE, idc_value, 
                currLB, flags);
      }
            
      // First add the definitions.
      for (size_t c = 0; c < lbs->children.size(); c++) {
        shared_ptr<AST> lb = lbs->child(c);        
        shared_ptr<AST> localDef = lb->child(0);
        //shared_ptr<AST> init = lb->child(1);
        RESOLVE(localDef, loopEnv, lamLevel, DEF_MODE, 
                id_value, currLB, flags);        
      }

      // Make them complete
      markComplete(loopEnv);
      
      // Then process all the next step initializers
      for (size_t c = 0; c < lbs->children.size(); c++) {
        shared_ptr<AST> lb = lbs->child(c);        
        shared_ptr<AST> step = lb->child(2);
        RESOLVE(step, loopEnv, lamLevel, USE_MODE, 
                idc_value, currLB, flags);        
      }
      
      // Process the condition/result
      // match at_looptest
      RESOLVE(ast->child(1), loopEnv, lamLevel, USE_MODE, 
              idc_value, currLB, flags);      
      
      // And finally process the body with a rich environment
      // match agt_expr, with my Parent's Incompleteness restrictions
      RESOLVE(ast->child(2), loopEnv, lamLevel, USE_MODE, 
              idc_value, currLB, flags);    
      break;
    }
    
  case at_looptest:
    {
      for (size_t c = 0; c < ast->children.size(); c++)
        RESOLVE(ast->child(c), env, lamLevel, USE_MODE, 
                idc_value, currLB, flags);
      break;
    }

  case at_let:
    {
      // match at_letbindings

      // RESOLVE(ast->child(0), env, lamLevel, NULL_MODE, 
      //         identType, currLB,  flags);
      // Handle let bindings with care.

      shared_ptr<ASTEnvironment > letEnv = env->newScope();
      shared_ptr<AST> lbs = ast->child(0);
      lbs->parentLB = currLB;

      ast->envs.env = letEnv;
      lbs->envs.env = letEnv;

      // Begin processing let-bindings
      lbs->flags &= ~LBS_PROCESSED;

      // First Evaluate ALL the Expressions, then bind the values
      // match agt_expr
      // For each individual binding // match at_letbinding+
      for (size_t c = 0; c < lbs->children.size(); c++) {
        shared_ptr<AST> lb = lbs->child(c);
        
        RESOLVE(lb->child(1), letEnv, lamLevel, USE_MODE, 
                idc_value, lbs, flags);
      }      
      
      // match agt_bindingPattern
      // For each individual binding // match at_letbinding+
      for (size_t c = 0; c < lbs->children.size(); c++) {
        shared_ptr<AST> lb = lbs->child(c);
        
        // match agt_bindingPattern
        RESOLVE(lb->child(0), letEnv, lamLevel, DEF_MODE, 
                id_value, lbs, flags);

        assert(lb->child(0)->astType == at_identPattern);
        lb->child(0)->child(0)->flags &= ~ID_IS_MUTATED;
      }

      // Now we are done with all let-bindings
      lbs->flags |= LBS_PROCESSED;
       
      // Evaluate the final Expression with a rich environment
      // match agt_expr, with my Parent's Incompleteness restrictions
      markComplete(letEnv);
      RESOLVE(ast->child(1), letEnv, lamLevel, USE_MODE, 
              idc_value, currLB, flags);
    
      // match at_constraints
      RESOLVE(ast->child(2), letEnv, lamLevel, USE_MODE, 
              idc_type, currLB,  flags & (~RSLV_NEW_TV_OK) & (~RSLV_INCOMPLETE_OK));
      break;
    }

  case at_letStar:
    {
      // match at_letbindings

      // Handle let bindings with care.
      shared_ptr<ASTEnvironment > letEnv = env->newScope();
      shared_ptr<AST> lbs = ast->child(0);
      lbs->parentLB = currLB;

      ast->envs.env = letEnv;
      lbs->envs.env = letEnv;

      
      // Begin processing let-bindings
      lbs->flags &= ~LBS_PROCESSED;

      // First Evaluate the Expressions, then bind the values
      // individually
      // match agt_expr
      // For each individual binding // match at_letbinding+
      for (size_t c = 0; c < lbs->children.size(); c++) {
        shared_ptr<AST> lb = lbs->child(c);
        
        RESOLVE(lb->child(1), letEnv, lamLevel, USE_MODE, 
                idc_value, lbs, flags);
        
        RESOLVE(lb->child(0), letEnv, lamLevel, DEF_MODE, 
                id_value, lbs, flags);
        assert(lb->child(0)->astType == at_identPattern);
        lb->child(0)->child(0)->flags &= ~ID_IS_MUTATED;
        
        ASTEnvironment::iterator itr = 
          letEnv->find(lb->child(0)->child(0)->s);
        itr->second->flags |= BF_COMPLETE;
      }

      // Now we are done with all let-bindings
      lbs->flags |= LBS_PROCESSED;
      
      // Evaluate the final Expression with a rich environment
      RESOLVE(ast->child(1), letEnv, lamLevel, USE_MODE, 
              idc_value, currLB, flags);
      
      break;
    }

  case at_letrec:
    {
      // match at_letbindings

      // RESOLVE(ast->child(0), env, lamLevel, NULL_MODE,
      // identType, currLB, 
      // incomplete, boundVars, flags);
      // Handle let bindings with care.

      // First bind, then evaluate.
      
      shared_ptr<ASTEnvironment > letEnv = env->newScope();
      shared_ptr<AST> lbs = ast->child(0);
      lbs->parentLB = currLB;

      ast->envs.env = letEnv;
      lbs->envs.env = letEnv;      

      // Begin processing let-bindings
      lbs->flags &= ~LBS_PROCESSED;

      // For each individual binding // match at_letbinding+
      for (size_t c = 0; c < lbs->children.size(); c++) {
        shared_ptr<AST> lb = lbs->child(c);
      
        // match agt_bindingPattern
        RESOLVE(lb->child(0), letEnv, lamLevel, DEF_MODE, 
                id_value, lbs, flags);        
        assert(lb->child(0)->astType == at_identPattern);
        lb->child(0)->child(0)->flags &= ~ID_IS_MUTATED;
      }      
    
      // For each individual binding // match at_letbinding+
      for (size_t c = 0; c < lbs->children.size(); c++) {
        shared_ptr<AST> lb = lbs->child(c);

        // match agt_expr
        RESOLVE(lb->child(1), letEnv, lamLevel, USE_MODE, 
                idc_value, lbs, flags);
        
      }

      // Now we are done with all let-bindings
      lbs->flags |= LBS_PROCESSED;


      // Evaluate the final Expression with a rich environment, 
      // with my Parent's Incompleteness restrictions
      // match agt_expr
      markComplete(letEnv);
      RESOLVE(ast->child(1), letEnv, lamLevel, USE_MODE, 
              idc_value, currLB, flags);

      break;
    }

    // CAREFUL: CAREFUL:    
    //
    // This is *NOT* dead code, though, it appears to be so, from the
    // way the above let-cases are written.  this case is used by the
    // (new) polyinstantiator to R&T let-binding instantiations. It is
    // OK to use it there because we don't have any more polymorphism
    // at that stage.
    //
    // THIS CASE MUST NOT BE USED BY OTHER LET FORMS
  case at_letbinding:
    {
      // The lamLevel is bogus here, but OK only for
      // the sake of polyinstantiation.      
      if (ast->flags & LB_REC_BIND) {
        RESOLVE(ast->child(0), env, lamLevel, DEF_MODE, 
                id_value, ast, flags);

        RESOLVE(ast->child(1), env, lamLevel, USE_MODE, 
                idc_value, ast, flags);        
      }        
      else {
        RESOLVE(ast->child(1), env, lamLevel, USE_MODE, 
                idc_value, ast, flags);
        
        RESOLVE(ast->child(0), env, lamLevel, DEF_MODE, 
                idc_value, ast, flags);
      }

      assert(ast->child(0)->astType == at_identPattern);
      ast->child(0)->child(0)->flags &= ~ID_IS_MUTATED;
      break;
    }
  }
  return errorFree;
}

static bool
initEnv(std::ostream& errStream,
        shared_ptr<AST> ast,
        shared_ptr<ASTEnvironment > aliasEnv,
        shared_ptr<ASTEnvironment > env)
{
  // See if I am processing the prelude or some other file.
  if (ast->astType == at_interface &&
     ast->child(0)->s == "bitc.prelude") {
    //    cout << "Processing Prelude " << std::endl;   

    return true;
  }
  
  //  cout << "Processing " << ast->child(0)->s << std::endl;
  // "use" everything in the prelude
  shared_ptr<ASTEnvironment > preenv = GC_NULL;
  UocMap::iterator itr = UocInfo::ifList.find("bitc.prelude");
  if (itr == UocInfo::ifList.end()) {
    errStream << ast->loc << ": "
              << "Internal Compiler Error. "
              << " Prelude has NOT been processed."
              << std::endl;
    ::exit(1);
  }
  preenv = itr->second->env;
  
  if (!preenv) {
    // GCFIX: Why does this return on error instead of exiting? This
    // is a FATAL compiler error!
    errStream << ast->loc << ": "
              << "Internal Compiler Error. "
              << " Prelude's environment is NULL "
              << std::endl;
    ::exit(1);
  }
  
  aliasPublicBindings(std::string(), aliasEnv, preenv, env);
  return true;
}


bool 
UocInfo::DoResolve(std::ostream& errStream, bool init, 
                   ResolverFlags rflags)
{
  bool errFree = true;

  if (Options::noPrelude)
    rflags |= RSLV_SYM_NO_PRELUDE;
  
  shared_ptr<ASTEnvironment > aliasEnv = ASTEnvironment::make("*aliases*");

  if (init) {    
    if (false) {
      assert(env);      
      assert(env->parent);
      env = env->parent->newDefScope();
    }
    else {
      env = ASTEnvironment::make(uocName);
    }      

    if ((rflags & RSLV_SYM_NO_PRELUDE) == 0)
      initEnv(std::cerr, uocAst, aliasEnv, env);
  }
  
  CHKERR(errFree, resolve(errStream, uocAst, aliasEnv, env, GC_NULL, 
                          USE_MODE, idc_type, GC_NULL, rflags));

  return errFree;
}

bool 
UocInfo::Resolve(std::ostream& errStream, bool init, 
                 ResolverFlags rflags, std::string mesg)
{
  bool errFree = true;
  errFree = DoResolve(errStream, init, rflags);
  if (!errFree)
    errStream << mesg
              << std::endl;
  return errFree;
}

bool
UocInfo::fe_symresolve(std::ostream& errStream,
                       bool init, unsigned long flags)
{
  return DoResolve(errStream, init, RSLV_NO_FLAGS);
}

 
