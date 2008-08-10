#ifndef AST_HXX
#define AST_HXX


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

#include <string>
#include <vector>


#include <stdint.h>
#include "FQName.hxx"
#include "LitValue.hxx"
#include "Environment.hxx"
#include "debug.hxx"

#include "shared_ptr.hxx"
#define AST_SMART_PTR boost::shared_ptr
#define AST_SUPERCLASS boost::enable_shared_from_this<AST>

 

//template <class T> typedef struct environment;

// Index into a pointer of an array or any object that implements the
// [] operator.

struct Type; 
struct TypeScheme; 
struct spStruct;
struct UocInfo;
struct Instance;
 
/** @brief Different classifications of identifiers that we might
 *  encounter. 
 *  
 *  > This enumeration performs a dual role:
 *  > 1) As as AST field, it stores the classification of the
 *  >    (identifier) AST
 *  >    
 *  > 2) As an argument to the symbol resolver, it notes the
 *  >    correct kind of identifier expected in a context.
 *  >    
 *  > Certain identifiers have multiple types or roles. 
 *  > For example:   
 *  > a) Structure type name: type and value constructor    
 *  > b) Union constructor with zero arguments: value and constructor   
 *  >    
 *  > That is, a union constructor such as nil can be used in a 
 *  > context that identifies either id_value or id_constructor.   
 *  > Therefore, some AST flags are used in tandem with identType   
 *  > in such cases. It is possible to remove identType field   
 *  > completely from ASTs and just use flag markings on ASTs.
 *  > We can then switch over the resolver's identType and check for
 *  > appropiate flags in the symbol resolver. This approach of using
 *  > both identType and flags is used since it simplifies the
 *  > checking in the common case to
 *  > argument-identType == ast->identType 
 * 
 */
enum IdentType {
  /** @brief An identifier whose classification is not yet decided. 
   *
   * These should no longer exist after the symbol resolution pass,
   * except in temporary ASTs that are introduced for expediency in
   * various later passes and then resolved. */
  id_unresolved,
  /** @brief Type name or type variable.
   * > That is, this identifier can be legally used in a type
   * > context.
   *
   * Flagged ID_IS_CTOR if this is a type constructor name.
   *
   * @bug The previous comment here read "possibly ID_IS_CTOR". I do
   * not understand why such a case should arise, given that we have a
   * separate classification for id_constructor. When is it
   * appropriate for an identifier to be id_type and simultaneously be
   * flagged ID_IS_CTOR?
   * > Structure names -- case (a) above.
   * > ID_IS_CTOR is not marked in the case of union name, type variables.
   */
  id_type,
  /** @bug needs documentation 
   * > 
   * > For all identifiers that denote value definitions. That is,
   * > identifiers that are either defined using define, recdef, let,
   * > letrec, or do or proclaimed use proclaim.
   * > These identifiers can be used in value contexts.
   */
  id_value,
  /** Constructor names. Implies ID_IS_CTOR
   *
   * @bug I hope this is wrong. If it implies ID_IS_CTOR, then
   * ID_IS_CTOR should always be set. Is that what is meant here? 
   * 
   * > ID_IS_CTOR is always set when an identifier is id_constructor
   * > but it is not true the other way. That is, ID_IS_CONSTRUCTOR
   * > is set in other cases (ex: for id_type on structure names).
   */
  id_constructor,
  /** @brief Structure or union field name. */
  id_field,			// field name
  /** @brief Identifier is a type class name. */
  id_typeclass,
  //  id_module,
  /** @brief Identifier is an interface name. */
  id_interface,
};

enum PrimOp {
  op_equals,
  op_plus,
  op_minus,
  op_mul,
  op_div,
  op_mod   
};

 std::string identTypeToString(IdentType id);

/* Declarations for AST FLAGS */
/** @brief Identifier is a type variable.
 *
 * @bug Why is this not an identifier category?
 * 
 * > Type variables have identType id_type, since type variables can
 * > be used in type contexts. The ID_IS_TVAR conveys additional
 * > information.
 */
#define ID_IS_TVAR       0x00000001u
/** @brief Identifier is a constructor.
 *
 * @bug Why is this not an identifier category?
 * > There is id_constructor category. This flag identifies
 * > identifiers that are type-names, that can also function as
 * > constructors (ex: structure names). Please see comment above
 * > the identtype enumeration definition.
 */
#define ID_IS_CTOR       0x00000002u
/** @brief Identifier is bound in type-level scope. */
#define ID_IS_GLOBAL     0x00000020u
/** @brief Identifier was internally generated by the compiler. */
#define ID_IS_GENSYM     0x00000040u
/** Set in the code generator to record the fact that a declaration has
 * already been emitted for this definition.
 *
 * @issue I am not convinced that this belongs here. I would have
 * naively expected this to be dealt with in a set<string> that was
 * private to the code generator.
 * > This can definitely be changed to use a set<string>
 * > implemmentation. This seemed like a easy and efficient
 * > implementation, especially since we were not using interned
 * > strings. 
 */
#define DEF_DECLARED     0x00000800u
 
/** Marked for type constructors and value constructors defined by
 * defstruct, defunion, and defexception. The type-scheme for such
 * definitions must be copied even if there are no free type variables
 * within them.
 */
#define ID_ENV_COPY       0x00002000u 

/** Set in the tail recursion analysis pass to indicate that a
 * use-occurrence of an identifier is a reference to the function
 * currently being defined. Consulted in the SSA pass to add an
 * LB_IS_DUMMY marker to the emitted let binding. Consulted in the
 * gen-c pass to determine when looping rather than recursion should
 * be used.
 *
 * @bug I am not clear why SSA should construct a dummy let binding in
 * this case in the first place. Some explanation would be appropriate
 * below on the documentation of LB_IS_DUMMY.
 */
#define SELF_TAIL        0x00040000u


/** The SSA pass constructs some dummy let bindings. This is used to
 * mark them so that no assignment for them will later be emitted in
 * the code generator.
 *
 * @bug Need to say something about @em why these are dummy let
 * bindings! It is not clear to me why they are needed at all. Is this
 * to fool the resolver?
 */
#define LB_IS_DUMMY      0x00080000u
/** Indicates a type variable that appears in a use occurrence, but
 * acts like a definition.
 *
 * @bug This is set in various places, and masked in others, but it
 * isn't tested anywhere. Can it be dropped?
 */
#define TVAR_IS_DEF      0x00100000u  // This is a type-variable that
			 	      // appears in a use-position, but
			 	      // acts like a definition. 
/** Indicates a global identifier that is not exported from its
 * defining unit of compilation. These are emitted by the back end
 * with a static marker to enable later optimization by the C
 * compiler.
 */
#define ID_IS_PRIVATE    0x00200000u
/** Identifier is a typeclass method name.
 *
 * @bug I remain horribly confused about which things are identifier
 * categories and which things are identifier flags. I would have
 * expected this one to be an identifier category.
 * > Type classe methods require some special treatement at certain
 * > times, but in general can be used in contests where a value is
 * > required. So, the identType is id_value, but this flag conveys
 * > extra information.
 */
#define ID_IS_METHOD     0x00400000u  // Typeclass method definitions
/** Do not emit an assignment for this let binding. This is set in the
 * SSA pass for SET!, and also for some other odd cases. It is treated
 * like LB_DUMMY in the back end.
 *
 * @bug Which raises the question of whether this shouldn't just be:
 *
 *   #define LB_POSTPONED LB_DUMMY
 *
 * @bug and also, once again, why dummy let bindings are getting
 * generated at all.
 */
#define LB_POSTPONED     0x00800000u
/** Marks a definition that is an external program entry point, and
 * therefore a seed for polyinstantiation.
 */
#define DEF_IS_ENTRYPT   0x02000000u

/** Marks a let binding as a member of a letrec. This is consulted in
 * Symtab.cxx and TypeInfer.cxx to determine whether the bound
 * identifier should be bound early or late. If it were someday
 * useful, this could be eliminated by introducing a distinct
 * at_letrecbinding.
 */
#define LB_REC_BIND      0x04000000u

/** Marks a BitC identifier as already having an external name, which
 * should be used during code generation in place of the BitC name.
 *
 * @bug Looking at the use cases, this appears to be wholly redudant,
 * because all of the associated ASTs will have an externalName field
 * of non-zero size. Can this be eliminated?
 * 
 * > This flag is not redundant. DEF_IS_EXTERNAL is marked for all
 * > external definitions, whether it has an external name or not
 * > only some external definitions have a external-name. For example,
 * > the check at gen-c.cxx:3003 ValuesTOH() cannot be replaced with 
 * > ast->getID()->externalName.size()
 */
#define DEF_IS_EXTERNAL  0x08000000u 
/**Used to mark a union consisting exclusively of constant
 * legs, which is really an enum declaration. Marking is performed in
 * TypeInfer, and is consulted in Type-size.cxx.
 *
 * @bug Regrettably our current handling of enumerations is
 * inadequate, because it doesn't allow us to specify the actual
 * enumeration values. We are therefore going to have to introduce a
 * defenum at some point.
 */
#define ENUM_UN          0x10000000u
/** Marks a union having only a single leg.
 * 
 * @bug It seems to me that a union having only one leg should be
 * syntactically rejected, in which case this flag should never
 * arise. Is there some USEFUL counter-example?
 *
 * @bug Conversely, if we are going to treat this as a union on the
 * theory that the developer isn't done yet and it will eventually
 * have more legs, why the hell bother to optimize it?
 *
 * > The programmer cannot provide any more legs after the union
 * > definition is complete. So, bug(2) cannot arise.
 * > bug(1) is maintained for completeness. If we reject single legged 
 * > unions, this goes away.
 */

#define SINGLE_LEG_UN    0x20000000u

/** Marks a union that is subject to one of the required Cardelli
 * optimizations. */
#define CARDELLI_UN      0x40000000u

/** Marks the NULLABLE union, which has a special representation known
 * to the code generator. */
#define NULLABLE_UN      0x80000000u  // Special handling for nullable 'a

///////////////// SECOND ROUND OF FLAGS  /////////////////////

/** Marks the place-holder identifier that is introduced in the
 * at_sw_leg AST to replicate the (switch ...) temporary identifier.
 *
 * @bug The only place this is used is to ensure that the ID is being
 * used on the LHS of a dot, unless SWITCHED_ID_OK is set (which it
 * always is by at_select) or to catch mis-use of the switch symbol in
 * CATCH blocks that catch multiple cases. I'm wondering if we should
 * not resolve both of these cases by requiring that the thing on the
 * RHS of a swith pattern or a catch pattern be a LAMBDA form taking a
 * single argument by value. Would that perhaps be cleaner? It would
 * resolve the "multiple matched exceptions" issue nicely.
 *
 * @bug Or is the problem here that we @em require the dot, because we
 * don't want it to be possible for this identifier to escape, thereby
 * revealing a leg structure type?  If this is the issue, it might be
 * a lot cleaner to just ensure that (SWITCH ...) is defined to be of
 * unit type.
 */
#define ID_FOR_SWITCH    0x00000001u
/** Expression is a location expression
 *
 * @bug This does not appear to be used anywhere, except for
 * self-propagation in the parser. Can it be dropped?
 */
#define AST_IS_LOCATION  0x00000002u
 /** Identifier is closed over by something. */
#define ID_IS_CAPTURED   0x00000004u 

 ////////////////////////////////////////////////////////////////////
 // The following Identifier-markers are applicable only to value
 // identifiers, not to type identifiers. They are used in closure
 // conversion to mark the various identifier nodes.
 //
 // @bug I wonder if ID_IS_DEF and ID_IS_USE shouldn't be marked for
 // type identifiers too. We can use (flags2&(ID_IS_DEF|ID_IS_CLOSED))
 ////////////////////////////////////////////////////////////////////

/** ID is a defining occurrence.
 *
 * @issue Shap thinks this should only occur in a DEFINE or
 * LET-BINDING or ARGUMENT position, yes?
 */
#define ID_IS_DEF        0x00000008u
/** ID is a use occurrence. */
#define ID_IS_USE        0x00000010u
/** ID is a use occurrence of a closed-over ID
 *
 * @bug I copied the existing comment here, but it seems to me that
 * this might also arise on a defining occurrence, no? In any case, it
 * seems to me that the distinction is already covered by ID_IS_USE,
 * and this flag should not encode that redundantly. */
#define ID_IS_CLOSED     0x00000020u
/** ID must be moved to the heap due to capture. */
#define ID_NEEDS_HEAPIFY 0x00000040u
/** ID is bound in a recursive context.
 *
 * @bug Can you add something here to clarify why ID_IS_RECBOUND and
 * LB_REC_BIND are not redundant?
 */
#define ID_IS_RECBOUND   0x00000080u  // ID is bound in recursive
                                      // context 
/** @bug This seems to be a hold-over from our earliest fixing
 * hack. It is set once and carefully cleared in many places, but
 * never tested. Can it be dropped?
 */
#define ID_IS_MUTATED    0x00000100u  // ID is target of SET!
/** @bug This exists only to set ID_IS_MUTATED, which doesn't appear
 *to be in use anymore. Can we drop it? */
#define ID_MUT_CLOSED    0x00000800u
/** This proclaim was introduced by the compiler. So, we can skip the
 * def/decl match. This is necessary only until we have a way to
 * input dummy types.
 *
 * @bug Can you sent me a note reminding me what a "dummy type" is in
 * this context, and what, if anything, we ought to do about inputting
 * them? Alternatively, can you explain why it is appropriate for this
 * to use a dummy type? I see that this has something to do with the
 * construction of closure records, but I don't understand the issue.
 *
 * >
 * >
 */
#define PROCLAIM_IS_INTERNAL 0x00001000u
/** Set in the SSA pass to identify trivial initializers so that the
 * code generator can avoid adding code in the per-UoC init procedure.
 */
#define DEF_IS_TRIVIAL_INIT  0x00002000u 
/** Used in the instantiator to indicate identifiers that have already
 * been mangled and should not be mangled a second time.
 *
 * @issue I don't understand why this is needed. The assignment of a
 * mangled name is, in effect, the assignment of an externalName. I
 * would think that it would make sense to place the mangled name into
 * the externalName field and check that, removing this flag. That
 * works equally well when a pre-existing external name has been
 * assigned, because we must not mangle those. Is this merely a
 * different choice of implemenation approach? Would the approach that
 * I am outlining work? 
 *
 * > Yes. This flag can be removed by using the externalname field.
 */
#define IDENT_MANGLED        0x00004000u
/** Original: For variables defined at non-generalizable
 * boundaries. ex: Lambda parameters, identifiers defined at switch /
 * catch, etc.
 *
 * @bug Okay, but I have no idea why these variables are handled
 * differently in the instantiator, and it would be really useful to
 * know that.
 */
#define LOCAL_NOGEN_VAR  0x00008000u  
/** Used to track the highest level of tvar-usage. If we see a tvar
 * use, and if LBS_PROCESSED is set for tvar->tvarLB, then we have
 * seen the use at a higer LB, and the tvarLB of tvar must point to
 * the current lbs.
 *
 * @bug This comment doesn't say what we are tracking. It appears to
 * me that we are tracking the outermost let-binding in which a type
 * variable appears. If so, we need to say here (a) that we need to
 * keep track of that, and (b) why.
 *
 * @issue I am not sure this comment is correct. If we have set
 * LBS_PROCESSED, then shouldn't tvar->tvarLB point to the outermost
 * let binding in which we saw a use?
 */
#define LBS_PROCESSED    0x00040000u  

 /** The current let-binding has been instantiated (used by the
  * polyinstantiator)
  *
  * @bug Checked, but never set. Drop?
  */
#define LB_INSTANTIATED  0x00080000u

 /** The Identifier is observably defined
  */
#define ID_OBSERV_DEF    0x00100000u

 /** Indicates a type variable that was temporarily created by the
  * polyinstantiator. Definition is OK.
  *
  * @bug What does it mean that the definition is OK?
  *
  * > The symbol resolver accepts type variables as defining
  * > occurences within expressions only at certain positions (when
  * > called with NEW_TV_OK). For example, this is legal within value
  * > definitions, but type variables not identified as arguments to 
  * > type definitions are legal within the type definition itself. 
  * > This flag marks type variables created by the polyinstantiator
  * > that must not be subject to the NEW_TV_OK check by the resolver,
  * > since the Instantiator makes some quasi RandTs in which this
  * > restriction might temporaroly not hold.
  */
#define TVAR_POLY_SPECIAL 0x00200000u 
 /** Indicates that this DEFUNION is actually a DEFREPR that was
  * converted to a union by the reprSimp pass.
  *
  * @bug This flag is later used in TypeInfer to perform various sanity
  * checks, some of which indicate fatal compiler errors and should
  * exit rather than continue (line 1006) or which I think should not
  * be possible (line 4236). Having that sort of code is good, but it
  * should be clearly identified as an internal sanity check.
  * 
  * > Both of these are actually programmer errors.
  * > Line 1006 line ensures that the progrmmer cannot provide
  * > representation constraints using both tagtype as well as the when
  * > clause. Line 4217 enforces different semantics for constructor
  * > matching for unions and reprs (as explained in the comment at
  * > line 4186.
  */
#define UNION_IS_REPR    0x00400000u  // Defrepr
 /** Indicates a field that is a discriminator field.
  *
  * @bug Looking at gen-c.cxx, I'm not entirely convinced that we are
  * handling this correctly in the defrepr case.
  *
  * > I think it is handled correctly in gen-c.cxx:1468
  * > I am not sure why you felt that the handling is wrong. can you
  * > please explain?
 */
#define FLD_IS_DISCM     0x00800000u
 /** Marked on an at_define. Indicates that this is a hoisted lambda
  * for a function that has a captured closure, and we therefore need
  * to emit a transition function.
  *
  * @bug Please add a comment at gen-c.cxx:821 that explains what the
  * heck a transition function is!
  * 
  * > Added a comment.
  */
#define LAM_NEEDS_TRANS  0x01000000u
 /** inner_ref is an indexing-ref when clear, indicated inner-ref is a
  * selection-ref
  *
  * @bug I cannot parse this comment. The words "when set" need to
  * appear somewhere here.
  *
  * @bug And when that is fixed, my follow-up question is "why do we
  * care?"
  *
  * > The semantics of this flag is:
  * > When set, it is an indexing inner_ref that is of the form
  * > (inner_ref (dup (array ..))) or (inner_ref (vector ... ))
  * >
  * > When clear, it is a selecting inner_ref that is of the form
  * > (inner_ref (dup (structv ... ))) or (inner_red (structr ... ))
  * > 
  * > These two types of inner-ref must be handled differently in most
  * > passes. For example, the index argument must be independently
  * > resolved and type checked but the field argumet must not.
  */
#define INNER_REF_NDX    0x02000000u
 /** Set in the parser to indicate that a parameter identifier is
  * by-reference. Consulted in gen-c.cxx to determine how the
  * corresponding C parameter should be emitted.
  *
  * @bug It seems to me that this is not the right way to go about
  * this. The two uses in gen-c.cxx should be consulting the type to
  * determine whether the identifier is by-ref. Having made that
  * change, I think that this flag should be dropped.
  * 
  * > The by-ref-ness is actually marked on the enclosing function
  * > type, and is not marked on the type of the individual
  * > arguments. For most purposes, the by-ref-ness of the
  * > identifier itself must be ignored. Since all by-ref parameters
  * > must all identifer ASTs, it is marked on the AST itself, 
  * > and similarly handled by set! case and gen-c.
  */
#define ARG_BYREF        0x04000000u  // Function parameter is By-Reference

/* Add All **AST Flags** that must be masked from definition before
   setting onto the use cases */

#define MASK_FLAGS_FROM_USE    (TVAR_IS_DEF | ID_IS_CTOR | ID_ENV_COPY |\
				DEF_IS_ENTRYPT )
#define MASK_FLAGS2_FROM_USE   (ID_IS_CAPTURED)

struct AST;

/** @brief Set of environments associated with a given AST node.
 *
 * See the comment at the envSet field of the AST class.
 */
struct EnvSet {
  boost::shared_ptr<ASTEnvironment> env;
  boost::shared_ptr<TSEnvironment> gamma;
  boost::shared_ptr<InstEnvironment> instEnv;
  
  EnvSet()
  {
    env = boost::GC_NULL;
    gamma = boost::GC_NULL;
    instEnv = boost::GC_NULL;
  }

  EnvSet(boost::shared_ptr<ASTEnvironment > _env, boost::shared_ptr<TSEnvironment >_gamma,
	 boost::shared_ptr<InstEnvironment > _instEnv)
  {
    env = _env;
    gamma = _gamma;
    instEnv = _instEnv;
  }

  EnvSet(EnvSet &envs)
  {
    env = envs.env;
    gamma = envs.gamma;
    instEnv = envs.instEnv;
  }

  void
  updateKey(const std::string from, const std::string to)
  {
    env->updateKey(from, to);
    gamma->updateKey(from, to);
  }

  /* Quasi-constructors: */
  static inline boost::shared_ptr<EnvSet>
  make()
  {
    EnvSet *tmp = new EnvSet();
    return boost::shared_ptr<EnvSet>(tmp);
  }

  static inline boost::shared_ptr<EnvSet>
  make(boost::shared_ptr<ASTEnvironment> _env, 
       boost::shared_ptr<TSEnvironment>_gamma,
       boost::shared_ptr<InstEnvironment> _instEnv)
  {
    EnvSet *tmp = new EnvSet(_env, _gamma, _instEnv);
    return boost::shared_ptr<EnvSet>(tmp);
  }

  static inline boost::shared_ptr<EnvSet>
  make(EnvSet &envs)
  {
    EnvSet *tmp = new EnvSet(envs);
    return boost::shared_ptr<EnvSet>(tmp);
  }
};


enum AstType {
  at_Null,
  at_AnyGroup,
  at_ident,
  at_ifident,
  at_usesel,
  at_boolLiteral,
  at_charLiteral,
  at_intLiteral,
  at_floatLiteral,
  at_stringLiteral,
  at_module,
  at_interface,
  at_defunion,
  at_declunion,
  at_defstruct,
  at_declstruct,
  at_defrepr,
  at_declrepr,
  at_reprctrs,
  at_reprctr,
  at_reprrepr,
  at_refCat,
  at_valCat,
  at_opaqueCat,
  at_defexception,
  at_deftypeclass,
  at_tcdecls,
  at_tyfn,
  at_tcapp,
  at_method_decls,
  at_method_decl,
  at_qualType,
  at_constraints,
  at_definstance,
  at_methods,
  at_proclaim,
  at_define,
  at_recdef,
  at_importAs,
  at_provide,
  at_import,
  at_ifsel,
  at_declares,
  at_declare,
  at_tvlist,
  at_constructors,
  at_constructor,
  at_fields,
  at_field,
  at_fill,
  at_reserved,
  at_bitfield,
  at_refType,
  at_byrefType,
  at_valType,
  at_fn,
  at_primaryType,
  at_fnargVec,
  at_arrayType,
  at_vectorType,
  at_mutableType,
  at_typeapp,
  at_exceptionType,
  at_dummyType,
  at_identPattern,
  at_tqexpr,
  at_unit,
  at_suspend,
  at_makevectorL,
  at_vector,
  at_array,
  at_begin,
  at_select,
  at_fqCtr,
  at_sel_ctr,
  at_array_nth,
  at_vector_nth,
  at_array_length,
  at_vector_length,
  at_lambda,
  at_argVec,
  at_apply,
  at_struct_apply,
  at_ucon_apply,
  at_if,
  at_when,
  at_and,
  at_or,
  at_not,
  at_cond,
  at_cond_legs,
  at_cond_leg,
  at_otherwise,
  at_setbang,
  at_deref,
  at_dup,
  at_inner_ref,
  at_allocREF,
  at_copyREF,
  at_mkClosure,
  at_setClosure,
  at_switch,
  at_sw_legs,
  at_sw_leg,
  at_try,
  at_throw,
  at_let,
  at_letbindings,
  at_letbinding,
  at_letrec,
  at_do,
  at_dobindings,
  at_dobinding,
  at_dotest,
  at_localFrame,
  at_frameBindings,
  at_letStar,
  at_identList,
  at_container,
  at_docString,
  at_letGather,
  agt_var,
  agt_literal,
  agt_tvar,
  agt_CompilationUnit,
  agt_definition,
  agt_type_definition,
  agt_tc_definition,
  agt_value_definition,
  agt_if_definition,
  agt_category,
  agt_fielditem,
  agt_qtype,
  agt_type,
  agt_expr,
  agt_expr_or_define,
  agt_eform,
  agt_ucon,
  agt_ow,
};

enum { at_NUM_ASTTYPE = agt_ow };

#ifndef AST_SMART_PTR
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
#define AST_SMART_PTR boost::shared_ptr
#endif /* AST_SMART_PTR */

#ifndef AST_LOCATION_TYPE
#include <libsherpa/LexLoc.hxx>
#define AST_LOCATION_TYPE sherpa::LexLoc
#endif /* AST_LOCATION_TYPE */

#ifndef AST_TOKEN_TYPE
#include <libsherpa/LToken.hxx>
#define AST_TOKEN_TYPE sherpa::LToken
#endif /* AST_TOKEN_TYPE */

#ifndef AST_SUPERCLASS
#define AST_SUPERCLASS boost::enable_shared_from_this<AST>
#endif /* AST_SUPERCLASS */

class AST :public AST_SUPERCLASS { 
  bool isOneOf(AstType);
public:
  AstType        astType;
  ::std::string    s;
  AST_LOCATION_TYPE loc;
  ::std::vector<AST_SMART_PTR<AST> > children;


 private:  
  static unsigned long long astCount;
  
 public:
  unsigned long long ID; // Unique ID of this AST

  LitValue litValue;
  unsigned long litBase;
  
  IdentType identType;
  unsigned long Flags;
  unsigned long Flags2;

  unsigned printVariant;	// which syntax to use for pretty printing

  boost::shared_ptr<TypeScheme> scheme;		// defining occurrences only
  boost::shared_ptr<Type> symType;		// the (pre-unified) type
  boost::shared_ptr<AST> symbolDef;

  bool isDecl;                  // is This a declaration or definition?

  bool isGlobal() { return (Flags & ID_IS_GLOBAL); }
  bool isFnxn(); // Function
  size_t nBits();
  bool isLocation();
  bool isLiteral();
  bool isTopLevelForm();
  bool leadsToTopLevelForm();
  void clearTypes(); // Clear the sumType and scheme fields 
                     // of this AST and ALL children RECURSIVELY.
                     // This needs to be done before re-typing an
                     // expression, for example post-clconv
                     // otherwise, old types will infulence
                     // new type records.
 
  /** On a declaration AST only: if a definition for it has been seen,
   * then defn points to the definition.
   */
  boost::shared_ptr<AST> defn;
  /** On a definition AST: pointer to the definitive declaration (the
   * first one). */
  boost::shared_ptr<AST> decl;

  /** If this is a global identifier, fqn is its canonical fully
   * qualified name. The interface component of an fqn is the IFNAME
   * of the interface in which the symbol was defined.  the ident
   * component is the identifier under which the symbol was defined at
   * the point of its original definition, which may or may not be the
   * name that was bound in an importing environment (because USE may
   * have aliased it under a different name).
   */
  FQName fqn;

  /** If this identifier is proclaimed with "external IDENT", this field
   * holds the external identifier string, otherwise it is empty.
   */
  std::string externalName;

  // The IFNAME of an interface in the case of import and provide
  //
  // @bug This should probably be retired. It's main surviving purpose
  // seems to be error diagnostics in most places, and recovering the
  // interface name of an imported interface. The recovery purpose can
  // be accomplished equally well by adding a distinguished entry
  // ":name:" to the aliasEnv of every interface and looking that
  // up. The advantage to that approach is that we won't need to
  // carry around a redundant string here in every AST.
  std::string ifName;

  /** A set of environments associated with this ast. 
   * - If this is a local name for an imported interface, these are
   *   pointers to the environments of the imported interface.
   * - In at_let, at_letrec, at_letStar, at_define ... these are the
   *   environments in effect at that binding.
   */
  EnvSet envs;

  /** @brief Pointer to defining form
   *
   * defForm has context sensitive meaning. 
   * This is basically a hack in the absence of parent pointers, where
   * I must reach back into the definition and get the defining form,
   * or some form of a containing datastructure.
   * 
   * In the case of top-level expressions, we use it to get the 
   * entire defining form from the identifier being defined
   * 
   * (def  defIdent ... )
   * ^        |
   * |________|
   *   defForm
   * 
   * In the case of let-expressions:
   * (The quoted phrases are just ast-types not syntax.)
   *
   *  _______________      __________________
   * |   defForm     |     |   defForm       |  
   * v               |     v                 |
   * (define   ... (let .. ("letbindings" ("letbinding" ident ... ))))
   *               ^             |        ^               | 
   *               |_____________|        |_______________|
   *                  defForm                 defForm
   * 
   *     ___________
   *    |           |
   *    v           | 
   *  (switch ... ("sw_leg" id ...) ...)
   *                  ^    |
   *                  |____|
   *     _____      __________
   *    |     |    |          |
   *    v     |    v          | 
   *  (do ("dobindings"  ("dobinding" id ...) ...)
   *                             ^     |
   *                             |_____|
   */

  boost::shared_ptr<AST> defForm;

  /** Used in the case of loop, tail-recursion
   *
   * @bug OK, but for *what*?
   */
  boost::shared_ptr<AST> defbps;
  
  /** If this is a union, tagType is the type of the tag. */
  boost::shared_ptr<Type> tagType;

  /** Number of bits in a bitfield type. 
   *  Also propagated updards onto: 
   *  at_field, at_fill, at_reserved, tagtype (declare) at_declares,
   *   Identifier of at_defunion 
   *   
   * @bug WHAT is propagated upwards into these? 
   * > The value of field_bits is propagated.
   */
  size_t field_bits;
  
  /** on at_field only when the field is a part of a union
   * discriminator (FLD_IS_DISCM)
   *
   * @bug OK, but when it is on one of these, what does it mean?
   *
   * @bug Or does this mean "set when the field is part of a union
   * discriminator (FLD_IS_DISCM), and appear on at_field only", in
   * which case, why should this be redundantly encoded?
   *
   * > On defreppr unions, this field is resent on fields that
   * > participate in union discrimination. This field stores the
   * > value (not just a true/false setting) that the field must have
   * > as indicated by the when clause. 
   */
  size_t unin_discm;

  /** Total number of fill bits that are not reported by the type
   * record.
   *
   * Appears on a at_defstruct or at_constructor only.
   */
  size_t total_fill;

  /** TypeScheme for the corresponding Structure definitions.
   *
   * Appears on Union constructors only.
   *
   * @bug The comment says TypeSchemes plural, but I only see a single
   * type scheme here. How is this actually being used?
   *
   * > Fixed the comment to be singular. There is only one structure
   * > defintion for any constructor
   */

  boost::shared_ptr<TypeScheme> stSigma;

  /** Pointer to the constructor that holds such a structure.
   *
   * @bug I have no @em idea what this means or is for.
   *
   * > Constructors with identical layout must share the same
   * > structure type since they can be used in the same switch leg.
   * > Structures equality is nominal in BitC (structures are
   * > identified by name, not by identical components). Therefore, if
   * > at a second constructor, a previously typed structure  with
   * > identical structure is found, the new constructor uses the
   * > structure defined at the old structure and stores a pointer to
   * > that constructor in stCtr.
   */
  boost::shared_ptr<AST> stCtr;

  // Tracking scope of type-variables:

  /** Let binding at which this type variable is scoped. 
   * This flag is set reliably set whenever the resolver is
   * called, so there is no need to worry while cloning the AST?
   */
  boost::shared_ptr<AST> tvarLB;
  /**LB within which this at_letbindings is contained, everything is
   * contained within the top-level definition. 
   * This flag is set reliably set whenever the resolver is
   * called, so there is no need to worry while cloning the AST?
   */
  boost::shared_ptr<AST> parentLB;

  /** The resolver marks the above two markers. No need to worry about
   * them in copy-operations.
   *
   * @bug I believe that the resolver marks them, but I have no idea
   * which "markers" we are referring to here. I think you mean that
   * both of these fields are reliably set whenever the resolver is
   * called, so there is no need to worry about them when cloning the
   * AST?
   *
   * > Yes, I have changed the comment for the above two fields with 
   * > your wording. I have duplicated the comments so that we don't
   * > have positional dependency of fields. 
   */

  static boost::shared_ptr<AST> makeBoolLit(const sherpa::LToken &tok);
  static boost::shared_ptr<AST> makeIntLit(const sherpa::LToken &tok);
  static boost::shared_ptr<AST> makeStringLit(const sherpa::LToken &tok);
  static boost::shared_ptr<AST> makeCharLit(const sherpa::LToken &tok);
  static boost::shared_ptr<AST> makeFloatLit(const sherpa::LToken &tok);

  /** Remove child @p n from this AST.
   *
   * @brief The only place this is called is in the parser, and there
   * only to strip documentation strings. That is actually a bad thing
   * to do, since we want to preserve them in the AST, and a literal
   * value really ought to be a perfectly legal (if useless)
   * expression. We should attempt to comment out the body of
   * stripDocString() and see if everything still works. If so, both
   * this and stripDocString() should be removed. */
  void disown(size_t s);
  
  /** Generate an identifier AST with a newly generated internal name.
   * This will not have any particular type assigned to it, and is
   * intended to be used in the front end passes before the type
   * checker is involved.
   */
  static boost::shared_ptr<AST> genIdent(const char *pfx = "tmp", const bool isTV = false);

  /** Generate an at_ident AST providing a temporary symbol that is
   * type-compatible with the type of the passed AST @p lhs.
   */
  static boost::shared_ptr<AST> genSym(boost::shared_ptr<AST> lhs, 
		     const char *pfx="tmp",
		     const bool isTV = false);

  /** For each AST type, return an associated keyword name.
   *
   * @issue In most cases, the keyword name is simply the ast name. Later
   * versions of astmaker actually emit a string translator, and names
   * can now be assigned in the .ast file. We should consider using
   * that instead of this. That is something for shap to look at.
   */
  std::string atKwd() const;

  /** @brief Append all ident ASTs from a Binding-pattern to the ids vector. 
   *
   * if getPattern is true, it adds the identPattern ASTs
   * instead of ident ASTs
   */
  void getIds(std::ostream &errStream, 
	      std::vector<boost::shared_ptr<AST> >& ids,
	      bool getPattern = false);  

  /** @brief Utility function to call symType->getType() */
  boost::shared_ptr<Type> getType(); 
  /** @brief Utility function to call symType->getType() */
  boost::shared_ptr<const Type> getType() const;
  
  /** Union constructors may be written as Ctr or
   * union.Ctr, this function returns the pure constructor.
   *
   * @bug I do not understand what "pure" means in this context. I am
   * looking for a comment that says something of the form: "given an
   * AST of some kind X, obtain the AST Y s.t. SOMETHING is true".
   *
   * > pure is the wrong word here. It returns the bare constructor.
   * > That is, given a constructor of the form Ctr or union-name.Ctr, 
   * > returns Ctr.
   */
  boost::shared_ptr<AST> getCtr(); 


  /** @brief Given a defining occurrence, return a new AST for a use
   * occurrence. */
  boost::shared_ptr<AST> Use();

  /** @brief Copy constructor -- make an exact (except ID) shallow
   * copy. */
  AST(boost::shared_ptr<AST> ast, bool shallowCopyChildren=true);

  static inline boost::shared_ptr<AST>
  make(boost::shared_ptr<AST> ast, bool shallowCopyChildren=true) {
    AST *tmp = new AST(ast,shallowCopyChildren);
    return boost::shared_ptr<AST>(tmp);
  }

  /** @brief  Make an exact copy, deep, including symbolDef, type,
   * etc. */
  boost::shared_ptr<AST> getTrueCopy();

  /** @brief  Make an exact copy, deep, but clear symbolDef, type,
   * etc.
   *
   * @bug This needs an explation of why those fields should be
   * cleared. It also needs a better procedure name. */
  boost::shared_ptr<AST> getDCopy();

  /** @briefRename identifier @p from to @p to within AST named by @p
   * this.
   *
   * Rename is based on symbolDef. @p from must be a defining form. If
   * (and only if) @p from is found within @p this AST, it is also
   * renamed.
   */
  void rename(boost::shared_ptr<AST> from, std::string newName);

  /** @brief Return pretty-printed representation of this AST in the
   * form of a string. */
  std::string asString() const;

  /** @brief Get the unique identifier for this AST */
  boost::shared_ptr<AST> getID();

  /** @brief Return true IFF this is a AST corresponds to a union leg.
   *
   * @bug I'm still not clear why values can be union legs. 
   * > Constructors like Nil which have no arguments also represent
   * > constructed values 
   */
  bool isUnionLeg();
  /** @brief Return true IFF this AST is a method name identifier.
   *
   * @bug I'm still not clear why this doesn't use an id_method
   * category.
   * > Methods are legal values and can be used in id_value contexts.
   */
  bool isMethod();

  /** @brief Pretty print this AST to @p out, annotating each with its
   * type if @p showTypes is true, and appending a final end of line
   * of @p endline is true. */
  void PrettyPrint(std::ostream& out, bool showTypes = false, 
		   bool endline=true) const;

  // For use in GDB:
  void PrettyPrint(bool decorated) const;

  AST(const AstType at = at_Null);
  // for literals:
  AST(const AstType at, const AST_TOKEN_TYPE& tok);
  AST(const AstType at, const AST_LOCATION_TYPE &loc);
  AST(const AstType at, const AST_LOCATION_TYPE &loc,
      AST_SMART_PTR<AST> child1);
  AST(const AstType at, const AST_LOCATION_TYPE &loc,
      AST_SMART_PTR<AST> child1,
      AST_SMART_PTR<AST> child2);
  AST(const AstType at, const AST_LOCATION_TYPE &loc,
      AST_SMART_PTR<AST> child1,
      AST_SMART_PTR<AST> child2,
      AST_SMART_PTR<AST> child3);
  AST(const AstType at, const AST_LOCATION_TYPE &loc,
      AST_SMART_PTR<AST> child1,
      AST_SMART_PTR<AST> child2,
      AST_SMART_PTR<AST> child3,
      AST_SMART_PTR<AST> child4);
  AST(const AstType at, const AST_LOCATION_TYPE &loc,
      AST_SMART_PTR<AST> child1,
      AST_SMART_PTR<AST> child2,
      AST_SMART_PTR<AST> child3,
      AST_SMART_PTR<AST> child4,
      AST_SMART_PTR<AST> child5);
  ~AST();

  // Helper quasi-constructors
  static inline AST_SMART_PTR<AST>
  make(const AstType at = at_Null)
  {
    AST *ast = new AST(at);
    return AST_SMART_PTR<AST>(ast);
  }

  static inline AST_SMART_PTR<AST>
  make(const AstType at, const AST_TOKEN_TYPE& tok)
  {
    AST *ast = new AST(at, tok);
    return AST_SMART_PTR<AST>(ast);
  }

  static inline AST_SMART_PTR<AST>
  make(const AstType at, const AST_LOCATION_TYPE &loc)
  {
    AST *ast = new AST(at, loc);
    return AST_SMART_PTR<AST>(ast);
  }

  static inline AST_SMART_PTR<AST>
  make(const AstType at, const AST_LOCATION_TYPE &loc,
       AST_SMART_PTR<AST> child1)
  {
    AST *ast = new AST(at, loc, child1);
    return AST_SMART_PTR<AST>(ast);
  }

  static inline AST_SMART_PTR<AST>
  make(const AstType at, const AST_LOCATION_TYPE &loc,
       const AST_SMART_PTR<AST> child1,
       const AST_SMART_PTR<AST> child2)
  {
    AST *ast = new AST(at, loc, child1, child2);
    return AST_SMART_PTR<AST>(ast);
  }

  static inline AST_SMART_PTR<AST>
  make(const AstType at, const AST_LOCATION_TYPE &loc,
       const AST_SMART_PTR<AST> child1,
       const AST_SMART_PTR<AST> child2,
       const AST_SMART_PTR<AST> child3)
  {
    AST *ast = new AST(at, loc, child1, child2,
                       child3);
    return AST_SMART_PTR<AST>(ast);
  }

  static inline AST_SMART_PTR<AST>
  make(const AstType at, const AST_LOCATION_TYPE &loc,
       const AST_SMART_PTR<AST> child1,
       const AST_SMART_PTR<AST> child2,
       const AST_SMART_PTR<AST> child3,
       const AST_SMART_PTR<AST> child4)
  {
    AST *ast = new AST(at, loc, child1, child2,
                       child3, child4);
    return AST_SMART_PTR<AST>(ast);
  }

  static inline AST_SMART_PTR<AST>
  make(const AstType at, const AST_LOCATION_TYPE &loc,
       const AST_SMART_PTR<AST> child1,
       const AST_SMART_PTR<AST> child2,
       const AST_SMART_PTR<AST> child3,
       const AST_SMART_PTR<AST> child4,
       const AST_SMART_PTR<AST> child5)
  {
    AST *ast = new AST(at, loc, child1, child2,
                       child3, child4, child5);
    return AST_SMART_PTR<AST>(ast);
  }


  const AST_SMART_PTR<AST>
  child(size_t i) const
  {
    return children[i];
  }

  AST_SMART_PTR<AST>&
  child(size_t i)
  {
    return children[i];
  }

  void addChild(AST_SMART_PTR<AST> cld);
  ::std::string getTokenString();

  void
  addChildrenFrom(AST_SMART_PTR<AST> other)
  {
    for(size_t i = 0; i < other->children.size(); i++)
      addChild(other->child(i));
  }

  static const char *tagName(const AstType at);
  const char *astTypeName() const;
  const char *astName() const;

  bool isMemberOfType(AstType) const;
  bool isValid() const;
};


#endif /* AST_HXX */
