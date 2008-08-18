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
#include <libsherpa/EnumSet.hxx>

#include "shared_ptr.hxx"
#define AST_SMART_PTR boost::shared_ptr
#define AST_SUPERCLASS boost::enable_shared_from_this<AST>

 

struct Type; 
struct TypeScheme; 
struct spStruct;
struct UocInfo;
struct Instance;
 
/// @brief Different classifications of identifiers that we might
/// encounter. 
///
/// An IdentType is assigned by the resolver to each identifier AST
/// node.  There are two types of constants in this
/// enumeration. Constants whose names are of the form
/// <code>id_<em>name</em></code> are identifier classifications. Each
/// of these describes a particular type of identifier. All identifier
/// AST nodes wll be assigned such a concrete classification.
///
/// Constants whose names are of the form
/// <code>idc_<em>name</em></code> are identifier
/// <em>categories</em>. These describe sets of classifications that
/// are legal in a given occurrence context. For example, a normal use
/// occurrence of an identifier in an expression might validly resolve
/// to <code>id_value</code> (normal values), <code>id_ucon0</code>
/// (union constructors having no elements, which are enumerands), or
/// <code>id_method</code> (use-references to method identifiers). The
/// identifier categories are primarily used in the resolver's
/// recursive descent pass, where they specify what identifiers are
/// legal in what positions.

enum IdentType {
  /// @brief An identifier whose classification is not yet decided. 
  ///
  ///This is the constructor-time default value that is assigned to
  ///the identType field of the AST. It should not occur on any
  ///identifier AST after the symbol resolution pass, except in
  ///temporary ASTs that are introduced for expediency in various
  ///later passes and then resolved.
  id_unresolved,
  
  /// @brief %Type variables.
  id_tvar,

  /// @brief Union/repr name.
  id_union,

  /// @brief Structure type name.
  id_struct,

  /// @brief %Type class name.
  id_typeclass,

  /// @brief %Type class method name.
  id_method,

  /// @brief Structure or union constructor field name.
  id_field,
  
  /// @brief Locally bound name that references the exported subset of
  /// an imported interface.
  ///
  /// This is the local name defined in this module, not the fully
  /// qualified name.
  id_interface,

  /// @brief An identifier defined at define, let, lambda,
  /// do, switch, case.
  id_value,

  /// @brief Union constructor having one or more argument.
  id_ucon,

  /// @brief Union constructor taking no arguments.
  id_ucon0,
  
  ///////////////////////////////////////////////////////////////////
  //
  // Anything below here is a category that is some union of
  // IdentTypes above
  //
  ///////////////////////////////////////////////////////////////////
  idc_FIRST_CATEGORY,
  
  /// @brief %Type, which is one of <code>id_tvar</code>,
  /// <code>id_union</code>, or <code>id_struct</code>.
  idc_type = idc_FIRST_CATEGORY,
  
  /// @brief Value, which is one of <code>id_value</code>,
  /// <code>id_ucon0</code>, or <code>id_method</code>.
  idc_value,

  /// @brief Union constructor, which is one of <code>id_ucon</ucode>
  /// or <code>id_ucon0</code>.
  idc_uctor,
  
  /// @brief Constructor, which is one of <code>id_struct</code>,
  /// <code>id_ucon</code>, or <code>id_ucon0</code>.
  idc_ctor,

  /// @brief An entity (value or constructor) that can be applied,
  /// which is one of <code>id_value</code>, <code>id_method</code>,
  /// <code>id_struct</code> or <code>id_ucon</code>.
  idc_apply,
};
 
/// @brief Produces a printable string corresponding to the values in
/// the above enumeration. 
 std::string identTypeToString(IdentType id);


/* Declarations for AST FLAGS */

 enum AstFlags {

   // @brief Useful to have an empty set initializer:
   NO_FLAGS = 0,

   /// @brief Identifier is bound in type-level scope.
   ID_IS_GLOBAL = 0x00000001u,
   /// @brief Identifier was internally generated by the compiler.
   ID_IS_GENSYM = 0x00000002u,

   /// Set in the tail recursion analysis pass to indicate that a
   /// use-occurrence of an identifier is a reference to the function
   /// currently being defined. Consulted in the SSA pass to add an
   /// LB_IS_DUMMY marker to the emitted let binding. Consulted in the
   /// gen-c pass to determine when looping rather than recursion should
   /// be used.
   SELF_TAIL = 0x00000004u,

   /// The SSA pass constructs some dummy let bindings. This is used to
   /// mark them so that no assignment for them will later be emitted in
   /// the code generator.
   ///
   /// The SSA pass uses a trick to convert expression style code
   /// into statement style code. For example, in the expression
   /// (if e1 e2 e3), if E1, E2 and E3 are SSA converted forms of 
   /// e1, e2 and e3 respectively, the SSA converter will produce
   /// 
   /// (let* ((temp ((if E1 
   ///                   (let* ((temp  E2)) temp)
   ///                   (let* ((temp E3)) temp)))))
   ///      ... rest of the code will use temp as the value of if-expr ... 
   /// 
   /// Here, for the BitC resolver's point of view, the inner temp is
   /// different from the outer temp. So, it will resolve and type check
   /// corectly. The outer temp will have the result of the appropriate
   /// inner temp. 
   ///
   /// From the C code generator's point of view, we will ignore the
   /// outer let* binding, since we cannot bind the result of
   /// if-statements. This outer let* binding is therefore marked
   /// LB_IS_DUMMY. The code generator will only declare one local
   /// variable temp, and assign the result of the correct brach to
   /// it. The rest of the code after the if statement is fine since it
   /// just knows to use the name temp, which now stores the correct value. 
   LB_IS_DUMMY = 0x00000008u,

   /// Do not emit an assignment for this let binding. This is a special
   /// case similar to LB_IS_DUMMY. It is generated for like array, vector,
   /// etc., which should be ignored by the code generator, as the
   /// initialization will follow in a loop. 
   LB_POSTPONED = LB_IS_DUMMY,

   /// Indicates a global identifier that is not exported from its
   /// defining unit of compilation. These are emitted by the back end
   /// with a static marker to enable later optimization by the C
   /// compiler.
   ID_IS_PRIVATE = 0x00000010u,

   /// Marks a definition that is an external program entry point, and
   /// therefore a seed for polyinstantiation.
   DEF_IS_ENTRYPT = 0x00000020u,

   /// Marks a let binding as a member of a letrec. This is consulted in
   /// Symtab.cxx and TypeInfer.cxx to determine whether the bound
   /// identifier should be bound early or late. If it were someday
   /// useful, this could be eliminated by introducing a distinct
   /// at_letrecbinding.
   LB_REC_BIND = 0x00000040u,

   /// Marks a BitC identifier as already having an external name, which
   /// should be used during code generation in place of the BitC name.
   /// 
   /// This flag is not redundant in light of the externalName
   /// field. DEF_IS_EXTERNAL is marked for all external definitions,
   /// whether it has an external name or not only some external
   /// definitions have a external-name.
   DEF_IS_EXTERNAL = 0x00000080u,

   /// Used to mark a union consisting exclusively of constant
   /// legs, which is really an enum declaration. Marking is performed in
   /// TypeInfer, and is consulted in Type-size.cxx.
   ///
   /// @bug Regrettably our current handling of enumerations is
   /// inadequate, because it doesn't allow us to specify the actual
   /// enumeration values. We are therefore going to have to introduce a
   /// defenum at some point.
   ENUM_UN = 0x00000100u,

   /// Marks a union having only a single leg.
   /// 
   /// @bug It seems to me that a union having only one leg should be
   /// syntactically rejected, in which case this flag should never
   /// arise. Is there some USEFUL counter-example?
   ///
   /// @note If we reject single legged unions, this goes away, it is just
   /// maintained for completeness.
   SINGLE_LEG_UN = 0x00000200u,

   /// Marks a union that is subject to one of the required Cardelli
   /// optimizations.
   CARDELLI_UN = 0x00000400u,

   /// Marks the NULLABLE union, which has a special representation known
   /// to the code generator.
   NULLABLE_UN = 0x00000800u,

   ///////////////// SECOND ROUND OF FLAGS  /////////////////////

   /// Marks the place-holder identifier that is introduced in the
   /// at_sw_leg AST to replicate the (switch ...) temporary identifier.
   ///
   /// This flag ensures that an identifier that stores a de-constructed
   /// value in a switch statement can only appear on the left of 
   /// a select (.) operator. The specification enforces this rule so that
   /// the de-constructed value does not escape as a whole (as a return
   /// value, by assignment or closure). 
   ID_FOR_SWITCH = 0x00001000u,

   /// Identifier is closed over by something.
   ID_IS_CAPTURED = 0x00002000u,

   /// Marks identifiers that are closed over by some lambda.
   ///
   /// Only use occurences that actually lie within an enclosing lambda
   /// are marked with this flag.
   ID_IS_CLOSED = 0x00004000u,

   /// ID must be moved to the heap due to capture.
   ID_NEEDS_HEAPIFY = 0x00008000u,

   /// @brief Mark if an identifier is shalowly mutated in the local
   /// context.
   ///
   /// The flag is set if the identifier is a target of
   /// a set!. This information is used in heuristic type inference.
   ID_IS_MUTATED = 0x00010000u,

   /// @brief Mark if the identifier's ID_IS_MUTATED flag can still be
   /// updated.
   ///
   /// This flag ensures the global identifiers' mutability is not
   /// affected by usage beyond its definition. In the case of global
   /// definitions, the identifier's mutability is marked closed, at the
   /// end of its definition. 
   ID_MUT_CLOSED = 0x00020000u,

   /// Marked on proclaimations generated by the compiler (ex: during
   /// closure conversion). 
   /// The symbol resolver warns about local proclaimations in source
   /// modules withoutdefinitions. The flag indicates that such warnings
   /// must not be produced for compiler generated proclaimations. 
   PROCLAIM_IS_INTERNAL = 0x00040000u,

   /// @brief Initialization for this def can be implemented directly by
   /// the C compiler.
   ///
   /// Set in the SSA pass to identify trivial initializers so
   /// that the code generator can avoid adding code in the per-UoC init
   /// procedure.
   DEF_IS_TRIVIAL_INIT = 0x00080000u,

   /// @brief Identifier has already been mangled.
   ///
   /// Used in the instantiator to indicate identifiers that have already
   /// been mangled and should not be mangled a second time.
   ///
   /// @issue I don't understand why this is needed. The assignment of a
   /// mangled name is, in effect, the assignment of an externalName. I
   /// would think that it would make sense to place the mangled name into
   /// the externalName field and check that, removing this flag. That
   /// works equally well when a pre-existing external name has been
   /// assigned, because we must not mangle those. Is this merely a
   /// different choice of implemenation approach? Would the approach that
   /// I am outlining work? 
   ///
   /// @note The instantiator can be made to work by using the approach
   /// you are suggesting. However, using this flag keeps the name
   /// mangling on the 's' field more regular. This is because, the 
   /// 's' field of all definitions are mangled with a canonical encoding
   /// of their type. If we employ the rule that having an external name
   /// singifies that the identifier has already been mangled, the 
   /// 's' field of definitions for which the programmer provided an
   /// external name will not be mangled at all. However, this still does
   /// not lead to an error because of name collision. We currently
   /// have a rule that definitions with programmer specified external
   /// names cannot be polymorphic, and will therefore be instantiated at
   /// most once.  
   IDENT_MANGLED = 0x00100000u,

   /// @brief Local, non-generalizable variable.
   ///
   /// Marked for variables defined at non-generalizable boundaries. ex:
   /// Lambda parameters, identifiers defined at switch/catch, etc.
   ///
   /// Local generalizable variables must be handled by creating a new
   /// let-binding with all concrete instantiations. Non-generalizable
   /// locals like lambda-parameters, identifier at switch, catch, 
   /// do, etc. can be trivially handled by performing a name change to
   /// a canonical one.
   LOCAL_NOGEN_VAR = 0x00200000u,

   /// @brief Used internally to track variable scoping.
   ///
   /// This flag is used in determining the outermost let-binding at
   /// which a type variable used (scoped). The actual pointer to the
   /// let-binding is preset in tvarLB field.
   /// 
   /// This flag is set on at_letbinding once we are done processing it in
   /// the symbol resolver. When we encounter the use of a type variable,
   /// if we have finished processing the let-binding named by its tvarLB
   /// field, then we know that the scope of the variable is actually 
   /// bigger, and thus update it. 
   LBS_PROCESSED = 0x00400000u,

   /// @brief Identifier is observably defined
   ID_OBSERV_DEF = 0x00800000u,

   /// @brief Type variable that was temporarily created by the
   /// polyinstantiator.
   ///
   /// The symbol resolver accepts type variables as defining
   /// occurences within expressions only at certain positions (when
   /// called with NEW_TV_OK). For example, this is legal within value
   /// definitions, but type variables not identified as arguments to 
   /// type definitions are legal within the type definition itself. 
   /// This flag marks type variables created by the polyinstantiator
   /// that must not be subject to the NEW_TV_OK check by the resolver,
   /// since the Instantiator makes some quasi RandTs in which this
   /// restriction might temporaroly not hold.
   TVAR_POLY_SPECIAL = 0x01000000u,

   /// @brief Indicates that this DEFUNION is actually a DEFREPR that was
   /// converted to a union by the reprSimp pass.
   UNION_IS_REPR = 0x02000000u,

   /// @brief Indicates a field that is a union discriminator (tag)
   /// field.
   FLD_IS_DISCM = 0x04000000u,

   /// @brief Marked on top-level at_define.
   ///
   /// Indicates that this is a hoisted lambda
   /// for a function that has a captured closure, and we therefore need
   /// to emit a transition function.
   LAM_NEEDS_TRANS = 0x08000000u,

   /// @brief Marks whether the inner_ref is indexing or selecting.
   ///
   /// When set, it is an indexing inner_ref that is of the form
   /// (inner_ref (dup (array ..))) or (inner_ref (vector ... ))
   /// 
   /// When clear, it is a selecting inner_ref that is of the form
   /// (inner_ref (dup (structv ... ))) or (inner_red (structr ... ))
   ///  
   /// These two types of inner-ref must be handled differently in most
   /// passes. For example, the index argument must be independently
   /// resolved and type checked but the field argumet must not.
   INNER_REF_NDX = 0x10000000u,

   /// @brief Parameter is by-reference.
   ///
   /// Set in the parser to indicate that a parameter identifier is
   /// by-reference. Consulted in gen-c.cxx to determine how the
   /// corresponding C parameter should be emitted.
   ///
   /// In theory, the by-ref-ness is a part of the type, and must
   /// be obtainable from the identifier's type. However, the
   /// implementation does not encode by-ref this way. The by-ref is
   /// noted 

   /// - On the components of a function type 
   /// - As an AST flag on the identifier AST.
   ///
   /// Encoding by-ref in this way lends itself to cleaner implementation.
   /// If by-ref were to be a type constructor, we must look beyond the
   /// by-ref constructor at every r-value usage of the identifier. 
   ARG_BYREF = 0x20000000u,

   /// Set of ast flag values that must be masked from definitions when
   /// copying to use cases.
   MASK_FLAGS_FROM_USE = (DEF_IS_ENTRYPT|ID_IS_CAPTURED)

 } /* AstFlags */;

struct AST;

/// @brief Set of environments associated with a given AST node.
///
/// See the comment at the envSet field of the AST class.
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

  // Quasi-constructors:
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
  sherpa::EnumSet<AstFlags> flags;

  unsigned printVariant;	// which syntax to use for pretty printing

  boost::shared_ptr<TypeScheme> scheme;		// defining occurrences only
  boost::shared_ptr<Type> symType;		// the (pre-unified) type
  boost::shared_ptr<AST> symbolDef;

  bool isDecl;                  // is This a declaration or definition?

  bool isGlobal() { return (flags & ID_IS_GLOBAL); }
  bool isFnxn(); // Function
  size_t nBits();
  bool isLocation();
  bool isLiteral();
  bool isTopLevelForm();
  bool isIdentType(IdentType idt);
  bool leadsToTopLevelForm();
  void clearTypes(); // Clear the sumType and scheme fields 
                     // of this AST and ALL children RECURSIVELY.
                     // This needs to be done before re-typing an
                     // expression, for example post-clconv
                     // otherwise, old types will infulence
                     // new type records.
 
  /// On a declaration AST only: if a definition for it has been seen,
  /// then defn points to the definition.
  boost::shared_ptr<AST> defn;

  /// On a definition AST: pointer to the definitive declaration (the
  /// first one).
  boost::shared_ptr<AST> decl;

  /// If this is a global identifier, fqn is its canonical fully
  /// qualified name. The interface component of an fqn is the IFNAME
  /// of the interface in which the symbol was defined.  the ident
  /// component is the identifier under which the symbol was defined at
  /// the point of its original definition, which may or may not be the
  /// name that was bound in an importing environment (because USE may
  /// have aliased it under a different name).
  FQName fqn;

  /// If this identifier is proclaimed with "external IDENT", this field
  /// holds the external identifier string, otherwise it is empty.
  std::string externalName;

  /// A set of environments associated with this ast. 
  /// - If this is a local name for an imported interface, these are
  ///   pointers to the environments of the imported interface.
  /// - In at_let, at_letrec, at_letStar, at_define ... these are the
  ///   environments in effect at that binding.
  EnvSet envs;

  /// @brief Pointer to defining form
  ///
  /// defForm has context sensitive meaning. 
  /// This is basically a hack in the absence of parent pointers, where
  /// I must reach back into the definition and get the defining form,
  /// or some form of a containing datastructure.
  /// 
  /// In the case of top-level expressions, we use it to get the 
  /// entire defining form from the identifier being defined
  /// 
  /// (def  defIdent ... )
  /// ^        |
  /// |________|
  ///   defForm
  /// 
  /// In the case of let-expressions:
  /// (The quoted phrases are just ast-types not syntax.)
  ///
  ///  _______________      __________________
  /// |   defForm     |     |   defForm       |  
  /// v               |     v                 |
  /// (define   ... (let .. ("letbindings" ("letbinding" ident ... ))))
  ///               ^             |        ^               | 
  ///               |_____________|        |_______________|
  ///                  defForm                 defForm
  /// 
  ///     ___________
  ///    |           |
  ///    v           | 
  ///  (switch ... ("sw_leg" id ...) ...)
  ///                  ^    |
  ///                  |____|
  ///     _____      __________
  ///    |     |    |          |
  ///    v     |    v          | 
  ///  (do ("dobindings"  ("dobinding" id ...) ...)
  ///                             ^     |
  ///                             |_____|

  boost::shared_ptr<AST> defForm;

  /// Used in the case of tail-recursion to in-place update the
  ///  function-arguments with new values before jumping to the start
  ///  of the function.
  boost::shared_ptr<AST> defbps;
  
  /// If this is a union, tagType is the type of the tag.
  boost::shared_ptr<Type> tagType;

  /// Number of bits in a bitfield type. 
  /// On following ASTs, this fiield  store the total accumulated
  /// field_bits in all component ASTs.
  /// 
  ///  at_field, at_fill, at_reserved, tagtype (declare) at_declares,
  ///  Identifier of at_defunion 
  size_t field_bits;
  
  /// On defreppr unions, this field is present on fields that
  /// participate in union discrimination. This field stores the
  /// value (not just a true/false setting) that the field must have
  /// as indicated by the when clause. 
  size_t unin_discm;

  /// Total number of fill bits that are not reported by the type
  /// record.  Appears on a at_defstruct or at_constructor only.
  size_t total_fill;

  /// TypeScheme for the Structure definition corresponding to a union
  /// constructor. 
  boost::shared_ptr<TypeScheme> stSigma;

  /// Union Constructors with identical layout must share the same
  /// structure type since they can be used in the same switch leg.
  /// Structures equality is nominal in BitC (structures are
  /// identified by name, not by identical components). Therefore, if
  /// at a second constructor, a previously typed structure  with
  /// identical structure is found, the new constructor uses the
  /// structure defined at the old structure and stores a pointer to
  /// that constructor in stCtr.
  boost::shared_ptr<AST> stCtr;

  // Tracking scope of type-variables:

  /// Let binding at which this type variable is scoped. 
  /// This flag is set reliably set whenever the resolver is
  /// called, so there is no need to worry while cloning the AST?
  boost::shared_ptr<AST> tvarLB;

  /// Let-Binding within which this at_letbindings is contained. In
  /// the case of the outer-most let-bindings, this field points to
  /// the top-level define/recdef. 
  /// This flag is set everytime the resolver is called, so there is
  /// no need to worry while cloning an AST. 
  boost::shared_ptr<AST> parentLB;

  static boost::shared_ptr<AST> makeBoolLit(const sherpa::LToken &tok);
  static boost::shared_ptr<AST> makeIntLit(const sherpa::LToken &tok);
  static boost::shared_ptr<AST> makeStringLit(const sherpa::LToken &tok);
  static boost::shared_ptr<AST> makeCharLit(const sherpa::LToken &tok);
  static boost::shared_ptr<AST> makeFloatLit(const sherpa::LToken &tok);

  /// Remove child @p n from this AST.
  ///
  /// @brief The only place this is called is in the parser, and there
  /// only to strip documentation strings. That is actually a bad thing
  /// to do, since we want to preserve them in the AST, and a literal
  /// value really ought to be a perfectly legal (if useless)
  /// expression. We should attempt to comment out the body of
  /// stripDocString() and see if everything still works. If so, both
  /// this and stripDocString() should be removed.
  ///
  /// @bug Shap needs to review this.
  void disown(size_t s);
  
  /// Generate an identifier AST with a newly generated internal name.
  /// This will not have any particular type assigned to it, and is
  /// intended to be used in the front end passes before the type
  /// checker is involved.
  static boost::shared_ptr<AST> 
    genIdent(const char *pfx = "tmp", const bool isTV = false);

  /// Generate an at_ident AST providing a temporary symbol that is
  /// type-compatible with the type of the passed AST @p lhs.
  static boost::shared_ptr<AST> genSym(boost::shared_ptr<AST> lhs, 
		     const char *pfx="tmp",
		     const bool isTV = false);

  /// For each AST type, return an associated keyword name.
  ///
  /// @issue In most cases, the keyword name is simply the ast name. Later
  /// versions of astmaker actually emit a string translator, and names
  /// can now be assigned in the .ast file. We should consider using
  /// that instead of this. That is something for shap to look at.
  std::string atKwd() const;

  /// @brief Append all ident ASTs from a Binding-pattern to the ids
  /// vector.
  ///
  /// if getPattern is true, it adds the identPattern ASTs
  /// instead of ident ASTs
  void getIds(std::ostream &errStream, 
	      std::vector<boost::shared_ptr<AST> >& ids,
	      bool getPattern = false);  

  /// @brief Utility function to call symType->getType()
  boost::shared_ptr<Type> getType(); 
  /// @brief Utility function to call symType->getType()
  boost::shared_ptr<const Type> getType() const;
  
  /// Given a union constructor of the form Ctr or union-name.Ctr, 
  /// returns Ctr.
  boost::shared_ptr<AST> getCtr(); 


  /// @brief Given a defining occurrence, return a new AST for a use
  /// occurrence.
  boost::shared_ptr<AST> Use();

  /// @brief Copy constructor -- make an exact (except ID) shallow
  /// copy.
  AST(boost::shared_ptr<AST> ast, bool shallowCopyChildren=true);

  static inline boost::shared_ptr<AST>
  make(boost::shared_ptr<AST> ast, bool shallowCopyChildren=true) {
    AST *tmp = new AST(ast,shallowCopyChildren);
    return boost::shared_ptr<AST>(tmp);
  }

  /// @brief  Make an exact copy, deep, including symbolDef, type,
  /// etc.
  boost::shared_ptr<AST> getTrueCopy();

  /// @brief  Make an exact deep copy.
  /// 
  /// This function clears fields such as symbolDef, type, etc. 
  /// This is because (1) we don't need them since new ASTs are almost
  /// immediately subject to resolver and type check. (2) We sometimes
  /// start from a copy of an AST and use it in a way that the original
  /// symbolDef/type are no longer valid, so we clear these fields
  /// preemptively. 
  boost::shared_ptr<AST> getDeepCopy();
  
  /// @briefRename identifier @p from to @p to within AST named by @p
  /// this.
  ///
  /// Rename is based on symbolDef. @p from must be a defining form. If
  /// (and only if) @p from is found within @p this AST, it is also
  /// renamed.
  void rename(boost::shared_ptr<AST> from, std::string newName);

  /// @brief Return pretty-printed representation of this AST in the
  /// form of a string.
  std::string asString() const;

  /// @brief Get the unique identifier for this AST
  boost::shared_ptr<AST> getID();

  /// @brief Return true IFF this is a AST corresponds to a union leg.
  bool isUnionLeg();
  /// @brief Return true IFF this AST is a method name identifier.
  bool isMethod();

  /// @brief Pretty print this AST to @p out, annotating each with its
  /// type if @p showTypes is true, and appending a final end of line
  /// of @p endline is true.
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
