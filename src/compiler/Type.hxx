#ifndef TYPE_HXX
#define TYPE_HXX

/**************************************************************************
 *
 * Copyright (C) 2008, Johns Hopkins University.
 * Copyright (C) 2010, Jonathan S. Shapiro
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

#include <stdlib.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <string>
#include <set>
#include <vector>

#include <libsherpa/INOstream.hxx>
#include <libsherpa/EnumSet.hxx>

#include "AST.hxx"
#include "debug.hxx"
#include "TvPrinter.hxx"
#include "Environment.hxx"
#include "TypeInfer.hxx"
#include "Trail.hxx"

// Elements of the TypeTag enumeration have moved to types.def

enum TypeTag { // What kind of type?
#define DEFTYPE(nm,prim,csimp,atom,scalar,ref,impint,impfloat) ty_##nm,
#include "types.def"
};

const char *TypeTagName(TypeTag tt);

typedef long long TargetSWord;

struct Type;
struct TCConstraints;

enum CompFlagValues {
  COMP_NO_FLAGS = 0u,

  /// @brief Union discriminator of defrepr
  COMP_UNIN_DISCM  = 0x01u,

  /// Field (component) in a defrepr leg is
  /// marked invalid within switch statement
  /// when used in conjuction with other
  /// constructors (only the common fields
  /// must be valid) The switched value is
  /// only valid within select operations,
  /// and is therefore only checked there.   
  COMP_INVALID     = 0x02u,

  /// This flag is marked on the components of a function argVec at
  /// positions where by-ref arguments are expected.
  ///
  /// The Flag is not marked on the Component types themselves
  /// because this will cause problems with flag propagation during
  /// unification (especially since the component type may be a type
  /// variable. We do not have a (by-ref 'a) type itself because that
  /// will result in types such as (mutable (byref 'a)) during
  /// inference, and we will need normalization.  Therefore, by-ref is
  /// marked on ty_argvec components. Two argVecs can unify only if
  /// all of their components and flags match.
  ///
  /// Valid on ty_argvec only.
  COMP_BYREF       = 0x04u,

  /// This flag is marked on the components of a function argVec at
  /// whenever we don't know the by-ref status of the argument.
  /// Whenever this flag is set, this parameter's by-ref status is
  /// open, and can later become by-ref due to unification. If this
  /// flag is clear, the absence of COMP-BYREF flag decisively states
  /// that the argument is passed by value only.
  ///
  /// This flag is used during inference when we construct a function
  /// type at the time of application, but do not know the exact
  /// by-ref state of the parameters. 
  ///
  /// Valid on ty_argvec only.
  COMP_MAYBE_BYREF = 0x08u,
};
typedef sherpa::EnumSet<CompFlagValues> CompFlagSet;

struct comp {
  std::string name;
  boost::shared_ptr<Type> typ;
  CompFlagSet flags;

  comp() {flags = COMP_NO_FLAGS;} 
  comp(boost::shared_ptr<Type> t, 
       CompFlagSet _flags = COMP_NO_FLAGS);
  comp(const std::string s, boost::shared_ptr<Type> t, 
       CompFlagSet _flags = COMP_NO_FLAGS);

  // Quasi-constructors
  static inline boost::shared_ptr<comp>
  make() {
    comp *tmp = new comp();
    return boost::shared_ptr<comp>(tmp);
  }

  static inline boost::shared_ptr<comp>
  make(boost::shared_ptr<Type> t, CompFlagSet _flags = COMP_NO_FLAGS) {
    comp *tmp = new comp(t, _flags);
    return boost::shared_ptr<comp>(tmp);
  }

  static inline boost::shared_ptr<comp>
  make(const std::string& s, 
       boost::shared_ptr<Type> t, CompFlagSet _flags = COMP_NO_FLAGS) {
    comp *tmp = new comp(s, t, _flags);
    return boost::shared_ptr<comp>(tmp);
  }
  //comp(const comp &c);
};  

// Array lengths can in some cases get unified even when their
// containing type cannot. See the comment on the arrLen field in
// class Type.
//
// This wrapper class was necessary for the early GCPtr
// implementation. We could remove it now, but having a wrapper to
// provide the make() function is useful.
struct ArrLen {
  uint64_t  len;
  ArrLen(uint64_t _len) {len = _len;}

  // Quasi-constructor
  static inline boost::shared_ptr<ArrLen>
  make(uint64_t _len) {
    ArrLen *tmp = new ArrLen(_len);
    return boost::shared_ptr<ArrLen>(tmp);
  }
};

/// @brief Mark values for types.
///
/// The following marks are present on types to ensure that
/// procedures that recurse over the structure of equi-recursive
/// types do not recurse infinitely. We need to use differernt markers
/// for procedures that are mutually recursive, or are otherwise used
/// simultaneously. Here, we actually use different markers for each
/// procedure that is recursive over type-structure, except for some
/// simple self-recursive predicates which share the mark MARK_PREDICATE.

enum MarkFlagValues {
  // Base Mark
  MARK_NONE                     = 0x0,

  // Shared by many predicates over Types
  MARK_PREDICATE                = 0x0000001u,

  // Basic Functions: these functions are called by almost all
  // routines, and must never share flags with anything.
  MARK_GET_BARE_TYPE            = 0x0000002u,
  MARK_GET_THE_TYPE             = 0x0000004u,
  
  // Functions that do not change type structure
  MARK_SIZE                     = 0x0000008u,
  MARK_MANGLED_STRING           = 0x0000010u,
  MARK_EMIT_ARR_VEC_FN_TYPES    = 0x0000020u,
  MARK_CHECK_CONSTRAINTS        = 0x0000040u,
  MARK_CHECK_MUT_CONSISTENCY    = 0x0000080u,
  MARK_COLLECT_FTVS_WRT_GAMMA   = 0x0000100u,
  MARK_COLLECT_ALL_FTVS         = 0x0000200u,
  MARK_MAXIMIZE_MUTABILITY      = 0x0000400u,
  MARK_MINIMIZE_MUTABILITY      = 0x0000800u,
  MARK_MINIMIZE_DEEP_MUTABILITY = 0x0001000u,
  MARK_MIN_MUT_CONSTLESS        = 0x0002000u,
  MARK_NORMALIZE_CONST          = 0x0004000u,

  // Functions that affect type structure/contents
  MARK_SIGN_MBS                 = 0x0008000u,
  MARK_ADJ_MAYBE                = 0x0010000u,
  MARK_FIXUP_FN_TYPES           = 0x0020000u,
  MARK_PROPAGATE_MUTABILITY     = 0x0040000u,
  MARK_NORMALIZE_MBFULL         = 0x0080000u,
  MARK_NORMALIZE_CONST_INPLACE  = 0x0100000u,
  MARK_ENSURE_MINIMIZABILITY    = 0x0200000u,
};
typedef sherpa::EnumSet<MarkFlagValues> MarkFlags;


/// @brief Definition for Type flags
enum TypeFlagValues {
  TY_NO_FLAGS     = 0x0,

  /// @brief Typeclass constraint is subsumed by another (ex: by a
  /// derived class).
  TY_CT_SUBSUMED  = 0x01u,
  /// @brief Typeclass constraint is due to self definition.
  TY_CT_SELF      = 0x02u,
  TY_RIGID        = 0x04u,
  /// @brief Candidate for Copy-Compatibility.
  ///
  /// This flag is for type variables that are in type argument
  /// position for structure or union types only. If set, it means
  /// that this type variable has not been captured within another
  /// reference type constructor, and the composite type is free to be
  /// copy-compatible at this position.
  TY_CCC          = 0x10u,
  /// @brief A temporary flag used in closure computation of FTVs in a
  /// TypeScheme.
  TY_CLOS         = 0x20u,
  /// @brief Flag used by adjMaybe to coerce only certain maybe-types.
  ///
  /// This flag must be marked on maybe-Var()s.  This flag is NOT
  /// cleared by the adjMaybe function.
  TY_COERCE       = 0x40u,
  /// @brief Flag used to fixup const types within composite
  /// data-structure definitons. 
  ///
  /// If an argument appears within a const type, it must be
  /// instantiated to a mb_Full type to ensure completeness of
  /// inference. This flag identifies all such type arguments to
  /// structure/union definitions that appear somewhere within a 
  /// const type. 
  TY_ARG_IN_CONST = 0x80u
};
typedef sherpa::EnumSet<TypeFlagValues> TypeFlags;

enum PrintOptionValues {
  PO_NO_FLAGS     = 0x0,

  /// @brief Suppress link field traversal
  ///
  /// The pretty printer normally traverses the unification link
  /// fields to find the resolved type. If this option is set,
  /// traversal is suppressed, which can be useful for debugging
  /// purposes.
  PO_NO_TRAVERSE  = 0x01u,

  /// @brief Display type linkages.
  ///
  /// If this option is set, the traversal information will be
  /// displayed, which can be useful for debugging purposes.
  PO_SHOW_LINKS  = 0x02u,

  /// @brief Display internal field types
  ///
  /// For debugging purposes it is often useful to see aggregate types
  /// displayed in "exploded" form.
  PO_SHOW_FIELDS  = 0x04u,
};
typedef sherpa::EnumSet<PrintOptionValues> PrintOptions;

// Specialization mask -- those flags which should NOT survive
// specialization. 
const TypeFlags TY_SP_MASK  =
  (TypeFlags(TY_CT_SELF) | TY_RIGID | TY_CCC | TY_CLOS | TY_COERCE | TY_ARG_IN_CONST);

struct Type;
typedef std::set<boost::shared_ptr<Type > > TypeSet;

struct TypeScheme;

struct Type : public boost::enable_shared_from_this<Type> {
  
  friend struct TypeScheme;

 private:  
  static unsigned long long typeCount; // To generate New Type Variables
  
  // Generator for unique Type record id's.
  // Use this ONLY in the constructor!
  static inline unsigned long long 
  genTypeID()
  {
    return ++typeCount;
  }

public:
  TypeTag typeTag;
  
  /// @brief Pointer to next element of a chain constrained by
  /// equality.
  ///
  /// This is set by the unifier.
  boost::shared_ptr<Type> link;
  
  /* If this is the type node that is authoritative
     to answer to the link, it will be the last one, and 
     link will be NULL. If so, examine the details */
  
  // Depending on type, store a reference 
  // to the actual type. The following 3 fields should be in a union, 
  // but using a union makes the C++ constructor logic unhappy....

  // The unique type record ID is used both to name type records for 
  // printing purposes and also to provide unique alpha variable names.
  // Since every post-unified alpha variable is represented by a Type
  // structure, we re-use the uniqueID of the Type structure as the
  // unique alpha-renaming of the alpha variable.

  const unsigned long long uniqueID; // unique type record ID

  // defAST points to the AST of the defining occurrence. This field
  // is set for ty_struct*, ty_union*, tu_ucon, ty_exn
  boost::shared_ptr<AST> defAst;  // Defining occurrence (or declare)

  // If type is a union constructor, points to identifier AST of the
  // defining occurrence of the defunion.  If type is a type class
  // method type, points to the identifier AST of the typeclass.
  boost::shared_ptr<AST> myContainer;

  // Note that we use two different type tags, ty_int/ty_impint,
  // ty_float/ty_impfloat, to deal with whether the concrete type of a
  // literal has been successfully decided.
  //
  // The minXRep fields tell us the smallest number of bits that is
  // guaranteed to hold the given value according to whether the
  // result of the unification is signed or unsigned. For floating
  // point values, we use minSignedRep to describe the required
  // exponent size. In the integer cases, a value of "65" indicates
  // that the literal must be handled as an arbitrary precision
  // integer.

  uint8_t   minSignedRep;        // minimum signed representation size
  uint8_t   minUnsignedRep;        // minimum signed representation size

  // Array length is actually a type variable, which ultimnately
  // resolves to  singleton types. Strictly speaking, we must use type
  // variables here. Since we do not have singleton types or dependent
  // types over arrays, we have arrlen in the type record
  // itself. However, we still need a level of indirection because the
  // lengths must also be unified during unification [think of
  // unification calls such as 
  //     U(t1->minimizeMutability() == t2->mininmizeMutability()) ]
  // Implicitely, an indirection entry with `0' in it is a length
  // variable. Copy constructor and TypeSpecialize DO NOT copy this
  // variable deeply.  This "variable" is not subject to
  // generalization.  
  //
  // The use case here arises from copy compatibility. In particular,
  // given a copy between
  //       (array (mutable T) n)  ==~  (array T n)
  //
  // we need to unify the T and the n, but we cannot unify the overall
  // types, because they differ w.r.t. mutability.
  
  boost::shared_ptr<ArrLen> arrLen;        // Length in the case of an array type
  
  LitValue  litValue;                // for literal types

  size_t    Isize;                // size in fixint
  TypeSet   fnDeps;                // Functional Dependencies (for 
                                //   Type classes only).
  
  std::vector<boost::shared_ptr<comp> > components;
  std::vector<boost::shared_ptr<Type> > typeArgs;  

  /// @brief Method names and their types for structures and objects
  std::vector<boost::shared_ptr<comp> > methods;
    
  // Mark Flags:  used for Traversal
  // Used to prevent infinite recursion
  // while printing infinitely recursivetypes.
  MarkFlags mark;                // General traversal
  unsigned pMark;                // Type printer
  boost::shared_ptr<Type> sp;    // Type specializer.
  TypeFlags flags;               

  // Main (Base) Constructor
  Type(const TypeTag ttag);
  // Copy Constructor.
  Type(boost::shared_ptr<Type> t); 
  Type(const TypeTag ttag, boost::shared_ptr<Type> child);
  Type(const TypeTag ttag, boost::shared_ptr<Type> child1, boost::shared_ptr<Type> child2);

  // Quasi-constructors
  static inline boost::shared_ptr<Type>
  make(const TypeTag ttag)
  {
    Type *tmp = new Type(ttag);
    return boost::shared_ptr<Type>(tmp);
  }

  static inline boost::shared_ptr<Type>
  make(boost::shared_ptr<Type> t)
  {
    Type *tmp = new Type(t);
    return boost::shared_ptr<Type>(tmp);
  }

  static inline boost::shared_ptr<Type>
  make(const TypeTag ttag, boost::shared_ptr<Type> child)
  {
    Type *tmp = new Type(ttag, child);
    return boost::shared_ptr<Type>(tmp);
  }

  static inline boost::shared_ptr<Type>
  make(const TypeTag ttag,
       boost::shared_ptr<Type> child1, 
       boost::shared_ptr<Type> child2)
  {
    Type *tmp = new Type(ttag, child1, child2);
    return boost::shared_ptr<Type>(tmp);
  }

  // Makes a deep copy , but ** LINKS TVARS TO ORIGINAL ONES ** 
  // This function calls TypeSpecialize on a typeScheme with
  // no free variables.
  // This is different from the type_instance() in 
  // the typeScheme class in that this function makes a copy
  // of non-tvars, while type_instance() returns the original type.
  boost::shared_ptr<Type> getDCopy();

private:
  boost::shared_ptr<const Type> getTypePrim() const;
  boost::shared_ptr<Type> getTypePrim();
  // Normalize Mutability Constructor Idempotence
  // (mutable (mutable t)) == (mutable t)
  boost::shared_ptr<Type> normalize_mut();
  
  // Normalize trivial forms of mbFull such as 
  // (copy-compat (mutable 'a) bool) to (mutable bool)
  // This normalization is done in-place.
  void normalize_mbFull(boost::shared_ptr<Trail> trail=Trail::make());

  // Inplace partial Const normalization
  // Normalize types such as (const bool) to bool inplace.
  // This const-reduction is ONLY DONE when a const constructor can be
  // dropped. It does not reduce (const ('a, 'b)) to 
  // ((const 'a), (const 'b))
  void normalize_const_inplace(boost::shared_ptr<Trail> trail=Trail::make());

  // Complete const normalization: Returns a new type where const only
  // wraps type variables or maybe types.
  // This is NOT an in-place normalization
  boost::shared_ptr<Type> normalize_const(const bool inConst=false);
  
public:
  boost::shared_ptr<Type> getType();  
  boost::shared_ptr <const Type> getType() const;
  
  // Get the type without mutability / fix / maybe
  boost::shared_ptr<Type> getBareType();  
  // Get the type without some combinations of the above
  boost::shared_ptr<Type> getTheType(bool mutableOK=false, bool maybeOK=false);   

  // The only reason the following functions are not marked const is
  // that they call getType(), which uses the mark flag. 
  bool isUnion(bool ignMut=true); // 1. Union type.
  bool isUcon(bool ignMut=true);  // 2. Union Constructor.
  bool isUval(bool ignMut=true);  // 3. Constructed union value (includes zero arity ctrs).
  bool isULeg(bool ignMut=true);  // 2 or 3.
  bool isUType(bool ignMut=true); // 1, 2, or 3.
  bool isMethod(); // 1, 2, or 3.
  bool isDecl();
  bool isException();
  bool isStruct();
  bool isArray();
  bool isVector();
  bool isObject();
  bool isTvar();
  bool isVariable(); // Checks beyond mutability maybe-ness
  bool isAtomic();
  bool isSimpleTypeForC();
  bool isScalar();
  bool isRefType();
  bool isValType();
  bool isByrefType();
  bool isArrayByref();
  bool isIndexableType();
  bool isNullableType();
  bool isConstrainedToRefType(boost::shared_ptr<TCConstraints> tcc);
  bool isFnxn();
  bool isBaseConstType(); // Integers, floats, string, bool, unit, dummy.
  bool isClosure();
  bool isImmutableRefType();
  bool isMutable();  // (mutable T)
  bool isMutType();  // (mutable T) or (mutable 'a)|T
  bool isConst();    // (const T)
  bool isEffectivelyConst(); // T such that allwrapped within consts
  bool isMaybe();
  bool isMbFull();
  bool isMbTop();
  bool isConcrete();
  bool isPrimaryType();
  bool isPrimInt();
  bool isPrimFloat();
  void SetTvarsTo(boost::shared_ptr<Type> t);
  void SetTvarsToUnit();
  bool isInteger();
  bool isIntegral();
  bool isbool();
  bool isFloat();
  bool isTypeClass();
  bool isPcst();
  bool isOfInfiniteType();
  bool isConstReducible(); // (const T) is equivalent to minimizeMutability(T)

  size_t nBits();
  bool needsCaptureConversion();

  // Check if values of a type cannot be returned or be captured
  // Currently true only for array-by-ref
  bool isNonEscaping();

  // Test if the type is a variable that can be substituted with
  // another type. Tests if the type is a variable (mbFull or mbTop)
  // wherein the variable (possible within Var() component) is 
  // not marked RIGID, unless, fllags indicate that rigidity 
  // must be ignored.  
  bool isUnifiableVar(UnifyFlags uflags=UFLG_NO_FLAGS);
  
  // Mark significant MB-tvars.
  // Mb-Tvars that need not be preserved semantically are:
  //  (1) at a copy position of a function argument or return type.
  //  (2) at a copy-argument-position of typeclass argument.
  // (1) is detected automatically, for (2) pass cppos-true at start.
  // Actually what this does is an "unmark" on the TY_COERCE flag, not
  // a new mark. The idea is that only generalizable FTVs should be
  // marked this way. So, mark all generalizable TVs with TY_COERCE,
  // and this routine will unmark all those coercions that will alter
  // semantic meaning.
  void markSignMbs(bool cppos=false);
  
  // Due to the above normalization, instantiated function types will
  // not be of the proper template (always with maybe types in
  // argment/return position. This will be require fixup at
  // instantiation. Note that the types of non-generalizable functions
  // are not affected by this fixup as they will not be removed due to
  // normalization in the first place,
  void fixupFnTypes();

  // To ensure completeness of inference, bare type variables cannot
  // occur within a const meta-constructor. They must only occur
  // within mbFull types. However, this construction is not possible
  // during structure/union definitions since we cannot create extra
  // variables apart from those specified in the argument list. 
  // Therefore, at the time of instantiation, fix the above problem
  // with construction, and instantiate all variables occuring at
  // shallow-positions within a const meta-constructor to mbFull types.
  void fixupConstArguments(boost::shared_ptr<Trail> trail);

  /* Produce Type ty_union[rv] from ty_ucon[rv] or ty_uval[rv]
     ONLY typeArgs are polylated */
  boost::shared_ptr<Type> getUnionType();
    
  size_t size();

  /* Methods used in Equational Unification */
  // Is this type Mutable upto function boundary?
  bool isDeepMut();
  // Is the type known to be Immutable upto function boundary?
  bool isDeepImmut();
  // Does this type contain variables only within functions or on the
  // lhs of a matybe type?
  bool isConcretizable();
  // Does this type contain variables only within functions,
  // references or on the lhs of a matybe type?
  bool isShallowConcretizable();

  // Normalize a type in-place. The following normalizations are
  // currently performed: 
  // 1) Normalization of mbFull.
  void normalize(boost::shared_ptr<Trail> trail=Trail::make());

  /* Methods that can be used for various kinds of 
     comparisons between two types */

private:  
  bool eql(boost::shared_ptr<Type> t, bool verbose, std::ostream &errStream,
           UnifyFlags uflags, bool keepSub,
           boost::shared_ptr<Trail> trail=Trail::make());
public:
  // Returns true of the type `t' is structurally equal to `this'
  // under alpha renaming (and declarations unify with definitions)
  // 
  // The next function strictlyEquals removes the above two
  // restrictions. 
  bool equals(boost::shared_ptr<Type> t, bool verbose=false,
              std::ostream &errStream=std::cerr);
  bool strictlyEquals(boost::shared_ptr<Type> t, bool verbose=false,
                      bool noAlphaRename=false,
                      std::ostream &errStream=std::cerr);  
  bool unifyWith(boost::shared_ptr<Type> t, bool verbose=false,
                 boost::shared_ptr<Trail> trail=Trail::make(),
                 std::ostream &errStream=std::cerr);

  // Unify Ignoring rigidity
  bool forcedUnify(boost::shared_ptr<Type> t, bool verbose=false,
                   std::ostream &errStream=std::cerr);
  
  // All Tvars are rigid?
  bool allTvarsRigid();

  // Equality under alpha renaming of all (including rigid)
  // variables. The following functions are the same as equals and
  // strictlyEquals except for the fact that they ignore rigidity.
  bool equalsA(boost::shared_ptr<Type> t, bool verbose=false,
               std::ostream &errStream=std::cerr);
  bool strictlyEqualsA(boost::shared_ptr<Type> t, bool verbose=false,
                       std::ostream &errStream=std::cerr);
  
  /* Test for copy compatibility 
     two versions based on inner function equal or equalsA? */
  bool copy_compatible(boost::shared_ptr<Type> t, bool verbose=false,
                           std::ostream &errStream=std::cerr);
  bool copy_compatibleA(boost::shared_ptr<Type> t, bool verbose=false,
                              std::ostream &errStream=std::cerr);
 
  /* Methods to support polymorphism */
  /* Is that Type variable contained in my type somewhere (deeply)? */
  bool boundInType(boost::shared_ptr<Type> tv);
  
  /* Is my type bound in the environment? Usually only used in the
     case of type variables to determine if it must be 
     generalized, or not */
  bool boundInGamma(boost::shared_ptr<const TSEnvironment > gamma);
  
  // Collect ALL ftvs regardless of gamma
  // This APPENDS TO the vector `tvs'. 
  void collectAllftvs(/* OUT */ TypeSet &tvs);

  // Collect the Free Type Variables in a type
  // that are unbound in gamma
  void collectftvsWrtGamma(/* OUT */ TypeSet& tvs,
                           boost::shared_ptr<const TSEnvironment > gamma);

  // Meta-polymorphism
  static boost::shared_ptr<Type> Kmono;
  static boost::shared_ptr<Type> Kpoly;
  
private:
  boost::shared_ptr<Type> 
  TypeSpecializeReal(const std::vector<boost::shared_ptr<Type> >& ftvs,
                     std::vector<boost::shared_ptr<Type> >& nftvs);
  
  // Clear the sp (specialization) field of type records recursively.
  void clear_sp();
  
public:
  boost::shared_ptr<Type> 
  TypeSpecialize(const std::vector<boost::shared_ptr<Type> >& ftvs,
                 std::vector<boost::shared_ptr<Type> >& nftvs);

  /* Methods to deal with mutability issues */
  // Fix all Maybe types surrounding type records containing a
  // polymorphic type variables, except in the case of those maybes
  // directly surrounding type variables, unless clearall is
  // mentioned. 
  bool fixMaybes(const TypeSet& ftvs, boost::shared_ptr<Trail> trail,
                 bool clearAll);

  // Wrapper for the above function with the clearAll flag set.
  void clearAllMaybes();
  
public:
  void adjMaybe(boost::shared_ptr<Trail> trail, bool markedOnly=false,
                bool minimize=false, bool adjFn=false);
  
  // Get the maximally-mutable, but copy-compatible type.
  boost::shared_ptr<Type> maximizeMutability(boost::shared_ptr<Trail>
                                             trail=Trail::make()); 
  // Get the minimally-mutable, but copy-compatible type.
  boost::shared_ptr<Type> minimizeMutability(boost::shared_ptr<Trail>
                                             trail=Trail::make()); 

  // Get the minimally-mutable version of this type, but interpret
  // const-meta-constructors at this step. This function is useful to
  // construct a maybe(full) type, since in 'a|p, p need not  
  // preserve const-ness.
  boost::shared_ptr<Type> minMutConstless(boost::shared_ptr<Trail> 
                                          trail=Trail::make());
  
  // Ensure that this type can be wrapped within a const type, that
  // is, all variables at shallow-positions are in a mbFull, so that
  // we do not lose completeness when we perform minimizeMutability()
  // If the markOnly flag is set, it only marks type variables that
  // must be instantiated to mbFull types (which can be done later). 
  // This marking is used to mark type arguments in structure/union
  // definitions that appear at copy-position within const. 
  void ensureMinimizability(boost::shared_ptr<Trail> trail, 
                            bool markOnly);
  
  boost::shared_ptr<Type>
  maximizeTopMutability(boost::shared_ptr<Trail> trail=Trail::make()); 
  boost::shared_ptr<Type>
  minimizeTopMutability(boost::shared_ptr<Trail> trail=Trail::make()); 
  boost::shared_ptr<Type>
  minimizeDeepMutability(boost::shared_ptr<Trail> trail=Trail::make());

public:
  bool checkMutConsistency(bool inMut=false, bool inMbFull=false);

  // Propagate Mutability inwards for unboxed composite types.
  // This case might fail with an error if there is an inner immutable
  // type, for example (mutable (pair (int32 bool)))
  bool
  propagateMutability(boost::shared_ptr<Trail> trail, 
                      const bool inMutable=false); 
  
  
  // Check if maximally / minimally mutable
  bool isMaxMutable();
  bool isMinMutable();
  
  /* Determine Candidacy for Copy-Compatibility for type variables
     only, argument is a composite-type that is searched 
     to determine ccc-ness */  
  bool determineCCC(boost::shared_ptr<Type> comp, bool inRefType=false);
  
  // See if nth typeArg is a CCC based on the TY_CCC flag markings
  bool argCCOK(size_t argN);  

  // See if nth typeArg is within a const type based on the
  // TY_ARG_IN_CONST flag markings 
  bool argInConst(size_t argN);  
  
  /* Print a type into a string for interactive or debugging display. */
protected:
  std::string 
  asBlockStringProducer(boost::shared_ptr<TvPrinter> tvP, 
                   PrintOptions options, bool parenWrap);
public:
  std::string 
  asBlockString(boost::shared_ptr<TvPrinter> tvP = TvPrinter::make(), 
                PrintOptions options = PO_NO_FLAGS);

  /* Print a type into a string for interactive or debugging display. */
  std::string 
  asSexprString(boost::shared_ptr<TvPrinter> tvP = TvPrinter::make(), 
                PrintOptions options = PO_NO_FLAGS);

  std::string
  asString(boost::shared_ptr<TvPrinter> tvP = TvPrinter::make(), 
           PrintOptions options = PO_NO_FLAGS);

  //  std::string 
  //  asString(boost::shared_ptr<TvPrinter> tvP = TvPrinter::make(), 
  //           bool traverse = true);

  void asXML(boost::shared_ptr<TvPrinter> tvP, sherpa::INOstream &out);
  std::string asXML(boost::shared_ptr<TvPrinter> tvP = TvPrinter::make());
  
  boost::shared_ptr<AST> 
  asAST(const sherpa::LexLoc &loc,
        boost::shared_ptr<TvPrinter> tvP = TvPrinter::make());
  // Ignore mutability, Ignore Top-level Mutability, or
  // Maximize mutability of type-args
  std::string mangledString(bool igMut=false, bool igTlMut=false,
                            bool maxArgMut=false);

  const char *typeTagName() const
  { return TypeTagName(typeTag); }
  static TypeTag LookupTypeTag(const std::string& nm);  
  static TypeTag getValTypeTag(TypeTag refTag);
  static TypeTag getRefTypeTag(TypeTag valTag);

  /* Typeclass special */
  bool addFnDep(boost::shared_ptr<Type> tc);

  /* PUBLIC Accessors (Conveniecnce Forms) */
  //Accessing Type Arguments and Components
  const boost::shared_ptr<Type> & TypeArg(size_t i) const
  {
    return typeArgs[i];
  }
  const boost::shared_ptr<comp> & Component(size_t i) const
  {
    return components[i];
  }
  boost::shared_ptr<Type> & TypeArg(size_t i)
  {
    return typeArgs[i];
  }
  boost::shared_ptr<comp> & Component(size_t i)
  {
    return components[i];
  }
  boost::shared_ptr<Type> & CompType(size_t i) const
  {
    return components[i]->typ;
  }
  std::string& CompName(size_t i) const
  {
    return components[i]->name;
  }
  CompFlagSet& CompFlags(size_t i) const
  {
    return components[i]->flags;
  }
  boost::shared_ptr<Type> & MethodType(size_t i) const
  {
    return methods[i]->typ;
  }
  std::string& MethodName(size_t i) const
  {
    return methods[i]->name;
  }
  CompFlagSet& MethodFlags(size_t i) const
  {
    return methods[i]->flags;
  }
  //Argument and return types of function-types
  boost::shared_ptr<Type> & Args() const
  {
    DEBUG(TYPE_ACC) assert(typeTag == ty_fn || typeTag == ty_tyfn || typeTag == ty_method);
    return CompType(0);
  }  
  boost::shared_ptr<Type> & Ret() const
  {
    DEBUG(TYPE_ACC) assert(typeTag == ty_fn || typeTag == ty_tyfn || typeTag == ty_method);
    return CompType(1);
  }  
  //The Inner type of Maybe-types
  boost::shared_ptr<Type> &Var() const
  {
    DEBUG(TYPE_ACC) assert(typeTag == ty_mbTop || typeTag == ty_mbFull);
    return CompType(0);
  }  
  boost::shared_ptr<Type> & Core() const
  {
    DEBUG(TYPE_ACC) assert(typeTag == ty_mbTop || typeTag == ty_mbFull);
    return CompType(1);
  }
  // The first component of an array/vector/mutable/ref type
  boost::shared_ptr<Type> & Base() const
  {
    DEBUG(TYPE_ACC) assert(typeTag == ty_mutable || 
                           typeTag == ty_const || 
                           typeTag == ty_byref || 
                           typeTag == ty_ref || 
                           typeTag == ty_array || 
                           typeTag == ty_vector);
    return CompType(0);
  }
};

inline
std::ostream& operator<<(std::ostream& strm, Type& t)
{
  strm << t.asString();
  return strm;
}

#endif /* TYPE_HXX */
