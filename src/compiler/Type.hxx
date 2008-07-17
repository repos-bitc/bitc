#ifndef TYPE_HXX
#define TYPE_HXX

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

#include <stdlib.h>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <string>
#include <libsherpa/UExcept.hxx>
#include "AST.hxx"
#include "debug.hxx"
#include "TvPrinter.hxx"
#include "Environment.hxx"
#include "Trail.hxx"
#include "INOstream.hxx"

// Elements of the Kind enumeration have moved to kind.def

enum Kind { // What kind of type?
#define DEFKIND(nm,prim,csimp,atom,scalar,ref,impint,impfloat) ty_##nm,
#include "kind.def"
};

const char *KindName(Kind k);

typedef long long TargetSWord;

struct Type;

#define COMP_UNIN_DISCM 0x1u // Union discriminator of defrepr
#define COMP_INVALID    0x2u // Field (component) in a defrepr leg is
                             // marked invalid within switch statement
                             // when used in conjuction with other
                             // constructors (only the common fields
                             // must be valid) The switced value is
                             // only valid within select operations,
                             // and is therefore only checked there.   
#define COMP_BYREF      0x4u // Valid on ty_argvec only.
                             // The Flag is not marked on the
                             // Component types themselves becaluse
                             // this will cause problems with flag
                             // propagation during unification
                             // (especially since the component type
                             // may be a type variable. We do not have
                             // a (by-ref 'a) type itself because that
                             // will result in types such as 
                             // (mutable (byref 'a)) during inference,
                             // and we will need normalization.
                             // Therefore, by-ref is marked on
                             // ty_argvec components. Two argVecs can
                             // unify only if all of their components
                             // and flags match. 
#define COMP_BYREF_P    0x8u // Valid on ty_argvec only.
                             // This flag indicates that this 
                             // component is open to be either ByREF
                             // or ByVALUE. We need this to unify the
                             // (extected) functions that we build
                             // with the actual functions.

struct comp : public Countable {
  std::string name;
  GCPtr<Type> typ;
  unsigned long flags;

  comp() {flags=0;} 
  comp(GCPtr<Type> t, unsigned long _flags=0);
  comp(const std::string s, GCPtr<Type> t, unsigned long _flags=0);
  //comp(const comp &c);
};  

// The only reason for this class is that we need
// Countable to use GCPtr for an indirection 
// over array length
struct ArrLen : public Countable {
  uint64_t  len;
  ArrLen(uint64_t _len) {len = _len;}
};


struct tvPrinter;
struct TypeScheme;

// Definition for Type flags
#define TY_CT_SUBSUMED  0x01u // Typeclass constraint is subsumed by 
                              // another (ex: by a derived class)
#define TY_CT_SELF      0x02u // Typeclass constraint is due to 
                              // self definition.
#define TY_RIGID        0x04u
//#define TY_RESTRICTED   0x08u // Depricated 
                              // All restricted variables are not
			      // marked. Only variables which are
			      // restricted due to restrictions on
			      // integer or floating point literals,
			      // which do not occur within another
			      // wrapper are marked, This marking is
			      // not binding. Type variables are
			      // always generalized within
			      // non-expansive expressions. 

#define TY_CCC          0x10u // Candidate for Copy-Compatibility
                              // This flag is for type variables that
                              // are in type argument position for
                              // structure or union types only. If
                              // set, it means that this type variable
                              // has not been captured within another
                              // reference type constructor, and the
                              // composite type is free to be 
                              // copy-compatible at this position.

#define TY_CLOS          0x20u // A temporary flag used in closure
                               // computation of FTVs in a TypeScheme.
#define TY_COERCE        0x40u // A flag used by adjMaybe to coerce
			       // only certain maybe-types. This flag
			       // must be marked on maybe-Var()s.
			       // This flag is NOT cleared by the 
			       // adjMaybe function. 
                               

// Specialization mask -- those flags which should NOT survive
// specialization. ((TY_RESTRICTED was here too.))
#define TY_SP_MASK    (TY_CT_SELF | TY_RIGID | TY_CCC | TY_CLOS | TY_COERCE) 

struct Type : public Countable {
  
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
  Kind kind;
  
  GCPtr<Type> link; // Linked into a chain of types 
                            // constrained by equality
  
  /* If this is the type node that is authoritative
     to answer to the link, it will be the last one, and 
     link will be NULL. If so, examine the details */
  
  // Depending on kind, store a reference 
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
  GCPtr<AST> defAst;			// Defining occurrence (or declare)

  // If type is a union constructor, points to identifier AST of the
  // defining occurrence of the defunion.  If type is a type class
  // method type, points to the identifier AST of the typeclass.
  GCPtr<AST> myContainer;

  // Note that we use two different kinds, ty_int/ty_impint,
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

  uint8_t   minSignedRep;	// minimum signed representation size
  uint8_t   minUnsignedRep;	// minimum signed representation size

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
  GCPtr<ArrLen> arrlen;	// Length in the case of an array type
  
  size_t    Isize;		// size in fixint
  GCPtr<CVector<GCPtr<Type> > > fnDeps; // Functional Dependencies (for 
                                   //   Type classes only).
  
  GCPtr<CVector<GCPtr<comp> > > components;
  GCPtr<CVector<GCPtr<Type> > > typeArgs;  
    
  // Mark Flags:  used for Traversal
  // Used to prevent infinite recursion
  // while printing infinitely recursivetypes.
  unsigned mark;                // General traversal
  unsigned pMark;               // Type printer
  GCPtr<Type> sp;			// Type specializer.
  unsigned flags;               

  // Main (Base) Constructor
  Type(const Kind k);
  // Copy Constructor.
  Type(GCPtr<Type> t); 
  Type(const Kind k, GCPtr<Type> child);
  Type(const Kind k, GCPtr<Type> child1, GCPtr<Type> child2);

  // Makes a deep copy , but ** LINKS TVARS TO ORIGINAL ONES ** 
  // This function calls TypeSpecialize on a typeScheme with
  // no free variables.
  // This is different from the type_instance() in 
  // the typeScheme class in that this function makes a copy
  // of non-tvars, while type_instance() returns the original type.
  GCPtr<Type> getDCopy();

private:
  GCPtr<const Type> getTypePrim() const;
  GCPtr<Type> getTypePrim();

public:
  GCPtr<Type> getType();  
  GCPtr <const Type> getType() const;
  
  // Get the type without mutability / fix / maybe
  GCPtr<Type> getBareType();  
  // Get the type without some combinations of the above
  GCPtr<Type> getTheType(bool mutableOK=false, bool maybeOK=false);   

  // The only reason the following unctions are not marked const is
  // that they call getType(), which uses the mark flag. 
  bool isUnion(bool ignMut=true); // 1. Union type.
  bool isUcon(bool ignMut=true);  // 2. Union Constructor.
  bool isUval(bool ignMut=true);  // 3. Constructed union value (includes zero arity ctrs).
  bool isULeg(bool ignMut=true);  // 2 or 3.
  bool isUType(bool ignMut=true); // 1, 2, or 3.
  bool isDecl();
  bool isException();
  bool isStruct();
  bool isTvar();
  bool isAtomic();
  bool isSimpleTypeForC();
  bool isScalar();
  bool isRefType();
  bool isValType();
  bool isByrefType();
  bool isFnxn();
  bool isBaseConstType(); // Integers, floats, string, bool, unit, dummy.
// #if 0
  bool isClosure();
// #endif
  bool isImmutableRefType();
  bool isMutable();
  bool isMaybe();
  bool isMbVar();
  bool isConcrete();
  bool isPrimaryType();
  bool isPrimInt();
  bool isPrimFloat();
  void SetTvarsTo(GCPtr<Type> t);
  void SetTvarsToUnit();
  bool isInteger();
  bool isIntegral();
  bool isbool();
  bool isFloat();
  bool isTypeClass();
  bool isPcst();
  bool isOfInfiniteType();
  size_t nBits();
  bool needsCaptureConversion();
  // Test if the type is a variable that can be substituted with
  // another type. Tests if the type is a variable (mbFull or mbTop)
  // wherein the variable (possible within Var() component) is 
  // not marked RIGID, unless, fllags indicate that rigidity 
  // must be ignored.  
  bool isUnifiableTvar(size_t flags=0);
  bool isUnifiableMbFull(size_t flags=0);
  bool isUnifiableMbTop(size_t flags=0);

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

  /* Produce Type ty_union[rv] from ty_ucon[rv] or ty_uval[rv]
     ONLY typeArgs are polylated */
  GCPtr<Type> getUnionType();
    
  size_t size();

  /* Methods used in Equational Unification */
  // Is this type Mutable upto function boundary?
  bool isDeepMut();
  // Is the type known to be Immutable upto function boundary?
  bool isDeepImmut();
  // Does this type contain variables only within functions or on the
  // lhs of a matybe type?
  bool isConcretizable();

  /* Methods that can be used for various kinds of 
     comparisons between two types */

private:  
  bool eql(GCPtr<Type> t, bool verbose, std::ostream &errStream,
	   unsigned long uflags, bool keepSub,
	   GCPtr<Trail> trail=new Trail);
public:
  // Returns true of the type `t' is structurally equal to `this'
  // under alpha renaming (and declarations unify with definitions)
  // 
  // The next function strictlyEquals removes the above two
  // restrictions. 
  bool equals(GCPtr<Type> t, bool verbose=false,
	      std::ostream &errStream=std::cerr);
  bool strictlyEquals(GCPtr<Type> t, bool verbose=false,
		      bool noAlphaRename=false,
		      std::ostream &errStream=std::cerr);  
  bool unifyWith(GCPtr<Type> t, bool verbose=false,
		 GCPtr<Trail> trail=new Trail,
		 std::ostream &errStream=std::cerr);

  // Unify Ignoring rigidity
  bool forcedUnify(GCPtr<Type> t, bool verbose=false,
		   std::ostream &errStream=std::cerr);
  
  // All Tvars are rigid?
  bool allTvarsRigid();

  // Equality under alpha renaming of all (including rigid)
  // variables. The following functions are the same as equals and
  // strictlyEquals except for the fact that they ignore rigidity.
  bool equalsA(GCPtr<Type> t, bool verbose=false,
	       std::ostream &errStream=std::cerr);
  bool strictlyEqualsA(GCPtr<Type> t, bool verbose=false,
		       std::ostream &errStream=std::cerr);
  
  /* Test for copy compatibility 
     two versions based on inner function equal or equalsA? */
  bool copy_compatible(GCPtr<Type> t, bool verbose=false,
			   std::ostream &errStream=std::cerr);
  bool copy_compatibleA(GCPtr<Type> t, bool verbose=false,
			      std::ostream &errStream=std::cerr);
 
  /* Methods to support polymorphism */
  /* Is that Type variable contained in my type somewhere (deeply)? */
  bool boundInType(GCPtr<Type> tv);
  
  /* Is my type bound in the environment? Usually only used in the
     case of type variables to determine if it must be 
     generalized, or not */
  bool boundInGamma(GCPtr<const Environment<TypeScheme> > gamma);
  
  // Collect ALL ftvs regardless of gamma
  // This APPENDS TO the vector `tvs'. 
  void collectAllftvs(GCPtr<CVector<GCPtr<Type> > > tvs);

  // Collect the Free Type Variables in a type
  // that are unbound in gamma
  void collectftvsWrtGamma(GCPtr<CVector<GCPtr<Type> > > tvs,
			   GCPtr<const Environment<TypeScheme> > gamma);

  // Meta-polymorphism
  static GCPtr<Type> Kmono;
  static GCPtr<Type> Kpoly;
  
private:
  GCPtr<Type> 
  TypeSpecializeReal(GCPtr<CVector<GCPtr<Type> > > ftvs,
		     GCPtr<CVector<GCPtr<Type> > > nftvs);
  
  // Clear the sp (specialization) field of type records recursively.
  void clear_sp();
  
public:
  GCPtr<Type> 
  TypeSpecialize(GCPtr<CVector<GCPtr<Type> > > ftvs,
		 GCPtr<CVector<GCPtr<Type> > > nftvs);

  /* Methods to deal with mutability issues */
  // Fix all Maybe types surrounding type records containing a
  // polymorphic type variables, except in the case of those maybes
  // directly surrounding type variables, unless clearall is
  // mentioned. 
  bool fixMaybes(GCPtr<CVector<GCPtr<Type> > > ftvs, 
		 GCPtr<Trail> trail,
		 bool clearAll);

  // Wrapper for the above function with the clearAll flag set.
  void clearAllMaybes();
  
public:
  void adjMaybe(GCPtr<Trail> trail, bool markedOnly=false,
		bool minimize=false, bool adjFn=false);
  
  // Get the maximally-mutable, but copy-compatible type.
  GCPtr<Type> maximizeMutability(GCPtr<Trail> trail=new Trail);
  // Get the minimally-mutable, but copy-compatible type.
  GCPtr<Type> minimizeMutability(GCPtr<Trail> trail=new Trail);
  GCPtr<Type> maximizeTopMutability(GCPtr<Trail> trail=new Trail);
  GCPtr<Type> minimizeTopMutability(GCPtr<Trail> trail=new Trail);
  GCPtr<Type> minimizeDeepMutability(GCPtr<Trail> trail=new Trail);

  // Check if maximally / minimally mutable
  bool isMaxMutable();
  bool isMinMutable();
  
  /* Determine Candidacy for Copy-Compatibility for type variables
     only, argument is a composite-type that is searched 
     to determine ccc-ness */  
  bool determineCCC(GCPtr<Type> comp, bool inRefType=false);
  
  // See if nth typeArg is a CCC based on the TY_CCC flag markings
  bool argCCOK(size_t argN);  

  
  /* Various kinds of printing */
  // Internal debugging purposes only, do not use for user output.
  std::string toString();  
  // Use Output
  std::string 
  asString(GCPtr<TvPrinter> tvP = new TvPrinter(), 
	   bool traverse = true);

  void asXML(GCPtr<TvPrinter> tvP, INOstream &out);
  std::string asXML(GCPtr<TvPrinter> tvP = new TvPrinter());
  
  GCPtr<AST> 
  asAST(const LexLoc &loc,
	GCPtr<TvPrinter> tvP = new TvPrinter());
  // Ignore mutability, Ignore Top-level Mutability, or
  // Maximize mutability of type-args
  std::string mangledString(bool igMut=false, bool igTlMut=false,
			    bool maxArgMut=false);

  const char *kindName() const
  { return KindName(kind); }
  static Kind LookupKind(const std::string& nm);  
  static Kind getValKind(Kind refKind);
  static Kind getRefKind(Kind valKind);

  /* Typeclass special */
  bool addFnDep(GCPtr<Type> tc);

  /* PUBLIC Accessors (Conveniecnce Forms) */
  //Accessing Type Arguments and Components
  GCPtr<Type> & TypeArg(size_t i) const
  {
    return (*typeArgs)[i];
  }
  GCPtr<comp> & Component(size_t i) const
  {
    return (*components)[i];
  }
  GCPtr<Type> & CompType(size_t i) const
  {
    return (*components)[i]->typ;
  }
  std::string& CompName(size_t i) const
  {
    return (*components)[i]->name;
  }
  unsigned long& CompFlags(size_t i) const
  {
    return (*components)[i]->flags;
  }
  //Functional Dependencies
  GCPtr<Type> & FnDep(size_t i) const
  {
    return (*fnDeps)[i];
  }
  //Argument and return types of function-types
  GCPtr<Type> & Args() const
  {
    TYPE_ACC_DEBUG assert(kind == ty_fn || kind == ty_tyfn);
    return CompType(0);
  }  
  GCPtr<Type> & Ret() const
  {
    TYPE_ACC_DEBUG assert(kind == ty_fn || kind == ty_tyfn);
    return CompType(1);
  }  
  //The Inner type of Maybe-types
  GCPtr<Type> &Var() const
  {
    TYPE_ACC_DEBUG assert(kind == ty_mbTop || kind == ty_mbFull);
    return CompType(0);
  }  
  GCPtr<Type> & Core() const
  {
    TYPE_ACC_DEBUG assert(kind == ty_mbTop || kind == ty_mbFull);
    return CompType(1);
  }
  // The first component of an array/vector/mutable/ref type
  GCPtr<Type> & Base() const
  {
    TYPE_ACC_DEBUG assert(kind == ty_mutable || 
 			  kind == ty_byref || 
 			  kind == ty_ref || 
			  kind == ty_array || 
			  kind == ty_vector);
    return (*components)[0]->typ;
  }
};

inline
std::ostream& operator<<(std::ostream& strm, Type& t)
{
  strm << t.asString();
  return strm;
}

// Markers used for type traversal
#define MARK1   0x0000001u
#define MARK2   0x0000002u
#define MARK3   0x0000004u
#define MARK4   0x0000008u
#define MARK5   0x0000010u
#define MARK6   0x0000020u
#define MARK7   0x0000040u
#define MARK8   0x0000080u
#define MARK9   0x0000100u
#define MARK10  0x0000200u
#define MARK11  0x0000400u
#define MARK12  0x0000800u
#define MARK13  0x0001000u
#define MARK14  0x0002000u
#define MARK15  0x0004000u
#define MARK16  0x0008000u
#define MARK17  0x0010000u
#define MARK18  0x0020000u
#define MARK19  0x0040000u
#define MARK20  0x0080000u
#define MARK21  0x0100000u
#define MARK22  0x0200000u
#define MARK23  0x0400000u
#define MARK24  0x0800000u
#define MARK25  0x1000000u
#define MARK26  0x2000000u

/* Flags used by Type-inference engine. 
   These flags are different from the Unifier's flags */
#define TI_NONE         0x00u
#define TI_TYP_EXP      0x01u
#define TI_TYP_APP      0x02u
#define TI_TCC_SUB      0x04u   
 
#define TI_COMP1        ((flags | TI_TYP_EXP) & (~TI_TYP_APP))
#define TI_COMP2        (flags & (~(TI_TYP_EXP | TI_TYP_APP)))
#define TI_CONSTR       (TI_COMP1)

#endif /* TYPE_HXX */


