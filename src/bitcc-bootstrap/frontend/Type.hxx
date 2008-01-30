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

#define CT_REMOVE        0x20u // A constraint marked for removal
                               // since it is satisfied. 
                               // Inter-procedural flag used in the
                               // implementation of unification only.
                               

// Specialization mask -- those flags which should NOT survive
// specialization. ((TY_RESTRICTED was here too.))
#define TY_SP_MASK    (TY_CT_SELF | TY_RIGID | TY_CCC | CT_REMOVE) 

struct Type : public Countable {

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
  uint64_t  arrlen;		// Length in the case of an array type
  size_t    Isize;		// size in fixint
  GCPtr<CVector<GCPtr<Type> > > fnDeps; // Functional Dependencies (for 
                                   //   Type classes only).
  
  GCPtr<CVector<GCPtr<comp> > > components;
  GCPtr<CVector<GCPtr<Type> > > typeArgs;  
    
  // Backpointer to the AST. Should be used only to attribute location
  // information for error reporting. Note carefully that if you have
  // done a getType(), you are probably looking at some other type
  // record (one that unified) that has an unrelated location. If you
  // need this, you need the one from the AST.symType pointer directly.
  //
  // Note that it is being misused in the error reporting logic!
  // ANS: I fixed this in most cases.
  NoGCPtr<AST> ast;

  // Mark Flags:  used for Traversal
  // Used to prevent infinite recursion
  // while printing infinitely recursivetypes.
  unsigned mark;                // General traversal
  unsigned pMark;               // Type printer
  GCPtr<Type> sp;			// Type specializer.
  unsigned flags;               

  // Main (Base) Constructor
  Type(const Kind k, GCPtr<AST> a, GCPtr<AST> defAST = 0);
  // Copy Constructor.
  Type(GCPtr<Type> t); 
  // Wrapper Constructor, obtains component nodes as arguments.
  Type(const Kind k, GCPtr<AST> ast, GCPtr<Type> child);
  Type(const Kind k, GCPtr<AST> ast, 
       GCPtr<Type> child1, GCPtr<Type> child2);
  // Wrapper Constructors, similar to above, but obtain AST from child1
  Type(const Kind k, GCPtr<Type> child);
  Type(const Kind k, GCPtr<Type> child1, GCPtr<Type> child2);

  // Makes a deep copy , but ** LINKS TVARS TO ORIGINAL ONES ** 
  // This function calls TypeSpecialize on a typeScheme with
  // no free variables.
  // This is different from the type_instance() in 
  // the typeScheme class in that this function makes a copy
  // of non-tvars, while type_instance() returns the original type.
  GCPtr<Type> getDCopy();

  // Makes a deep copy, including type variables.
  GCPtr<Type> getTrueCopy();

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

  bool isUnion(); // 1. Union type.
  bool isUcon();  // 2. Union Constructor.
  bool isUval();  // 3. Constructed union value (includes zero arity ctrs).
  bool isULeg();  // 2 or 3.
  bool isUType(); // 1, 2, or 3.
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
  bool isOfInfiniteType();
  size_t nBits();
  bool needsCaptureConversion();
  /* Produce Type ty_union[rv] from ty_ucon[rv] or ty_uval[rv]
     ONLY typeArgs are polylated */
  GCPtr<Type> getUnionType();
    
  size_t size();

  /* Methods used in Equational Unification */
  // Is this type Mutable upto function boundary?
  bool isDeepMut();
  // Is there no mutability upto function boundary?
  // Type variables OK
  bool isDeepImmut();
  
  // Is the type known to be Immutable upto function boundary?
  // Type variables not OK
  bool isDeepImmutable();

  /* Methods that can be used for various kinds of 
     comparisons between two types */

private:  
  bool eql(GCPtr<Type> t, bool verbose, std::ostream &errStream,
	   unsigned long uflags, bool keepSub,
	   GCPtr<Trail> trail=new Trail);
public:
  // Returns true of the type `t' is structurally equal to `this'
  // under alpha renaming -- module:
  // i)   mutabality 
  // ii)  declarations unify with definitions
  // 
  // However, it should be noted that this is rarely an issue 
  // if the function used once the inference process is OVER.
  //
  // If one is still not satisfied, he can use the next function
  // strictlyEquals which removes the above two restrictions.
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

  // The following functions are the same as equals and strictlyEquals
  // except for the fact that they ignore rigidity.
  bool compatible(GCPtr<Type> t, bool verbose=false,
	      std::ostream &errStream=std::cerr);
  bool strictlyCompatible(GCPtr<Type> t, bool verbose=false,
			  std::ostream &errStream=std::cerr);

  /* Test for copy compatibility 
     two versions based on inner function equal or compatible? */
  bool copy_compatible_eql(GCPtr<Type> t, bool verbose=false,
			   std::ostream &errStream=std::cerr);
  bool copy_compatible_compat(GCPtr<Type> t, bool verbose=false,
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
  
public:
  GCPtr<Type> 
  TypeSpecialize(GCPtr<CVector<GCPtr<Type> > > ftvs,
		 GCPtr<CVector<GCPtr<Type> > > nftvs);

  // Remove Value restricted type variables from ftvs.  
  void removeRestricted(GCPtr<CVector<GCPtr<Type> > > &ftvs,
		   bool remove,
		   GCPtr<CVector<GCPtr<Type> > > removed);

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
  
private:
  void groundMaybe(GCPtr<Trail> trail);
public:
  void adjMaybe(GCPtr<Trail> trail);

  // Get the maximally-mutable, but copy-compatible type.
  GCPtr<Type> maximizeMutability(GCPtr<Trail> trail=new Trail);
  // Get the minimally-mutable, but copy-compatible type.
  GCPtr<Type> minimizeMutability(GCPtr<Trail> trail=new Trail);
  GCPtr<Type> maximizeTopMutability(GCPtr<Trail> trail=new Trail);
  GCPtr<Type> minimizeTopMutability(GCPtr<Trail> trail=new Trail);

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
  bool TCCspecialized();
  bool TCCconcrete(GCPtr<CVector<GCPtr<Type> > > ftvs);

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
    TYPE_ACC_DEBUG assert(kind == ty_fn);
    return (*components)[0]->typ;
  }  
  GCPtr<Type> & Ret() const
  {
    TYPE_ACC_DEBUG assert(kind == ty_fn);
    return (*components)[1]->typ;
  }  
  //The Inner type of Maybe-types
  GCPtr<Type> & Inner() const
  {
    TYPE_ACC_DEBUG assert(kind == ty_mbTop || kind == ty_mbFull);
    return (*components)[0]->typ;
  }
  // The first component of an array/vector/mutable/ref type
  GCPtr<Type> & Base() const
  {
    TYPE_ACC_DEBUG assert(kind == ty_mutable || 
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
#define MARK1   0x000001u
#define MARK2   0x000002u
#define MARK3   0x000004u
#define MARK4   0x000008u
#define MARK5   0x000010u
#define MARK6   0x000020u
#define MARK7   0x000040u
#define MARK8   0x000080u
#define MARK9   0x000100u
#define MARK10  0x000200u
#define MARK11  0x000400u
#define MARK12  0x000800u
#define MARK13  0x001000u
#define MARK14  0x002000u
#define MARK15  0x004000u
#define MARK16  0x008000u
#define MARK17  0x010000u
#define MARK18  0x020000u
#define MARK19  0x040000u
#define MARK20  0x080000u
#define MARK21  0x100000u
#define MARK22  0x200000u
#define MARK23  0x400000u

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



