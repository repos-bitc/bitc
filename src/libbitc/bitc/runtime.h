#ifndef BITC_RUNTIME_H
#define BITC_RUNTIME_H

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

/***************************************************************
                        The Bitc Runtime
****************************************************************/
#if defined(__cplusplus)
#error "bitcc output is not (yet) suitable for C++ compilation"
#endif

#if !defined(__STDC_VERSION__) || (__STDC_VERSION__ < 19901L)
#error "bitcc output requires C99 support"
#endif

#include <stdio.h>
#ifndef __cplusplus
/* Twits at Sun aren't smart enough to make their header files compatible
   with C++. */
#include <stdbool.h>
#endif
#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <gc/gc.h>
#include <setjmp.h>
#include <inttypes.h>
#include <assert.h>

typedef char     bitc_unit_t;
typedef uint32_t bitc_char_t;
typedef bool     bitc_bool_t;

/* word is machine dependent */
typedef uintptr_t   bitc_word_t;
typedef bitc_word_t bitc_tvar_t;

typedef int8_t   bitc_int8_t;
typedef int16_t  bitc_int16_t;
typedef int32_t  bitc_int32_t;
typedef int64_t  bitc_int64_t;
typedef uint8_t  bitc_uns8_t;
typedef uint16_t bitc_uns16_t;
typedef uint32_t bitc_uns32_t;
typedef uint64_t bitc_uns64_t;

typedef float    bitc_float_t;
typedef double   bitc_double_t;
/* FIX: quad is WRONG */
typedef long double bitc_quad_t;

#define INLINE static inline
#define BITC_UNIT '\000'
#define MAYBE_UNUSED __attribute__((unused))

/// @brief Common "base class" for all exception objects.
///
/// We use bitc_exception_t as a common "base" class (i.e. as the
/// first element) of every exception structure that is emitted by the
/// C emitter. This gives us something that we can cast to at the site
/// of the catch in order to perform the dispatch across the catch
/// blocks.
///
/// The inclusion of the @p name field gives us something to
/// print for debugging purposes. At the moment, it is also used as
/// our unique exception code. To support this, we emit a C
/// declaration of the form:
///
/// <pre>const char *CMANGLE(excpt-id) = "CMANGLE(excpt-id)"</pre>
///
/// for each exception at the point of declaration.
///
/// @bug The string strategy works fine when we are doing
/// whole-program compiles, but it does not generalize. It relies on
/// the linker to perform cross-file string constang merging, which is
/// not implemented by many (or even most) linkers. A better strategy
/// would be to emit an uninitialized common symbol of (e.g.) type
/// integer, which <em>must</em> be merged by the linker, and use the
/// address of that symbol as the exception ID.
typedef struct bitc_exception_t {
  const char *__name;
} bitc_exception_t;

extern jmp_buf firstJB;
extern jmp_buf *curCatchBlock;
extern bitc_exception_t *curException;

#define bitc_throw(x) do {		       \
    curException = (bitc_exception_t *) x;     \
    longjmp(*curCatchBlock, 1);		       \
  }while(0)

extern bitc_exception_t val_ExOutOfMemory;
extern bitc_exception_t val_ExIndexBoundsError;

#define TY_VECTOR_OF_STRINGS K_6string

#define GC_ALLOC(x) bitc_malloc(x)
#define GC_ALLOC_ATOMIC(x) bitc_malloc_atomic(x)

/// @brief Trivial wrapper for allocating storage that may contain
/// pointers.
///
/// This exists to let us convert malloc failures into BitC
/// exceptions.
INLINE void *
bitc_malloc(size_t sz)
{
  void *ptr = GC_malloc(sz);
  if(ptr == NULL)
    bitc_throw(&val_ExOutOfMemory);
    
  return ptr;
}
 
/// @brief Trivial wrapper for allocating storage that may @em not
/// contain pointers.
///
/// This exists to let us convert malloc failures into BitC
/// exceptions.
INLINE void *
bitc_malloc_atomic(size_t sz)
{
  void *ptr = GC_malloc_atomic(sz);
  if(ptr == NULL)
    bitc_throw(&val_ExOutOfMemory);
  
  return ptr;
}

/// @brief C representation of the BitC string type.
///
/// This is basically the same as the vector structure, but we need a
/// common declaration for it that spans all generated source files.
typedef struct {
  bitc_word_t length;
  /** @brief Immutable, unicode UTF-8! */
  const char *s;
} bitc_string_t;

/* Forward declaration, solely so that we can attach the MAYBE_UNUSED
   attribute. GCC won't let us attach that on the definition. */
static bitc_string_t *
mkStringLiteral(const char *s) MAYBE_UNUSED;

/// @brief Constructor for string literals when they are emitted by the
/// compiler.
///
/// This is currently used for any string literal that appears in the
/// input source code, and also for the elements of the argv[] (and
/// eventually env[]) structure. Note that the actual string pointer
/// in those cases will point to static global storage. If your
/// copying collector is not savvy, it may or may not copy those
/// string constants.
static bitc_string_t *
mkStringLiteral(const char *s)
{
  size_t len = strlen(s);
  bitc_string_t *tmp = (bitc_string_t *) 
    GC_ALLOC_ATOMIC(sizeof(bitc_string_t));
  tmp->length = len;
  tmp->s = s;
  return tmp;
}

/* Support for constructing closures of external functions */
#define LAMNM(NM) NM

/* relies on C99 VARIADIC MACROS */
#define DEFUN(NM, ...) LAMNM(NM)(__VA_ARGS__)
#define DEFCLOSURE(NM)
#define DEFCLOSURE_INLINE(NM)
/* Primitive index comparison */

/// @brief Less-than operator for type Word.
INLINE bitc_bool_t
DEFUN(bitc_index_lt, bitc_word_t a, bitc_word_t b)

{
  return (a < b);
}
DEFCLOSURE_INLINE(bitc_index_lt);

/* Primitive Operations */

#define DEFEQL(TY,MTY)							\
  INLINE bitc_bool_t							\
  DEFUN(_17bitc_DTprelude_DT___EQ_EQ_SHFN2##MTY##MTY##_4bool, TY arg1, TY arg2) \
  {									\
    return (arg1 == arg2);						\
  }									\
  DEFCLOSURE_INLINE(_17bitc_DTprelude_DT___EQ_EQ_SHFN2##MTY##MTY##_4bool)

#define DEFNEQ(TY,MTY)							\
  INLINE bitc_bool_t							\
  DEFUN(_17bitc_DTprelude_DT___EX_EQ_SHFN2##MTY##MTY##_4bool, TY arg1, TY arg2) \
  {									\
    return (arg1 != arg2);						\
  }									\
  DEFCLOSURE_INLINE(_17bitc_DTprelude_DT___EX_EQ_SHFN2##MTY##MTY##_4bool)

#define DEFLESS(TY,MTY)							\
  INLINE bitc_bool_t							\
  DEFUN(_16bitc_DTprelude_DT___LT_SHFN2##MTY##MTY##_4bool, TY arg1, TY arg2) \
  {									\
    return (arg1 < arg2);						\
  }									\
  DEFCLOSURE_INLINE(_16bitc_DTprelude_DT___LT_SHFN2##MTY##MTY##_4bool)

#define DEFGREATER(TY,MTY)						\
  INLINE bitc_bool_t							\
  DEFUN(_16bitc_DTprelude_DT___GT_SHFN2##MTY##MTY##_4bool, TY arg1, TY arg2) \
  {									\
    return (arg1 > arg2);						\
  }									\
  DEFCLOSURE_INLINE(_16bitc_DTprelude_DT___GT_SHFN2##MTY##MTY##_4bool)

#define DEFLESSOREQUAL(TY,MTY)						\
  INLINE bitc_bool_t							\
  DEFUN(_17bitc_DTprelude_DT___LT_EQ_SHFN2##MTY##MTY##_4bool, TY arg1, TY arg2) \
  {									\
    return (arg1 <= arg2);						\
  }									\
  DEFCLOSURE_INLINE(_17bitc_DTprelude_DT___LT_EQ_SHFN2##MTY##MTY##_4bool)

#define DEFGREATEROREQUAL(TY,MTY)					\
  INLINE bitc_bool_t							\
  DEFUN(_17bitc_DTprelude_DT___GT_EQ_SHFN2##MTY##MTY##_4bool, TY arg1, TY arg2) \
  {									\
    return (arg1 >= arg2);						\
  }									\
  DEFCLOSURE_INLINE(_17bitc_DTprelude_DT___GT_EQ_SHFN2##MTY##MTY##_4bool)

#define DEFADD(TY,MTY)						\
  INLINE TY							\
  DEFUN(_16bitc_DTprelude_DT___PL_SHFN2##MTY##MTY##MTY, TY arg1, TY arg2) \
  {								\
    return (arg1 + arg2);					\
  }								\
  DEFCLOSURE_INLINE(_16bitc_DTprelude_DT___PL_SHFN2##MTY##MTY##MTY)

#define DEFSUBTRACT(TY,MTY)					\
  INLINE TY							\
  DEFUN(_16bitc_DTprelude_DT___HY_SHFN2##MTY##MTY##MTY, TY arg1, TY arg2) \
  {								\
    return (arg1 - arg2);					\
  }								\
  DEFCLOSURE_INLINE(_16bitc_DTprelude_DT___HY_SHFN2##MTY##MTY##MTY)

#define DEFMULTIPLY(TY,MTY)					\
  INLINE TY							\
  DEFUN(_16bitc_DTprelude_DT___ST_SHFN2##MTY##MTY##MTY, TY arg1, TY arg2) \
  {								\
    return (arg1 * arg2);					\
  }								\
  DEFCLOSURE_INLINE(_16bitc_DTprelude_DT___ST_SHFN2##MTY##MTY##MTY)

#define DEFDIVIDE(TY,MTY)					\
  INLINE TY							\
  DEFUN(_16bitc_DTprelude_DT___FS_SHFN2##MTY##MTY##MTY, TY arg1, TY arg2) \
  {								\
    return (arg1 / arg2);					\
  }								\
  DEFCLOSURE_INLINE(_16bitc_DTprelude_DT___FS_SHFN2##MTY##MTY##MTY)

#define DEFMODULO(TY,MTY)					\
  INLINE TY							\
  DEFUN(_16bitc_DTprelude_DT___PC_SHFN2##MTY##MTY##MTY, TY arg1, TY arg2) \
  {								\
    return (arg1 % arg2);					\
  }								\
  DEFCLOSURE_INLINE(_16bitc_DTprelude_DT___PC_SHFN2##MTY##MTY##MTY)

#define DEFBAND(TY,MTY)						\
  INLINE TY							\
  DEFUN(_20bitc_DTprelude_DT__b_and_SHFN2##MTY##MTY##MTY, TY arg1, TY arg2) \
  {								\
    return (arg1 & arg2);					\
  }								\
  DEFCLOSURE_INLINE(_20bitc_DTprelude_DT__b_and_SHFN2##MTY##MTY##MTY)

#define DEFBOR(TY,MTY)						\
  INLINE TY							\
  DEFUN(_19bitc_DTprelude_DT__b_or_SHFN2##MTY##MTY##MTY, TY arg1, TY arg2) \
  {								\
    return (arg1 | arg2);					\
  }								\
  DEFCLOSURE_INLINE(_19bitc_DTprelude_DT__b_or_SHFN2##MTY##MTY##MTY)

#define DEFBNOT(TY,MTY)						\
  INLINE TY							\
  DEFUN(_20bitc_DTprelude_DT__b_not_SHFN1##MTY##MTY, TY arg1)	\
  {								\
    return (~arg1);						\
  }								\
  DEFCLOSURE_INLINE(_20bitc_DTprelude_DT__b_not_SHFN1##MTY##MTY)

#define DEFBXOR(TY,MTY)						\
  INLINE TY							\
  DEFUN(_20bitc_DTprelude_DT__b_xor_SHFN2##MTY##MTY##MTY, TY arg1, TY arg2) \
  {								\
    return (arg1 ^ arg2);					\
  }								\
  DEFCLOSURE_INLINE(_20bitc_DTprelude_DT__b_xor_SHFN2##MTY##MTY##MTY)

#define DEFBLS(TY,MTY)						\
  INLINE TY							\
  DEFUN(_17bitc_DTprelude_DT___LT_LT_SHFN2##MTY##_4word##MTY, TY arg1, bitc_word_t arg2) \
  {								\
    return (arg1 << arg2);					\
  }								\
  DEFCLOSURE_INLINE(_17bitc_DTprelude_DT___LT_LT_SHFN2##MTY##_4word##MTY)

#define DEFBRS(TY,MTY)						\
  INLINE TY							\
  DEFUN(_17bitc_DTprelude_DT___GT_GT_SHFN2##MTY##_4word##MTY, TY arg1, bitc_word_t arg2) \
  {								\
    return (arg1 >> arg2);					\
  }								\
  DEFCLOSURE_INLINE(_17bitc_DTprelude_DT___GT_GT_SHFN2##MTY##_4word##MTY)


#define DEFORD(TY,MTY) \
  DEFEQL(TY,MTY);      \
  DEFNEQ(TY,MTY);      \
  DEFLESS(TY, MTY);    \
  DEFGREATER(TY, MTY);	  \
  DEFLESSOREQUAL(TY, MTY);			\
  DEFGREATEROREQUAL(TY, MTY)

#define DEFARITH(TY,MTY) \
  DEFADD(TY,MTY);				\
  DEFSUBTRACT(TY, MTY);			        \
  DEFMULTIPLY(TY, MTY);			        \
  DEFDIVIDE(TY, MTY);			        \
  DEFMODULO(TY, MTY);			        \
  DEFORD(TY, MTY)

#define DEFFLOATARITH(TY,MTY) \
  DEFADD(TY,MTY);				\
  DEFSUBTRACT(TY, MTY);			        \
  DEFMULTIPLY(TY, MTY);			        \
  DEFDIVIDE(TY, MTY);			        \
  DEFORD(TY, MTY)

#define DEFBITARITH(TY, MTY) \
  DEFBAND(TY, MTY);	     \
  DEFBOR(TY, MTY);	     \
  DEFBNOT(TY, MTY);	     \
  DEFBXOR(TY, MTY);	     \
  DEFBLS(TY, MTY);	     \
  DEFBRS(TY, MTY);	     

DEFARITH(bitc_int64_t,_5int64);
DEFARITH(bitc_int32_t,_5int32);
DEFARITH(bitc_int16_t,_5int16);
DEFARITH(bitc_int8_t,_4int8);
DEFARITH(bitc_uns64_t,_6uint64);
DEFARITH(bitc_uns32_t,_6uint32);
DEFARITH(bitc_uns16_t,_6uint16);
DEFARITH(bitc_uns8_t,_5uint8);
DEFARITH(bitc_word_t,_4word);
DEFFLOATARITH(bitc_float_t, _5float);
DEFFLOATARITH(bitc_double_t, _6double);

DEFBITARITH(bitc_uns8_t,_5uint8);
DEFBITARITH(bitc_uns16_t,_6uint16);
DEFBITARITH(bitc_uns32_t,_6uint32);
DEFBITARITH(bitc_uns64_t,_6uint64);
DEFBITARITH(bitc_word_t,_4word);

DEFORD(bitc_char_t,_4char);
DEFORD(bitc_bool_t,_4bool);

/* String type is ORD, but requires special comparator: */
INLINE bitc_bool_t
DEFUN(_17bitc_DTprelude_DT___EQ_EQ_SHFN2_6string_6string_4bool, bitc_string_t *arg1, bitc_string_t *arg2)
{
  return ((arg1->length == arg2->length) &&
	  (__builtin_strcmp((const char *)arg1->s,(const char *)arg2->s) == 0));
}						\
DEFCLOSURE_INLINE(_17bitc_DTprelude_DT___EQ_EQ_SHFN2_6string_6string_4bool);

INLINE bitc_bool_t
DEFUN(_16bitc_DTprelude_DT___LT_SHFN2_6string_6string_4bool, bitc_string_t *arg1, bitc_string_t *arg2)
{
  return (__builtin_strcmp((const char *)arg1->s,(const char *)arg2->s) < 0);
}						\
DEFCLOSURE_INLINE(_16bitc_DTprelude_DT___LT_SHFN2_6string_6string_4bool);
    

INLINE bitc_bool_t
DEFUN(_16bitc_DTprelude_DT___GT_SHFN2_6string_6string_4bool, bitc_string_t *arg1, bitc_string_t *arg2)
{
  return (__builtin_strcmp((const char *)arg1->s,(const char *)arg2->s) > 0);
}						\
DEFCLOSURE_INLINE(_16bitc_DTprelude_DT___GT_SHFN2_6string_6string_4bool);

INLINE bitc_bool_t
DEFUN(_17bitc_DTprelude_DT___LT_EQ_SHFN2_6string_6string_4bool, bitc_string_t *arg1, bitc_string_t *arg2)
{
  return (__builtin_strcmp((const char *)arg1->s,(const char *)arg2->s) <= 0);
}						\
DEFCLOSURE_INLINE(_17bitc_DTprelude_DT___LT_EQ_SHFN2_6string_6string_4bool);

INLINE bitc_bool_t
DEFUN(_17bitc_DTprelude_DT___GT_EQ_SHFN2_6string_6string_4bool, bitc_string_t *arg1, bitc_string_t *arg2)
{
  return (__builtin_strcmp((const char *)arg1->s,(const char *)arg2->s) >= 0);
}						\
DEFCLOSURE_INLINE(_17bitc_DTprelude_DT___GT_EQ_SHFN2_6string_6string_4bool);

/* CAST Operations */

#define DEFSIGNEX(TY1,MTY1,TY2,MTY2)					\
  INLINE TY2								\
  DEFUN(_24bitc_DTprelude_DTsign_HYextend_SHFN1##MTY1##MTY2, TY1 arg1) \
  {									\
    return (TY2) arg1;							\
  }									\
  DEFCLOSURE_INLINE(_24bitc_DTprelude_DTsign_HYextend_SHFN1##MTY1##MTY2)


#define DEFZEROEX(TY1,MTY1,TY2,MTY2)					\
  INLINE TY2								\
  DEFUN(_24bitc_DTprelude_DTzero_HYextend_SHFN1##MTY1##MTY2, TY1 arg1) \
  {									\
    return (TY2) arg1;							\
  }									\
  DEFCLOSURE_INLINE(_24bitc_DTprelude_DTzero_HYextend_SHFN1##MTY1##MTY2)

#define DEFTRUNCATE(TY1,MTY1,TY2,MTY2)					\
  INLINE TY2								\
  DEFUN(_21bitc_DTprelude_DTtruncate_SHFN1##MTY1##MTY2, TY1 arg1)	\
  {									\
    return (TY2) arg1;							\
  }									\
  DEFCLOSURE_INLINE(_21bitc_DTprelude_DTtruncate_SHFN1##MTY1##MTY2)

#define DEFSIGNED(TY1,MTY1,TY2,MTY2)					\
  INLINE TY2								\
  DEFUN(_19bitc_DTprelude_DTsigned_SHFN1##MTY1##MTY2, TY1 arg1)	\
  {									\
    return (TY2) arg1;							\
  }									\
  DEFCLOSURE_INLINE(_19bitc_DTprelude_DTsigned_SHFN1##MTY1##MTY2)

#define DEFUNSIGNED(TY1,MTY1,TY2,MTY2)					\
  INLINE TY2								\
  DEFUN(_21bitc_DTprelude_DTunsigned_SHFN1##MTY1##MTY2, TY1 arg1)	\
  {									\
    return (TY2) arg1;							\
  }									\
  DEFCLOSURE_INLINE(_21bitc_DTprelude_DTunsigned_SHFN1##MTY1##MTY2)

#define DEFCAST(TY1,MTY1,TY2,MTY2)					\
  INLINE TY2								\
  DEFUN(_20bitc_DTprelude_DTconvert_SHFN1##MTY1##MTY2, TY1 arg1)	\
  {									\
    return (TY2) arg1;							\
  }									\
  DEFCLOSURE_INLINE(_20bitc_DTprelude_DTconvert_SHFN1##MTY1##MTY2)

#define DEFSIGNEDOPS(TY1,MTY1,TY2,MTY2) \
  DEFSIGNEX(TY1,MTY1,TY2,MTY2);		\
  DEFTRUNCATE(TY2,MTY2,TY1,MTY1)

#define DEFUNSIGNEDOPS(TY1,MTY1,TY2,MTY2) \
  DEFZEROEX(TY1,MTY1,TY2,MTY2);		  \
  DEFTRUNCATE(TY2,MTY2,TY1,MTY1)

#define DEFRESIGN(TY1,MTY1,TY2,MTY2) \
  DEFSIGNED(TY1,MTY1,TY2,MTY2);	     \
  DEFUNSIGNED(TY2,MTY2,TY1,MTY1)



/**************************************************************
 ** The following were generated using the genops.sh script. **
 **                                                          **
 **   DO NOT EDIT THEM BY HAND!! FIX THE SCRIPT!!            **
 **************************************************************/
DEFUNSIGNEDOPS(bitc_uns8_t,  BF1_5uint8,   bitc_uns8_t,  _5uint8);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF2_5uint8,   bitc_uns8_t,  _5uint8);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF3_5uint8,   bitc_uns8_t,  _5uint8);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF4_5uint8,   bitc_uns8_t,  _5uint8);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF5_5uint8,   bitc_uns8_t,  _5uint8);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF6_5uint8,   bitc_uns8_t,  _5uint8);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF7_5uint8,   bitc_uns8_t,  _5uint8);
DEFUNSIGNEDOPS(bitc_uns8_t,     _5uint8,   bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF1_5uint8,   bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF2_5uint8,   bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF3_5uint8,   bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF4_5uint8,   bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF5_5uint8,   bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF6_5uint8,   bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF7_5uint8,   bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns8_t,     _5uint8,   bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF1_5uint8,   bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF2_5uint8,   bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF3_5uint8,   bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF4_5uint8,   bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF5_5uint8,   bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF6_5uint8,   bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF7_5uint8,   bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns8_t,     _5uint8,   bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF1_5uint8,   bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF2_5uint8,   bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF3_5uint8,   bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF4_5uint8,   bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF5_5uint8,   bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF6_5uint8,   bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns8_t,  BF7_5uint8,   bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns16_t, BF1_6uint16,  bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns16_t, BF2_6uint16,  bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns16_t, BF3_6uint16,  bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns16_t, BF4_6uint16,  bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns16_t, BF5_6uint16,  bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns16_t, BF6_6uint16,  bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns16_t, BF7_6uint16,  bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns16_t, BF8_6uint16,  bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns16_t, BF9_6uint16,  bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns16_t, BF10_6uint16, bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns16_t, BF11_6uint16, bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns16_t, BF12_6uint16, bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns16_t, BF13_6uint16, bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns16_t, BF14_6uint16, bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns16_t, BF15_6uint16, bitc_uns16_t, _6uint16);
DEFUNSIGNEDOPS(bitc_uns16_t,    _6uint16,  bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns16_t, BF1_6uint16,  bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns16_t, BF2_6uint16,  bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns16_t, BF3_6uint16,  bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns16_t, BF4_6uint16,  bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns16_t, BF5_6uint16,  bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns16_t, BF6_6uint16,  bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns16_t, BF7_6uint16,  bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns16_t, BF8_6uint16,  bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns16_t, BF9_6uint16,  bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns16_t, BF10_6uint16, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns16_t, BF11_6uint16, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns16_t, BF12_6uint16, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns16_t, BF13_6uint16, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns16_t, BF14_6uint16, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns16_t, BF15_6uint16, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns16_t,    _6uint16,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns16_t, BF1_6uint16,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns16_t, BF2_6uint16,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns16_t, BF3_6uint16,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns16_t, BF4_6uint16,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns16_t, BF5_6uint16,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns16_t, BF6_6uint16,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns16_t, BF7_6uint16,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns16_t, BF8_6uint16,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns16_t, BF9_6uint16,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns16_t, BF10_6uint16, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns16_t, BF11_6uint16, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns16_t, BF12_6uint16, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns16_t, BF13_6uint16, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns16_t, BF14_6uint16, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns16_t, BF15_6uint16, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF1_6uint32,  bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF2_6uint32,  bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF3_6uint32,  bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF4_6uint32,  bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF5_6uint32,  bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF6_6uint32,  bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF7_6uint32,  bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF8_6uint32,  bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF9_6uint32,  bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF10_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF11_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF12_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF13_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF14_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF15_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF16_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF17_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF18_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF19_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF20_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF21_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF22_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF23_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF24_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF25_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF26_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF27_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF28_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF29_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF30_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t, BF31_6uint32, bitc_uns32_t, _6uint32);
DEFUNSIGNEDOPS(bitc_uns32_t,    _6uint32,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF1_6uint32,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF2_6uint32,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF3_6uint32,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF4_6uint32,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF5_6uint32,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF6_6uint32,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF7_6uint32,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF8_6uint32,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF9_6uint32,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF10_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF11_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF12_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF13_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF14_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF15_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF16_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF17_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF18_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF19_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF20_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF21_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF22_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF23_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF24_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF25_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF26_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF27_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF28_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF29_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF30_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns32_t, BF31_6uint32, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF1_6uint64,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF2_6uint64,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF3_6uint64,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF4_6uint64,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF5_6uint64,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF6_6uint64,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF7_6uint64,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF8_6uint64,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF9_6uint64,  bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF10_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF11_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF12_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF13_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF14_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF15_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF16_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF17_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF18_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF19_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF20_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF21_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF22_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF23_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF24_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF25_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF26_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF27_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF28_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF29_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF30_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF31_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF32_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF33_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF34_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF35_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF36_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF37_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF38_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF39_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF40_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF41_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF42_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF43_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF44_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF45_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF46_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF47_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF48_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF49_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF50_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF51_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF52_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF53_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF54_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF55_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF56_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF57_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF58_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF59_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF60_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF61_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF62_6uint64, bitc_uns64_t, _6uint64);
DEFUNSIGNEDOPS(bitc_uns64_t, BF63_6uint64, bitc_uns64_t, _6uint64);
DEFSIGNEDOPS(bitc_int8_t,  BF1_4int8,   bitc_int8_t,  _4int8);
DEFSIGNEDOPS(bitc_int8_t,  BF2_4int8,   bitc_int8_t,  _4int8);
DEFSIGNEDOPS(bitc_int8_t,  BF3_4int8,   bitc_int8_t,  _4int8);
DEFSIGNEDOPS(bitc_int8_t,  BF4_4int8,   bitc_int8_t,  _4int8);
DEFSIGNEDOPS(bitc_int8_t,  BF5_4int8,   bitc_int8_t,  _4int8);
DEFSIGNEDOPS(bitc_int8_t,  BF6_4int8,   bitc_int8_t,  _4int8);
DEFSIGNEDOPS(bitc_int8_t,  BF7_4int8,   bitc_int8_t,  _4int8);
DEFSIGNEDOPS(bitc_int8_t,     _4int8,   bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int8_t,  BF1_4int8,   bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int8_t,  BF2_4int8,   bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int8_t,  BF3_4int8,   bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int8_t,  BF4_4int8,   bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int8_t,  BF5_4int8,   bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int8_t,  BF6_4int8,   bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int8_t,  BF7_4int8,   bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int8_t,     _4int8,   bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int8_t,  BF1_4int8,   bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int8_t,  BF2_4int8,   bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int8_t,  BF3_4int8,   bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int8_t,  BF4_4int8,   bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int8_t,  BF5_4int8,   bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int8_t,  BF6_4int8,   bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int8_t,  BF7_4int8,   bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int8_t,     _4int8,   bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int8_t,  BF1_4int8,   bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int8_t,  BF2_4int8,   bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int8_t,  BF3_4int8,   bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int8_t,  BF4_4int8,   bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int8_t,  BF5_4int8,   bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int8_t,  BF6_4int8,   bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int8_t,  BF7_4int8,   bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int16_t, BF1_5int16,  bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int16_t, BF2_5int16,  bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int16_t, BF3_5int16,  bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int16_t, BF4_5int16,  bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int16_t, BF5_5int16,  bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int16_t, BF6_5int16,  bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int16_t, BF7_5int16,  bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int16_t, BF8_5int16,  bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int16_t, BF9_5int16,  bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int16_t, BF10_5int16, bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int16_t, BF11_5int16, bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int16_t, BF12_5int16, bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int16_t, BF13_5int16, bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int16_t, BF14_5int16, bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int16_t, BF15_5int16, bitc_int16_t, _5int16);
DEFSIGNEDOPS(bitc_int16_t,    _5int16,  bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int16_t, BF1_5int16,  bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int16_t, BF2_5int16,  bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int16_t, BF3_5int16,  bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int16_t, BF4_5int16,  bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int16_t, BF5_5int16,  bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int16_t, BF6_5int16,  bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int16_t, BF7_5int16,  bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int16_t, BF8_5int16,  bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int16_t, BF9_5int16,  bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int16_t, BF10_5int16, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int16_t, BF11_5int16, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int16_t, BF12_5int16, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int16_t, BF13_5int16, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int16_t, BF14_5int16, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int16_t, BF15_5int16, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int16_t,    _5int16,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int16_t, BF1_5int16,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int16_t, BF2_5int16,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int16_t, BF3_5int16,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int16_t, BF4_5int16,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int16_t, BF5_5int16,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int16_t, BF6_5int16,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int16_t, BF7_5int16,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int16_t, BF8_5int16,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int16_t, BF9_5int16,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int16_t, BF10_5int16, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int16_t, BF11_5int16, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int16_t, BF12_5int16, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int16_t, BF13_5int16, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int16_t, BF14_5int16, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int16_t, BF15_5int16, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF1_5int32,  bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF2_5int32,  bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF3_5int32,  bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF4_5int32,  bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF5_5int32,  bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF6_5int32,  bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF7_5int32,  bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF8_5int32,  bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF9_5int32,  bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF10_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF11_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF12_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF13_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF14_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF15_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF16_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF17_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF18_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF19_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF20_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF21_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF22_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF23_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF24_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF25_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF26_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF27_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF28_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF29_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF30_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t, BF31_5int32, bitc_int32_t, _5int32);
DEFSIGNEDOPS(bitc_int32_t,    _5int32,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF1_5int32,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF2_5int32,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF3_5int32,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF4_5int32,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF5_5int32,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF6_5int32,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF7_5int32,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF8_5int32,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF9_5int32,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF10_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF11_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF12_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF13_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF14_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF15_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF16_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF17_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF18_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF19_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF20_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF21_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF22_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF23_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF24_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF25_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF26_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF27_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF28_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF29_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF30_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int32_t, BF31_5int32, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF1_5int64,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF2_5int64,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF3_5int64,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF4_5int64,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF5_5int64,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF6_5int64,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF7_5int64,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF8_5int64,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF9_5int64,  bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF10_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF11_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF12_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF13_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF14_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF15_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF16_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF17_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF18_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF19_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF20_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF21_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF22_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF23_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF24_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF25_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF26_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF27_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF28_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF29_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF30_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF31_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF32_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF33_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF34_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF35_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF36_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF37_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF38_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF39_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF40_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF41_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF42_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF43_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF44_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF45_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF46_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF47_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF48_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF49_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF50_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF51_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF52_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF53_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF54_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF55_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF56_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF57_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF58_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF59_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF60_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF61_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF62_5int64, bitc_int64_t, _5int64);
DEFSIGNEDOPS(bitc_int64_t, BF63_5int64, bitc_int64_t, _5int64);
DEFRESIGN(bitc_uns8_t,  _5uint8,  bitc_int8_t,  _4int8);
DEFRESIGN(bitc_uns16_t, _6uint16, bitc_int16_t, _5int16);
DEFRESIGN(bitc_uns32_t, _6uint32, bitc_int32_t, _5int32);
DEFRESIGN(bitc_uns64_t, _6uint64, bitc_int64_t, _5int64);
DEFCAST(bitc_uns8_t,  _5uint8,  bitc_word_t,   _4word);
DEFCAST(bitc_int8_t,  _4int8,  bitc_word_t,   _4word);
DEFCAST(bitc_word_t,   _4word,   bitc_uns8_t,   _5uint8);
DEFCAST(bitc_word_t,   _4word,   bitc_int8_t,  _4int8);
DEFCAST(bitc_uns16_t, _6uint16, bitc_word_t,  _4word);
DEFCAST(bitc_int16_t, _5int16, bitc_word_t,  _4word);
DEFCAST(bitc_word_t,  _4word,  bitc_uns16_t,  _6uint16);
DEFCAST(bitc_word_t,  _4word,  bitc_int16_t, _5int16);
DEFCAST(bitc_uns32_t, _6uint32, bitc_word_t,  _4word);
DEFCAST(bitc_int32_t, _5int32, bitc_word_t,  _4word);
DEFCAST(bitc_word_t,  _4word,  bitc_uns32_t,  _6uint32);
DEFCAST(bitc_word_t,  _4word,  bitc_int32_t, _5int32);
DEFCAST(bitc_uns64_t, _6uint64, bitc_word_t,  _4word);
DEFCAST(bitc_int64_t, _5int64, bitc_word_t,  _4word);
DEFCAST(bitc_word_t,  _4word,  bitc_uns64_t,  _6uint64);
DEFCAST(bitc_word_t,  _4word,  bitc_int64_t, _5int64);
DEFCAST(bitc_word_t,  _4word,  bitc_word_t,  _4word);
DEFCAST(bitc_float_t, _5float, bitc_word_t,  _4word);
DEFCAST(bitc_double_t, _6double, bitc_word_t,  _4word);
DEFCAST(bitc_word_t,  _4word,  bitc_float_t,  _5float);
DEFCAST(bitc_word_t,  _4word,  bitc_double_t, _6double);

/**************************************************************
 **             End of Script generated code                 **
 **************************************************************/

/**************************************************************
 **                  Procedure Objects                       **
 **************************************************************/

/*****************************************************************
 * Every procedure object is an overlay structure consisting
 * of a raw code block that contains the location of the closure
 * record as a literal constant and a data overlay that allows
 * the garbage collector to relocate the closure record at need.
 *
 * On machines having a PC-relative load (or which can contrive
 * to simulate one efficiently), that addressing mode is probably
 * the best one to use.
 *****************************************************************/

/// @brief Architecture-specific run time function that allocates and
/// initialized procedure objects.
///
/// @defgroup ProcObjectImplementations Implementations of bitc_emit_procedure_object
///
/// Logically, all BitC procedures that a (possibly empty) closure.
/// As an optimization, the BitC compiler will elide any members of
/// the closure record that are statically global, and will then elide
/// the closure record itself if it has no surviving members. This
/// leaves us with a design in which no top-level procedure will ever
/// be emitted with an implementation that expects a closure
/// record. All top-level procedures therefore obey the native calling
/// convention directly. The challenge is to arrive at an
/// implementation of inner procedures that <em>also</em> obeys the
/// native calling convention.
///
/// Inner procedures are hoisted to top-level by the
/// implementation. The hoisted procedure may or may not expect to
/// receive a closure record pointer depending on which identifiers
/// were closed over. If it does not, then its behavior is exactly
/// like that of a top-level procedure, and no further action is
/// required.
///
/// When an inner procedure requires a closure record, it is rewritten
/// during hoisting in such a way as to render the closure record type
/// and the associated procedure argument explicit. For such
/// procedures, the compiler allocates the required closure record on
/// the heap at the procedure construction site and emits a call to
/// bitc_emit_procedure_object() to generate a heap-allocated
/// trampoline function. It then returns the address of the generated
/// trampoline function as the address of the inner procedure. 
/// 
/// Two optimizations on this are possible:
///
/// - Recursive calls proceed directly through the emitted procedure
///   label. This optimization is obligatory, and is implemented
///   (albeit not correctly) by the current compiler.
/// - If the inner procedure does not escape, the closure record can
///   be stack allocated in the caller and the hoisted implementation
///   can be called directly. This is @em not implemented in the
///   current compiler. 
///
/// The current BitC code generator assumes that the closure pointer
/// will be injected as the first parameter of the hoisted inner
/// procedure. On architectures that place parameters in registers,
/// this requires a non-trivial architecture dependent rewriting of
/// the call frame. Rather than try to implement a generic foreign
/// function interface library, which has stymied many other
/// implementations, we let the C compiler do the heavy lifting
/// here. For each hoisted procedure, we emit a helper procedure that
/// looks like:
///
/// <pre>
/// ReturnType
/// hoistedProcStub(Ty1 arg1, ... TyN argN) {
///   void *__theClosureEnv;
///   BITC_GET_CLOSURE_ENV(theClosureEnv);
///   return hoistedProc(theClosureEnv, arg1, ... argn);
/// }
/// </pre>
///
/// With this helper in place, what we need from the trampoline code
/// is an assembly stub that:
/// -# Stuffs the environment pointer into an architecture-dependent
///    location from which BITC_GET_CLOSURE_ENV() can fetch it, and
/// -# Performs a JUMP to hoistedProcStub, which does the rest of
///    the work. Note that hoistedProcStub has exactly the same
///    signature as hoistedProc, so the frame that was constructed by
///    the caller can be re-used.
///
/// Finally, since closure records may be garbage collected, we add
/// one further constraint, which is that the closure record pointer
/// that is embedded in the heap-allocated trampoline should appear at
/// a naturally aligned address for pointers according to the
/// requirements of the target architecture.
///
/// <h1>Porting Advice</h1>
///
/// While other implementations are possible, the preferred method to
/// use in the stub trampoline is to use a register as a transfer
/// buffer for the closure record pointer. This method is compatible
/// with concurrent runtimes, and is generally faster than
/// reading/writing a global location. Look for a register that is
/// call-clobbered but not used for parameters. If return values are
/// returned via registers on your target, the return value register
/// is usually a good choice. Registers that are tied up in the stack
/// frame fabrication, such as the stack pointer, frame pointer, or
/// return register (a.k.a. link register) are probably not good
/// choices.
///
/// Having found your register, there are two possible strategies for
/// implementing your trampoline:
///
/// <h2>Load-Immediate Method</h2>
///
/// This method is preferred, but it is only feasible on architectures
/// that can load a full-register-width immediate constant out of the
/// instruction stream from a naturally aligned position. It is
/// currently used on IA32 and X86_64. We expect that a variant will
/// eventually be used on Coldfire. This method is preferred mainly
/// because it does not require any references via D-space, which on most
/// architectures will eliminate a load delay stall.
///
/// Aside: This approach also preserves the possibility of a copying
/// collector that can blindly relocate procedure objects, but the
/// current implementation does not use a copying collector. A copying
/// implementation is possible for the stub-relative method (below),
/// but the collector needs to be made aware of how to rewrite the
/// stubs in that case.
///
/// To use this approach, write some assembly code that uses that
/// instruction to load some recognizable constant value:
///
/// <pre>
/// ldimm @%rclosure,$0xdeadbeef   ;; closure record to register
/// jmp $0xfeedface               ;; jump to stub</pre>
///
/// On CISC architectures it may be necessary to insert NOP
/// instructions before the immediate load in order to get your
/// constant to be naturally aligned. Now use
/// <kbd>objdump&nbsp;--disssemble</kbd> to determine what byte
/// sequence results from these instructions. Figure out where the
/// 0xdeadbeef constant ended up, and define the bitc_Procedure union
/// accordingly such that the @p env.ptr field ends up overlaying that
/// location. Your version of bitc_emit_procedure_object() will copy
/// this byte sequence into the @p code leg of the union, replacing
/// the $0xfeedface with the passed stub procedure address @p stubW
/// and then store the passed environment pointer value @p envP into
/// the @p env.ptr slot.
///
/// <h2>Procedure-Object-Relative Method</h2>
///
/// On architectures that do not have an easy way to load a
/// naturally-aligned immediate value, there is generally some
/// instruction sequence that synthesizes constant values, such as
/// <code>loadhi</code> followed by <code>add</code>. Since we want
/// the target address of the closure record to appear naturally
/// aligned, the preferred solution is to synthesize a PC-relative
/// load:
///
/// <pre>
/// ldc @%rclosure,$proc-address      ;; load address of proc object
/// mov @%rclosure,4(@%rclosure)       ;; fetch from env.ptr slot
/// jmp $0xfeedface                  ;; jump to stub</pre>
/// .long 0xdeadbeef                 ;; closure record ptr
///
/// This method is currently used on SPARC and SPARC64. It is probably
/// needed on MIPS as well. In this implementation, it becomes the
/// responsibility of the garbage collector to patch the instructions
/// emitted by the LDC pseudo-op if the procedure object is relocated.
///
/// As before, you can now use <kbd>objdump&nbsp;--disssemble</kbd> to
/// determine what byte sequence results from these
/// instructions. Figure out where the 0xdeadbeef constant ended up,
/// and define the bitc_Procedure union accordingly such that the @p
/// env.ptr field ends up overlaying that location. Also patch up the
/// offset used in the <code>mov</code> instruction to replace the "4"
/// with the proper offset of @p env.ptr.
/// Your version of
/// bitc_emit_procedure_object() will copy this byte sequence into the
/// @p code leg of the union, replacing the $proc-address with the
/// heap address of the procedure object, $0xfeedface with the
/// passed stub procedure address @p stubW, and storing the passed
/// environment pointer value @p envP into the @p env.ptr slot.
///
/// <h2>Notes on Specific Implementations</h2>
void *
bitc_emit_procedure_object(void *stubP, void *envP) MAYBE_UNUSED;

#if defined(__i386__)

typedef union {
  uint8_t code[13];
  struct {
    uint8_t pad[4];
    void *ptr;
  } env;
} bitc_Procedure;

#define BITC_GET_CLOSURE_ENV(nm) \
  void *nm; \
  __asm__ __volatile__("movl %%eax,%0" : "=g" (nm))

#elif defined(__x86_64__)

typedef union {
  uint8_t code[25];
  struct {
    uint8_t pad[16];
    void *ptr;
  } env;
} bitc_Procedure;

#define BITC_GET_CLOSURE_ENV(nm) \
  void *nm; \
  __asm__ __volatile__("movq %%rax,%0" : "=g" (nm))

#elif defined(__sparc__)

typedef union {
  uint32_t code[3];
  struct {
    uint32_t pad[3];
    void *ptr;
  } env;
} bitc_Procedure;

#define BITC_GET_CLOSURE_ENV(nm) \
  void *nm; \
  __asm__ __volatile__("mov %0,%%g0" : "=g" (nm))

#elif defined(__sparc64__)

typedef union {
  uint32_t code[7];
  struct {
    /* Note extra pad word to make ptr be naturally aligned, pending
       confirm from JWA about whether this is required. */
    uint32_t pad[8];
    void *ptr;
  } env;
} bitc_Procedure;

#define BITC_GET_CLOSURE_ENV(nm) \
  void *nm; \
  __asm__ __volatile__("mov %0,%%g0" : "=g" (nm))

#else
#  error "Target architecture not (yet) supported"
#endif


/***************************************************************
                        End of Bitc Runtime
****************************************************************/

#endif /* BITC_RUNTIME_H */
