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

/* For DEFEXCEPTION, we emit a C declaration of the form

  const char *CMANGLE(excpt-id) = "CMANGLE(excpt-id)"

Later, use the following structure as the common "base" class for
the exception structure that is actually being thrown. The reason
to have the structure is so that we can do a cast to it at the site
of the CATCH.

This is a debugging trick to make the exception human-readable in a
debugger. For production purposes, it would be perfectly acceptable
to emit a non-static const char having no meaningful value and use
the address of that. */
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

INLINE void *
bitc_malloc(size_t sz)
{
  void *ptr = GC_malloc(sz);
  if(ptr == NULL)
    bitc_throw(&val_ExOutOfMemory);
    
  return ptr;
}
 
INLINE void *
bitc_malloc_atomic(size_t sz)
{
  void *ptr = GC_malloc_atomic(sz);
  if(ptr == NULL)
    bitc_throw(&val_ExOutOfMemory);
  
  return ptr;
}

/* The Bitc String */

typedef struct {
  bitc_word_t length;
  const char *s;		/* immutable, unicode UTF-8! */
} bitc_string_t;

static bitc_string_t *
mkStringLiteral(const char *s) MAYBE_UNUSED;

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
 **             End of Script generatede code                **
 **************************************************************/

/**************************************************************
              Support for Closure Conversion
 **************************************************************/

/**** Support for run-time generation of closure objects   ****/

/* Current-Closure-pointer must be in thread-local storage. However,
   there is only one thread for now. */

/*********  The following is highly machine dependent       ****/
/***************************************************************** 
  For a procedure that has a closure, we emit the following things:

   A. The "real" procedure:

      ret-type __real_procname(Closure *clp, Arg1 a1, ..., Argn an) {
        ... real procedure body ...
      }

      This is the procedure that will do all of the work.

   B. A "transition" procedure:

      ret-type __transition_procname(Arg1 a1, ..., Argn an) {
        return __real_procname(CurrentClosurePointer, a1, ... an);
      }

      The only reason we are emitting this procedure is because it lets
      us use the C compiler to re-arrange the stack frame, and the C
      compiler already knows the rules for doing that. The variable
      CurrentClosurePointer is a thread-local global variable that
      is provided by the run time layer.

   C. Finally, we will hand-emit a machine-dependent code sequence
      that does the following:

         mov CurrentClosurePointer <- $ProperClosureValue
         jmp $procname-transition

      This is the actual closure object. The closure object will get
      emitted in a data structure that is organized as follows:

         +-----------------------+
         |  code bytes for the   |
         |  instructions above   |
         | NOPS to word boundary |
         +-----------------------+
         |   ProperClosureValue  |   ( a pointer )
         +-----------------------+

      The value of ProperClosureValue is redundantly stored so that it
      can be found easily by the garbage collector. On machines that
      have PC-relative load, it is possible to actually *use* this
      value.
******************************************************************/

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
  __asm__ __volatile__("movl %%rax,%0" : "=g" (nm))

#else
#  error "Target architecture not (yet) supported"
#endif


/***************************************************************
                        End of Bitc Runtime
****************************************************************/

#endif /* BITC_RUNTIME_H */
