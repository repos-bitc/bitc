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

#include <string.h>
#include <malloc.h>
#include <assert.h>

#include <iostream>

#include "GC.hxx"
#include "GCRoot.hxx"

struct ObHdr {
  unsigned mark : 1;
  unsigned free : 1;
  union {
    void (*markFn)(void *obPtr);
    ObHdr *nextFree;
  } u;
};

void_GCRoot void_GCRoot::TrueRoot;

// This is a quasi-bebop allocator. It allocates size-based slabs 
// rather than pages.
typedef unsigned long WORD;

struct ObSlabHdr {
  struct ObSlab *next;
  //  WORD hwSize;		       // including header, 0 implies variable
};

struct ObSlab {
  ObSlabHdr hdr;
  WORD heap[0];
};

enum { nPool = 3,
       obsPerSlab = 2048 };
static struct Pool {
  ObHdr *nextFree;
  ObSlab *slab;
  unsigned long long nAllocated;
  unsigned long long nCollected;
  unsigned long long nWords;

  Pool()
  {
    nextFree = 0;
    slab = 0;
    nAllocated = 0;
    nCollected = 0;
    nWords = 0;
  }
} obPool[nPool + 1];

inline static unsigned
wsize(size_t sz)
{
  return (sz + (sizeof(WORD)-1)) / sizeof(WORD);
}

#define OBHDR_WSIZE (wsize(sizeof(ObHdr)))
#define HDR(ob) ((ObHdr *) (((unsigned char *)(ob)) - sizeof(ObHdr)))
#define OB(hdr) (((unsigned char *)(hdr)) + sizeof(ObHdr))
#define ISPTR(p) ( ( (WORD)(p) & 1u ) == 0)

void
expandPool(unsigned pool)
{
  size_t obWords = (1u << pool) + OBHDR_WSIZE;
  size_t obBytes = sizeof(WORD) * obWords;

  ObSlab *slab = (ObSlab *) malloc(sizeof(ObSlabHdr) + (obsPerSlab * obBytes));

  for (unsigned i = 0; i < (obsPerSlab - 1); i++) {
    ObHdr *h1 = (ObHdr*) &slab->heap[i * obWords];
    ObHdr *h2 = (ObHdr*) &slab->heap[i+1 * obWords];
    h1->u.nextFree = h2;
    h1->free = 1;
  }

  ObHdr *h1 = (ObHdr*) &slab->heap[(obsPerSlab - 1) * obWords];
  h1->u.nextFree = obPool[pool].nextFree;
  h1->free = 1;

  slab->hdr.next = obPool[pool].slab;
  obPool[pool].slab = slab;
  obPool[pool].nextFree = ((ObHdr *)&slab->heap[0]);
  obPool[pool].nWords += obsPerSlab * obWords;

  std::cout << "Expanded pool " << pool << " (size "
	    << (1u << pool) << ")." << std::endl;
}

ObHdr *
allocCustom(size_t wsz)
{
  size_t obBytes = sizeof(WORD) * (wsz + OBHDR_WSIZE);
  ObSlab *slab = (ObSlab *) malloc(sizeof(ObSlabHdr) + obBytes);

  slab->hdr.next = obPool[nPool].slab;
  obPool[nPool].slab = slab;
  obPool[nPool].nWords += wsz;
  obPool[nPool].nAllocated++;

  std::cout << "Expanded oversize pool." << std::endl;

  ObHdr *hdr = (ObHdr *)slab->heap;
  hdr->free = 0;

  return hdr;
}

void *
nzallocate(size_t sz, void (*vp)(...))
{
  unsigned nWords = wsize(sz);

  ObHdr *hdr;

  if (nWords <= (1u << nPool)) {
    unsigned pool = 0;
    while (nWords > (1u << pool))
      pool++;

    if (!obPool[pool].nextFree) {
      majorGC();
      if (!obPool[pool].nextFree) {
	expandPool(pool);
      }
    }

    hdr = obPool[pool].nextFree;
    obPool[pool].nextFree = hdr->u.nextFree;
    obPool[pool].nAllocated++;
  }
  else {
    hdr = allocCustom(nWords);
  }
    
  hdr->u.markFn = (void (*)(void *)) vp;
  hdr->free = 0;
  hdr->mark = 0;

  return OB(hdr);
}

void *
allocate(size_t sz, void (*vp)(...))
{
  void *pOb = nzallocate(sz, vp);
  memset(pOb, 0, sz);
  return pOb;
}

void
mark(void *pOb)
{
  ObHdr *obHdr = HDR(pOb);
  obHdr->mark = 1;
}

void
clear(void *pOb)
{
  ObHdr *obHdr = HDR(pOb);
  obHdr->mark = 0;
}

void minorGC(void)
{
  majorGC();
}

void majorGC(void)
{
  std::cout << "Major collection..." << std::endl;

  // Clear the standard-size objects:
  for (unsigned pool = 0; pool < nPool; pool++) {
    for (ObSlab *slab = obPool[pool].slab; slab; slab = slab->hdr.next) {
      size_t obWords = (1u << pool) + OBHDR_WSIZE;

      for (unsigned i = 0; i < obsPerSlab; i++)
	((ObHdr *)(&slab->heap[i * obWords]))->mark = 0;
    }
  }
  // Clear the oversize objects:
  for (ObSlab *slab = obPool[nPool].slab; slab; slab = slab->hdr.next)
    ((ObHdr *)(&slab->heap[0]))->mark = 0;

  // Mark the world:
  for (void_GCRoot *root = void_GCRoot::TrueRoot.next;
       root != &void_GCRoot::TrueRoot;
       root = root->next) {
    if (root->ptr && ISPTR(root->ptr))
      mark(root->ptr);
  }

  // Sweep the standard-size objects:
  for (unsigned pool = 0; pool < nPool; pool++) {
    for (ObSlab *slab = obPool[pool].slab; slab; slab = slab->hdr.next) {
      size_t obWords = (1u << pool) + OBHDR_WSIZE;

      for (unsigned i = 0; i < obsPerSlab; i++) {
	ObHdr *hdr = (ObHdr *) (&slab->heap[i * obWords]);
	if (!hdr->mark && !hdr->free) {
	  hdr->free = 1;
	  hdr->u.nextFree = obPool[pool].nextFree;
	  obPool[pool].nextFree = hdr;;
	  obPool[pool].nCollected++;
	}
      }
    }
  }

  // Sweep the oversize objects. This is tricky because we may need
  // to free them as we go, so we stick them back into the obPool in
  // a funny way (c.f. NREVERSE):
  {
    ObSlab *slab = obPool[nPool].slab;
    obPool[nPool].slab = 0;

    while (slab) {
      ObSlab *next = slab->hdr.next;

      ObHdr *hdr = (ObHdr *)(&slab->heap[0]);
      if (hdr->mark) {
	slab->hdr.next = obPool[nPool].slab;
	obPool[nPool].slab = slab;
      }
      else
	free(slab);

      slab = next;
    }
  }

  for (unsigned pool = 0; pool < nPool+1; pool++) {
    std::cout << "Pool " << pool
	      << " allocated " << obPool[pool].nAllocated
	      << " collected " << obPool[pool].nCollected
	      << " words " << obPool[pool].nWords
	      << std::endl;
  }

  // FIX: Now need to mark things!
}

// #define TESTING
#ifdef TESTING
int main(int argc, char *argv[])
{
  for (int i = 0; i < obsPerSlab * 2 ; i++) {
    (void) nzallocate(4, 0);
#if 0
    std::cout << "Allocated: " 
	      << std::hex << (unsigned long)pObject << std::dec 
	      << std::endl;
#endif
  }
  (void) nzallocate(4096, 0);

  majorGC();
}
#endif /* TESTING */
