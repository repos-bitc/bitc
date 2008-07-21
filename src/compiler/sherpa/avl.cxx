/**************************************************************************
 *
 * Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, The EROS
 *   Group, LLC. 
 * Copyright (C) 2004, 2005, 2006, Johns Hopkins University.
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

#include <stddef.h>
#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#ifdef AVL_TEST
#include <stdio.h>
#include <getopt.h>
#endif

#include <string>

#include "UExcept.hxx"
#include "avl.hxx"

#ifndef max
#define max(a,b) (((a) < (b)) ? (b) : (a))
#endif

#ifndef abs
#define abs(a) (((a) >= 0) ? (a) : - (a))
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

namespace sherpa {

  AvlTree<int> avli;
  AvlMap<int,const char *> avlm;

  BaseAvlNode::BaseAvlNode()
  {
    lchild = 0;
    rchild = 0;
    balance = 0;
  }

  BaseAvlNode::~BaseAvlNode()
  {
  }

  BaseAvlTree::BaseAvlTree(int (*cmp)(const BaseAvlNode& n1, 
				      const BaseAvlNode& n2))
  {
    cmpfn = cmp;
    root = 0;
  }

  BaseAvlTree::~BaseAvlTree()
  {
    GCPtr<BaseAvlNode> nd;

    while ((nd = choose()))
      remove(*nd);
  }

#define isBalanced(nd) ((nd == 0) || (nd->balance == 0))

  /** left_rotate: Perform a left rotate around the tree root, returning
   *  the new root:
   *
   *    A           C
   *  B   C  =>   A   E
   *     D E     B D
   */
  GCPtr<BaseAvlNode>
  BaseAvlNode::left_rotate(GCPtr<BaseAvlNode> A)
  {
    GCPtr<BaseAvlNode> C;
    GCPtr<BaseAvlNode> D;
    assert(A->rchild);

    /* Should only be doing left_rotate when we are right-heavy. */
    assert(A->balance > 0);

    C = A->rchild;
    D = C->lchild;

    C->lchild = A;
    A->rchild = D;

    return C;
  }

  /** right_rotate: Perform a right rotate around the tree root, returning
   *  the new root:
   *
   *    A           B
   *  B   C  =>   D   A
   * D E             E C
   */
  GCPtr<BaseAvlNode> 
  BaseAvlNode::right_rotate(GCPtr<BaseAvlNode> A)
  {
    GCPtr<BaseAvlNode> B;
    GCPtr<BaseAvlNode> E;

    assert(A->lchild);

    /* Should only be doing right_rotate when we are left-heavy. */
    assert(A->balance < 0);

    B = A->lchild;
    E = B->rchild;

    B->rchild = A;
    A->lchild = E;

    return B;
  }

  /** rebalance_right is called when the height of the right tree is TWO
   *  larger than the height of the left subtree.  Returns non-zero if
   *  tree height changed.
   */
  int
  BaseAvlNode::rebalance_right(GCPtr<BaseAvlNode>& theRoot)
  {
    GCPtr<BaseAvlNode> A = theRoot;
    GCPtr<BaseAvlNode> C = A->rchild;
    GCPtr<BaseAvlNode> newTree;

    assert (C);

    if (C->balance < 0) {
      /** Double rotate case. We proceed in this case by doing two
       *  rotations. First a right-rotate on C and then a left-rotate on
       *  A.
       *
       *     A              A                D
       *   B    C   =>    B     D    =>   A     C
       *      D   E           F   C      B F   G E
       *     F G                 G E
       */
      GCPtr<BaseAvlNode> D = A->rchild->lchild;
      A->rchild = right_rotate(C);
      newTree = left_rotate(A);

      switch(D->balance) {
      case -1:
	C->balance = 1;
	A->balance = 0;
	break;
      case 0:
	C->balance = 0;
	A->balance = 0;
	break;
      case 1:
	C->balance = 0;
	A->balance = -1;
	break;
      }

      D->balance = 0;
      theRoot = newTree;
      return 1;			/* tree height changed */
    }
    else {
      /** left_rotate: Perform a left rotate around the tree root, returning
       *  the new root:
       *
       *    A           C
       *  B   C  =>   A   E
       *     D E     B D
       */

      newTree = left_rotate(A);

      /* If right subtree (previously C, now A) was balanced, then
	 ht(D)==ht(E) > ht(B). and resulting tree is now left heavy. */
      if (C->balance == 0) {
	newTree->balance = -1;
	A->balance = 1;

	theRoot = newTree;
	return 0;			/* Tree height unchanged */
      }
      else { /* C->balance == 1 */
	newTree->balance = 0;
	A->balance = 0;
	theRoot = newTree;

	return 1;			/* Tree height changed */
      }
    }
  }

  /** rebalance_left is called when the height of the left tree is TWO
   *  larger than the height of the right subtree.  Returns non-zero if
   *  tree height changed.
   */
  int
  BaseAvlNode::rebalance_left(GCPtr<BaseAvlNode>& theRoot)
  {
    GCPtr<BaseAvlNode> A = theRoot;
    GCPtr<BaseAvlNode> B = A->lchild;
    GCPtr<BaseAvlNode> newTree;

    if (B->balance > 0) {
      /** Double rotate case. We proceed in this case by doing two
       *  rotations. First a left-rotate on B and then a right-rotate on A.
       *
       *      A              A              E
       *   B     C   =>    E    C   =>   B     A
       *  D  E           B  G           D F   G C
       *    F G         D F
       */

      GCPtr<BaseAvlNode> E = A->lchild->rchild;

      A->lchild = left_rotate(B);

      newTree = right_rotate(A);

      switch(E->balance) {
      case -1:
	B->balance = 0;
	A->balance = 1;
	break;
      case 0:
	B->balance = 0;
	A->balance = 0;
	break;
      case 1:
	B->balance = -1;
	A->balance = 0;
	break;
      }

      E->balance = 0;
      theRoot = newTree;

      return 1;			/* Tree height changed */
    } else {
      /** right_rotate: Perform a right rotate around the tree root, returning
       *  the new root:
       *
       *    A           B
       *  B   C  =>   D   A
       * D E             E C
       */

      newTree = right_rotate(A);

      if (B->balance == 0) {
	newTree->balance = 1;
	A->balance = -1;

	theRoot = newTree;
	return 0;			/* Tree height unchanged */
      }
      else {
	newTree->balance = 0;
	A->balance = 0;

	theRoot = newTree;
	return 1;			/* Tree height changed */
      }
    }
  }

  /* avl_do_insert: insert new node into subtree rooted at /^root/,
     returns 1 if tree has grown taller, 0 otherwise. */
  int
  BaseAvlTree::do_insert(GCPtr<BaseAvlNode>& theRoot, GCPtr<BaseAvlNode> nd)
  {
    if (!theRoot) {
      theRoot = nd;
      return 1;			/* tree has grown taller */
    }

    {
      int cmp = cmpfn(*theRoot, *nd);
      if (cmp > 0) {
	if (do_insert(theRoot->lchild, nd)) {
	  /* If resulting left subtree is imbalanced, then previously must
	     have been balanced. Add weight to the left. */
	  theRoot->balance--;

	  switch ( theRoot->balance ) {
	  case 0:
	    return 0;
	  case -1:
	    return 1;
	  case -2:
	    (void) BaseAvlNode::rebalance_left(theRoot);
	    return 0;
	  case 1:
	    /* The double rotate case can return with a positive balance
	       of 1 */
	    return 0;
	  default:
	    assert(FALSE);
	  }
	}
      }
      else if (cmp < 0) {
	if (do_insert(theRoot->rchild, nd)) {
	  /* Symmetric with above */

	  /* If resulting right subtree is imbalanced, then previously must
	     have been balanced. Add weight to the right. */
	  theRoot->balance++;

	  switch( theRoot->balance ) {
	  case 0:
	    return 0;
	  case 1:
	    return 1;
	  case 2:
	    (void) BaseAvlNode::rebalance_right(theRoot);
	    return 0;
	  case -1:
	    /* The double rotate case can return a root balance of -1 */
	    return 0;
	  default:
	    assert(FALSE);
	  }
	}
      }
    }

    /* No change (bogus insert). Tree may have been unbalanced before,
       but if so we have already taken the necessary corrective
       action. Return 0 to suppress any further tree twirling. */
    return 0;
  }

  void 
  BaseAvlTree::insert(GCPtr<BaseAvlNode> nd)
  {
    nd->balance = 0;
    nd->lchild = NULL;
    nd->rchild = NULL;
  
    do_insert(root, nd);
  }

  GCPtr<BaseAvlNode>* 
  BaseAvlTree::find_greatest(GCPtr<BaseAvlNode> *pnd)
  {
    while ((*pnd)->rchild)
      pnd = &(*pnd)->rchild;

    return pnd;
  }

  /* avl_do_remove: delete node from subtree rooted at /^root/,
     returns 1 if tree has grown shorter, 0 otherwise. */
  int
  BaseAvlTree::do_remove(GCPtr<BaseAvlNode>& theRoot, GCPtr<BaseAvlNode> nd)
  {
    if (!theRoot) {
      theRoot = nd;
      nd->balance = 0;
      nd->lchild = NULL;
      nd->rchild = NULL;

      return 0;			/* was not found */
    }

    {
      int cmp = cmpfn(*theRoot, *nd);
      if (cmp > 0) {
	if (do_remove(theRoot->lchild, nd)) {
	  /* Removed from left tree and left tree grew shorter */
	  theRoot->balance++;

	  assert ( theRoot->balance >= 0 );

	  /* If our balance is now zero, then we shrunk. */
	  if ( theRoot->balance == 0)
	    return 1;
	  /* If our balance is now mildly right-leaning, then we haven't
	     shrunk. */
	  if ( theRoot->balance == 1)
	    return 0;

	  assert ( theRoot->balance == 2);
	  return BaseAvlNode::rebalance_right(theRoot);
	}
      }
      else if (cmp < 0) {
	if (do_remove(theRoot->rchild, nd)) {
	  /* Symmetric with above */

	  /* Removed from right tree and right tree grew shorter */
	  theRoot->balance--;

	  assert ( theRoot->balance <= 0 );

	  /* If our balance is now zero, then we shrunk. */
	  if ( theRoot->balance == 0)
	    return 1;

	  /* If our balance is now mildly right-leaning, then we haven't
	     shrunk. */
	  if ( theRoot->balance == -1)
	    return 0;

	  assert ( theRoot->balance == -2);
	  return BaseAvlNode::rebalance_left(theRoot);
	}
      }
      else {
	assert(nd == theRoot);

	/* This is the node we wish to remove. If either of it's
	   children is NULL, then keep the other child and indicate that
	   the tree has shrunk. */
	if (!nd->lchild) {
	  theRoot = nd->rchild;
	  return 1;
	}

	if (!nd->rchild) {
	  theRoot = nd->lchild;
	  return 1;
	}

	/* Both subtrees are occupied. Proceed as follows:
	 *
	 * Proceed as follows: Choose either the left or right subtree
	 * arbitrarily (I chose left). Find the node within that tree
	 * that is lexically adjacent to the node we want to delete
	 * (either the greatest node in the left tree or the least node
	 * in the right tree) and EXCHANGE that node with the node that
	 * we want to delete. While the overall tree rooted here is now
	 * temporarily wrong, both the tree rooted at lchild and the
	 * tree rooted at rchild remain proper AVL trees. Once we finish
	 * deleting the dead node, the entire tree will once again be
	 * correct. 
	 *
	 * Having swapped them, proceed with the delete in the left
	 * subtree, and then rebalance theRoot according to whether the
	 * left subtree has shrunk. */

	{
	  GCPtr<BaseAvlNode> *ppVictim = find_greatest(&nd->lchild);
	  GCPtr<BaseAvlNode> victim = *ppVictim;
	  GCPtr<BaseAvlNode> vlchild = victim->lchild;
	  GCPtr<BaseAvlNode> vrchild = victim->rchild; /* should be null! */
	  GCPtr<BaseAvlNode> ndlchild = nd->lchild;
	  int victimBalance = victim->balance;

	  assert(!vrchild);

	  /* Move the victim up into the position that /nd/ now occupies */
	  theRoot = victim;
	  victim->rchild = nd->rchild;
	  nd->lchild = vlchild;
	  nd->rchild = vrchild;	/* ought to be null */
	  victim->balance = nd->balance;
	  nd->balance = victimBalance;

	  /* Need to be careful if victim was nd->lchild, because if
	     that is the case then we have already overwritten the
	     pointer slot when we clobbered nd->lchild: */
	  if (&nd->lchild == ppVictim) {
	    victim->lchild = nd;
	  }
	  else {
	    victim->lchild = ndlchild;
	    (*ppVictim) = nd;
	  }

	  if (do_remove(theRoot->lchild, nd)) {
	    /* Removed from left tree and left tree grew shorter */
	    theRoot->balance++;

	    assert ( theRoot->balance >= 0 );

	    /* If our balance is now zero, then we shrunk. */
	    if ( theRoot->balance == 0)
	      return 1;
	    /* If our balance is now mildly right-leaning, then we haven't
	       shrunk. */
	    if ( theRoot->balance == 1)
	      return 0;

	    assert ( theRoot->balance == 2);
	    return BaseAvlNode::rebalance_right(theRoot);
	  }
	}
      }
    }

    /* No change (bogus insert). Tree may have been unbalanced before,
       but if so we have already taken the necessary corrective
       action. Return 0 to suppress any further tree twirling. */
    return 0;
  }

  /* Remove a node from an AVL tree. */
  void
  BaseAvlTree::remove(BaseAvlNode& key)
  {
    GCPtr<BaseAvlNode> nd;

    /* Passed /nd/ value may actually be a key, and the do_remove()
       logic wants an honest node pointer. Do a lookup first to get a
       known-valid node pointer: */

    nd = lookup(key);

    if (nd)
      do_remove(root, nd);
  }

  GCPtr<BaseAvlNode> 
  BaseAvlTree::lookup(const BaseAvlNode& key)
  {
    GCPtr<BaseAvlNode> theRoot = root;

    while (theRoot) {
      int cmp = cmpfn(*theRoot, key);

      if (cmp == 0)
	return theRoot;
      else if (cmp < 0)
	theRoot = theRoot->rchild;
      else
	theRoot = theRoot->lchild;
    }

    return NULL;			/* not found */
  }

  GCPtr<BaseAvlNode> 
  BaseAvlTree::lookupGLE(const BaseAvlNode& key)
  {
    GCPtr<BaseAvlNode> theRoot = root;
    GCPtr<BaseAvlNode> greatest = NULL;

    while (theRoot) {
      int cmp = cmpfn(*theRoot, key);

      /* Exact match must be the greatest key less than or equal to
	 search key */
      if (cmp == 0)
	return theRoot;

      /* We're only interested in less than */
      if (cmp < 0) {

	/* Update the greatest so far */
	greatest = theRoot;

	/* Move right */
	theRoot = theRoot->rchild;
      } else
	theRoot = theRoot->lchild;
    }
    return greatest;
  }

  GCPtr<BaseAvlNode> 
  BaseAvlTree::choose()
  {
    return root;
  }

  GCPtr<BaseAvlNode> 
  BaseAvlTree::least()
  {
    GCPtr<BaseAvlNode> nd = root;

    if (!nd)
      return NULL;

    while (nd->lchild)
      nd = nd->lchild;

    return nd;
  }

  GCPtr<BaseAvlNode> 
  BaseAvlTree::greatest()
  {
    GCPtr<BaseAvlNode> nd = root;

    if (!nd)
      return NULL;

    while (nd->rchild)
      nd = nd->rchild;

    return nd;
  }

  /* This is really sleazy. We want an interator that will iterate over
   * all of the elements of a tree exactly once, where the iterator
   * function is entitled to insert or delete elements from the
   * tree. The problem is that the new elements might be inserted before
   * any current index into the tree structure, and the "exactly once"
   * rule precludes backing up.
   *
   * What we do is build a temporary internal tree, process each node by
   * first removing it, processing it, and then inserting it into the
   * output tree. Finally, we clobber the original tree with the content
   * of the output tree.
   *
   * Note the assignment to out.root at the end. This is to deal with
   * "owning" containers. At the end of the function the 'out' tree is
   * bitwise identical to the '*this' tree, and only the '*this' tree
   * will survive. We don't want the elements (which are aliased at that
   * point) deleted.
   */
  void 
  BaseAvlTree::iterate(AvlIterFunc avlFunc, const void *aux)
  {
    BaseAvlTree out = *this;
    GCPtr<BaseAvlNode> nd;

    out.root = NULL;

    while ((nd = choose())) {
      do_remove(root, nd);
      if (avlFunc(nd, aux))
	out.insert(nd);
    }

    *this = out;
    out.root = NULL;		// ensure no destruction of nodes!
  }

} /* namespace sherpa */

#ifdef AVL_TEST
static int verbose;
#define VERBOSE if(verbose)

/** Compute the height of an AVL tree. */
static int
avl_height(GCPtr<BaseAvlNode> nd)
{
  int lheight;
  int rheight;

  if (nd == NULL)
    return 0;

  lheight = avl_height(nd->lchild);
  rheight = avl_height(nd->rchild);
  
  return max(lheight, rheight) + 1;
}

/** Validate the balance fields of an AVL tree */
static int
avl_validate_balance(GCPtr<BaseAvlNode> nd)
{
  int lheight;
  int rheight;

  if (nd == NULL)
    return TRUE;

  lheight = avl_height(nd->lchild);
  rheight = avl_height(nd->rchild);

  if (abs(lheight - rheight) > 1)
    return FALSE;

  if (lheight == rheight && nd->balance != 0)
    return FALSE;

  if (lheight > rheight && nd->balance != -1)
    return FALSE;

  if (lheight < rheight && nd->balance != 1)
    return FALSE;

  if (!avl_validate_balance(nd->lchild))
    return FALSE;

  if (!avl_validate_balance(nd->rchild))
    return FALSE;

  return TRUE;
}

/* Helper routine for the structure validation logic. Idea is to first
   set the user fields to a known value, then to set them to a second
   known value but check for them already being that value to identify
   cycles. */
static int
avl_setuser(GCPtr<BaseAvlNode> nd, unsigned short user, int shouldCheck)
{
  if (nd == NULL)
    return TRUE;

  if (shouldCheck && nd->user == user)
    return FALSE;

  nd->user = user;
  if (avl_setuser(nd->lchild, user, shouldCheck) &&
      avl_setuser(nd->rchild, user, shouldCheck))
    return TRUE;

  return FALSE;
}

/** Check for circularities in the AVL structure */
static int
avl_validate_structure(GCPtr<BaseAvlNode> nd)
{
  avl_setuser(nd, 0, FALSE);

  return avl_setuser(nd, 1, TRUE);
}

static int
avl_validate(AvlTree *tree)
{
  /* Must validate structure first */
  if (!avl_validate_structure(tree->root)) {
    fprintf(stderr, "Tree contains circularities\n");
    return FALSE;
  }

  if (!avl_validate_balance(tree->root)) {
    fprintf(stderr, "Tree balance fields are wrong\n");
    return FALSE;
  }

  return TRUE;
}

static void 
do_avl_dump(FILE *out, GCPtr<BaseAvlNode> nd, void (*show)(FILE *, GCPtr<BaseAvlNode> ), 
	    int depth)
{
  int i;

  if (nd && nd->rchild)
    do_avl_dump(out, nd->rchild, show, depth + 2);
  
  for (i = 0; i < depth; i++)
    fputc(' ', out);
  show(out, nd);

  if (nd && nd->lchild)
    do_avl_dump(out, nd->lchild, show, depth + 2);
}

void 
avl_dump(FILE *out, AvlTree *tree, void (*show)(FILE *, GCPtr<BaseAvlNode> ))
{
  int good = avl_validate(tree);

  do_avl_dump(out, tree->root, show, 0);
  fprintf(out, "\n");
  assert(good);
}

typedef struct MyNode MyNode;
struct MyNode {
  BaseAvlNode avl;
  int key;
};

static int
mycmp(GCPtr<BaseAvlNode> an1, GCPtr<BaseAvlNode> an2)
{
  MyNode *n1 = (MyNode *) an1;
  MyNode *n2 = (MyNode *) an2;

  if (n1->key < n2->key)
    return -1;
  else if (n1->key > n2->key)
    return 1;
  else
    return 0;
}

static GCPtr<BaseAvlNode> 
make_node(int i)
{
  MyNode *nd = calloc(1, sizeof(MyNode));
  nd->key = i;
  return (GCPtr<BaseAvlNode> ) nd;
}

static void
myshow(FILE *out, GCPtr<BaseAvlNode> and)
{
  MyNode *nd = (MyNode *) and;
  char balance = 'b';

  if (nd && and->balance < 0)
    balance = 'l';
  if (nd && and->balance > 0)
    balance = 'r';

  if (nd)
    fprintf(out, "0x%08x [%c]: %d\n", (unsigned) nd, balance, nd->key);
  else
    fprintf(out, "0x%08x [b]\n", (unsigned) nd);

  fflush(out);
}

/* Eight values ought to be sufficient to generate all possible
   insertion permutations, but I'm using 10 because I want to make
   sure that all of the delete permutations are done correctly. This
   takes an incredibly long time to run, since it runs all
   permutations on 10 inputs, which takes O(10!)
   operations. Fortunately, we only run this when something is
   broken. */
int values[] =  { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, -1 };

void
tree_test(AvlTree *tree)
{
  int i;
  for (i = 1; values[i] >= 0; i++) {
    if (values[i] == 0)
      continue;

    int thisValue = values[i];
    values[i] = 0;

    GCPtr<BaseAvlNode> nd = make_node(thisValue);

    VERBOSE fprintf(stdout, "OP: Insert %d (0x%08x)\n", thisValue, 
		    (unsigned) nd);
    VERBOSE fflush(stdout);
    avl_insert(tree, nd);

    VERBOSE avl_dump(stdout, tree, myshow);
    assert (avl_validate(tree));

    {
      MyNode lookup_nd;
      GCPtr<BaseAvlNode> theNode;
      lookup_nd.key = thisValue;

      theNode = avl_lookup(tree, (GCPtr<BaseAvlNode> ) &lookup_nd);
      VERBOSE fprintf(stdout, "CHECK: lookup %d (0x%08x)\n", thisValue, 
		      (unsigned) theNode);
      VERBOSE fflush(stdout);

      assert(theNode == nd);
    }


    tree_test(tree);

    VERBOSE fprintf(stdout, "OP: Remove %d (0x%08x)\n", thisValue, 
		    (unsigned) nd);
    VERBOSE fflush(stdout);

    {
      MyNode lookup_nd;
      GCPtr<BaseAvlNode> theNode;
      lookup_nd.key = thisValue;

      theNode = avl_lookup(tree, (GCPtr<BaseAvlNode> ) &lookup_nd);
      VERBOSE fprintf(stdout, "CHECK: lookup %d (0x%08x)\n", thisValue, 
		      (unsigned) theNode);
      VERBOSE fflush(stdout);

      assert(theNode == nd);
    }

    avl_remove(tree, nd);
    VERBOSE avl_dump(stdout, tree, myshow);
    assert (avl_validate(tree));

    free(nd);

    values[i] = thisValue;
  }
}

/* This is a test harness helper. If the big ugly test above fails,
   pipe its output to 'grep OP' and then list the failing sequence of
   operations here as follows:

   inserts become positive entries
   deletes become negative entries
   end of list is represented by zero.

   This will allow you to replay the failing sequence (which could be
   very long) up to the moment before the disaster within a debugger,
   whereupon you can try to figure out what in hell was going on. */
int op[] = { 2, 3, 4, 5, 6, 7, -7, -6, 7, 6, -6, -7, -5, 6, 5, 7, -7, -5, 0 };

void 
specific_test(AvlTree *tree)
{
  int i = 0;

  for (i = 0; op[i]; i++) {
    int thisValue = abs(op[i]);
    if (op[i] > 0) {
      GCPtr<BaseAvlNode> nd = make_node(thisValue);
      fprintf(stdout, "OP %d: Insert %d (0x%08x)\n", i, thisValue, (unsigned) nd);
      fflush(stdout);
      avl_insert(tree, nd);

      {
	MyNode lookup_nd;
	GCPtr<BaseAvlNode> theNode;
	lookup_nd.key = thisValue;

	theNode = avl_lookup(tree, (GCPtr<BaseAvlNode> ) &lookup_nd);
	fprintf(stdout, "CHECK: lookup %d (0x%08x)\n", thisValue, (unsigned) theNode);

	avl_dump(stdout, tree, myshow);
	assert (avl_validate(tree));
	assert(theNode == nd);
      }
    }
    else {
      MyNode lookup_nd;
      lookup_nd.key = thisValue;

      GCPtr<BaseAvlNode> nd = avl_lookup(tree, (GCPtr<BaseAvlNode> ) &lookup_nd);

      fprintf(stdout, "OP %d: Remove %d (0x%08x)\n", i, thisValue, (unsigned) nd);
      fflush(stdout);

      avl_remove(tree, nd);

      avl_dump(stdout, tree, myshow);
      assert (avl_validate(tree));
    }
  }
}

int 
main(int argc, char **argv)
{
  int c;
  AvlTree *tree = avl_create(mycmp, malloc);

  while ((c = getopt(argc, argv, "v")) != -1) {
    switch(c) {
    case 'v':
      verbose = 1;
      break;
    default:
      fprintf(stderr, "Usage: avltest [-v]\n");
      exit(1);
    }
  }

  // specific_test(tree);

  tree_test(tree);

#if 0
  GCPtr<BaseAvlNode> nd1 = make_node(1);
  GCPtr<BaseAvlNode> nd2 = make_node(2);
  GCPtr<BaseAvlNode> nd3 = make_node(3);
  GCPtr<BaseAvlNode> nd4 = make_node(4);
  GCPtr<BaseAvlNode> nd5 = make_node(5);
  GCPtr<BaseAvlNode> nd6 = make_node(6);
  GCPtr<BaseAvlNode> nd7 = make_node(7);
  GCPtr<BaseAvlNode> nd8 = make_node(8);

  avl_dump(stdout, tree, myshow);

  fprintf(stdout, "Insert nd1\n");
  avl_insert(tree, nd1);
  avl_dump(stdout, tree, myshow);

  fprintf(stdout, "Insert nd2\n");
  avl_insert(tree, nd2);
  avl_dump(stdout, tree, myshow);

  fprintf(stdout, "Remove nd2\n");
  avl_remove(tree, nd2);
  avl_dump(stdout, tree, myshow);

  fprintf(stdout, "Re-Insert nd2\n");
  avl_insert(tree, nd2);
  avl_dump(stdout, tree, myshow);

  fprintf(stdout, "Remove nd1\n");
  avl_remove(tree, nd1);
  avl_dump(stdout, tree, myshow);

  fprintf(stdout, "Re-Insert nd1\n");
  avl_insert(tree, nd1);
  avl_dump(stdout, tree, myshow);

  fprintf(stdout, "Insert nd5\n");
  avl_insert(tree, nd5);
  avl_dump(stdout, tree, myshow);

  fprintf(stdout, "Insert nd3\n");
  avl_insert(tree, nd3);
  avl_dump(stdout, tree, myshow);

  fprintf(stdout, "Remove nd3\n");
  avl_remove(tree, nd3);
  avl_dump(stdout, tree, myshow);

  fprintf(stdout, "Re-Insert nd3\n");
  avl_insert(tree, nd3);
  avl_dump(stdout, tree, myshow);

  fprintf(stdout, "Insert nd4\n");
  avl_insert(tree, nd4);
  avl_dump(stdout, tree, myshow);

  fprintf(stdout, "Insert nd6\n");
  avl_insert(tree, nd6);
  avl_dump(stdout, tree, myshow);

  fprintf(stdout, "Insert nd7\n");
  avl_insert(tree, nd7);
  avl_dump(stdout, tree, myshow);

  fprintf(stdout, "Remove nd6\n");
  avl_remove(tree, nd6);
  avl_dump(stdout, tree, myshow);

  fprintf(stdout, "Remove nd5\n");
  avl_remove(tree, nd5);
  avl_dump(stdout, tree, myshow);

  fprintf(stdout, "Insert nd5\n");
  avl_insert(tree, nd5);
  avl_dump(stdout, tree, myshow);

  fprintf(stdout, "Insert nd6\n");
  avl_insert(tree, nd6);
  avl_dump(stdout, tree, myshow);

  fprintf(stdout, "Insert nd8\n");
  avl_insert(tree, nd8);
  avl_dump(stdout, tree, myshow);
#endif

  exit(0);
}
#endif
