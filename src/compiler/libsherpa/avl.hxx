#ifndef LIBSHERPA_AVL_HXX
#define LIBSHERPA_AVL_HXX

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

/* This is mildly tricky, because we really don't want to have to
 * polyinstantiate the source code for the whole AVL
 * implementation. We therefore divide matters into a generic
 * BaseAvlNode structure and a generic BaseAvlTree data structure --
 * the latter serves as the container and stores a pointer to the
 * comparison routine.
 *
 * We then use templates to instantiate type-specific avl trees.
 */

#include <libsherpa/UExcept.hxx>
#include <libsherpa/GCPtr.hxx>

// FIX: Following comment needs repair

namespace sherpa {

/// @brief Core tree node structure.
///
/// Note that there is neither a key nor a datum field.  The intended
/// use of this structure is for the client to create a containing
/// structure of which this is the first member, as in:
///
///   struct MyNode {
///     BaseAvlNode avl;
///      SomeType      key;
///      SomeOtherType value;
///   }
///
/// and then provide a comparison procedure that casts the BaseAvlNode
/// object to a typed BaseAvlNode pointer in order to perform comparison
/// functions. In this way, the library is left independent of the key
/// type or the value type, and can be used in some circumstances
/// (like sets) where neither is actually needed.
  class BaseAvlNode : public Countable {
  friend class BaseAvlTree;
  GCPtr<BaseAvlNode> lchild;
  GCPtr<BaseAvlNode> rchild;
  short    balance;		/* legal values are 0, 1, -1 */

  static GCPtr<BaseAvlNode> left_rotate(GCPtr<BaseAvlNode> );
  static GCPtr<BaseAvlNode> right_rotate(GCPtr<BaseAvlNode> );
  static int rebalance_right(GCPtr<BaseAvlNode>& theRoot);
  static int rebalance_left(GCPtr<BaseAvlNode>& theRoot);

 public:
  BaseAvlNode();
  virtual ~BaseAvlNode();

  unsigned short user;		/* user-available field */
};

class BaseAvlNode;

/// @brief Common base class for the template AVL tree class.
///
/// Note that on desctruction all of the internally allocated node 
/// structures are deleted. This means that outstanding pointers to
/// them should not be retained.
class BaseAvlTree {
protected:
  typedef int (*BaseCmpFn_t)(const BaseAvlNode& n1, const BaseAvlNode& n2);
private:
  BaseCmpFn_t cmpfn;
  GCPtr<BaseAvlNode> root;

  int do_insert(GCPtr<BaseAvlNode>& theRoot, GCPtr<BaseAvlNode> nd);
  int do_remove(GCPtr<BaseAvlNode>& theRoot, GCPtr<BaseAvlNode> nd);

  /** @brief Given a subtree pointer, find the slot pointing to the
      greatest element in that subtree. */
  static GCPtr<BaseAvlNode> *find_greatest(GCPtr<BaseAvlNode> *pnd);
public:
  BaseAvlTree(BaseCmpFn_t cmp);
  ~BaseAvlTree();

protected:
  /** Insert a new node into an existing AVL tree. The lchild/rchild
      fields of the inserted node will be set to NULL prior to
      insertion. */
  void insert(GCPtr<BaseAvlNode> nd);

  /** Selects an arbitrary node from within the avl tree (if any)
   * and returns it, or NULL if no nodes remain. The returned
   * node remains in the tree. */
  GCPtr<BaseAvlNode> choose();

  /** Returns the least node in the avl tree (if any) or NULL. The
   * returned node remains in the tree. */
  GCPtr<BaseAvlNode> least();

  /** Returns the greatest node in the avl tree (if any) or NULL. The
   * returned node remains in the tree. */
  GCPtr<BaseAvlNode> greatest();

  /** Remove a node from an AVL tree. On return, the node structure is
   *  no longer referenced by the tree (though it may be referenced by
   *  other user data structures).  It is the caller's responsibility
   *  to deallocate the storage of the Node structure if
   *  appropriate. */
  void remove(BaseAvlNode& node);

  /** Test whether and existing value exists within an AVL tree.
   *
   *  It may seem wasteful that the client must supply an BaseAvlNode
   *  rather than some more compact AvlKey structure. In our
   *  experience with tree structures it is usually the case that the
   *  search key structure is stack allocated. Since the only fields
   *  that need to be initialized are the ones consulted by the
   *  comparator routine, allocating a full BaseAvlNode actually isn't any
   *  more expensive -- it just bumps the stack by a few more
   *  words. The advantage of reusing the BaseAvlNode structure for this
   *  purpose is that it allows the tree itself to remain completely
   *  agnostic about key types and value types.
   */
  GCPtr<BaseAvlNode> lookup(const BaseAvlNode& key);

  /** Similar to avl_lookup(), but returns the BaseAvlNode whose key is
   * the greatest (according to provided comparator) key less than or
   * equal to the search key. */
  GCPtr<BaseAvlNode> lookupGLE(const BaseAvlNode& key);

  /* When iterating over an AvlTree, this function is called on every
     node. If it returns false, no attempt will be made to reinsert the
     node into the tree, and the called procedure should free the occupied
     storage. */
  typedef bool (*AvlIterFunc)(GCPtr<BaseAvlNode>, const void *aux);

  /* calls avlFunc on every node in a tree */
  void iterate(AvlIterFunc avlFunc, const void *aux = 0);
};

// Forward declaration:
template<typename KeyType> class AvlTree;

template<typename KeyType>
class AvlNode : public BaseAvlNode {
  friend class AvlTree<KeyType>;

 public:
  KeyType key;

  AvlNode(KeyType k)
    : key(k)
  {
  }

  virtual ~AvlNode()
  {
  }

protected:
  inline static int compare(const AvlNode& n1, const AvlNode& n2)
  {
      if (n1.key < n2.key)
	return -1;
      if (n1.key > n2.key)
	return 1;
      return 0;
  }
};

// Forward declaration
template<typename KeyType, typename ValueType> class AvlMap;

template<typename KeyType, typename ValueType>
class AvlMapNode : public AvlNode<KeyType> {
  friend class AvlMap<KeyType, ValueType>;

 public:
  ValueType value;

  AvlMapNode(KeyType k, ValueType v)
    : AvlNode<KeyType>(k), value(v)
  {
  }
  virtual ~AvlMapNode()
  {
  }
};

template<typename KeyType>
class AvlTree : public BaseAvlTree {
  typedef AvlNode<KeyType> NodeType;
  typedef AvlNode<KeyType> KeyNodeType;
 public:
  AvlTree()
    : BaseAvlTree((BaseCmpFn_t) AvlNode<KeyType>::compare)
    {
    }
  ~AvlTree()
    {
    }

  /** Insert a new node into an existing AVL tree. The lchild/rchild
      fields of the inserted node will be set to NULL prior to
      insertion. */
  inline void insert(const KeyType& key)
    {
      BaseAvlTree::insert(new NodeType(key));
    }

  /** Selects an arbitrary node from within the avl tree (if any)
   * and returns it, or NULL if no nodes remain. The returned
   * node remains in the tree. */
  inline GCPtr<NodeType> choose()
    {
      GCPtr<BaseAvlNode> n = BaseAvlTree::choose();
      return n ? n.upcast<NodeType>() : NULL;
    }

  /** Returns the least node in the avl tree (if any) or NULL. The
   * returned node remains in the tree. */
  inline GCPtr<NodeType> least()
    {
      GCPtr<BaseAvlNode> n = BaseAvlTree::least();
      return n ? n.upcast<NodeType>() : NULL;
    }

  /** Returns the least node in the avl tree (if any) or NULL. The
   * returned node remains in the tree. */
  inline GCPtr<NodeType> greatest()
    {
      GCPtr<BaseAvlNode> n = BaseAvlTree::greatest();
      return n ? n.upcast<NodeType>() : NULL;
    }

  /** Remove a node from an AVL tree. On return, the node structure
   *  has been freed.
   */
  inline void remove(NodeType &node)
    {
      BaseAvlTree::remove(node);
    }

  /** Test whether and existing value exists within an AVL tree.
   *
   *  It may seem wasteful that the client must supply an BaseAvlNode
   *  rather than some more compact AvlKey structure. In our
   *  experience with tree structures it is usually the case that the
   *  search key structure is stack allocated. Since the only fields
   *  that need to be initialized are the ones consulted by the
   *  comparator routine, allocating a full BaseAvlNode actually isn't any
   *  more expensive -- it just bumps the stack by a few more
   *  words. The advantage of reusing the BaseAvlNode structure for this
   *  purpose is that it allows the tree itself to remain completely
   *  agnostic about key types and value types.
   */
  inline GCPtr<NodeType> lookup(const KeyNodeType& key)
    {
      GCPtr<BaseAvlNode> n = BaseAvlTree::lookup(key);
      return n ? n.upcast<NodeType>() : NULL;
    }
  inline GCPtr<NodeType> lookup(KeyType key)
    {
      return lookup(NodeType(key));
    }

  /** Similar to avl_lookup(), but returns the BaseAvlNode whose key is
   * the greatest (according to provided comparator) key less than or
   * equal to the search key. */
  inline GCPtr<NodeType> lookupGLE(const NodeType& key)
    {
      GCPtr<BaseAvlNode> n = BaseAvlTree::lookupGLE(key);
      return n ? n.upcast<NodeType>() : NULL;
    }
  inline GCPtr<NodeType> lookupGLE(KeyType key)
    {
      return lookupGLE(NodeType(key));
    }

  /* When iterating over an AvlTree, this function is called on every
     node. If it returns false, no attempt will be made to reinsert the
     node into the tree, and the called procedure should free the occupied
     storage. */
  typedef bool (*AvlIterFunc)(GCPtr<NodeType>, const void *aux);

  /* calls avlFunc on every node in a tree */
  inline void iterate(AvlIterFunc avlFunc, const void *aux = 0)
    {
      BaseAvlTree::iterate((BaseAvlTree::AvlIterFunc)avlFunc, aux);
    }
};

template<typename KeyType, typename ValueType>
class AvlMap : public BaseAvlTree {
 public:
  typedef AvlMapNode<KeyType, ValueType> NodeType;
  typedef AvlNode<KeyType> KeyNodeType;

  AvlMap()
    : BaseAvlTree((BaseCmpFn_t) AvlMapNode<KeyType,ValueType>::compare)
    {
    }
  ~AvlMap()
    {
    }

  bool contains(const KeyType& key)
  {
    return lookup(key) ? true : false;
  }

  /** Insert a new node into an existing AVL tree. The lchild/rchild
      fields of the inserted node will be set to NULL prior to
      insertion. */
  inline void insert(const KeyType& key, const ValueType& v)
    {
      BaseAvlTree::insert(new NodeType(key,v));
    }

  /** Selects an arbitrary node from within the avl tree (if any)
   * and returns it, or NULL if no nodes remain. The returned
   * node remains in the tree. */
  inline GCPtr<NodeType> choose()
    {
      GCPtr<BaseAvlNode> n = BaseAvlTree::choose();
      return n ? n.upcast<NodeType>() : NULL;
    }

  /** Returns the least node in the avl tree (if any) or NULL. The
   * returned node remains in the tree. */
  inline GCPtr<NodeType> least()
    {
      GCPtr<BaseAvlNode> n = BaseAvlTree::least();
      return n ? n.upcast<NodeType>() : NULL;
    }

  /** Returns the least node in the avl tree (if any) or NULL. The
   * returned node remains in the tree. */
  inline GCPtr<NodeType> greatest()
    {
      GCPtr<BaseAvlNode> n = BaseAvlTree::greatest();
      return n ? n.upcast<NodeType>() : NULL;
    }

  /** Remove a node from an AVL tree. On return, the node structure
   *  has been freed.
   */
  inline void remove(NodeType &node)
    {
      BaseAvlTree::remove(node);
    }

#if 0
  inline void removeByKey(const KeyType& key)
    {
      GCPtr<NodeType> nd = lookup(key);
      if (!nd)
	THROW(ExBadValue, "Key not in map");
      BaseAvlTree::remove(*nd);
    }
#endif

  /** Test whether and existing value exists within an AVL tree.
   *
   *  It may seem wasteful that the client must supply an BaseAvlNode
   *  rather than some more compact AvlKey structure. In our
   *  experience with tree structures it is usually the case that the
   *  search key structure is stack allocated. Since the only fields
   *  that need to be initialized are the ones consulted by the
   *  comparator routine, allocating a full BaseAvlNode actually isn't any
   *  more expensive -- it just bumps the stack by a few more
   *  words. The advantage of reusing the BaseAvlNode structure for this
   *  purpose is that it allows the tree itself to remain completely
   *  agnostic about key types and value types.
   */
  inline GCPtr<NodeType> lookup(const KeyNodeType& key)
    {
      GCPtr<BaseAvlNode> n = BaseAvlTree::lookup(key);
      return n ? n.upcast<NodeType>() : NULL;
    }
  inline GCPtr<NodeType> lookup(const KeyType& key)
    {
      return lookup(KeyNodeType(key));
    }

  /** Similar to avl_lookup(), but returns the BaseAvlNode whose key is
   * the greatest (according to provided comparator) key less than or
   * equal to the search key. */
  inline GCPtr<NodeType> lookupGLE(const NodeType& key)
    {
      GCPtr<BaseAvlNode> n = BaseAvlTree::lookupGLE(key);
      return n ? n.upcast<NodeType>() : NULL;
    }
  inline GCPtr<NodeType> lookupGLE(KeyType key)
    {
      return lookupGLE(NodeType(key));
    }

  /* When iterating over an AvlMap, this function is called on every
     node. If it returns zero, no attempt will be made to reinsert the
     node into the tree, and the called procedure should free the occupied
     storage. */
  typedef bool (*AvlIterFunc)(GCPtr<NodeType>, const void *aux);

  /* calls avlFunc on every node in a tree */
  inline void iterate(AvlIterFunc avlFunc, const void *aux = 0)
    {
      BaseAvlTree::iterate((BaseAvlTree::AvlIterFunc)avlFunc, aux);
    }

  /** Ensure that @p key maps to @p value, inserting a new node if
      necessary. */
  inline void set(const KeyType& key, const ValueType& value)
  {
    GCPtr<NodeType> nd = lookup(key);
    if (nd)
      nd->value = value;
    else
      insert(key, value);
  }
};

template<typename KeyType>
struct AvlSet : public AvlMap<KeyType, bool> {
  typedef AvlMapNode<KeyType, bool> NodeType;

  void insert(const KeyType& key)
  {
    if (!contains(key))
      AvlMap<KeyType, bool>::insert(key, true);
  }

  void remove(const KeyType& key)
  {
    GCPtr<NodeType> nd = lookup(key);
    if (!nd)
      THROW(excpt::BadValue, "Key not in set");
    AvlMap<KeyType, bool>::remove(*nd);
  }
};

} /* namespace sherpa */

// Local Variables:
// mode:c++
// End:

#endif /* LIBSHERPA_AVL_HXX */
