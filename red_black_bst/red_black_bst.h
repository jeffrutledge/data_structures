/**
 * \file red_black_bst.h
 *
 * \author Jeffrey Rutledge jrutledge@hmc.edu
 *
 * \brief Implements the RedBlackBst class
 */

#ifndef RED_BLACK_BST_H
#define RED_BLACK_BST_H

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <functional>

template <typename ValueType, typename LessCompare = std::less<ValueType>>
class RedBlackBst {
 private:
  enum IteratorMutability {MUT = true, CONST = false};
  // Forward declaration of private classes
  template <IteratorMutability mutability>
  class Iterator;
  template <IteratorMutability mutability>
  class ReverseIterator;

 public:
  // STL container type definitions
  using value_type = ValueType;
  using size_type = size_t;
  using difference_type = std::ptrdiff_t;
  using reference = value_type&;
  using const_reference = const value_type&;
  using iterator = Iterator<MUT>;
  using const_iterator = Iterator<CONST>;
  using reverse_iterator = ReverseIterator<MUT>;
  using const_reverse_iterator = ReverseIterator<CONST>;
  
  /// Default Constructor
  RedBlackBst() : less_{}, size_{0}, root_{nullptr} {};
  /// Disabled Copy Constructor
  RedBlackBst(const RedBlackBst& rhs) = delete;
  /// Disabled Assignment Operator
  RedBlackBst& operator=(const RedBlackBst& rhs) = delete;
  /// Destructor
  ~RedBlackBst() {
    delete root_;
    root_ = nullptr;
  }
  /**
   * \brief Inserts the given item into the tree
   *
   * \post No changes to tree if given item is already in the tree
   */
  bool Insert(const ValueType& value) {
    if (InsertNode(value, &root_)) {
      // Make sure root is black
      if (node_type(root_) == RED_INTERNAL) {
        root_->node_color = Node::BLACK;
      }
      ++size_;
      return true;
    } else {
      return false;
    }
  }
  /**
   * \brief Looks for item in tree
   *
   * \return True if item is in tree, otherwise false
   */
  bool Find(const ValueType& value) const {
    return FindNode(value, root_);
  }
  /**
   * \brief Returns the average depth of every node (not just leaves)
   *
   * \remark Cannot be called on an empty tree.
   */
  double CalculateAverageNodeDepth() const {
    assert(!empty());
    return (double)SumOfDepthsInSubTree(root_, 0) / (double)size();
  }
  /**
   * \brief Returns the height of the tree
   *
   * \details Empty trees have a height of -1
   */
  long long CalculateHeight() const {
    return HeightOfNode(root_) - 1;
  }
  /// Returns the number of items currently stored in tree
  inline size_t size() const {
    return size_;
  }

 private:
  //----------------------------------------------------------------------------
  // NODE STRUCT
  //----------------------------------------------------------------------------
  /// A struct to store the nodes of the tree
  struct Node {
    /// Nodes can either be red or black
    enum NodeColor {RED, BLACK};
    /// Disable Default Constructor
    Node() = delete;
    /**
     * \brief Constructs a node with the given value. Sets the node's color to
     * red and sets sub trees to nullptr.
     */
    Node(const ValueType& value) : value{value},
                                   node_color{RED},
                                   smaller{nullptr},
                                   larger{nullptr} {};
    /**
     * \brief Constructs a node with the given value and node_color. Sets
     * sub trees to nullptr.
     */
    Node(const ValueType& value, const NodeColor node_color) 
        : value{value},
          node_color{node_color},
          smaller{nullptr},
          larger{nullptr} {};
    /// Constructs a node with the given value, type, and sub trees
    Node(const ValueType& value, const NodeColor node_color,
         Node* const& smaller, Node* const& larger) : value{value},
                                                      node_color{node_color},
                                                      smaller{smaller},
                                                      larger{larger} {};
    /// Destructor for nodes, recursively deletes all children
    ~Node() {
      delete smaller;
      smaller = nullptr;
      delete larger;
      larger = nullptr;
    };
    /// The value stored at this node
    ValueType value;
    /// The current color of this node is.
    NodeColor node_color;
    /**
     * \brief A pointer to the root of the smaller sub tree
     *
     * \detail All values in this tree will be smaller than the value of this
     * node
     */
    Node* smaller;
    /**
     * \brief A pointer to the root of the larger sub tree
     *
     * \detail All values in this tree will be larger than the value of this
     * node
     */
    Node* larger;
  };

  //----------------------------------------------------------------------------
  // ITERATOR
  //----------------------------------------------------------------------------
  /**
   * \brief Iterates through the tree from least to greatest
   * \remark Increment and Decrement are not guaranteed to be constant time
   */
  template <IteratorMutability mutability>
  class Iterator {
   public:
    using value_type = ValueType;
    using reference = typename std::conditional<mutability, value_type&, 
                                                const value_type&>::type;
    using pointer = typename std::conditional<mutability, value_type*,
                                              const value_type*>::type;
    using difference_type = std::ptrdiff_t;
    using iterator_category = std::bidirectional_iterator_tag;
    using const_reference = const value_type&;

    Iterator& operator++();
    Iterator& operator--();
    reference operator*() const;
    bool operator==(const Iterator& rhs) const;
    bool operator!=(const Iterator& rhs) const;

   private:
    friend class RedBlackBst;
    Iterator();
    Node* current_node_;
  };

  //----------------------------------------------------------------------------
  // PRIVATE MEMBERS
  //----------------------------------------------------------------------------
  /// States of balance and imbalance of black nodes on insert
  enum InsertBalanceState {
      /// Balanced State: No consecutive red nodes looking down two levels
      BALANCED,
      /// Imbalanced State
      BOTH_CHILDREN_RED,
      /// Imbalanced State
      SMALLER_INNER_RED_GRANDCHILD,
      /// Imbalanced State
      LARGER_INNER_RED_GRANDCHILD,
      /// Imbalanced State
      SMALLER_OUTER_RED_GRANDCHILD,
      /// Imbalanced State
      LARGER_OUTER_RED_GRANDCHILD};
  /**
   * \brief Types of nodes in the red black tree
   *
   * \detail Internal nodes are non-leaf nodes.
   * Leaf nodes are the nullptr nodes at the tip of the tree, and are considered
   * black.
   */
  enum NodeType {RED_INTERNAL, BLACK_INTERNAL, BLACK_LEAF};
  /// Recursive function that returns the height of the given Node
  static long long HeightOfNode(const Node*const& node) {
    if (node == nullptr) {
      return 0;
    } else {
      return 1 +
             std::max(HeightOfNode(node->smaller),
                      HeightOfNode(node->larger));
    }
  }
  /**
   * \brief Recursive function that returns the sum of the depths of all of
   * the nodes in the sub tree formed with the given node as its root.
   */
  static size_t SumOfDepthsInSubTree(const Node*const& node,
                                     const size_t current_depth) {
    if (node == nullptr) {
      return 0;
    } else {
      return current_depth +
             SumOfDepthsInSubTree(node->smaller, current_depth + 1) +
             SumOfDepthsInSubTree(node->larger, current_depth + 1);
    }
  }
  /**
   * \brief A recursive helper function that looks for a node with the same
   * value as is given
   *
   * \return True if a node is found, false if the search reaches a leaf without
   * finding the node
   */
  bool FindNode(const ValueType& value, const Node* const node) const {
    if (node == nullptr) {
      return false;
    } else if (less_(value, node->value)) {
      return FindNode(value, node->smaller);
    } else if (less_(node->value, value)) {
      return FindNode(value, node->larger);
    }
    return true;
  }
  /**
   * \brief A recursive helper function that looks for the correct leaf to
   * insert the given value.
   *
   * \return True if the value is inserted, or false if the value is a duplicate
   * and is not inserted.
   */
  bool InsertNode(const ValueType& value, Node**const node) {
    bool hasInserted = false;
    if (*node == nullptr) {
      *node = new Node(value);
      return true;
    } else if (less_(value, (*node)->value)) {
      hasInserted = InsertNode(value, &(*node)->smaller);
    } else if (less_((*node)->value, value)) {
      hasInserted = InsertNode(value, &(*node)->larger);
    } else {
      // Duplicate, so do not insert
      return false;
    }

    if (hasInserted && node_type(*node) == BLACK_INTERNAL) {
      balance_node(node_balance_state(*node), node);
    }
    return hasInserted;
  }
  /**
   * \brief Returns the balance state of the node
   * 
   * \remark Can only be called on black internal (non-leafs) nodes
   */
  static inline InsertBalanceState node_balance_state(const Node* const& node) {
    NodeType type_of_smaller = node_type(node->smaller);
    NodeType type_of_larger = node_type(node->larger);

    if (type_of_smaller == RED_INTERNAL) {
      if (type_of_larger == RED_INTERNAL) {
        return BOTH_CHILDREN_RED;
      } else if (node_type(node->smaller->smaller) == RED_INTERNAL) {
        return SMALLER_OUTER_RED_GRANDCHILD;
      } else if (node_type(node->smaller->larger) == RED_INTERNAL) {
        return SMALLER_INNER_RED_GRANDCHILD;
      }
    } else if (type_of_larger == RED_INTERNAL) {
      if (node_type(node->larger->larger) == RED_INTERNAL) {
        return LARGER_OUTER_RED_GRANDCHILD;
      } else if (node_type(node->larger->smaller) == RED_INTERNAL) {
        return LARGER_INNER_RED_GRANDCHILD;
      }
    }
    return BALANCED;
  }
  /**
   * \brief Returns the type of the given node
   *
   * \remark nullptr nodes (leaves) are considered black
   */
  static inline NodeType node_type(const Node* const& node) {
    if (node != nullptr) {
      if (node->node_color == Node::RED) {
        return RED_INTERNAL;
      }
      return BLACK_INTERNAL;
    }
    return BLACK_LEAF;
  }
  /// Performs the correct modifications to rebalance the tree
  static inline void balance_node(const InsertBalanceState balance_state,
                                  Node** const node) {
    switch (balance_state) {
      case (BALANCED) : {
        break;
      }
      case (BOTH_CHILDREN_RED) : {
        (*node)->node_color = Node::RED;
        (*node)->smaller->node_color = Node::BLACK;
        (*node)->larger->node_color = Node::BLACK;
        break;
      }
      case (SMALLER_INNER_RED_GRANDCHILD) : {
        RotateLeft((*node)->smaller);
        RotateRight((*node), true);
        break;
      }
      case (LARGER_INNER_RED_GRANDCHILD) : {
        RotateRight((*node)->larger);
        RotateLeft((*node), true);
        break;
      }
      case (SMALLER_OUTER_RED_GRANDCHILD) : {
        RotateRight((*node), true);
        break;
      }
      case (LARGER_OUTER_RED_GRANDCHILD) : {
        RotateLeft((*node), true);
        break;
      }
    }
  }
  /**
   * \brief A recursive function that looks for the correct leaf to insert the
   * given value at, and then rotates this node back to the node it was
   * originally called on.
   */
  void InsertAtRoot(const ValueType& value, Node*& node);
  /**
   * \brief Performs a right rotation with the given node as the initial top.
   *
   * \param swap_colors If true the color of the top and new_top nodes will be
   * swapped
   *
   * \post The given node's larger child (right) ends up at the top.
   * The tree is still a BST.
   */
  inline static void RotateRight(Node*& top, const bool swap_colors = false) {
    Node* new_top = top->smaller;

    // Swap colors, if requested
    if (swap_colors) {
      typename Node::NodeColor previous_top_color = top->node_color;
      top->node_color = new_top->node_color;
      new_top->node_color = previous_top_color;
    }

    top->smaller = new_top->larger;
    new_top->larger = top;
    top = new_top;
  }
  /**
   * \brief Performs a left rotation with the given node as the initial top
   *
   * \param swap_colors If true the color of the top and new_top nodes will be
   * swapped
   *
   * \post The given node's smaller child (left) ends up at the top.
   * The tree is still a BST.
   */
  inline static void RotateLeft(Node*& top, const bool swap_colors = false) {
    Node* new_top = top->larger;

    // Swap colors, if requested
    if (swap_colors) {
      typename Node::NodeColor previous_top_color = top->node_color;
      top->node_color = new_top->node_color;
      new_top->node_color = previous_top_color;
    }

    top->larger = new_top->smaller;
    new_top->smaller = top;
    top = new_top;
  }
  /// Returns true if the tree has no nodes
  inline bool empty() const {
    return size_ == 0;
  }
  /// A functor that performs a less than operation on the ValueType
  LessCompare less_;
  /// Number of items in the tree
  size_t size_;
  /// The root of this tree
  Node* root_;
};

#endif  // RED_BLACK_BST_H
