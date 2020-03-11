use std::cell::RefCell;
use std::rc::{Rc, Weak};

pub type NodePtr<T> = Rc<RefCell<Tree<T>>>;
pub type WeakNodePtr<T> = Weak<RefCell<Tree<T>>>;

#[derive(Debug)]
pub struct Tree<T> {
    pub data: T,
    pub parent: Option<WeakNodePtr<T>>,
    pub siblings: Vec<NodePtr<T>>,
}

pub struct TreeIter<T> {
    next: Option<NodePtr<T>>,
    stack: Vec<NodePtr<T>>,
}

impl<T> Iterator for TreeIter<T> {
    type Item = NodePtr<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.next.clone() {
            if !next.borrow().siblings.is_empty() {
                let mut stack = next.borrow().siblings.clone();
                stack.reverse();
                self.next = stack.pop();
                self.stack.extend_from_slice(&stack);
            } else {
                self.next = self.stack.pop();
            }
            return Some(next);
        }
        None
    }
}

impl<T> Tree<T> {
    /// Simple stack-based iterator.
    pub fn iter(&self) -> TreeIter<T> {
        let mut stack: Vec<NodePtr<T>> = self.siblings.iter().cloned().rev().collect();
        TreeIter {
            next: stack.pop(),
            stack,
        }
    }
}

impl<T> Drop for Tree<T> {
    fn drop(&mut self) {
        let mut stack = std::mem::replace(&mut self.siblings, Vec::new());
        while let Some(current) = stack.pop() {
            if Rc::strong_count(&current) == 1 {
                let mut current = current.borrow_mut();
                stack.extend_from_slice(&current.siblings);
                current.siblings.clear();
            }
        }
    }
}

impl<T> ToString for Tree<T>
where
    T: ToString,
{
    /// Converts tree to s-expression string.
    ///
    /// Each node will be wrapped in parentheses, with it's value
    /// going first, and subnodes afterwards.
    ///
    /// # Examples
    ///
    /// ```
    /// let root = Tree::new(0);
    /// Tree::add_child(&root, 1);
    /// Tree::add_child(&root, 2);
    ///
    /// assert_eq!(&root.borrow().to_string(), "(0 (1) (2))");
    /// ```
    fn to_string(&self) -> String {
        let mut string = format!("({}", self.data.to_string());

        if !self.siblings.is_empty() {
            let mut queue = self.siblings.to_vec().clone();
            queue.reverse();
            let mut depth = 0;

            while let Some(current) = queue.pop() {
                let mut pushed = false;
                string.push_str(&format!(" ({}", current.borrow().data.to_string()));
                depth += 1;

                for node in current.borrow().siblings.iter() {
                    if !node.borrow().siblings.is_empty() {
                        queue.push(node.clone());
                        pushed = true;
                    } else {
                        string.push_str(&format!(" ({})", node.borrow().data.to_string()));
                    }
                }

                if !pushed {
                    string.push_str(")");
                    depth -= 1;
                }
            }
            string = string.trim().to_string();

            while depth > 0 {
                string.push_str(")");
                depth -= 1;
            }
        }
        string.push_str(")");
        string
    }
}

impl<T> PartialEq for Tree<T>
where
    T: PartialEq,
{
    /// Compare trees.
    ///
    /// Compares tree values and all subtrees from one tree with another.
    ///
    /// # Examples
    ///
    /// ```
    /// let root1 = Tree::new(0);
    /// Tree::add_child(&root1, 1);
    /// Tree::add_child(&root1, 2);
    ///
    /// let root2 = Tree::new(0);
    /// Tree::add_child(&root2, 1);
    /// Tree::add_child(&root2, 2);
    ///
    /// assert_eq!(root1, root2);
    ///
    /// Tree::add_child(&root2, 3);
    /// assert_ne!(root1, root2);
    /// ```
    fn eq(&self, other: &Tree<T>) -> bool {
        if self.data != other.data {
            return false;
        } else if !self.siblings.is_empty() || !other.siblings.is_empty() {
            if self.siblings.len() == other.siblings.len() {
                for (i, j) in self.iter().zip(other.iter()) {
                    if i.borrow().data != j.borrow().data {
                        return false;
                    }
                }
            } else {
                return false;
            }
        }
        true
    }
}

// #[allow(dead_code)]
impl<T> Tree<T>
where
    T: Clone,
{
    /// Create new tree.
    ///
    /// Returns refernce-counted RefCell to the tree's root with
    /// specified value.
    ///
    /// # Examples
    ///
    /// ```
    /// let root = Tree::new(0);
    ///
    /// assert_eq!(root.boorrow().to_string(), "(0)");
    /// ```
    pub fn new(data: T) -> NodePtr<T> {
        Rc::from(RefCell::from(Tree {
            data,
            parent: None,
            siblings: vec![],
        }))
    }

    /// Add subnode to current node with specified value.
    ///
    /// Nodes are added to the end of node stack for current node.
    ///
    /// # Examples
    ///
    /// ```
    /// let root = Tree::new(0);
    ///
    /// let one = Tree::add_child(&root, 1);
    ///
    /// assert_eq!(root.borrow().to_string(), "(0 (1))");
    /// ```
    pub fn push_child(node: &NodePtr<T>, data: T) -> NodePtr<T> {
        let new_node = Rc::from(RefCell::from(Tree {
            data,
            parent: Some(Rc::downgrade(node)),
            siblings: vec![],
        }));
        node.borrow_mut().siblings.push(new_node.clone());
        new_node
    }

    /// Adopts node as a subnode for specified root node.
    ///
    /// Changes node's parent to be root, and pushes node to list of
    /// root's subnodes.
    ///
    /// # Examples
    ///
    /// ```
    /// let root = Tree::new(0);
    /// let one = Tree::new(1);
    ///
    /// Tree::adopt_tree(&root, one);
    ///
    /// assert_eq!(root, Tree::get_parent(&Tree::push_tree(&Tree::new(0), 1)).unwrap());
    /// ```
    pub fn push_tree(root: &NodePtr<T>, tree: NodePtr<T>) -> NodePtr<T> {
        tree.borrow_mut().parent = Some(Rc::downgrade(root));
        root.borrow_mut().siblings.push(tree);
        root.borrow().siblings.last().unwrap().clone()
    }

    /// Replaces one node with another node.
    ///
    /// Keeps current pointer and parent, updates node's data and
    /// list of subnodes from the other node.
    ///
    /// # Examples
    ///
    /// ```
    /// let root = Tree::new(22);
    /// let forty_two = Tree::add_child(&root, 43);
    /// Tree::replace_tree(&forty_two, Tree::new(42));
    ///
    /// assert_eq!(root, Tree::get_parent(&Tree::add_child(&Tree::new(22), 42)).unwrap());
    /// ```
    pub fn replace_tree(tree1: &NodePtr<T>, tree2: NodePtr<T>) {
        tree1.borrow_mut().data = tree2.borrow().data.clone();
        tree1.borrow_mut().siblings.clear();
        for c in tree2.borrow().siblings.iter() {
            Tree::push_tree(tree1, c.clone());
        }
    }

    /// Clone tree
    ///
    /// Creates copy of tree that uses the same layout, with the same
    /// data, but different set of pointers to the heap.
    ///
    /// # Examples
    ///
    /// ```
    /// let root = Tree::new(0);
    /// Tree::add_child(&root, 1);
    /// Tree::add_child(&root, 2);
    ///
    /// let new = Tree::clone_tree(&root);
    /// assert_eq!(root, new);
    /// ```
    pub fn clone_tree(node: &NodePtr<T>) -> NodePtr<T> {
        let root = Tree::new(node.borrow().data.clone());
        root.borrow_mut().parent = node.borrow().parent.clone();
        for child in node.borrow().siblings.iter() {
            Tree::push_tree(&root, Self::clone_tree(child));
        }
        root
    }

    /// Get node parent
    ///
    /// Returns `Some(NodePtr<T>)` parent of passed node. Returns
    /// `None` if no parent exists.
    pub fn parent(node: &NodePtr<T>) -> Option<NodePtr<T>> {
        Weak::upgrade(node.borrow().parent.as_ref()?)
    }
}

#[cfg(test)]
mod tests {
    use super::{NodePtr, Tree};

    fn to_string_rec<T>(node: &NodePtr<T>) -> String
    where
        T: ToString + std::fmt::Display,
    {
        let mut string = String::new();
        build_string(node, &mut string);
        string.trim().to_owned()
    }

    fn build_string<T>(node: &NodePtr<T>, string: &mut String)
    where
        T: ToString + std::fmt::Display,
    {
        string.push_str(&format!("({} ", node.borrow().data.to_string()));
        for n in node.borrow().siblings.iter() {
            build_string(n, string);
        }
        *string = string.trim().to_owned();
        string.push_str(") ");
    }

    #[test]
    fn to_string() {
        // 0
        let root = Tree::new(0);
        assert_eq!(root.borrow().to_string(), to_string_rec(&root));
        assert_eq!(root.borrow().to_string(), "(0)");

        //   0
        //  / \
        // 1   2
        let root = Tree::new(0);
        Tree::push_child(&root, 1);
        Tree::push_child(&root, 2);
        assert_eq!(root.borrow().to_string(), to_string_rec(&root));
        assert_eq!(root.borrow().to_string(), "(0 (1) (2))");

        //     0
        //    / \
        //   1   4
        //  / \   \
        // 2   3   5
        let root = Tree::new(0);
        let first = Tree::push_child(&root, 1);
        let second = Tree::push_child(&root, 4);
        Tree::push_child(&first, 2);
        Tree::push_child(&first, 3);
        Tree::push_child(&second, 5);
        assert_eq!(root.borrow().to_string(), to_string_rec(&root));
        assert_eq!(root.borrow().to_string(), "(0 (1 (2) (3)) (4 (5)))");

        //       0
        //    /  |  \
        //   1   4   6
        //  / \  |  /|\
        // 2   3 5 7 8 9
        let root = Tree::new(0);
        let first = Tree::push_child(&root, 1);
        let second = Tree::push_child(&root, 4);
        let third = Tree::push_child(&root, 6);
        Tree::push_child(&first, 2);
        Tree::push_child(&first, 3);
        Tree::push_child(&second, 5);
        Tree::push_child(&third, 7);
        Tree::push_child(&third, 8);
        Tree::push_child(&third, 9);
        assert_eq!(root.borrow().to_string(), to_string_rec(&root));
        assert_eq!(
            root.borrow().to_string(),
            "(0 (1 (2) (3)) (4 (5)) (6 (7) (8) (9)))"
        );

        //"(0 (1 (2 3) 4) (5 (6 (7 8 (9 10) 11) 12 13) 14) 16)"
        // let root = Tree::new(0);
        // let first = Tree::push_child(&root, 1);
        // let first_sub = Tree::push_child(&first, 2);
        // Tree::push_child(&first_sub, 3);
        // Tree::push_child(&first, 4);
    }

    #[test]
    fn compare() {
        //   0     0
        //  / \   / \
        // 1   2 1   2
        let root1 = Tree::new(0);
        Tree::push_child(&root1, 1);
        Tree::push_child(&root1, 2);
        let root2 = Tree::new(0);
        Tree::push_child(&root2, 1);
        Tree::push_child(&root2, 2);
        assert_eq!(root1, root2);

        //     0         0
        //    / \       / \
        //   1   4     1   4
        //  / \   \   / \   \
        // 2   3   5 2   3   5
        let root1 = Tree::new(0);
        let one1 = Tree::push_child(&root1, 1);
        let four1 = Tree::push_child(&root1, 4);
        let two1 = Tree::push_child(&one1, 2);
        let three1 = Tree::push_child(&one1, 3);
        let five1 = Tree::push_child(&four1, 5);

        let root2 = Tree::new(0);
        let one2 = Tree::push_child(&root2, 1);
        let four2 = Tree::push_child(&root2, 4);
        let two2 = Tree::push_child(&one2, 2);
        let three2 = Tree::push_child(&one2, 3);
        let five2 = Tree::push_child(&four2, 5);
        assert_eq!(root1, root2);

        // changing various nodes
        Tree::replace_tree(&five2, Tree::new(6));
        assert_ne!(root1, root2);
        Tree::replace_tree(&five2, Tree::new(5));
        assert_eq!(root1, root2);

        Tree::replace_tree(&three2, Tree::new(9));
        assert_ne!(root1, root2);
        Tree::replace_tree(&three2, Tree::new(3));
        assert_eq!(root1, root2);

        Tree::replace_tree(&two1, Tree::new(-1));
        assert_ne!(root1, root2);
        Tree::replace_tree(&two1, Tree::new(5));
        Tree::replace_tree(&two2, Tree::new(5));
        assert_eq!(root1, root2);

        Tree::replace_tree(&two1, Tree::new(2));
        Tree::replace_tree(&two2, Tree::new(2));
        Tree::replace_tree(&three1, Tree::new(2));
        Tree::replace_tree(&three2, Tree::new(7));
        assert_ne!(root1, root2);
        Tree::replace_tree(&three1, Tree::new(3));
        Tree::replace_tree(&three2, Tree::new(3));
        assert_eq!(root1, root2);

        Tree::replace_tree(&one1, Tree::new(0));
        assert_ne!(root1, root2);
        Tree::replace_tree(&one2, Tree::new(0));
        assert_eq!(root1, root2);

        Tree::replace_tree(&five1, Tree::new(0));
        assert_ne!(root1, root2);
        Tree::replace_tree(&five2, Tree::new(0));
        assert_eq!(root1, root2);

        Tree::replace_tree(&root1, Tree::new(22));
        assert_ne!(root1, root2);
        assert_eq!(root1, Tree::new(22));

        let root1 = Tree::new(0);
        let mut node = root1.clone();
        for n in 1..700000 {
            node = Tree::push_child(&node, n);
        }

        let root2 = Tree::new(0);
        let mut node = root2.clone();
        for n in 1..700000 {
            node = Tree::push_child(&node, n);
        }
        assert_eq!(root1, root2);
    }

    #[test]
    fn push_child() {
        let root = Tree::new(0);
        let one = Tree::push_child(&root, 1);
        assert_eq!(root.borrow().to_string(), "(0 (1))");
        Tree::push_child(&root, 2);
        assert_eq!(root.borrow().to_string(), "(0 (1) (2))");
        Tree::push_child(&one, 3);
        assert_eq!(root.borrow().to_string(), "(0 (1 (3)) (2))");
    }

    // #[test]
    fn _clone_tree() {
        //     0
        //    / \
        //   1   4
        //  / \   \
        // 2   3   5
        let root = Tree::new(0);
        let one = Tree::push_child(&root, 1);
        let four = Tree::push_child(&root, 4);
        Tree::push_child(&one, 2);
        Tree::push_child(&one, 3);
        Tree::push_child(&four, 5);

        let new = Tree::clone_tree(&root);
        assert_eq!(root, new);
        assert_eq!(new.borrow().to_string(), "(0 (1 (2) (3)) (4 (5)))");
        Tree::replace_tree(&new, Tree::new(0));
        assert_ne!(root, new);

        //       0
        //    /  |  \
        //   1   4   6
        //  / \  |  /|\
        // 2   3 5 7 8 9
        let root = Tree::new(0);
        let first = Tree::push_child(&root, 1);
        let second = Tree::push_child(&root, 4);
        let third = Tree::push_child(&root, 6);
        Tree::push_child(&first, 2);
        Tree::push_child(&first, 3);
        Tree::push_child(&second, 5);
        Tree::push_child(&third, 7);
        Tree::push_child(&third, 8);
        Tree::push_child(&third, 9);

        let new = Tree::clone_tree(&root);
        assert_eq!(root, new);
        Tree::replace_tree(&new, Tree::new(0));
        assert_ne!(root, new);

        let root = Tree::new(0);
        let mut node = root.clone();
        for n in 1..700000 {
            node = Tree::push_child(&node, n);
        }

        let new = Tree::clone_tree(&root);
        assert_eq!(root, new);

        let mut node = root.clone();
        for n in 1..700000 {
            node = Tree::push_child(&node, -n);
        }

        let new = Tree::clone_tree(&root);
        assert_eq!(root, new);
    }
}
