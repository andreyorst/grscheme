use std::cell::RefCell;
use std::rc::{Rc, Weak};

pub type NodePtr<T> = Rc<RefCell<Tree<T>>>;
pub type WeakNodePtr<T> = Weak<RefCell<Tree<T>>>;

#[derive(Debug)]
pub struct Tree<T> {
    pub data: T,
    pub parent: Option<WeakNodePtr<T>>,
    pub nodes: Vec<NodePtr<T>>,
}

impl<T> Drop for Tree<T> {
    fn drop(&mut self) {
        let mut stack = std::mem::replace(&mut self.nodes, Vec::new());
        while let Some(current) = stack.pop() {
            if Rc::strong_count(&current) == 1 {
                let mut current = current.borrow_mut();
                stack.extend_from_slice(&current.nodes);
                current.nodes.clear();
            }
        }
    }
}

impl<T> ToString for Tree<T>
where
    T: ToString + std::fmt::Debug,
{
    /// Converts tree to s-expression string.
    ///
    /// Each node will be wrapped in parentheses, with it's value
    /// going first, and subnodes afterwards.
    ///
    /// # Examples
    ///
    /// ```
    /// let root = Tree::root(0);
    /// Tree::add_node(&root, 1);
    /// Tree::add_node(&root, 2);
    ///
    /// assert_eq!(&root.borrow().to_string(), "(0 (1) (2))");
    /// ```
    fn to_string(&self) -> String {
        let mut string = format!("({}", self.data.to_string());

        if !self.nodes.is_empty() {
            let mut stack = vec![];
            let mut depth = 1;
            stack.extend_from_slice(&self.nodes);
            stack.reverse();

            while let Some(current) = stack.pop() {
                let mut pushed = false;

                string.push_str(&format!(" ({}", current.borrow().data.to_string()));
                depth += 1;

                for node in current.borrow().nodes.iter() {
                    if !node.borrow().nodes.is_empty() {
                        stack.push(node.clone());
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
        string
    }
}

impl<T> PartialEq for Tree<T>
where
    T: PartialEq + std::fmt::Debug,
{
    /// Compare trees.
    ///
    /// Compares tree values and all subtrees from one tree with another.
    ///
    /// # Examples
    ///
    /// ```
    /// let rooot1 = Tree::root(0);
    /// Tree::add_node(&root1, 1);
    /// Tree::add_node(&root1, 2);
    ///
    /// let rooot2 = Tree::root(0);
    /// Tree::add_node(&root2, 1);
    /// Tree::add_node(&root2, 2);
    ///
    /// assert_eq!(root1, root2);
    ///
    /// Tree::add_node(&root2, 3);
    /// assert_ne!(root1, root2);
    /// ```
    fn eq(&self, other: &Tree<T>) -> bool {
        let mut stacked_self = vec![];
        let mut stacked_other = vec![];
        let mut stack = self.nodes.to_vec();
        while let Some(current) = stack.pop() {
            stacked_self.extend(current.borrow().nodes.iter().cloned());
            stack.extend_from_slice(&current.borrow().nodes);
        }
        let mut stack = other.nodes.to_vec();
        while let Some(current) = stack.pop() {
            stacked_other.extend(current.borrow().nodes.iter().cloned());
            stack.extend_from_slice(&current.borrow().nodes);
        }
        self.data == other.data && stacked_self == stacked_other
    }
}

impl<T> Tree<T>
where
    T: Clone,
{
    pub fn root(data: T) -> NodePtr<T> {
        Rc::from(RefCell::from(Tree {
            data,
            parent: None,
            nodes: vec![],
        }))
    }

    pub fn add_node(node: &NodePtr<T>, data: T) -> NodePtr<T> {
        let new_node = Rc::from(RefCell::from(Tree {
            data,
            parent: Some(Rc::downgrade(node)),
            nodes: vec![],
        }));
        node.borrow_mut().nodes.push(new_node.clone());
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
    /// let root = Tree::root(0);
    /// let one = Tree::root(1);
    ///
    /// Tree::adopt_node(&root, one);
    ///
    /// assert_eq!(root, Tree::get_parent(&Tree::add_node(&Tree::root(0), 1)).unwrap());
    /// ```
    pub fn adopt_node(root: &NodePtr<T>, node: NodePtr<T>) {
        node.borrow_mut().parent = Some(Rc::downgrade(root));
        root.borrow_mut().nodes.push(node);
    }

    /// Replaces one node with another node.
    ///
    /// Keeps current pointer and parent, updates node's data and
    /// list of subnodes from the other node.
    ///
    /// # Examples
    ///
    /// ```
    /// let root = Tree::root(22);
    /// let forty_two = Tree::add_node(&root, 43);
    /// Tree::replace_node(&forty_two, Tree::root(42));
    ///
    /// assert_eq!(root, Tree::get_parent(&Tree::add_node(&Tree::root(22), 42)).unwrap());
    /// ```
    pub fn replace_node(node1: &NodePtr<T>, node2: NodePtr<T>) {
        node1.borrow_mut().data = node2.borrow().data.clone();
        node1.borrow_mut().nodes.clear();
        for c in node2.borrow().nodes.iter() {
            Tree::adopt_node(node1, c.clone());
        }
    }

    pub fn get_data(node: &NodePtr<T>) -> T {
        node.borrow().data.clone()
    }

    pub fn node_count(node: &NodePtr<T>) -> usize {
        node.borrow().nodes.len()
    }

    pub fn clone_node(node: &NodePtr<T>) -> NodePtr<T> {
        let root = Tree::root(node.borrow().data.clone());
        root.borrow_mut().parent = node.borrow().parent.clone();
        for child in node.borrow().nodes.iter() {
            Tree::adopt_node(&root, Self::clone_node(child));
        }
        root
    }

    pub fn get_parent(node: &NodePtr<T>) -> Option<NodePtr<T>> {
        Weak::upgrade(node.borrow().parent.as_ref()?)
    }

    pub fn set_parent(node: &NodePtr<T>, new_parent: Option<NodePtr<T>>) {
        node.borrow_mut().parent = match new_parent {
            Some(p) => Some(Rc::downgrade(&p)),
            None => None,
        }
    }
}
#[cfg(test)]
mod tests {
    use super::{NodePtr, Tree};
    #[test]
    fn compare() {
        //   0     0
        //  / \   / \
        // 1   2 1   2
        let root1 = Tree::root(0);
        Tree::add_node(&root1, 1);
        Tree::add_node(&root1, 2);
        let root2 = Tree::root(0);
        Tree::add_node(&root2, 1);
        Tree::add_node(&root2, 2);
        assert_eq!(root1, root2);


        //     0         0
        //    / \       / \
        //   1   4     1   4
        //  / \   \   / \   \
        // 2   3   5 2   3   5
        let root1 = Tree::root(0);
        let one1 = Tree::add_node(&root1, 1);
        let four1 = Tree::add_node(&root1, 4);
        let two1 = Tree::add_node(&one1, 2);
        let three1 = Tree::add_node(&one1, 3);
        let five1 = Tree::add_node(&four1, 5);

        let root2 = Tree::root(0);
        let one2 = Tree::add_node(&root2, 1);
        let four2 = Tree::add_node(&root2, 4);
        let two2 = Tree::add_node(&one2, 2);
        let three2 = Tree::add_node(&one2, 3);
        let five2 = Tree::add_node(&four2, 5);
        assert_eq!(root1, root2);

        // changing various nodes
        Tree::replace_node(&five2, Tree::root(6));
        assert_ne!(root1, root2);
        Tree::replace_node(&five2, Tree::root(5));
        assert_eq!(root1, root2);

        Tree::replace_node(&three2, Tree::root(9));
        assert_ne!(root1, root2);
        Tree::replace_node(&three2, Tree::root(3));
        assert_eq!(root1, root2);

        Tree::replace_node(&two1, Tree::root(-1));
        assert_ne!(root1, root2);
        Tree::replace_node(&two1, Tree::root(5));
        Tree::replace_node(&two2, Tree::root(5));
        assert_eq!(root1, root2);

        Tree::replace_node(&two1, Tree::root(2));
        Tree::replace_node(&two2, Tree::root(2));
        Tree::replace_node(&three1, Tree::root(2));
        Tree::replace_node(&three2, Tree::root(7));
        assert_ne!(root1, root2);
        Tree::replace_node(&three1, Tree::root(3));
        Tree::replace_node(&three2, Tree::root(3));
        assert_eq!(root1, root2);

        Tree::replace_node(&one1, Tree::root(0));
        assert_ne!(root1, root2);
        Tree::replace_node(&one2, Tree::root(0));
        assert_eq!(root1, root2);

        Tree::replace_node(&five1, Tree::root(0));
        assert_ne!(root1, root2);
        Tree::replace_node(&five2, Tree::root(0));
        assert_eq!(root1, root2);

        Tree::replace_node(&root1, Tree::root(22));
        assert_ne!(root1, root2);
        assert_eq!(root1, Tree::root(22));

        // let root1 = Tree::root(0);
        // let mut node = root1.clone();
        // for n in 1..70000 {
        //     node = Tree::add_node(&node, n);
        // }
        // let root2 = Tree::root(0);
        // let mut node = root2.clone();
        // for n in 1..70000 {
        //     node = Tree::add_node(&node, n);
        // }
        // assert_eq!(root1, root2);
    }
}
