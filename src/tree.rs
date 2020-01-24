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

impl<T> ToString for Tree<T> {
    fn to_string(&self) -> String {
        "".to_owned()
    }
}

impl<T> PartialEq for Tree<T>
where
    T: PartialEq + std::fmt::Debug,
{
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
