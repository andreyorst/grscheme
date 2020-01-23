use std::cell::RefCell;
use std::rc::{Rc, Weak};

pub type NodePtr<T> = Rc<RefCell<Tree<T>>>;
pub type WeakNodePtr<T> = Weak<RefCell<Tree<T>>>;

#[derive(Debug)]
pub struct Tree<T> {
    pub data: T,
    pub parent: Option<WeakNodePtr<T>>,
    pub childs: Vec<NodePtr<T>>,
}

impl<T> Drop for Tree<T> {
    fn drop(&mut self) {
        let mut stack = std::mem::replace(&mut self.childs, Vec::new());
        while let Some(current) = stack.pop() {
            if Rc::strong_count(&current) == 1 {
                let mut current = current.borrow_mut();
                stack.extend_from_slice(&current.childs);
                current.childs.clear();
            }
        }
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
            childs: vec![],
        }))
    }

    pub fn add_child(node: &NodePtr<T>, data: T) -> NodePtr<T> {
        let new_node = Rc::from(RefCell::from(Tree {
            data,
            parent: Some(Rc::downgrade(node)),
            childs: vec![],
        }));
        node.borrow_mut().childs.push(new_node.clone());
        new_node
    }

    pub fn adopt_node(root: &NodePtr<T>, node: NodePtr<T>) {
        node.borrow_mut().parent = Some(Rc::downgrade(root));
        root.borrow_mut().childs.push(node);
    }

    pub fn replace_node(node1: &NodePtr<T>, node2: NodePtr<T>) {
        node1.borrow_mut().parent = Some(Rc::downgrade(&node2));
        node1.borrow_mut().data = node2.borrow().data.clone();
        node1.borrow_mut().childs.clear();
        for c in node2.borrow().childs.iter() {
            Tree::adopt_node(node1, c.clone());
        }
    }

    pub fn get_data(node: &NodePtr<T>) -> T {
        node.borrow().data.clone()
    }

    pub fn child_count(node: &NodePtr<T>) -> usize {
        node.borrow().childs.len()
    }

    pub fn clone_node(node: &NodePtr<T>) -> NodePtr<T> {
        let root = Tree::root(node.borrow().data.clone());
        root.borrow_mut().parent = node.borrow().parent.clone();
        for child in node.borrow().childs.iter() {
            Tree::adopt_node(&root, Self::clone_node(child));
        }
        root
    }

    pub fn get_parent(node: &NodePtr<T>) -> Option<NodePtr<T>> {
        Weak::upgrade(node.borrow().parent.as_ref()?)
    }

    pub fn set_parent(node: &NodePtr<T>, new_parent: Option<NodePtr<T>>) {
        match new_parent {
            Some(p) => node.borrow_mut().parent = Some(Rc::downgrade(&p)),
            _ => node.borrow_mut().parent = None,
        }
    }
}
