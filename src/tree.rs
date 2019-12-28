use std::cell::RefCell;
use std::rc::{Rc, Weak};

pub type NodePtr = Rc<RefCell<Tree>>;
pub type WeakNodePtr = Weak<RefCell<Tree>>;

pub struct Tree {
    pub data: String,
    pub parent: Option<WeakNodePtr>,
    pub childs: Vec<NodePtr>,
}

impl Tree {
    pub fn root(data: String) -> NodePtr {
        Rc::from(RefCell::from(Tree {
            data,
            parent: None,
            childs: vec![],
        }))
    }

    pub fn add_child(node: &NodePtr, data: String) -> NodePtr {
        let new_node = Rc::from(RefCell::from(Tree {
            data,
            parent: Some(Rc::downgrade(node)),
            childs: vec![],
        }));
        node.borrow_mut().childs.push(new_node.clone());
        new_node
    }

    pub fn _adopt_node(node1: &NodePtr, node2: &NodePtr) -> NodePtr {
        node2.borrow_mut().parent = Some(Rc::downgrade(node1));
        node1.borrow_mut().childs.push(node2.clone());
        node2.clone()
    }

    pub fn print_tree(node: &NodePtr) {
        print!("(");
        Tree::print_tree_rec(node);
        println!(")");
    }

    fn print_tree_rec(node: &NodePtr) {
        print!("{}", node.borrow().data);
        for n in node.borrow().childs.iter() {
            print!("(");
            Tree::print_tree_rec(n);
            print!(")");
        }
    }

    pub fn remove_last_child(node: &NodePtr) {
        if let Some(c) = node.borrow_mut().childs.pop() {
            Tree::remove_node(&c);
        }
    }

    pub fn remove_node(node: &NodePtr) {
        for n in node.borrow().childs.iter() {
            Tree::remove_node(n);
        }
        node.borrow_mut().childs.clear()
    }
}
