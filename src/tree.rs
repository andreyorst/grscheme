use std::cell::RefCell;
use std::rc::{Rc, Weak};

pub type NodePtr = Rc<RefCell<Tree>>;
pub type WeakNodePtr = Weak<RefCell<Tree>>;

pub struct Tree {
    pub data: String,
    pub extra_up: bool,
    pub parent: Option<WeakNodePtr>,
    pub childs: Vec<NodePtr>,
}

impl Tree {
    pub fn root(data: String) -> NodePtr {
        Rc::from(RefCell::from(Tree {
            data,
            extra_up: false,
            parent: None,
            childs: vec![],
        }))
    }

    pub fn add_child(node: &NodePtr, data: String) -> NodePtr {
        let new_node = Rc::from(RefCell::from(Tree {
            data,
            extra_up: false,
            parent: Some(Rc::downgrade(node)),
            childs: vec![],
        }));
        node.borrow_mut().childs.push(new_node.clone());
        new_node
    }

    pub fn clone_tree(node: &NodePtr) -> NodePtr {
        let root = Tree::root(node.borrow().data.clone());
        for child in node.borrow().childs.iter() {
            Self::adopt_node(&root, &Self::clone_tree(child));
        }
        root
    }

    pub fn _move_childs(to: &NodePtr, from: &NodePtr) {
        to.borrow_mut().childs.append(&mut from.borrow_mut().childs);
    }

    pub fn adopt_node(node1: &NodePtr, node2: &NodePtr) -> NodePtr {
        node2.borrow_mut().parent = Some(Rc::downgrade(node1));
        node1.borrow_mut().childs.push(node2.clone());
        node2.clone()
    }

    pub fn _print_tree(node: &NodePtr) {
        print!("(");
        Self::_print_tree_rec(node);
        println!(")");
    }

    fn _print_tree_rec(node: &NodePtr) {
        print!("{}", node.borrow().data);
        for n in node.borrow().childs.iter() {
            print!("(");
            Self::_print_tree_rec(n);
            print!(")");
        }
    }

    pub fn _remove_last_child(node: &NodePtr) {
        if let Some(c) = node.borrow_mut().childs.pop() {
            Self::_remove_node(&c);
        }
    }

    pub fn _remove_node(node: &NodePtr) {
        for n in node.borrow().childs.iter() {
            Self::_remove_node(n);
        }
        node.borrow_mut().childs.clear()
    }

    pub fn get_parent(node: &NodePtr) -> Option<NodePtr> {
        match &node.borrow().parent {
            Some(p) => Weak::upgrade(&p),
            None => None,
        }
    }
}
