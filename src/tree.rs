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

    #[allow(dead_code)]
    pub fn move_childs(to: &NodePtr, from: &NodePtr) {
        to.borrow_mut().childs.append(&mut from.borrow_mut().childs);
    }

    pub fn adopt_node(root: &NodePtr, node: &NodePtr) -> NodePtr {
        node.borrow_mut().parent = Some(Rc::downgrade(root));
        root.borrow_mut().childs.push(node.clone());
        node.clone()
    }

    #[allow(dead_code)]
    pub fn print_tree(node: &NodePtr) {
        println!("{}", Self::tree_to_string(node));
    }

    pub fn tree_to_string(node: &NodePtr) -> String {
        let mut vec = vec!["(".to_owned()];
        Self::tree_to_vec(node, &mut vec);
        vec.push(")".to_owned());
        vec.join("")
    }
    fn tree_to_vec(node: &NodePtr, vec: &mut Vec<String>) {
        vec.push(match node.borrow().data.as_ref() {
            "(" => "op_paren".to_owned(),
            ")" => "cl_paren".to_owned(),
            _ => node.borrow().data.clone(),
        });
        for n in node.borrow().childs.iter() {
            vec.push("(".to_owned());
            Self::tree_to_vec(n, vec);
            vec.push(")".to_owned());
        }
    }

    #[allow(dead_code)]
    pub fn remove_node(node: &NodePtr) {
        for n in node.borrow().childs.iter() {
            Self::remove_node(n);
        }
        node.borrow_mut().childs.clear()
    }

    pub fn get_parent(node: &NodePtr) -> Option<NodePtr> {
        Weak::upgrade(node.borrow().parent.as_ref()?)
    }
}
