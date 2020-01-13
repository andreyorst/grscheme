use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

pub type NodePtr = Rc<RefCell<Tree>>;
pub type WeakNodePtr = Weak<RefCell<Tree>>;

#[derive(Debug)]
pub struct Tree {
    pub data: String,
    pub extra_up: bool,
    pub scope: HashMap<String, NodePtr>,
    pub parent: Option<WeakNodePtr>,
    pub childs: Vec<NodePtr>,
}

impl Drop for Tree {
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

impl Tree {
    pub fn root(data: String) -> NodePtr {
        Rc::from(RefCell::from(Tree {
            data,
            extra_up: false,
            scope: HashMap::new(),
            parent: None,
            childs: vec![],
        }))
    }

    pub fn add_child(node: &NodePtr, data: String) -> NodePtr {
        let new_node = Rc::from(RefCell::from(Tree {
            data,
            extra_up: false,
            scope: HashMap::new(),
            parent: Some(Rc::downgrade(node)),
            childs: vec![],
        }));
        node.borrow_mut().childs.push(new_node.clone());
        new_node
    }

    pub fn adopt_node(root: &NodePtr, node: NodePtr) {
        node.borrow_mut().parent = Some(Rc::downgrade(root));
        root.borrow_mut().childs.push(node);
    }

    pub fn replace_node(node1: &NodePtr, node2: NodePtr) {
        node1.borrow_mut().parent = Some(Rc::downgrade(&node2));
        node1.borrow_mut().data = node2.borrow().data.clone();
        node1.borrow_mut().extra_up = node2.borrow().extra_up;
        node1.borrow_mut().childs.clear();
        node1.borrow_mut().scope = node2.borrow().scope.clone();
        for c in node2.borrow().childs.iter() {
            Tree::adopt_node(node1, c.clone());
        }
    }

    #[allow(dead_code)]
    pub fn print_tree(node: &NodePtr) {
        println!("{}", Self::tree_to_string(node));
    }

    pub fn tree_to_string(node: &NodePtr) -> String {
        let mut string = String::from("(");
        Self::build_string(node, &mut string);
        string.push_str(")");
        string
    }

    fn build_string(node: &NodePtr, string: &mut String) {
        let data = match node.borrow().data.as_ref() {
            "(" => "op_paren".to_owned(),
            ")" => "cl_paren".to_owned(),
            _ => node.borrow().data.clone(),
        };
        string.push_str(&data);
        for n in node.borrow().childs.iter() {
            string.push_str("(");
            Self::build_string(n, string);
            string.push_str(")");
        }
    }

    pub fn get_data(node: &NodePtr) -> String {
        node.borrow().data.clone()
    }

    pub fn child_count(node: &NodePtr) -> usize {
        node.borrow().childs.len()
    }

    pub fn clone_node(node: &NodePtr) -> NodePtr {
        let root = Tree::root(node.borrow().data.clone());
        root.borrow_mut().parent = node.borrow().parent.clone();
        root.borrow_mut().scope = node.borrow().scope.clone();
        root.borrow_mut().extra_up = node.borrow().extra_up;
        for child in node.borrow().childs.iter() {
            Tree::adopt_node(&root, Self::clone_node(child));
        }
        root
    }

    pub fn get_parent(node: &NodePtr) -> Option<NodePtr> {
        Weak::upgrade(node.borrow().parent.as_ref()?)
    }

    pub fn set_parent(node: &NodePtr, new_parent: Option<NodePtr>) {

        node.borrow_mut().parent = Rc::downgrade(new_parent);
    }
}
