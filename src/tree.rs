use std::cell::RefCell;
use std::rc::Rc;

pub type NodePtr = Rc<RefCell<Tree>>;

pub struct Tree {
    pub data: String,
    pub root: Option<NodePtr>,
    pub parent: Option<NodePtr>,
    pub childs: Vec<NodePtr>,
}

impl Tree {
    pub fn root(data: String) -> NodePtr {
        let root = Rc::new(RefCell::new(Tree {
            data,
            root: None,
            parent: None,
            childs: vec![],
        }));
        root.borrow_mut().root = Some(root.clone());
        root
    }

    pub fn add_child(node: &NodePtr, data: String) -> NodePtr {
        let new_node = Rc::new(RefCell::new(Tree {
            data,
            root: node.borrow().root.clone(),
            parent: Some(node.clone()),
            childs: vec![],
        }));
        node.borrow_mut().childs.push(new_node.clone());
        new_node
    }

    pub fn _adopt_node(node1: &NodePtr, node2: &NodePtr) -> NodePtr {
        node2.borrow_mut().parent = Some(node1.clone());
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
}
