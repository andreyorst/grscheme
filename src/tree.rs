use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

type Link<T> = Rc<RefCell<Tree<T>>>;

pub struct Tree<T>
where
    T: Display,
{
    data: T,
    root: Option<Link<T>>,
    parent: Option<Link<T>>,
    childs: Vec<Link<T>>,
}

impl<T> Tree<T>
where
    T: Display,
{
    pub fn root(data: T) -> Link<T> {
        let root = Rc::new(RefCell::new(Tree {
            data,
            root: None,
            parent: None,
            childs: vec![],
        }));
        root.borrow_mut().root = Some(root.clone());
        root
    }

    pub fn add_child(node: &Link<T>, data: T) -> Link<T> {
        let new_node = Rc::new(RefCell::new(Tree {
            data,
            root: node.borrow().root.clone(),
            parent: Some(node.clone()),
            childs: vec![],
        }));
        node.borrow_mut().childs.push(new_node.clone());
        new_node
    }

    pub fn adopt_node(node1: &Link<T>, node2: &Link<T>) -> Link<T> {
        node2.borrow_mut().parent = Some(node1.clone());
        node1.borrow_mut().childs.push(node2.clone());
        node2.clone()
    }

    pub fn print_tree(node: &Link<T>) {
        print!("(");
        Tree::print_tree_rec(node);
        println!(")");
    }

    fn print_tree_rec(node: &Link<T>) {
        print!("{}", node.borrow().data);
        for n in node.borrow().childs.iter() {
            print!("(");
            Tree::print_tree_rec(n);
            print!(")");
        }
    }
}
