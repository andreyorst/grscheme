use std::collections::HashMap;
use std::io;
use std::io::Write;

fn main() {
    repl();
}

struct Id {
    _name: String,
    data: String,
    procedure: bool,
    _arguments: Vec<String>,
}

impl Id {
    fn _new() -> Id {
        Id {
            _name: String::new(),
            data: String::new(),
            procedure: false,
            _arguments: vec![],
        }
    }
    fn variable(name: String, data: String) -> Id {
        Id {
            _name: name,
            data,
            procedure: false,
            _arguments: vec![],
        }
    }
}

struct Env {
    stack: Vec<String>,
    global_ids: HashMap<String, Id>,
}

impl Env {
    fn new() -> Env {
        Env {
            stack: Vec::new(),
            global_ids: HashMap::new(),
        }
    }

    fn eval(&mut self, program: String) -> String {
        self.parse(program)
    }

    fn parse(&mut self, program: String) -> String {
        let mut in_comment = false;
        let mut in_word = false;
        let mut word = String::new();

        for c in program.chars() {
            if !in_comment {
                match c {
                    '(' => {
                        in_word = false;
                        self.stack.push(c.to_string());
                    }
                    ')' => {
                        if word.len() > 0 {
                            self.stack.push(word.clone());
                            word.clear();
                        }
                        self.reduce_expr();
                        in_word = false;
                    }
                    ' ' | '\t' | '\n' => in_word = false,
                    ';' => in_comment = true,
                    _ => in_word = true,
                }
                if in_word {
                    word.push(c);
                } else {
                    if word.len() > 0 {
                        self.stack.push(word.clone());
                        word.clear();
                    }
                }
            } else if c == '\n' {
                in_comment = false;
            }
        }
        self.stack.pop().unwrap()
    }

    fn get_id_value(&mut self, id: &String) -> String {
        if id.trim().parse::<i32>().is_ok() {
            return id.clone()
        } else if self.global_ids.contains_key(id) {
            let id = self.global_ids.get(id).unwrap().data.clone();
            return self.get_id_value(&id)
        } else {
            panic!("unknown symbol");
        }
    }

    fn reduce_expr(&mut self) {
        let mut operands: Vec<String> = Vec::new();
        let mut res: String = String::new();
        loop {
            let item = self.stack.pop().expect("empty stack");
            match item.as_ref() {
                "(" => break,
                &_ => operands.push(item.clone()),
            }
        }
        let op = operands.pop().expect("empty stack");
        match op.as_ref() {
            "define" => res = self.define(&mut operands),
            "+" | "-" | "*" | "/" => {
                for operand in operands.iter_mut() {
                    *operand = self.get_id_value(&operand);
                }
                res = Env::math(&mut operands, &op);
            }
            &_ => {
                if self.global_ids.contains_key(&op) && self.global_ids.get(&op).unwrap().procedure
                {
                    println!("call '{}", op);
                } else {
                    panic!("'{}' not defined as procedure", op);
                }
            }
        }
        self.stack.push(res);
    }

    fn define(&mut self, operands: &mut Vec<String>) -> String {
        let name = operands.pop().expect("not enough arguments to define");
        let data = operands.pop().expect("not enough arguments to define");

        self.global_ids.insert(name.clone(), Id::variable(name.clone(), data.clone()));
        name.clone()
    }

    fn math(operands: &mut Vec<String>, op: &String) -> String {
        let mut res: i32 = operands
            .pop()
            .expect("not enough arguments")
            .trim()
            .parse()
            .expect("not a number");
        loop {
            let val = operands.pop();
            if val.is_some() {
                let val: i32 = val.unwrap().trim().parse().expect("not a number");
                match op.as_ref() {
                    "+" => res += val,
                    "-" => res -= val,
                    "*" => res *= val,
                    "/" => {
                        if val != 0 {
                            res /= val;
                        } else {
                            panic!("division by zero");
                        }
                    }
                    &_ => panic!("wrong operator"),
                }
            } else {
                break;
            }
        }
        res.to_string()
    }

    fn read_balanced_input() -> String {
        let mut paren_count = 0;
        let mut bracket_count = 0;
        let mut curly_count = 0;
        let mut escaped = false;
        let mut inside_string = false;
        let mut comment = false;
        let mut line_without_comments = String::new();
        let mut line = String::new();
        let mut program = String::new();

        print!("> ");
        io::stdout().flush().ok();

        loop {
            io::stdin().read_line(&mut line).unwrap_or_default();
            for c in line.chars() {
                if !escaped && !inside_string && !comment {
                    match c {
                        '(' => paren_count += 1,
                        ')' => paren_count -= 1,
                        '[' => bracket_count += 1,
                        ']' => bracket_count -= 1,
                        '{' => curly_count += 1,
                        '}' => curly_count -= 1,
                        '"' => inside_string = true,
                        '\\' => escaped = true,
                        ';' => comment = true,
                        _ => (),
                    }
                } else if escaped {
                    escaped = false;
                } else if inside_string && c == '"' {
                    inside_string = false;
                }

                if !comment {
                    line_without_comments.push(c);
                }
            }
            comment = false;
            if line_without_comments.len() != 0 {
                program = format!("{}{}", program, line_without_comments);
                line_without_comments.clear();
            }
            line.clear();
            if paren_count == 0 && curly_count == 0 && bracket_count == 0 && !inside_string {
                break;
            }
        }
        program
    }
}

fn repl() {
    let mut env = Env::new();
    loop {
        println!("{}", env.eval(Env::read_balanced_input()));
    }
}
