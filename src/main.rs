use std::collections::HashMap;
use std::io;
use std::io::Write;

fn main() {
    repl();
}

#[derive(Debug)]
struct Id {
    _name: String,
    data: String,
}

impl Id {
    fn variable(name: String, data: String) -> Id {
        Id { _name: name, data }
    }
}

#[derive(Debug)]
enum Token {
    Define,
    Lambda { inside: bool },
    IfElse,
    List,
    Symbol,
    Other,
}

#[derive(Debug)]
struct Interpreter {
    program: String,
    stack: Vec<String>,
    scope_stack: Vec<HashMap<String, Id>>,
    token_stack: Vec<Token>,
    eval: bool,
    total_unmatched_paren_count: u32,
    inside_string: bool,
    inside_word: bool,
    current_word: String,
}

impl Interpreter {
    fn new() -> Interpreter {
        Interpreter {
            program: String::new(),
            stack: vec![],
            scope_stack: vec![HashMap::new()],
            token_stack: Vec::new(),
            eval: true,
            total_unmatched_paren_count: 0,
            inside_string: false,
            inside_word: false,
            current_word: String::new(),
        }
    }

    fn eval(&mut self) -> String {
        let mut comment = false;

        // TODO: Remove this clone and use program from self
        let program = self.program.clone();

        for c in program.chars() {
            if !comment {
                match c {
                    '(' => {
                        self.current_word.push(c);
                        self.stack_push_word();
                    }
                    ')' => {
                        if self.current_word.len() > 0 {
                            self.stack_push_word();
                        }
                        self.current_word = c.to_string();
                        self.stack_push_word();
                        self.parse_paren();
                    }
                    '\'' => self.parse_single_quote(),
                    ' ' | '\t' | '\n' => self.inside_word = false,
                    ';' => {
                        comment = true;
                        continue;
                    }
                    _ => self.inside_word = true,
                }
                if self.inside_word {
                    self.current_word.push(c);
                } else if self.current_word.len() > 0 {
                    self.stack_push_word();
                }
            } else if c == '\n' {
                comment = false;
                self.inside_word = false;
            }
        }
        let res = self.stack.pop();
        match res {
            Some(s) => s,
            None => "".to_owned(),
        }
    }

    fn stack_push_word(&mut self) {
        let word = self.current_word.clone();
        self.tokenize(&word);
        self.stack.push(word);
        self.inside_word = false;
        self.current_word.clear();
    }

    fn tokenize(&mut self, item: &str) {
        let mut quote = false;
        for c in item.chars() {
            match c {
                '\'' => quote = true,
                '(' | ')' => break,
                _ => {
                    if quote {
                        self.token_stack.push(Token::Symbol);
                        return;
                    }
                    break;
                }
            }
        }
        match item {
            "define" => self.token_stack.push(Token::Define),
            "'(" => self.token_stack.push(Token::List),
            "lambda" => self.token_stack.push(Token::Lambda { inside: false }),
            "if" => self.token_stack.push(Token::IfElse),
            &_ => (),
        }
    }

    fn parse_single_quote(&mut self) {
        self.eval = false;
        if self.inside_word {
            panic!("quote is not allowed as word character");
        }
        self.inside_word = true;
    }

    fn parse_paren(&mut self) {
        self.inside_word = false;
        match self.token_stack.last().unwrap_or(&Token::Other) {
            Token::Lambda { inside: _ } => self.reduce_lambda(),
            Token::Define => self.reduce_define(),
            Token::IfElse => self.reduce_ifelse(),
            Token::List => self.reduce_list(),
            Token::Symbol => (),
            _ => self.reduce_expr(),
        }
    }

    fn reduce_list(&mut self) {
        self.token_stack.pop();
        let mut list_values: Vec<String> = Vec::new();
        loop {
            let item = self.stack.pop().expect("empty_stack 0");
            match item.as_ref() {
                ")" => (),
                "'(" => break,
                _ => list_values.push(item),
            }
        }
        let list_values: String = list_values
            .iter()
            .rev()
            .map(|s| s.clone())
            .collect::<Vec<String>>()
            .join(" ");
        let mut list = String::from("'(");
        list.push_str(&list_values);
        list.push(')');
        self.stack.push(list);
    }

    fn reduce_define(&mut self) {
        self.token_stack.pop();
        let mut operands: Vec<String> = Vec::new();
        loop {
            let item = self.stack.pop().expect("empty stack 2");
            match item.as_ref() {
                ")" | "define" => (),
                "(" => break,
                &_ => operands.push(item.clone()),
            }
        }
        if operands.len() != 2 {
            panic!("wrong amount of arguments to define: {}", operands.len());
        }
        let name = operands.pop().unwrap();
        let data = operands.pop().unwrap();
        let data = self.get_id_value(&data);
        self.scope_stack
            .last_mut()
            .expect("empty scope stack")
            .insert(name.clone(), Id::variable(name.clone(), data.clone()));
    }

    fn reduce_lambda(&mut self) {}

    fn reduce_ifelse(&mut self) {}

    fn get_id_value(&mut self, id: &String) -> String {
        if id.trim().parse::<i32>().is_ok() {
            return id.clone();
        } else if id.chars().next().unwrap() == '\'' {
            return id.clone();
        }
        match self.look_up_id(id) {
            Some(id) => id.data.clone(),
            None => panic!("unknown symbol: {}", id),
        }
    }

    fn look_up_id(&mut self, id: &str) -> Option<&Id> {
        for scope in self.scope_stack.iter().rev() {
            if scope.contains_key(id) {
                return scope.get(id);
            }
        }
        None
    }

    fn reduce_expr(&mut self) {
        let mut operands: Vec<String> = Vec::new();
        let mut res: String = String::new();
        loop {
            let item = self.stack.pop().expect("empty stack 3");
            match item.as_ref() {
                ")" => (),
                "(" => break,
                &_ => operands.push(item.clone()),
            }
        }
        let op = operands.pop().expect("empty stack 4");
        match op.as_ref() {
            "+" | "-" | "*" | "/" => {
                for operand in operands.iter_mut() {
                    *operand = self.get_id_value(&operand);
                }
                res = Interpreter::eval_math(&mut operands, &op);
            }
            "=" | "<" | "<=" | ">" | ">=" => {
                for operand in operands.iter_mut() {
                    *operand = self.get_id_value(&operand);
                }
                res = Interpreter::eval_cmp(&mut operands, &op);
            }
            &_ => {}
        }
        if res.len() > 0 {
            self.stack.push(res);
        }
    }

    fn eval_math(operands: &mut Vec<String>, op: &String) -> String {
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

    fn eval_cmp(operands: &mut Vec<String>, op: &String) -> String {
        let mut res = false;
        let mut left: i32 = operands
            .pop()
            .expect("not enough arguments")
            .trim()
            .parse()
            .expect("not a number");
        loop {
            let right = operands.pop();
            if right.is_some() {
                let right: i32 = right.unwrap().trim().parse().expect("not a number");
                res = match op.as_ref() {
                    "=" => left == right,
                    "<" => left < right,
                    "<=" => left <= right,
                    ">" => left > right,
                    ">=" => left >= right,
                    &_ => panic!("wrong operator"),
                };
                left = right;
            } else {
                break;
            }
        }

        if res { "#t" } else { "#f" }.to_string()
    }

    fn read_balanced_input(&mut self) {
        let mut paren_count = 0;
        let mut bracket_count = 0;
        let mut curly_count = 0;
        let mut escaped = false;
        let mut inside_string = false;
        let mut comment = false;
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
                        ';' => {
                            comment = true;
                            continue;
                        }
                        _ => (),
                    }
                } else if escaped {
                    escaped = false;
                } else if inside_string && c == '"' {
                    inside_string = false;
                }
            }
            comment = false;
            if line.len() != 0 {
                program = format!("{}{}", program, line);
                line.clear();
            }
            if paren_count == 0 && curly_count == 0 && bracket_count == 0 && !inside_string {
                break;
            }
        }
        self.program = program;
    }
}

fn repl() {
    let mut interpreter = Interpreter::new();
    loop {
        interpreter.read_balanced_input();
        let res = interpreter.eval();
        if res.len() > 0 {
            println!("{}", res);
        }
    }
}
