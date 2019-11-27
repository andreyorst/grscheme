use std::collections::HashMap;
use std::io;
use std::io::Write;

fn main() {
    Interpreter::repl();
}

#[derive(Debug, PartialEq, Clone)]
enum Token {
    Procedure,
    Id,
    Lambda { paren_count: u32 },
    Define,
    Eval,
    Apply,
    Quote,
    List { paren_count: u32 },
    Symbol,
}

#[derive(Debug)]
struct Interpreter {
    stack: Vec<StackItem>,
    scope_stack: Vec<HashMap<String, String>>,
}

#[derive(Debug)]
struct StackItem {
    token: Token,
    data: String,
}

impl StackItem {
    fn from(token: Token, data: &str) -> StackItem {
        StackItem {
            token,
            data: String::from(data),
        }
    }
}

impl Interpreter {
    fn new() -> Interpreter {
        Interpreter {
            stack: vec![],
            scope_stack: vec![HashMap::new()],
        }
    }

    fn parse(&mut self, program: &String) -> Option<String> {
        let mut comment = false;
        let mut inside_word;
        let mut word = String::new();

        for c in program.chars() {
            if !comment {
                match c {
                    '(' => {
                        self.stack_push(&c.to_string());
                        continue;
                    }
                    ')' => {
                        if word.len() > 0 {
                            self.stack_push(&word);
                            word.clear();
                        }
                        self.stack_push(&c.to_string());
                        continue;
                    }
                    '\'' => {
                        self.stack_push(&c.to_string());
                        continue;
                    }
                    ' ' | '\t' | '\n' => inside_word = false,
                    ';' => {
                        comment = true;
                        continue;
                    }
                    _ => inside_word = true,
                }
                if inside_word {
                    word.push(c);
                } else if word.len() > 0 {
                    self.stack_push(&word);
                    word.clear();
                }
            } else if c == '\n' {
                comment = false;
            }
        }

        match self.stack.pop() {
            Some(s) => Some(s.data),
            None => None,
        }
    }

    fn stack_push(&mut self, item: &str) {
        let token = self.tokenize(&item);
        self.stack.push(StackItem::from(token, &item));
        println!("{:?}", self.stack.last());
    }

    fn tokenize(&self, word: &str) -> Token {
        let last_token = match self.stack.last() {
            Some(x) => &x.token,
            None => &Token::Id,
        };

        match word {
            "(" => match last_token {
                Token::Quote => Token::List { paren_count: 0 },
                Token::List { paren_count } => Token::List {
                    paren_count: paren_count + 1,
                },
                Token::Lambda { paren_count } => Token::Lambda {
                    paren_count: paren_count + 1,
                },
                _ => Token::Eval,
            },
            ")" => match last_token {
                Token::List { paren_count } => {
                    if *paren_count == 0 {
                        Token::Apply
                    } else {
                        Token::List {
                            paren_count: paren_count - 1,
                        }
                    }
                }
                Token::Lambda { paren_count } => {
                    if *paren_count == 0 {
                        Token::Apply
                    } else {
                        Token::Lambda {
                            paren_count: paren_count - 1,
                        }
                    }
                }

                _ => Token::Apply,
            },
            "define" => Token::Define,
            "lambda" => Token::Lambda { paren_count: 0 },
            "'" | "quote" => Token::Quote,
            &_ => match last_token {
                Token::Eval => Token::Procedure,
                Token::Procedure => Token::Id,
                Token::Quote => Token::Symbol,
                _ => last_token.clone(),
            },
        }
    }

    fn _look_up_id(&mut self, id: &str) -> Option<&String> {
        // if id.trim().parse::<i32>().is_ok() {
        //     Some(id.to_owned())
        // }
        for scope in self.scope_stack.iter().rev() {
            if scope.contains_key(id) {
                return scope.get(id);
            }
        }
        None
    }

    fn read_balanced_input() -> String {
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
        program
    }

    fn repl() {
        let mut interpreter = Interpreter::new();
        loop {
            let program = Interpreter::read_balanced_input();
            if program.len() > 0 {
                match interpreter.parse(&program) {
                    Some(res) => println!("{}", res),
                    None => break,
                }
            } else {
                println!();
                break;
            }
        }
    }

}
