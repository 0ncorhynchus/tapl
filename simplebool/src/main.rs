use simplebool::parser::{Parser, Token, Tokenizer};
use simplebool::{Context, Term, Type};
use std::collections::HashMap;
use std::io::{self, Write};

fn print(term: &Term, ctx: &Context) -> String {
    match term {
        Term::Var(idx) => ctx.get_name(*idx).expect("Invalid Index"),
        Term::Abs(name, ty, t) => format!(
            "λ{}:{}. {}",
            name,
            ty,
            print(t, &ctx.add(name.clone(), ty.clone()))
        ),
        Term::App(t1, t2) => format!("({} {})", print(t1, ctx), print(t2, ctx),),
        Term::True => format!("true"),
        Term::False => format!("false"),
        Term::If(cond, jump1, jump2) => format!(
            "if {} then {} else {}",
            print(cond, ctx),
            print(jump1, ctx),
            print(jump2, ctx),
        ),
    }
}

struct Environment {
    primitives: HashMap<String, (Type, Term)>,
}

impl Environment {
    fn new() -> Self {
        Self {
            primitives: HashMap::new(),
        }
    }

    #[allow(dead_code)]
    fn register(&mut self, name: &str, ty: Type, value: Term) {
        self.primitives.insert(name.to_string(), (ty, value));
    }

    fn eval_and_print(&self, tokens: Vec<Token>) {
        let context = Context::new(
            self.primitives
                .iter()
                .map(|(n, (ty, t))| (n.clone(), ty.clone(), t.clone()))
                .collect(),
        );

        let mut parser = Parser::new(tokens.into_iter());
        match parser.parse(&context) {
            Ok(term) => {
                let mut term = term;
                for (name, (ty, value)) in &self.primitives {
                    term = Box::new(Term::App(
                        Box::new(Term::Abs(name.clone(), ty.clone(), term)),
                        Box::new(value.clone()),
                    ))
                }
                println!("{}", print(&term.eval(), &context));
            }
            Err(err) => {
                eprintln!("{:?}", err);
            }
        }
    }
}

fn main() -> io::Result<()> {
    let env = Environment::new();
    loop {
        print!("> ");

        io::stdout().flush()?;

        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;
        let buffer = buffer.trim();

        if buffer == "quit" {
            break;
        }
        if buffer.is_empty() {
            continue;
        }

        let tokenizer = Tokenizer::new(buffer.chars());
        let tokens: Result<Vec<_>, _> = tokenizer.collect();
        let tokens = match tokens {
            Ok(tokens) => tokens,
            Err(err) => {
                eprintln!("{:?}", err);
                continue;
            }
        };

        env.eval_and_print(tokens);
    }
    Ok(())
}
