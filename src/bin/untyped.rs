use std::io::{self, Write};
use tapl::untyped::*;

fn print(term: &Term, ctx: &Context) -> String {
    match term {
        Term::Var(idx) => ctx.get_name(*idx).expect("Invalid Index"),
        Term::Abs(name, t) => format!("λ{}. {}", name, print(t, &ctx.add(name.clone()))),
        Term::App(t1, t2) => format!("({} {})", print(t1, ctx), print(t2, ctx),),
    }
}

fn main() -> io::Result<()> {
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

        let tokenizer = parser::Tokenizer::new(buffer.chars());
        let tokens: Result<Vec<_>, _> = tokenizer.collect();
        let tokens = match tokens {
            Ok(tokens) => tokens,
            Err(err) => {
                eprintln!("{:?}", err);
                continue;
            }
        };

        let mut parser = parser::Parser::new(tokens.into_iter());
        match parser.parse() {
            Ok(term) => {
                println!("{}", print(&term.eval(), &Context::Empty));
            }
            Err(err) => {
                eprintln!("{:?}", err);
            }
        }
    }
    Ok(())
}
