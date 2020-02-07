use std::io::{self, Write};
use tapl::untyped::*;

fn main() -> io::Result<()> {
    loop {
        print!("> ");

        io::stdout().flush()?;

        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;

        if buffer.trim() == "quit" {
            break;
        }

        let tokenizer = noname::Tokenizer::new(buffer.chars());
        let tokens: Result<Vec<_>, _> = tokenizer.collect();
        let tokens = match tokens {
            Ok(tokens) => tokens,
            Err(err) => {
                eprintln!("{:?}", err);
                continue;
            }
        };

        let mut parser = noname::Parser::new(tokens.into_iter());
        match parser.parse() {
            Ok(term) => {
                println!("{:?}", term);
            }
            Err(err) => {
                eprintln!("{:?}", err);
            }
        }
    }
    Ok(())
}
