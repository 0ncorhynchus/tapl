use std::io::{self, Write};
use untyped::*;

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
                println!("Input:  {}", term);
                println!("Output: {}", term.eval());
            }
            Err(err) => {
                eprintln!("{:?}", err);
            }
        }
    }
    Ok(())
}
