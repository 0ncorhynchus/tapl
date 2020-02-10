mod parser;
mod term;

use crate::parser::*;
use crate::term::*;
use std::io::{self, Write};

fn main() -> io::Result<()> {
    loop {
        print!("> ");
        io::stdout().flush()?;

        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;

        if buffer.trim() == "quit" {
            break;
        }

        let lexer = Lexer::new(buffer.chars());
        let tokens: Result<Vec<Token>, _> = lexer.collect();
        let tokens = match tokens {
            Ok(tokens) => tokens,
            Err(err) => {
                eprintln!("{:?}", err);
                continue;
            }
        };

        let token_strings: Vec<_> = tokens.iter().map(|token| token.to_string()).collect();
        println!("input:     {}", token_strings.join(" "));

        let parser = Parser::new(tokens.into_iter());
        for parsed in parser {
            match parsed {
                Ok(parsed) => {
                    println!("parsed:    {:?}", parsed);
                    println!("evaluated: {:?}", eval(*parsed));
                }
                Err(err) => {
                    eprintln!("{:?}", err);
                    break;
                }
            }
        }
    }
    Ok(())
}
