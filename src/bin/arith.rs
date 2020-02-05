use std::fmt;
use std::io;
use std::io::Write;
use std::str::FromStr;
use tapl::arith::*;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Token {
    True,
    False,
    If,
    Then,
    Else,
    Zero,
    Succ,
    Pred,
    IsZero,
    OpenParenthesis,
    CloseParenthesis,
}

#[derive(Debug)]
enum LexerError {
    UnknownToken(String),
    EOF,
}

struct Lexer<I> {
    iter: I,
    last_char: Option<char>,
}

impl<I> Lexer<I>
where
    I: Iterator<Item = char>,
{
    fn new(iter: I) -> Self {
        let mut iter = iter;
        let last_char = iter.next();

        Self { iter, last_char }
    }

    fn get_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespaces();

        if let Some(c) = self.last_char {
            match c {
                '(' => {
                    self.read_char();
                    Ok(Token::OpenParenthesis)
                }
                ')' => {
                    self.read_char();
                    Ok(Token::CloseParenthesis)
                }
                '0' => {
                    self.read_char();
                    Ok(Token::Zero)
                }
                _ => self.read_ascii_alphanumerics(c).parse(),
            }
        } else {
            Err(LexerError::EOF)
        }
    }

    fn read_char(&mut self) -> Option<char> {
        self.last_char = self.iter.next();
        self.last_char
    }

    fn skip_whitespaces(&mut self) {
        if let Some(c) = self.last_char {
            if !c.is_whitespace() {
                return;
            }
        } else {
            return;
        }

        while let Some(c) = self.read_char() {
            if !c.is_whitespace() {
                break;
            }
        }
    }

    fn read_ascii_alphanumerics(&mut self, initial: char) -> String {
        let mut token = initial.to_string();

        while let Some(c) = self.read_char() {
            if !c.is_ascii_alphanumeric() {
                break;
            }
            token.push(c);
        }

        token
    }
}

impl<I: Iterator<Item = char>> Iterator for Lexer<I> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.get_token() {
            Ok(token) => Some(Ok(token)),
            Err(err) => match err {
                LexerError::EOF => None,
                _ => Some(Err(err)),
            },
        }
    }
}

impl FromStr for Token {
    type Err = LexerError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "true" => Ok(Self::True),
            "false" => Ok(Self::False),
            "if" => Ok(Self::If),
            "then" => Ok(Self::Then),
            "else" => Ok(Self::Else),
            "0" => Ok(Self::Zero),
            "succ" => Ok(Self::Succ),
            "pred" => Ok(Self::Pred),
            "iszero" => Ok(Self::IsZero),
            "(" => Ok(Self::OpenParenthesis),
            ")" => Ok(Self::CloseParenthesis),
            _ => Err(LexerError::UnknownToken(s.to_string())),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::If => write!(f, "if"),
            Self::Then => write!(f, "then"),
            Self::Else => write!(f, "else"),
            Self::Zero => write!(f, "0"),
            Self::Succ => write!(f, "succ"),
            Self::Pred => write!(f, "pred"),
            Self::IsZero => write!(f, "iszero"),
            Self::OpenParenthesis => write!(f, "("),
            Self::CloseParenthesis => write!(f, ")"),
        }
    }
}

#[derive(Debug)]
enum ParserError {
    UnexpectedToken(Token),
    UnexpectedEOF,
}

struct Parser<I> {
    iter: I,
    last_token: Option<Token>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    fn new(iter: I) -> Self {
        let mut iter = iter;
        let last_token = iter.next();
        Self { iter, last_token }
    }

    fn parse(&mut self) -> Result<Box<Term>, ParserError> {
        let token = self.get_next_token()?;
        match token {
            Token::True => Ok(Box::new(Term::True)),
            Token::False => Ok(Box::new(Term::False)),
            Token::If => {
                let cond = self.parse()?;
                self.expect_token(Token::Then)?;
                let jump1 = self.parse()?;
                self.expect_token(Token::Else)?;
                let jump2 = self.parse()?;
                Ok(Box::new(Term::If(cond, jump1, jump2)))
            }
            Token::Zero => Ok(Box::new(Term::Zero)),
            Token::Succ => Ok(Box::new(Term::Succ(self.parse()?))),
            Token::Pred => Ok(Box::new(Term::Pred(self.parse()?))),
            Token::IsZero => Ok(Box::new(Term::IsZero(self.parse()?))),
            Token::OpenParenthesis => {
                let token = self.parse()?;
                self.expect_token(Token::CloseParenthesis)?;
                Ok(token)
            }
            _ => Err(ParserError::UnexpectedToken(token)),
        }
    }

    fn get_next_token(&mut self) -> Result<Token, ParserError> {
        let next_token = self.last_token;
        self.last_token = self.iter.next();
        next_token.ok_or(ParserError::UnexpectedEOF)
    }

    fn expect_token(&mut self, token: Token) -> Result<(), ParserError> {
        let next = self.get_next_token()?;
        if next == token {
            Ok(())
        } else {
            Err(ParserError::UnexpectedToken(next))
        }
    }
}

impl<I> Iterator for Parser<I>
where
    I: Iterator<Item = Token>,
{
    type Item = Result<Box<Term>, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.last_token.is_none() {
            None
        } else {
            Some(self.parse())
        }
    }
}

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
                },
                Err(err) => {
                    eprintln!("{:?}", err);
                }
            }
        }
    }
    Ok(())
}
