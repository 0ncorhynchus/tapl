use super::{Index, Term};
use std::num::ParseIntError;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token {
    Lambda,
    Var(Index),
    OpenParenthesis,
    CloseParenthesis,
}

#[derive(Debug, PartialEq)]
pub enum TokenizerError {
    InvalidIndex(ParseIntError),
    EOF,
}

impl From<ParseIntError> for TokenizerError {
    fn from(err: ParseIntError) -> Self {
        Self::InvalidIndex(err)
    }
}

pub struct Tokenizer<I> {
    iter: I,
    last_char: Option<char>,
}

impl<I> Tokenizer<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(iter: I) -> Self {
        let mut iter = iter;
        let last_char = iter.next();

        Self { iter, last_char }
    }

    pub fn get_token(&mut self) -> Result<Token, TokenizerError> {
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
                '\\' => {
                    self.read_char();
                    Ok(Token::Lambda)
                }
                _ => Ok(Token::Var(self.read_digits(c).parse()?)),
            }
        } else {
            Err(TokenizerError::EOF)
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

    fn read_digits(&mut self, initial: char) -> String {
        let mut token = initial.to_string();

        while let Some(c) = self.read_char() {
            if !c.is_ascii_digit() {
                break;
            }
            token.push(c);
        }

        token
    }
}

impl<I: Iterator<Item = char>> Iterator for Tokenizer<I> {
    type Item = Result<Token, TokenizerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.get_token() {
            Ok(token) => Some(Ok(token)),
            Err(err) => match err {
                TokenizerError::EOF => None,
                _ => Some(Err(err)),
            },
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    UnexpectedToken(Token),
    UnexpectedEOF,
}

pub struct Parser<I> {
    iter: I,
    next_token: Option<Token>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(iter: I) -> Self {
        let mut iter = iter;
        let next_token = iter.next();
        Self { iter, next_token }
    }

    pub fn parse(&mut self) -> Result<Box<Term>, ParserError> {
        let mut current = self.parse_next()?;
        loop {
            if self.next_token == Some(Token::CloseParenthesis) || self.next_token.is_none() {
                break;
            }
            current = Box::new(Term::App(current, self.parse_next()?));
        }
        Ok(current)
    }

    fn parse_next(&mut self) -> Result<Box<Term>, ParserError> {
        let token = self.get_next_token()?;
        match token {
            Token::Lambda => Ok(Box::new(Term::Abs("".to_string(), self.parse()?))),
            Token::Var(index) => Ok(Box::new(Term::Var(index, 0))),
            Token::OpenParenthesis => self.parse_until_close(),
            _ => Err(ParserError::UnexpectedToken(token)),
        }
    }

    fn parse_until_close(&mut self) -> Result<Box<Term>, ParserError> {
        let mut current = self.parse_next()?;
        loop {
            if self.next_token == Some(Token::CloseParenthesis) {
                // consume ')'
                self.next_token = self.iter.next();
                break;
            }
            current = Box::new(Term::App(current, self.parse_next()?));
        }
        Ok(current)
    }

    fn get_next_token(&mut self) -> Result<Token, ParserError> {
        let next_token = self.next_token;
        self.next_token = self.iter.next();
        next_token.ok_or(ParserError::UnexpectedEOF)
    }
}

impl<I> Iterator for Parser<I>
where
    I: Iterator<Item = Token>,
{
    type Item = Result<Box<Term>, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next_token.is_none() {
            None
        } else {
            Some(self.parse())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenizer() {
        let line = r"\ \ 1 (0 2)";
        let mut tokenizer = Tokenizer::new(line.chars());
        assert_eq!(tokenizer.get_token(), Ok(Token::Lambda));
        assert_eq!(tokenizer.get_token(), Ok(Token::Lambda));
        assert_eq!(tokenizer.get_token(), Ok(Token::Var(1)));
        assert_eq!(tokenizer.get_token(), Ok(Token::OpenParenthesis));
        assert_eq!(tokenizer.get_token(), Ok(Token::Var(0)));
        assert_eq!(tokenizer.get_token(), Ok(Token::Var(2)));
        assert_eq!(tokenizer.get_token(), Ok(Token::CloseParenthesis));
        assert_eq!(tokenizer.get_token(), Err(TokenizerError::EOF));
    }

    #[test]
    fn test_tokenizer_iterator() {
        let line = r"\ \ 1 (0 2)";
        let mut tokenizer = Tokenizer::new(line.chars());
        assert_eq!(tokenizer.next(), Some(Ok(Token::Lambda)));
        assert_eq!(tokenizer.next(), Some(Ok(Token::Lambda)));
        assert_eq!(tokenizer.next(), Some(Ok(Token::Var(1))));
        assert_eq!(tokenizer.next(), Some(Ok(Token::OpenParenthesis)));
        assert_eq!(tokenizer.next(), Some(Ok(Token::Var(0))));
        assert_eq!(tokenizer.next(), Some(Ok(Token::Var(2))));
        assert_eq!(tokenizer.next(), Some(Ok(Token::CloseParenthesis)));
        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn test_parser_eof_error() {
        let tokens = vec![Token::Lambda];
        let mut parser = Parser::new(tokens.into_iter());
        assert_eq!(parser.parse(), Err(ParserError::UnexpectedEOF));
        assert_eq!(parser.parse(), Err(ParserError::UnexpectedEOF));
    }

    #[test]
    fn test_parser() {
        let tokens = vec![Token::Lambda, Token::Var(0), Token::Var(1)];
        let mut parser = Parser::new(tokens.into_iter());
        assert_eq!(
            parser.parse(),
            Ok(Box::new(Term::Abs(
                "".to_string(),
                Box::new(Term::App(
                    Box::new(Term::Var(0, 0)),
                    Box::new(Term::Var(1, 0))
                ))
            )))
        );
    }

    #[test]
    fn test_parser_concat() {
        let tokens = vec![Token::Var(0), Token::Var(1), Token::Var(2)];
        let mut parser = Parser::new(tokens.into_iter());
        assert_eq!(
            parser.parse(),
            Ok(Box::new(Term::App(
                Box::new(Term::App(
                    Box::new(Term::Var(0, 0)),
                    Box::new(Term::Var(1, 0))
                )),
                Box::new(Term::Var(2, 0))
            )))
        );
    }

    #[test]
    fn test_parser_parenthesis() {
        let tokens = vec![
            Token::OpenParenthesis,
            Token::Lambda,
            Token::Var(0),
            Token::Var(1),
            Token::CloseParenthesis,
            Token::Var(0),
        ];
        let mut parser = Parser::new(tokens.into_iter());
        assert_eq!(
            parser.parse(),
            Ok(Box::new(Term::App(
                Box::new(Term::Abs(
                    "".to_string(),
                    Box::new(Term::App(
                        Box::new(Term::Var(0, 0)),
                        Box::new(Term::Var(1, 0))
                    ))
                )),
                Box::new(Term::Var(0, 0))
            )))
        );
    }

    #[test]
    fn test_parser_simple_parenthesis() {
        let tokens = vec![
            Token::OpenParenthesis,
            Token::Var(0),
            Token::CloseParenthesis,
        ];
        let mut parser = Parser::new(tokens.into_iter());
        assert_eq!(parser.parse(), Ok(Box::new(Term::Var(0, 0))));

        let tokens = vec![
            Token::OpenParenthesis,
            Token::Lambda,
            Token::Var(0),
            Token::CloseParenthesis,
        ];
        let mut parser = Parser::new(tokens.into_iter());
        assert_eq!(
            parser.parse(),
            Ok(Box::new(Term::Abs(
                "".to_string(),
                Box::new(Term::Var(0, 0))
            )))
        );
    }
}
