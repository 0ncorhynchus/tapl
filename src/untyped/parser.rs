use super::{Context, Term};

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Lambda(String),
    Var(String),
    OpenParenthesis,
    CloseParenthesis,
}

#[derive(Debug, PartialEq)]
pub enum TokenizerError {
    InvalidInitial(char),
    UnexpectedEOF,
    PeriodAbsence,
    EOF,
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
                    // consume '\'
                    self.read_char();
                    let var = self.read_variable()?;
                    self.skip_whitespaces();
                    if self.last_char == Some('.') {
                        // consume '.'
                        self.read_char();
                        Ok(Token::Lambda(var))
                    } else {
                        Err(TokenizerError::PeriodAbsence)
                    }
                }
                _ => Ok(Token::Var(self.read_variable()?)),
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

    fn read_variable(&mut self) -> Result<String, TokenizerError> {
        self.skip_whitespaces();
        if let Some(c) = self.last_char {
            if c.is_ascii_alphabetic() {
                Ok(self.read_ascii_alphanumerics(c))
            } else {
                Err(TokenizerError::InvalidInitial(c))
            }
        } else {
            Err(TokenizerError::UnexpectedEOF)
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
    UnknownVariable(String),
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
        self.parse_(Context::Empty)
    }

    fn parse_(&mut self, ctx: Context) -> Result<Box<Term>, ParserError> {
        let mut current = self.parse_next(&ctx)?;
        loop {
            if self.next_token == Some(Token::CloseParenthesis) || self.next_token.is_none() {
                break;
            }
            current = Box::new(Term::App(current, self.parse_next(&ctx)?));
        }
        Ok(current)
    }

    fn parse_next(&mut self, ctx: &Context) -> Result<Box<Term>, ParserError> {
        let token = self.get_next_token()?;
        match token {
            Token::Lambda(var) => Ok(Box::new(Term::Abs(var.clone(), self.parse_(ctx.add(var))?))),
            Token::Var(name) => {
                if let Some(index) = ctx.find(&name) {
                    Ok(Box::new(Term::Var(index)))
                } else {
                    Err(ParserError::UnknownVariable(name))
                }
            }
            Token::OpenParenthesis => self.parse_until_close(&ctx),
            _ => Err(ParserError::UnexpectedToken(token)),
        }
    }

    fn parse_until_close(&mut self, ctx: &Context) -> Result<Box<Term>, ParserError> {
        let mut current = self.parse_next(ctx)?;
        loop {
            if self.next_token == Some(Token::CloseParenthesis) {
                // consume ')'
                self.next_token = self.iter.next();
                break;
            }
            current = Box::new(Term::App(current, self.parse_next(ctx)?));
        }
        Ok(current)
    }

    fn get_next_token(&mut self) -> Result<Token, ParserError> {
        let mut next_token = self.iter.next();
        std::mem::swap(&mut next_token, &mut self.next_token);
        next_token.ok_or(ParserError::UnexpectedEOF)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenizer() {
        let line = r"\x. \y. x (y z)";
        let mut tokenizer = Tokenizer::new(line.chars());
        assert_eq!(tokenizer.get_token(), Ok(Token::Lambda("x".to_string())));
        assert_eq!(tokenizer.get_token(), Ok(Token::Lambda("y".to_string())));
        assert_eq!(tokenizer.get_token(), Ok(Token::Var("x".to_string())));
        assert_eq!(tokenizer.get_token(), Ok(Token::OpenParenthesis));
        assert_eq!(tokenizer.get_token(), Ok(Token::Var("y".to_string())));
        assert_eq!(tokenizer.get_token(), Ok(Token::Var("z".to_string())));
        assert_eq!(tokenizer.get_token(), Ok(Token::CloseParenthesis));
        assert_eq!(tokenizer.get_token(), Err(TokenizerError::EOF));
    }

    #[test]
    fn test_tokenizer_iterator() {
        let line = r"\x. \y. x (y z)";
        let mut tokenizer = Tokenizer::new(line.chars());
        assert_eq!(tokenizer.next(), Some(Ok(Token::Lambda("x".to_string()))));
        assert_eq!(tokenizer.next(), Some(Ok(Token::Lambda("y".to_string()))));
        assert_eq!(tokenizer.next(), Some(Ok(Token::Var("x".to_string()))));
        assert_eq!(tokenizer.next(), Some(Ok(Token::OpenParenthesis)));
        assert_eq!(tokenizer.next(), Some(Ok(Token::Var("y".to_string()))));
        assert_eq!(tokenizer.next(), Some(Ok(Token::Var("z".to_string()))));
        assert_eq!(tokenizer.next(), Some(Ok(Token::CloseParenthesis)));
        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn test_parser_eof_error() {
        let tokens = vec![Token::Lambda("x".to_string())];
        let mut parser = Parser::new(tokens.into_iter());
        assert_eq!(parser.parse(), Err(ParserError::UnexpectedEOF));
        assert_eq!(parser.parse(), Err(ParserError::UnexpectedEOF));
    }

    #[test]
    fn test_parser_unknown_variable() {
        let tokens = vec![Token::Var("x".to_string())];
        let mut parser = Parser::new(tokens.into_iter());
        assert_eq!(
            parser.parse(),
            Err(ParserError::UnknownVariable("x".to_string()))
        );
    }

    #[test]
    fn test_parser() {
        let tokens = vec![
            Token::Lambda("x".to_string()),
            Token::Var("x".to_string()),
            Token::Var("y".to_string()),
        ];
        let mut parser = Parser::new(tokens.into_iter());
        let ctx = Context::Cons(&Context::Empty, "y".to_string());
        assert_eq!(
            parser.parse_(ctx),
            Ok(Box::new(Term::Abs(
                "x".to_string(),
                Box::new(Term::App(Box::new(Term::Var(0)), Box::new(Term::Var(1))))
            )))
        );
    }

    #[test]
    fn test_parser_concat() {
        let tokens = vec![
            Token::Var("x".to_string()),
            Token::Var("y".to_string()),
            Token::Var("z".to_string()),
        ];
        let mut parser = Parser::new(tokens.into_iter());
        let z = Context::Cons(&Context::Empty, "z".to_string());
        let y = Context::Cons(&z, "y".to_string());
        let x = Context::Cons(&y, "x".to_string());
        assert_eq!(
            parser.parse_(x),
            Ok(Box::new(Term::App(
                Box::new(Term::App(Box::new(Term::Var(0)), Box::new(Term::Var(1)))),
                Box::new(Term::Var(2))
            )))
        );
    }

    #[test]
    fn test_parser_parenthesis() {
        let tokens = vec![
            Token::OpenParenthesis,
            Token::Lambda("x".to_string()),
            Token::Var("x".to_string()),
            Token::Var("y".to_string()),
            Token::CloseParenthesis,
            Token::Var("y".to_string()),
        ];
        let mut parser = Parser::new(tokens.into_iter());
        let ctx = Context::Cons(&Context::Empty, "y".to_string());
        assert_eq!(
            parser.parse_(ctx),
            Ok(Box::new(Term::App(
                Box::new(Term::Abs(
                    "x".to_string(),
                    Box::new(Term::App(Box::new(Term::Var(0)), Box::new(Term::Var(1))))
                )),
                Box::new(Term::Var(0))
            )))
        );
    }

    #[test]
    fn test_parser_simple_parenthesis() {
        let tokens = vec![
            Token::OpenParenthesis,
            Token::Var("x".to_string()),
            Token::CloseParenthesis,
        ];
        let mut parser = Parser::new(tokens.into_iter());
        let ctx = Context::Cons(&Context::Empty, "x".to_string());
        assert_eq!(parser.parse_(ctx), Ok(Box::new(Term::Var(0))));

        let tokens = vec![
            Token::OpenParenthesis,
            Token::Lambda("x".to_string()),
            Token::Var("x".to_string()),
            Token::CloseParenthesis,
        ];
        let mut parser = Parser::new(tokens.into_iter());
        assert_eq!(
            parser.parse(),
            Ok(Box::new(Term::Abs("x".to_string(), Box::new(Term::Var(0)))))
        );
    }
}
