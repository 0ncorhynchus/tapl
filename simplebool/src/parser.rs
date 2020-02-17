use super::{Context, Term, Type};

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Lambda,
    Identifier(String),
    OpenParenthesis,
    CloseParenthesis,
    Dot,
    Colon,
    Arrow,
    True,
    False,
    If,
    Then,
    Else,
}

#[derive(Debug, PartialEq)]
pub enum TokenizerError {
    InvalidInitial(char),
    EOF,
    ExpectedGT(char),
    UnexpectedEOF,
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
                '.' => {
                    self.read_char();
                    Ok(Token::Dot)
                }
                '\\' => {
                    self.read_char();
                    Ok(Token::Lambda)
                }
                ':' => {
                    self.read_char();
                    Ok(Token::Colon)
                }
                '-' => {
                    let c = self.read_char().ok_or(TokenizerError::UnexpectedEOF)?;
                    if c == '>' {
                        self.read_char();
                        Ok(Token::Arrow)
                    } else {
                        Err(TokenizerError::ExpectedGT(c))
                    }
                }
                _ => {
                    if c.is_ascii_alphabetic() {
                        let ident = self.read_ascii_alphanumerics(c);
                        match ident.as_str() {
                            "true" => Ok(Token::True),
                            "false" => Ok(Token::False),
                            "if" => Ok(Token::If),
                            "then" => Ok(Token::Then),
                            "else" => Ok(Token::Else),
                            _ => Ok(Token::Identifier(ident)),
                        }
                    } else {
                        Err(TokenizerError::InvalidInitial(c))
                    }
                }
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
    Expected(Token),
    ExpectedIdentifier,
    UnknownType(String),
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

    pub fn parse(&mut self, ctx: &Context) -> Result<Box<Term>, ParserError> {
        self.parse_until(ctx, &None)
    }

    fn parse_next(&mut self, ctx: &Context, end: &Option<Token>) -> Result<Box<Term>, ParserError> {
        let token = self.get_next_token()?;
        match token {
            Token::Lambda => {
                if let Token::Identifier(ident) = self.get_next_token()? {
                    if self.get_next_token()? != Token::Colon {
                        return Err(ParserError::Expected(Token::Colon));
                    }

                    let ty = self.parse_type(ctx, false)?;

                    Ok(Box::new(Term::Abs(
                        ident.clone(),
                        ty.clone(),
                        self.parse_until(&ctx.add(ident, ty), end)?,
                    )))
                } else {
                    Err(ParserError::ExpectedIdentifier)
                }
            }
            Token::Identifier(name) => {
                if let Some(index) = ctx.find(&name) {
                    Ok(Box::new(Term::Var(index)))
                } else {
                    Err(ParserError::UnknownVariable(name))
                }
            }
            Token::OpenParenthesis => {
                let term = self.parse_until(&ctx, &Some(Token::CloseParenthesis))?;
                assert_eq!(self.get_next_token()?, Token::CloseParenthesis);
                Ok(term)
            }
            Token::True => Ok(Box::new(Term::True)),
            Token::False => Ok(Box::new(Term::False)),
            Token::If => {
                let cond = self.parse_until(&ctx, &Some(Token::Then))?;
                assert_eq!(self.get_next_token()?, Token::Then);
                let jump1 = self.parse_until(&ctx, &Some(Token::Else))?;
                assert_eq!(self.get_next_token()?, Token::Else);
                let jump2 = self.parse_until(&ctx, end)?;
                Ok(Box::new(Term::If(cond, jump1, jump2)))
            }
            _ => Err(ParserError::UnexpectedToken(token)),
        }
    }

    fn parse_type(&mut self, ctx: &Context, is_in_paren: bool) -> Result<Type, ParserError> {
        let mut types = vec![self.parse_simple_type(ctx)?];
        loop {
            match self.get_next_token()? {
                Token::Arrow => {
                    types.push(self.parse_simple_type(ctx)?);
                }
                Token::Dot => {
                    if is_in_paren {
                        return Err(ParserError::Expected(Token::CloseParenthesis));
                    }
                    break;
                }
                Token::CloseParenthesis => {
                    if !is_in_paren {
                        return Err(ParserError::Expected(Token::Dot));
                    }
                    break;
                }
                _ => {
                    if is_in_paren {
                        return Err(ParserError::Expected(Token::CloseParenthesis));
                    } else {
                        return Err(ParserError::Expected(Token::Dot));
                    }
                }
            }
        }

        let mut iter = types.into_iter().rev();
        let mut current = iter.next().unwrap();
        iter.for_each(|ty| {
            let mut tmp = Type::Bool;
            std::mem::swap(&mut current, &mut tmp);
            current = Type::Arrow(Box::new(ty), Box::new(tmp));
        });
        Ok(current)
    }

    fn parse_simple_type(&mut self, ctx: &Context) -> Result<Type, ParserError> {
        match self.get_next_token()? {
            Token::Identifier(name) => {
                if name != "Bool" {
                    Err(ParserError::UnknownType(name))
                } else {
                    Ok(Type::Bool)
                }
            }
            Token::OpenParenthesis => self.parse_type(ctx, true),
            _ => Err(ParserError::ExpectedIdentifier),
        }
    }

    fn parse_until(
        &mut self,
        ctx: &Context,
        end: &Option<Token>,
    ) -> Result<Box<Term>, ParserError> {
        let mut current = self.parse_next(ctx, end)?;
        match end {
            Some(end_token) => {
                while let Some(next_token) = &self.next_token {
                    if next_token == end_token {
                        // Don't consume
                        return Ok(current);
                    }
                    current = Box::new(Term::App(current, self.parse_next(ctx, end)?));
                }
                Err(ParserError::Expected(end_token.clone()))
            }
            None => {
                while let Some(_) = &self.next_token {
                    current = Box::new(Term::App(current, self.parse_next(ctx, end)?));
                }
                Ok(current)
            }
        }
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
        let line = r"\x. \y. x (y if true then z else false)";
        let mut tokenizer = Tokenizer::new(line.chars());
        assert_eq!(tokenizer.get_token(), Ok(Token::Lambda));
        assert_eq!(
            tokenizer.get_token(),
            Ok(Token::Identifier("x".to_string()))
        );
        assert_eq!(tokenizer.get_token(), Ok(Token::Dot));
        assert_eq!(tokenizer.get_token(), Ok(Token::Lambda));
        assert_eq!(
            tokenizer.get_token(),
            Ok(Token::Identifier("y".to_string()))
        );
        assert_eq!(tokenizer.get_token(), Ok(Token::Dot));
        assert_eq!(
            tokenizer.get_token(),
            Ok(Token::Identifier("x".to_string()))
        );
        assert_eq!(tokenizer.get_token(), Ok(Token::OpenParenthesis));
        assert_eq!(
            tokenizer.get_token(),
            Ok(Token::Identifier("y".to_string()))
        );
        assert_eq!(tokenizer.get_token(), Ok(Token::If));
        assert_eq!(tokenizer.get_token(), Ok(Token::True));
        assert_eq!(tokenizer.get_token(), Ok(Token::Then));
        assert_eq!(
            tokenizer.get_token(),
            Ok(Token::Identifier("z".to_string()))
        );
        assert_eq!(tokenizer.get_token(), Ok(Token::Else));
        assert_eq!(tokenizer.get_token(), Ok(Token::False));
        assert_eq!(tokenizer.get_token(), Ok(Token::CloseParenthesis));
        assert_eq!(tokenizer.get_token(), Err(TokenizerError::EOF));
    }

    #[test]
    fn test_tokenizer_iterator() {
        let line = r"\x. \y. x (y if true then z else false)";
        let mut tokenizer = Tokenizer::new(line.chars());
        assert_eq!(tokenizer.next(), Some(Ok(Token::Lambda)));
        assert_eq!(
            tokenizer.next(),
            Some(Ok(Token::Identifier("x".to_string())))
        );
        assert_eq!(tokenizer.next(), Some(Ok(Token::Dot)));
        assert_eq!(tokenizer.next(), Some(Ok(Token::Lambda)));
        assert_eq!(
            tokenizer.next(),
            Some(Ok(Token::Identifier("y".to_string())))
        );
        assert_eq!(tokenizer.next(), Some(Ok(Token::Dot)));
        assert_eq!(
            tokenizer.next(),
            Some(Ok(Token::Identifier("x".to_string())))
        );
        assert_eq!(tokenizer.next(), Some(Ok(Token::OpenParenthesis)));
        assert_eq!(
            tokenizer.next(),
            Some(Ok(Token::Identifier("y".to_string())))
        );
        assert_eq!(tokenizer.next(), Some(Ok(Token::If)));
        assert_eq!(tokenizer.next(), Some(Ok(Token::True)));
        assert_eq!(tokenizer.next(), Some(Ok(Token::Then)));
        assert_eq!(
            tokenizer.next(),
            Some(Ok(Token::Identifier("z".to_string())))
        );
        assert_eq!(tokenizer.next(), Some(Ok(Token::Else)));
        assert_eq!(tokenizer.next(), Some(Ok(Token::False)));
        assert_eq!(tokenizer.next(), Some(Ok(Token::CloseParenthesis)));
        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn test_parser_eof_error() {
        let tokens = vec![
            Token::Lambda,
            Token::Identifier("x".to_string()),
            Token::Colon,
            Token::Identifier("Bool".to_string()),
            Token::Dot,
        ];
        let mut parser = Parser::new(tokens.into_iter());
        let empty = Context::empty();
        assert_eq!(parser.parse(&empty), Err(ParserError::UnexpectedEOF));
        assert_eq!(parser.parse(&empty), Err(ParserError::UnexpectedEOF));
    }

    #[test]
    fn test_parser_unknown_variable() {
        let tokens = vec![Token::Identifier("x".to_string())];
        let mut parser = Parser::new(tokens.into_iter());
        let empty = Context::empty();
        assert_eq!(
            parser.parse(&empty),
            Err(ParserError::UnknownVariable("x".to_string()))
        );
    }

    #[test]
    fn test_parser() {
        let tokens = vec![
            Token::Lambda,
            Token::Identifier("x".to_string()),
            Token::Colon,
            Token::Identifier("Bool".to_string()),
            Token::Dot,
            Token::Identifier("x".to_string()),
            Token::Identifier("y".to_string()),
        ];
        let mut parser = Parser::new(tokens.into_iter());
        let empty = Context::empty();
        let ctx = Context::Cons(&empty, "y".to_string(), Type::Bool);
        assert_eq!(
            parser.parse(&ctx),
            Ok(Box::new(Term::Abs(
                "x".to_string(),
                Type::Bool,
                Box::new(Term::App(Box::new(Term::Var(0)), Box::new(Term::Var(1))))
            )))
        );
    }

    #[test]
    fn test_parser_concat() {
        let tokens = vec![
            Token::Identifier("x".to_string()),
            Token::Identifier("y".to_string()),
            Token::Identifier("z".to_string()),
        ];
        let mut parser = Parser::new(tokens.into_iter());
        let empty = Context::empty();
        let z = Context::Cons(&empty, "z".to_string(), Type::Bool);
        let y = Context::Cons(&z, "y".to_string(), Type::Bool);
        let x = Context::Cons(&y, "x".to_string(), Type::Bool);
        assert_eq!(
            parser.parse(&x),
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
            Token::Lambda,
            Token::Identifier("x".to_string()),
            Token::Colon,
            Token::Identifier("Bool".to_string()),
            Token::Dot,
            Token::Identifier("x".to_string()),
            Token::Identifier("y".to_string()),
            Token::CloseParenthesis,
            Token::Identifier("y".to_string()),
        ];
        let mut parser = Parser::new(tokens.into_iter());
        let empty = Context::empty();
        let ctx = Context::Cons(&empty, "y".to_string(), Type::Bool);
        assert_eq!(
            parser.parse(&ctx),
            Ok(Box::new(Term::App(
                Box::new(Term::Abs(
                    "x".to_string(),
                    Type::Bool,
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
            Token::Identifier("x".to_string()),
            Token::CloseParenthesis,
        ];
        let mut parser = Parser::new(tokens.into_iter());
        let empty = Context::empty();
        let ctx = Context::Cons(&empty, "x".to_string(), Type::Bool);
        assert_eq!(parser.parse(&ctx), Ok(Box::new(Term::Var(0))));

        let tokens = vec![
            Token::OpenParenthesis,
            Token::Lambda,
            Token::Identifier("x".to_string()),
            Token::Colon,
            Token::Identifier("Bool".to_string()),
            Token::Dot,
            Token::Identifier("x".to_string()),
            Token::CloseParenthesis,
        ];
        let mut parser = Parser::new(tokens.into_iter());
        let empty = Context::empty();
        assert_eq!(
            parser.parse(&empty),
            Ok(Box::new(Term::Abs(
                "x".to_string(),
                Type::Bool,
                Box::new(Term::Var(0))
            )))
        );
    }

    #[test]
    fn test_parser_parenthesis_error() {
        let tokens = vec![Token::Identifier("x".to_string()), Token::CloseParenthesis];
        let mut parser = Parser::new(tokens.into_iter());
        let empty = Context::empty();
        let ctx = Context::Cons(&empty, "x".to_string(), Type::Bool);
        assert_eq!(
            parser.parse(&ctx),
            Err(ParserError::UnexpectedToken(Token::CloseParenthesis))
        );
    }

    #[test]
    fn test_parser_if() {
        let tokens = vec![
            Token::Lambda,
            Token::Identifier("x".to_string()),
            Token::Colon,
            Token::Identifier("Bool".to_string()),
            Token::Dot,
            Token::If,
            Token::Identifier("x".to_string()),
            Token::Then,
            Token::False,
            Token::Else,
            Token::True,
        ];
        let mut parser = Parser::new(tokens.into_iter());
        let empty = Context::empty();
        assert_eq!(
            parser.parse(&empty),
            Ok(Box::new(Term::Abs(
                "x".to_string(),
                Type::Bool,
                Box::new(Term::If(
                    Box::new(Term::Var(0)),
                    Box::new(Term::False),
                    Box::new(Term::True)
                ))
            )))
        );
    }
}
