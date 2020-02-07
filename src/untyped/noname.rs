use std::num::ParseIntError;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token {
    Lambda,
    Var(usize),
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
}
