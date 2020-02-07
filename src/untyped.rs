pub mod noname;

use std::boxed::Box;

// de Bruijn index
type Index = usize;

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Var(Index, Index),
    Abs(String, Box<Self>),
    App(Box<Self>, Box<Self>),
}

impl Term {
    pub fn shift(&mut self, d: Index) {
        self.shift_(0, d);
    }

    fn shift_(&mut self, c: Index, d: Index) {
        match self {
            Self::Var(x, n) => {
                if *x >= c {
                    *x += d;
                }
                *n += d;
            }
            Self::Abs(_x, t2) => {
                t2.shift_(c + 1, d);
            }
            Self::App(t1, t2) => {
                t1.shift_(c, d);
                t2.shift_(c, d);
            }
        }
    }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shift() {
        let mut t = Term::Var(2, 2);
        t.shift(0);
        assert_eq!(t, Term::Var(2, 2));
        t.shift(1);
        assert_eq!(t, Term::Var(3, 3));

        let mut t = Term::Abs("".to_string(), Box::new(Term::Var(0, 1)));
        t.shift(0);
        assert_eq!(t, Term::Abs("".to_string(), Box::new(Term::Var(0, 1))));
        t.shift(1);
        assert_eq!(t, Term::Abs("".to_string(), Box::new(Term::Var(0, 2))));

        let mut t = Term::App(Box::new(Term::Var(0, 0)), Box::new(Term::Var(0, 0)));
        t.shift(0);
        assert_eq!(
            t,
            Term::App(Box::new(Term::Var(0, 0)), Box::new(Term::Var(0, 0)))
        );
        t.shift(1);
        assert_eq!(
            t,
            Term::App(Box::new(Term::Var(1, 1)), Box::new(Term::Var(1, 1)))
        );
    }

    #[test]
    fn test_shift_example() {
        let mut t = Term::Abs(
            "".to_string(),
            Box::new(Term::Abs(
                "".to_string(),
                Box::new(Term::App(
                    Box::new(Term::Var(1, 2)),
                    Box::new(Term::App(
                        Box::new(Term::Var(0, 2)),
                        Box::new(Term::Var(2, 2)),
                    )),
                )),
            )),
        );
        t.shift(2);
        assert_eq!(
            t,
            Term::Abs(
                "".to_string(),
                Box::new(Term::Abs(
                    "".to_string(),
                    Box::new(Term::App(
                        Box::new(Term::Var(1, 4)),
                        Box::new(Term::App(
                            Box::new(Term::Var(0, 4)),
                            Box::new(Term::Var(4, 4)),
                        )),
                    )),
                )),
            )
        );
    }
}
