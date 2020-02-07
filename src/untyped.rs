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

    pub fn subst(&mut self, j: Index, s: &Term) {
        self.subst_(0, j, s);
    }

    fn subst_(&mut self, c: Index, j: Index, s: &Term) {
        match self {
            Term::Var(x, _n) => {
                if *x == j + c {
                    *self = s.clone();
                    self.shift(c);
                }
            }
            Term::Abs(_x, t1) => {
                t1.subst_(c + 1, j, s);
            }
            Term::App(t1, t2) => {
                t1.subst_(c, j, s);
                t2.subst_(c, j, s);
            }
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

    #[test]
    fn test_subst_var() {
        let mut t = Term::Var(0, 1);
        t.subst(0, &Term::Var(1, 1));
        assert_eq!(t, Term::Var(1, 1));

        t.subst(0, &Term::Var(0, 1));
        assert_eq!(t, Term::Var(1, 1));
    }

    #[test]
    fn test_subst_abs() {
        let mut t = Term::Abs("".to_string(), Box::new(Term::Var(2, 3)));
        t.subst(1, &Term::Var(2, 2));
        assert_eq!(t, Term::Abs("".to_string(), Box::new(Term::Var(3, 3))));
    }

    #[test]
    fn test_subst_app() {
        let mut t = Term::App(Box::new(Term::Var(0, 2)), Box::new(Term::Var(1, 2)));
        t.subst(0, &Term::Var(2, 2));
        assert_eq!(
            t,
            Term::App(Box::new(Term::Var(2, 2)), Box::new(Term::Var(1, 2)))
        );
    }
}
