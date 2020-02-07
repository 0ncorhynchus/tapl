pub mod noname;

use std::boxed::Box;
use std::fmt;

// de Bruijn index
type Index = i32;

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

    pub fn subst_top(&mut self, s: Term) {
        let mut s = s;
        s.shift(1);
        self.subst(0, &s);
        self.shift(-1);
    }

    pub fn is_val(&self) -> bool {
        match self {
            Self::Abs(_, _) => true,
            _ => false,
        }
    }

    fn eval1(self) -> Option<Self> {
        if let Self::App(t1, t2) = self {
            match *t1 {
                Self::Abs(x, mut t12) => {
                    if t2.is_val() {
                        t12.subst_top(*t2);
                        return Some(*t12);
                    } else {
                        Some(Term::App(
                            Box::new(Self::Abs(x, t12)),
                            Box::new(t2.eval1()?),
                        ))
                    }
                }
                _ => Some(Term::App(Box::new(t1.eval1()?), t2)),
            }
        } else {
            None
        }
    }

    pub fn eval(self) -> Self {
        if let Some(t) = self.clone().eval1() {
            t.eval()
        } else {
            self
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Var(x, _n) => write!(f, "{}", x),
            Self::Abs(_name, t) => write!(f, "(λ. {})", t),
            Self::App(t1, t2) => write!(f, "({} {})", t1, t2),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shift_var() {
        let mut t = Term::Var(2, 2);
        t.shift(0);
        assert_eq!(t, Term::Var(2, 2));
        t.shift(1);
        assert_eq!(t, Term::Var(3, 3));
    }

    #[test]
    fn test_shift_abs() {
        let mut t = Term::Abs("".to_string(), Box::new(Term::Var(0, 1)));
        t.shift(0);
        assert_eq!(t, Term::Abs("".to_string(), Box::new(Term::Var(0, 1))));
        t.shift(1);
        assert_eq!(t, Term::Abs("".to_string(), Box::new(Term::Var(0, 2))));
    }

    #[test]
    fn test_shift_app() {
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

    #[test]
    fn test_eval() {
        assert_eq!(Term::Var(0, 0).eval(), Term::Var(0, 0));

        let t = Term::App(Box::new(Term::Var(1, 0)), Box::new(Term::Var(1, 1)));
        assert_eq!(t.clone(), t.eval());

        let abs = Term::Abs("".to_string(), Box::new(Term::Var(0, 0)));
        assert_eq!(abs.clone().eval(), abs);

        let app = Term::App(Box::new(abs.clone()), Box::new(abs.clone()));
        assert_eq!(app.eval(), abs);

        let app = Term::App(Box::new(abs), Box::new(Term::Var(0, 0)));
        assert_eq!(app.clone().eval(), app);

        let f = Term::Abs("".to_string(), Box::new(Term::Var(1, 1)));
        let val = Term::Abs("".to_string(), Box::new(Term::Var(0, 2)));
        let app = Term::App(Box::new(f), Box::new(val));
        assert_eq!(app.eval(), Term::Var(0, 0));
    }
}
