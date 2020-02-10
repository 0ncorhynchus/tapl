pub mod parser;

use std::boxed::Box;
use std::fmt;

// de Bruijn index
type Index = i32;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Arrow(Box<Self>, Box<Self>),
    Bool,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Bool => write!(f, "Bool"),
            Self::Arrow(from, to) => write!(f, "{}->{}", from, to),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Var(Index),
    Abs(String, Type, Box<Self>),
    App(Box<Self>, Box<Self>),
}

impl Term {
    pub fn shift(&mut self, d: Index) {
        self.shift_(0, d);
    }

    fn shift_(&mut self, c: Index, d: Index) {
        match self {
            Self::Var(x) => {
                if *x >= c {
                    *x += d;
                }
            }
            Self::Abs(_x, _ty, t2) => {
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
            Term::Var(x) => {
                if *x == j + c {
                    *self = s.clone();
                    self.shift(c);
                }
            }
            Term::Abs(_x, _ty, t1) => {
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
            Self::Abs(_, _, _) => true,
            _ => false,
        }
    }

    fn eval1(self) -> Option<Self> {
        if let Self::App(t1, t2) = self {
            match *t1 {
                Self::Abs(x, ty, mut t12) => {
                    if t2.is_val() {
                        t12.subst_top(*t2);
                        return Some(*t12);
                    } else {
                        Some(Term::App(
                            Box::new(Self::Abs(x, ty, t12)),
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
        let mut current = self;
        while let Some(t) = current.clone().eval1() {
            current = t;
        }
        current
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Var(x) => write!(f, "{}", x),
            Self::Abs(_name, _ty, t) => write!(f, "(λ. {})", t),
            Self::App(t1, t2) => write!(f, "({} {})", t1, t2),
        }
    }
}

#[derive(Debug)]
pub enum Context<'a> {
    Primitives(Vec<(String, Term)>),
    Cons(&'a Self, String),
}

impl<'a> Context<'a> {
    pub fn new(primitives: Vec<(String, Term)>) -> Context<'static> {
        Context::Primitives(primitives)
    }

    pub fn empty() -> Context<'static> {
        Context::Primitives(vec![])
    }

    pub fn add(&self, name: String) -> Context {
        Context::Cons(self, name)
    }

    pub fn find(&self, name: &str) -> Option<Index> {
        self.find_(name, 0)
    }

    pub fn find_(&self, name: &str, idx: Index) -> Option<Index> {
        match self {
            Self::Primitives(primitives) => primitives
                .iter()
                .enumerate()
                .find(|(_i, (n, _t))| n.as_str() == name)
                .map(|(i, _)| (i as Index) + idx),
            Self::Cons(ctx, n) => {
                if n == name {
                    Some(idx)
                } else {
                    ctx.find_(name, idx + 1)
                }
            }
        }
    }

    pub fn get_name(&self, index: Index) -> Option<String> {
        match self {
            Self::Primitives(primitives) => primitives.get(index as usize).map(|(n, _t)| n.clone()),
            Self::Cons(ctx, name) => {
                if index == 0 {
                    Some(name.clone())
                } else {
                    ctx.get_name(index - 1)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shift_var() {
        let mut t = Term::Var(2);
        t.shift(0);
        assert_eq!(t, Term::Var(2));
        t.shift(1);
        assert_eq!(t, Term::Var(3));
    }

    #[test]
    fn test_shift_abs() {
        let mut t = Term::Abs("".to_string(), Type::Bool, Box::new(Term::Var(0)));
        t.shift(0);
        assert_eq!(
            t,
            Term::Abs("".to_string(), Type::Bool, Box::new(Term::Var(0)))
        );
        t.shift(1);
        assert_eq!(
            t,
            Term::Abs("".to_string(), Type::Bool, Box::new(Term::Var(0)))
        );
    }

    #[test]
    fn test_shift_app() {
        let mut t = Term::App(Box::new(Term::Var(0)), Box::new(Term::Var(0)));
        t.shift(0);
        assert_eq!(t, Term::App(Box::new(Term::Var(0)), Box::new(Term::Var(0))));
        t.shift(1);
        assert_eq!(t, Term::App(Box::new(Term::Var(1)), Box::new(Term::Var(1))));
    }

    #[test]
    fn test_subst_var() {
        let mut t = Term::Var(0);
        t.subst(0, &Term::Var(1));
        assert_eq!(t, Term::Var(1));

        t.subst(0, &Term::Var(0));
        assert_eq!(t, Term::Var(1));
    }

    #[test]
    fn test_subst_abs() {
        let mut t = Term::Abs("".to_string(), Type::Bool, Box::new(Term::Var(2)));
        t.subst(1, &Term::Var(2));
        assert_eq!(
            t,
            Term::Abs("".to_string(), Type::Bool, Box::new(Term::Var(3)))
        );
    }

    #[test]
    fn test_subst_app() {
        let mut t = Term::App(Box::new(Term::Var(0)), Box::new(Term::Var(1)));
        t.subst(0, &Term::Var(2));
        assert_eq!(t, Term::App(Box::new(Term::Var(2)), Box::new(Term::Var(1))));
    }

    #[test]
    fn test_eval() {
        assert_eq!(Term::Var(0).eval(), Term::Var(0));

        let t = Term::App(Box::new(Term::Var(1)), Box::new(Term::Var(1)));
        assert_eq!(t.clone(), t.eval());

        let abs = Term::Abs("".to_string(), Type::Bool, Box::new(Term::Var(0)));
        assert_eq!(abs.clone().eval(), abs);

        let app = Term::App(Box::new(abs.clone()), Box::new(abs.clone()));
        assert_eq!(app.eval(), abs);

        let app = Term::App(Box::new(abs), Box::new(Term::Var(0)));
        assert_eq!(app.clone().eval(), app);

        let f = Term::Abs("".to_string(), Type::Bool, Box::new(Term::Var(1)));
        let val = Term::Abs("".to_string(), Type::Bool, Box::new(Term::Var(0)));
        let app = Term::App(Box::new(f), Box::new(val));
        assert_eq!(app.eval(), Term::Var(0));
    }
}
