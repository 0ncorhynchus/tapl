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

impl Type {
    fn is_arrow(&self) -> bool {
        match self {
            Type::Arrow(_, _) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Bool => write!(f, "Bool"),
            Self::Arrow(from, to) => {
                if from.is_arrow() {
                    write!(f, "({})->{}", from, to)
                } else {
                    write!(f, "{}->{}", from, to)
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Var(Index),
    Abs(String, Type, Box<Self>),
    App(Box<Self>, Box<Self>),
    True,
    False,
    If(Box<Self>, Box<Self>, Box<Self>),
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
            Self::If(cond, jump1, jump2) => {
                cond.shift_(c, d);
                jump1.shift_(c, d);
                jump2.shift_(c, d);
            }
            _ => {}
        }
    }

    pub fn subst(&mut self, j: Index, s: &Term) {
        self.subst_(0, j, s);
    }

    fn subst_(&mut self, c: Index, j: Index, s: &Term) {
        match self {
            Self::Var(x) => {
                if *x == j + c {
                    *self = s.clone();
                    self.shift(c);
                }
            }
            Self::Abs(_x, _ty, t1) => {
                t1.subst_(c + 1, j, s);
            }
            Self::App(t1, t2) => {
                t1.subst_(c, j, s);
                t2.subst_(c, j, s);
            }
            Self::If(cond, jump1, jump2) => {
                cond.subst_(c, j, s);
                jump1.subst_(c, j, s);
                jump2.subst_(c, j, s);
            }
            _ => {}
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
            Self::Abs(_, _, _) | Self::True | Self::False => true,
            _ => false,
        }
    }

    fn eval1(self) -> Option<Self> {
        match self {
            Self::App(t1, t2) => match *t1 {
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
            },
            Self::If(cond, jump1, jump2) => match *cond {
                Term::True => Some(*jump1),
                Term::False => Some(*jump2),
                _ => Some(Term::If(Box::new(cond.eval1()?), jump1, jump2)),
            },
            _ => None,
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
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::If(cond, jump1, jump2) => write!(f, "if {} then {} else {}", cond, jump1, jump2),
        }
    }
}

#[derive(Debug)]
pub enum Context<'a> {
    Primitives(Vec<(String, Type, Term)>),
    Cons(&'a Self, String, Type),
}

impl<'a> Context<'a> {
    pub fn new(primitives: Vec<(String, Type, Term)>) -> Context<'static> {
        Context::Primitives(primitives)
    }

    pub fn empty() -> Context<'static> {
        Context::Primitives(vec![])
    }

    pub fn add(&self, name: String, ty: Type) -> Context {
        Context::Cons(self, name, ty)
    }

    pub fn find(&self, name: &str) -> Option<Index> {
        self.find_(name, 0)
    }

    pub fn find_(&self, name: &str, idx: Index) -> Option<Index> {
        match self {
            Self::Primitives(primitives) => primitives
                .iter()
                .enumerate()
                .find(|(_i, (n, _ty, _t))| n.as_str() == name)
                .map(|(i, _)| (i as Index) + idx),
            Self::Cons(ctx, n, _ty) => {
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
            Self::Primitives(primitives) => {
                primitives.get(index as usize).map(|(n, _ty, _t)| n.clone())
            }
            Self::Cons(ctx, name, _ty) => {
                if index == 0 {
                    Some(name.clone())
                } else {
                    ctx.get_name(index - 1)
                }
            }
        }
    }

    pub fn type_of(&self, term: &Term) -> Option<Type> {
        match term {
            Term::Var(idx) => self.get_type(*idx),
            Term::Abs(name, ty, body) => {
                let ctx = self.add(name.clone(), ty.clone());
                Some(Type::Arrow(
                    Box::new(ty.clone()),
                    Box::new(ctx.type_of(body)?),
                ))
            }
            Term::App(t1, t2) => {
                let ty1 = self.type_of(t1)?;
                let ty2 = self.type_of(t2)?;
                if let Type::Arrow(from, to) = ty1 {
                    if *from == ty2 {
                        Some(*to)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Term::True | Term::False => Some(Type::Bool),
            Term::If(cond, jump1, jump2) => {
                if self.type_of(cond)? == Type::Bool {
                    let ty1 = self.type_of(jump1)?;
                    if ty1 == self.type_of(jump2)? {
                        Some(ty1)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
    }

    fn get_type(&self, index: Index) -> Option<Type> {
        match self {
            Self::Primitives(primitives) => primitives
                .get(index as usize)
                .map(|(_n, ty, _t)| ty.clone()),
            Self::Cons(ctx, _name, ty) => {
                if index == 0 {
                    Some(ty.clone())
                } else {
                    ctx.get_type(index - 1)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_is_arrow() {
        assert!(!Type::Bool.is_arrow());
        assert!(Type::Arrow(Box::new(Type::Bool), Box::new(Type::Bool)).is_arrow());
    }

    #[test]
    fn test_type_format() {
        assert_eq!(Type::Bool.to_string(), "Bool");
        assert_eq!(
            Type::Arrow(Box::new(Type::Bool), Box::new(Type::Bool)).to_string(),
            "Bool->Bool"
        );
        assert_eq!(
            Type::Arrow(
                Box::new(Type::Bool),
                Box::new(Type::Arrow(Box::new(Type::Bool), Box::new(Type::Bool)))
            )
            .to_string(),
            "Bool->Bool->Bool"
        );
        assert_eq!(
            Type::Arrow(
                Box::new(Type::Arrow(Box::new(Type::Bool), Box::new(Type::Bool))),
                Box::new(Type::Bool)
            )
            .to_string(),
            "(Bool->Bool)->Bool"
        );
    }

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
        assert_eq!(Term::True.eval(), Term::True);
        assert_eq!(Term::False.eval(), Term::False);

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

    #[test]
    fn test_eval_if() {
        let t = Term::If(
            Box::new(Term::True),
            Box::new(Term::Var(0)),
            Box::new(Term::Var(1)),
        );
        assert_eq!(t.eval(), Term::Var(0));

        let t = Term::If(
            Box::new(Term::False),
            Box::new(Term::Var(0)),
            Box::new(Term::Var(1)),
        );
        assert_eq!(t.eval(), Term::Var(1));

        let id = Term::Abs("".to_string(), Type::Bool, Box::new(Term::Var(0)));
        let t = Term::If(
            Box::new(Term::App(Box::new(id), Box::new(Term::True))),
            Box::new(Term::Var(0)),
            Box::new(Term::Var(1)),
        );
        assert_eq!(t.eval(), Term::Var(0));

        let id = Term::Abs("".to_string(), Type::Bool, Box::new(Term::Var(0)));
        let t = Term::If(
            Box::new(Term::App(Box::new(id), Box::new(Term::False))),
            Box::new(Term::Var(0)),
            Box::new(Term::Var(1)),
        );
        assert_eq!(t.eval(), Term::Var(1));
    }
}
