use std::boxed::Box;

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    True,
    False,
    If(Box<Term>, Box<Term>, Box<Term>),
    Zero,
    Succ(Box<Term>),
    Pred(Box<Term>),
    IsZero(Box<Term>),
}

impl Term {
    pub fn is_numeric_val(&self) -> bool {
        match self {
            Term::Zero => true,
            Term::Succ(prev) => prev.is_numeric_val(),
            _ => false,
        }
    }

    pub fn is_val(&self) -> bool {
        if self.is_numeric_val() {
            return true;
        }
        match self {
            Term::True => true,
            Term::False => true,
            _ => false,
        }
    }
}

pub fn eval1(t: Term) -> Option<Term> {
    match t {
        Term::If(c, t1, t2) => match c.as_ref() {
            Term::True => Some(*t1),
            Term::False => Some(*t2),
            _ => Some(Term::If(Box::new(eval1(*c)?), t1, t2)),
        },
        Term::Succ(t) => Some(Term::Succ(Box::new(eval1(*t)?))),
        Term::Pred(t) => match *t {
            Term::Zero => Some(Term::Zero),
            Term::Succ(t) => Some(*t),
            _ => Some(Term::Pred(Box::new(eval1(*t)?))),
        },
        Term::IsZero(t) => match *t {
            Term::Zero => Some(Term::True),
            _ => {
                if t.is_numeric_val() {
                    Some(Term::False)
                } else {
                    Some(Term::IsZero(Box::new(eval1(*t)?)))
                }
            }
        },
        _ => None,
    }
}

pub fn eval(t: Term) -> Term {
    let tmp = t.clone();
    if let Some(t) = eval1(t) {
        eval(t)
    } else {
        tmp
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_numeric_val() {
        assert_eq!(Term::Zero.is_numeric_val(), true);
        assert_eq!(Term::True.is_numeric_val(), false);
        assert_eq!(Term::False.is_numeric_val(), false);
        assert_eq!(
            Term::If(
                Box::new(Term::True),
                Box::new(Term::Zero),
                Box::new(Term::Succ(Box::new(Term::Zero)))
            )
            .is_numeric_val(),
            false
        );
        assert_eq!(Term::Succ(Box::new(Term::Zero)).is_numeric_val(), true);
        assert_eq!(Term::Succ(Box::new(Term::True)).is_numeric_val(), false);
    }

    #[test]
    fn test_is_val() {
        assert_eq!(Term::Zero.is_val(), true);
        assert_eq!(Term::True.is_val(), true);
        assert_eq!(Term::False.is_val(), true);
        assert_eq!(
            Term::If(
                Box::new(Term::True),
                Box::new(Term::Zero),
                Box::new(Term::Succ(Box::new(Term::Zero)))
            )
            .is_val(),
            false
        );
        assert_eq!(Term::Succ(Box::new(Term::Zero)).is_val(), true);
        assert_eq!(Term::Succ(Box::new(Term::True)).is_val(), false);
    }

    #[test]
    fn test_eval1() {
        assert_eq!(eval1(Term::True), None);
        assert_eq!(eval1(Term::False), None);
        assert_eq!(eval1(Term::Zero), None);
        assert_eq!(eval1(Term::Pred(Box::new(Term::Zero))), Some(Term::Zero));
        assert_eq!(
            eval1(Term::Pred(Box::new(Term::Succ(Box::new(Term::Zero))))),
            Some(Term::Zero)
        );
    }

    #[test]
    fn test_eval() {
        assert_eq!(eval(Term::True), Term::True);
        assert_eq!(eval(Term::False), Term::False);
        assert_eq!(eval(Term::Zero), Term::Zero);
        assert_eq!(
            eval(Term::Pred(Box::new(Term::True))),
            Term::Pred(Box::new(Term::True))
        );
        assert_eq!(eval(Term::Pred(Box::new(Term::Zero))), Term::Zero);
    }
}
