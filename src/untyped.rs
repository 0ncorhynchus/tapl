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
        self.walk(0, d);
    }

    fn walk(&mut self, c: Index, d: Index) {
        match self {
            Self::Var(x, n) => {
                if *x >= c {
                    *x += d;
                }
                *n += d;
            }
            Self::Abs(_x, t2) => {
                t2.walk(c + 1, d);
            }
            Self::App(t1, t2) => {
                t1.walk(c, d);
                t2.walk(c, d);
            }
        }
    }
}
