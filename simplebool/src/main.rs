use simplebool::parser::{Parser, Token, Tokenizer};
use simplebool::{Context, Term, Type};
use std::collections::HashMap;
use std::io::{self, Write};

fn print(term: &Term, ctx: &Context) -> String {
    match term {
        Term::Var(idx) => ctx.get_name(*idx).expect("Invalid Index"),
        Term::Abs(name, ty, t) => format!("λ{}:{}. {}", name, ty, print(t, &ctx.add(name.clone()))),
        Term::App(t1, t2) => format!("({} {})", print(t1, ctx), print(t2, ctx),),
    }
}

struct Environment {
    primitives: HashMap<String, Term>,
}

impl Environment {
    fn new() -> Self {
        Self {
            primitives: HashMap::new(),
        }
    }

    fn register(&mut self, name: &str, value: Term) {
        self.primitives.insert(name.to_string(), value);
    }

    fn eval_and_print(&self, tokens: Vec<Token>) {
        let context = Context::new(
            self.primitives
                .iter()
                .map(|(n, t)| (n.clone(), t.clone()))
                .collect(),
        );

        let mut parser = Parser::new(tokens.into_iter());
        match parser.parse(&context) {
            Ok(term) => {
                let mut term = term;
                for (name, value) in &self.primitives {
                    term = Box::new(Term::App(
                        Box::new(Term::Abs(name.clone(), Type::Bool, term)),
                        Box::new(value.clone()),
                    ))
                }
                println!("{}", print(&term.eval(), &context));
            }
            Err(err) => {
                eprintln!("{:?}", err);
            }
        }
    }
}

macro_rules! term {
    (lambda $name:ident ( $( $body:tt )* )) => {
        Term::Abs(stringify!($name).to_string(), Type::Bool, Box::new(term!($($body)*)))
    };
    ($t1:tt $t2:tt) => {
        Term::App(Box::new(term!($t1)), Box::new(term!($t2)))
    };
    (($($t:tt)+)) => {
        term!($($t)+)
    };
    ($ident:ident) => {
        $ident.clone()
    };
    ($index:expr) => {
        Term::Var($index)
    };
}

fn main() -> io::Result<()> {
    let mut env = Environment::new();
    let tru = term!(lambda t (lambda f (1)));
    let fls = term!(lambda t (lambda f (0)));
    let and = term!(lambda b (lambda c ((1 0) fls)));
    env.register("tru", tru.clone());
    env.register("fls", fls.clone());
    env.register("test", term!(lambda l (lambda m (lambda n ((2 1) 0)))));
    env.register("and", and.clone());
    env.register("or", term!(lambda b (lambda c ((1 tru) 0))));
    env.register("not", term!(lambda b ((0 fls) tru)));

    let pair = term!(lambda f (lambda s (lambda b ((0 2) 1))));
    let fst = term!(lambda p (0 tru));
    let snd = term!(lambda p (0 fls));
    env.register("pair", pair.clone());
    env.register("fst", fst.clone());
    env.register("snd", snd.clone());

    let c0 = term!(lambda s (lambda z (0)));
    let c1 = term!(lambda s (lambda z (1 0)));
    env.register("c0", c0.clone());
    env.register("c1", c1.clone());
    env.register("c2", term!(lambda s (lambda z (1 (1 0)))));
    env.register("c3", term!(lambda s (lambda z (1 (1 (1 0))))));
    env.register("c4", term!(lambda s (lambda z (1 (1 (1 (1 0)))))));

    env.register("succ", term!(lambda n (lambda s (lambda z (1 ((2 1) 0))))));
    env.register("succ2", term!(lambda n (lambda s (lambda z ((2 1) (1 0))))));

    let plus = term!(lambda m (lambda n (lambda s (lambda z ((3 1) ((2 1) 0))))));
    env.register("plus", plus.clone());
    env.register("times", term!(lambda m (lambda n ((1 (plus 0)) c0))));

    let iszro = term!(lambda m ((0 (lambda x (fls))) tru));
    env.register("iszro", iszro.clone());

    let zz = term!((pair c0) c0);
    let ss = term!(lambda p ((pair (snd 0)) ((plus c1) (snd 0))));
    let pred = term!(lambda m (fst ((0 ss) zz)));
    env.register("zz", zz);
    env.register("ss", ss);
    env.register("pred", pred.clone());

    let equal = term!(lambda m (lambda n ((and (iszro ((0 pred) 1))) (iszro ((1 pred) 0)))));
    env.register("equal", equal);

    let fix =
        term!(lambda f ((lambda x (1 (lambda y ((1 1) 0)))) (lambda x (1 (lambda y ((1 1) 0))))));
    env.register("fix", fix);

    loop {
        print!("> ");

        io::stdout().flush()?;

        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;
        let buffer = buffer.trim();

        if buffer == "quit" {
            break;
        }
        if buffer.is_empty() {
            continue;
        }

        let tokenizer = Tokenizer::new(buffer.chars());
        let tokens: Result<Vec<_>, _> = tokenizer.collect();
        let tokens = match tokens {
            Ok(tokens) => tokens,
            Err(err) => {
                eprintln!("{:?}", err);
                continue;
            }
        };

        env.eval_and_print(tokens);
    }
    Ok(())
}
