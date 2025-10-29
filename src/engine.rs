use std::cell::RefCell;
use std::collections::HashMap;

use crate::parser::ast;
use crate::parser::{
    Assignment, BinaryExpr, Cond, Expr, ExprOp, IntegerLiteral, Lvalue, Node, Object, ScopeAccess,
    Statement,
};
use crate::scheduler::Scheduler;
use anyhow::{Result, anyhow};
use pest::Span;

pub struct Arg<'a> {
    pub name: &'a str,
    pub value: RefCell<HashMap<&'a str, Expr<'a>>>,
    pub span: Span<'a>,
}

impl<'a> Arg<'a> {
    pub fn new(name: &'a str, span: Span<'a>) -> Self {
        Self {
            name,
            span,
            value: RefCell::new(HashMap::new()),
        }
    }
}

pub struct Hook<'a> {
    pub statements: Vec<Statement<'a>>,
    pub arg: Arg<'a>,
}

pub struct Engine<'a> {
    pub hooks: HashMap<&'a str, Hook<'a>>,
    // TODO: bound variables to scopes
    pub vars: RefCell<HashMap<&'a str, Box<Expr<'a>>>>,
    pub global_vars: RefCell<HashMap<&'a str, Expr<'a>>>,
    pub builtins: HashMap<&'a str, u64>,
}

impl<'a> Engine<'a> {
    pub fn init(buf: &'a str) -> Result<Self> {
        let mut hooks = HashMap::new();

        let parsed = ast::parse(buf)?;
        parsed.hooks.into_iter().for_each(|h| {
            let arg = Arg::new(h.arg.name, h.arg.span());
            hooks.insert(
                h.attach_point.name,
                Hook {
                    arg,
                    statements: h.block.statements,
                },
            );
        });

        let engine = Self {
            hooks,
            vars: RefCell::new(HashMap::new()),
            global_vars: RefCell::new(HashMap::new()),
            builtins: HashMap::from([("RL_CPU_ANY", 1 << 20)]),
        };

        parsed.assigns.into_iter().for_each(|a| {
            engine.global_assignment_eval(a);
        });

        Ok(engine)
    }

    pub fn get_hook(&self, name: &'a str) -> &Hook<'a> {
        self.hooks.get(name).unwrap() // FIXME
    }

    fn resolve_ident(&self, name: &'a str, span: Span<'a>) -> Expr<'a> {
        match self.vars.borrow().get(name) {
            Some(x) => self.expr_eval(x),
            None => match self.builtins.get(name) {
                // TODO: currently we consider all the builtins as integers
                // should be generic in the future
                Some(&x) => Expr::Integer(Box::new(IntegerLiteral {
                    value: x as _,
                    span,
                })),
                None => Expr::Integer(Box::new(IntegerLiteral { value: 0, span })),
            },
        }
    }

    fn binary_expr_eval(&self, expr: &Box<BinaryExpr<'a>>, span: Span<'a>) -> Expr<'a> {
        let lhs = self.expr_eval(&expr.lhs);
        let rhs = self.expr_eval(&expr.rhs);
        match (lhs, rhs) {
            (Expr::Integer(l), Expr::Integer(r)) => Expr::Integer(Box::new(IntegerLiteral {
                value: Self::eval_int_op(&expr.op, l.value, r.value),
                span,
            })),
            (Expr::String(l), Expr::String(r)) => {
                Self::eval_str_op(&expr.op, &l.value, &r.value, span)
            }
            _ => unimplemented!(), // binary expressions are not supported on other types
        }
    }

    fn eval_int_op(op: &ExprOp, l: i64, r: i64) -> i64 {
        match op {
            ExprOp::Add => l + r,
            ExprOp::Sub => l - r,
            ExprOp::Mul => l * r,
            ExprOp::Div => l / r,
            ExprOp::Le => (l <= r) as i64,
            ExprOp::Lt => (l < r) as i64,
            ExprOp::Ge => (l >= r) as i64,
            ExprOp::Gt => (l > r) as i64,
            ExprOp::Eq => (l == r) as i64,
            ExprOp::Ne => (l != r) as i64,
            ExprOp::And => (l != 0 && r != 0) as i64,
            ExprOp::Or => (l != 0 || r != 0) as i64,
        }
    }

    fn eval_str_op(op: &ExprOp, l: &str, r: &str, span: Span<'a>) -> Expr<'a> {
        match op {
            ExprOp::Eq => Expr::Integer(Box::new(IntegerLiteral {
                value: (l == r) as i64,
                span,
            })),
            ExprOp::Ne => Expr::Integer(Box::new(IntegerLiteral {
                value: (l != r) as i64,
                span,
            })),
            _ => unimplemented!(),
        }
    }

    fn scope_expr_eval(&self, scop: &Box<ScopeAccess<'a>>) -> Expr<'a> {
        match self.vars.borrow().get(scop.name) {
            Some(x) => match &**x {
                Expr::Object(o) => match o.value.get(scop.field) {
                    Some(v) => v.clone(),
                    None => Expr::Integer(Box::new(IntegerLiteral {
                        value: 0,
                        span: scop.span(),
                    })),
                },
                _ => unreachable!(),
            },
            // TODO: should raise error
            None => Expr::Integer(Box::new(IntegerLiteral {
                value: 0,
                span: scop.span(),
            })),
        }
    }

    fn expr_eval(&self, expr: &Box<Expr<'a>>) -> Expr<'a> {
        let span = expr.span();
        match &**expr {
            Expr::Identifier(ident) => self.resolve_ident(ident.name, span),
            Expr::BinaryExpr(expr) => self.binary_expr_eval(expr, span),
            Expr::ScopeAccess(scop) => self.scope_expr_eval(scop),
            e => e.clone(),
        }
    }

    fn global_assignment_eval(&self, assign: Assignment<'a>) {
        let expr = self.expr_eval(&assign.rvalue);
        let name = match *assign.lvalue {
            Lvalue::Identifier(ident) => ident.name,
            _ => unreachable!(),
        };
        self.global_vars.borrow_mut().insert(name, expr);
    }

    fn assignment_eval(&self, assign: &Box<Assignment<'a>>) {
        let expr = self.expr_eval(&assign.rvalue);

        match &*assign.lvalue {
            Lvalue::Identifier(ident) => {
                if self.global_vars.borrow().contains_key(ident.name) {
                    self.global_vars.borrow_mut().insert(ident.name, expr);
                } else {
                    self.vars.borrow_mut().insert(ident.name, Box::new(expr));
                }
            }
            Lvalue::ScopeAccess(sc) => {
                if let Some(e) = self.vars.borrow_mut().get_mut(sc.name) {
                    if let Expr::Object(obj) = &mut **e {
                        obj.value.insert(sc.field, expr);
                    }
                }
            }
        }
    }

    fn cond_eval(&self, cond: &Box<Cond<'a>>) -> Result<()> {
        let expr = self.expr_eval(&cond.expr);
        match expr {
            Expr::Integer(n) => {
                if n.value != 0 {
                    self.block_eval(&cond.body.statements)?;
                }
            }
            Expr::String(n) => {
                if n.value != "" {
                    self.block_eval(&cond.body.statements)?;
                }
            }
            _ => {} // TODO: support more types
        }
        Ok(())
    }

    fn block_eval(&self, statements: &Vec<Statement<'a>>) -> Result<()> {
        for stmt in statements.iter() {
            match stmt {
                Statement::Assignment(asgn) => self.assignment_eval(asgn),
                Statement::Cond(cond) => self.cond_eval(cond)?,
                _ => {} // skip
            }
        }
        Ok(())
    }

    pub fn hook_eval(&self, name: &'a str) -> Result<()> {
        let hook = match self.hooks.get(name) {
            Some(h) => h,
            None => return Err(anyhow!("hook '{}' not found!", name)),
        };
        // initialize hook argument
        {
            let value = Expr::Object(Box::new(Object {
                value: hook.arg.value.borrow().clone(),
                span: hook.arg.span,
            }));

            self.vars
                .borrow_mut()
                .insert(hook.arg.name, Box::new(value));
        }
        self.block_eval(&hook.statements)
    }

    pub fn eval(&self, hook: &Hook<'a>) -> Result<HashMap<&'a str, Expr<'a>>> {
        // initialize hook argument
        let value = Expr::Object(Box::new(Object {
            value: hook.arg.value.borrow().clone(),
            span: hook.arg.span,
        }));

        self.vars
            .borrow_mut()
            .insert(hook.arg.name, Box::new(value));

        self.block_eval(&hook.statements)?;

        let vars = self.vars.borrow();
        let Expr::Object(arg) = &**vars.get(hook.arg.name).unwrap() else {
            return Err(anyhow!("arg should be an object"));
        };
        Ok(arg.value.to_owned())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::*;

    enum Expected<'a> {
        Int(i64),
        Str(&'a str),
    }

    macro_rules! assert_var {
        ($engine:expr, $var:expr, $expected:expr) => {
            match $engine.vars.borrow_mut().get($var) {
                Some(boxed) => match boxed.as_ref() {
                    Expr::Integer(expr) => match $expected {
                        Expected::Int(v) => {
                            assert_eq!(expr.value, v, "variable '{}' mismatch", $var)
                        }
                        _ => panic!("Expected type mismatch for variable '{}'", $var),
                    },
                    Expr::String(expr) => match $expected {
                        Expected::Str(v) => {
                            assert_eq!(expr.value, v, "variable '{}' mismatch", $var)
                        }
                        _ => panic!("Expected type mismatch for variable '{}'", $var),
                    },
                    _ => panic!("variable '{}' has unsupported type", $var),
                },
                None => panic!("variable '{}' doesn't exist", $var),
            }
        };
    }

    #[test]
    fn test_smoke() {
        let mut engine = Engine::init(
            r#"
            global_var1 = 69;

            on dequeue(task) {
                x = 12;
                anotherx = 13;
                name = "schedra";

                y = x; // comment
                z = 1 + 2 - 1 * 3;
                true = 1 || 0;
                false = 12 && 0;

                global_var1 = 55;

                task.dispatch = 1;
                task.weight = 10;

                if x > 1 {
                    cond = 99;
                }

                if name == "schedra" {
                    cond_str = 10;
                }

                if task.dispatch {
                    cond_scope = 15;
                }
            }
        "#,
        )
        .unwrap();

        assert!(engine.hook_eval("monitor").is_err());
        assert!(engine.hook_eval("dequeue").is_ok());
        assert_var!(engine, "x", Expected::Int(12));
        assert_var!(engine, "anotherx", Expected::Int(13));
        assert_var!(engine, "name", Expected::Str("schedra"));
        assert_var!(engine, "y", Expected::Int(12));
        assert_var!(engine, "z", Expected::Int(0));
        assert_var!(engine, "true", Expected::Int(1));
        assert_var!(engine, "false", Expected::Int(0));
        assert_var!(engine, "cond", Expected::Int(99));
        assert_var!(engine, "cond_str", Expected::Int(10));
        assert_var!(engine, "cond_scope", Expected::Int(15));

        let global_vars = engine.global_vars.borrow();
        match global_vars.get("global_var1") {
            Some(Expr::Integer(e)) => assert_eq!(e.value, 55),
            _ => panic!("global variable not found."),
        }

        let hook = engine.hooks.get("dequeue").unwrap();
        assert_eq!(hook.arg.name, "task");
        match engine.vars.borrow().get(hook.arg.name) {
            Some(arg) => match arg.as_ref() {
                Expr::Object(obj) => {
                    let values = obj.value.values().collect::<Vec<_>>();
                    assert_eq!(values.len(), 2);
                }
                _ => panic!("arg is not an object!"),
            },
            _ => panic!("hook arg not found!"),
        }
    }
}
