use std::cell::RefCell;
use std::collections::HashMap;

use crate::parser::ast;
use crate::parser::{
    Assignment, BinaryExpr, Cond, Expr, ExprOp, Identifier, IntegerLiteral, Lvalue, Node, Object,
    Statement,
};
use anyhow::{Result, anyhow};
use pest::Span;

#[derive(Debug, Default)]
pub struct DispatchedTask {
    pub pid: i32,
}

pub enum ArgValue<'a> {
    Task(&'a DispatchedTask),
}

pub struct Arg<'a> {
    pub name: &'a str,
    pub value: HashMap<&'a str, Expr<'a>>,
    pub span: Span<'a>,
}

impl<'a> Arg<'a> {
    pub fn new(name: &'a str, span: Span<'a>) -> Self {
        Self {
            name,
            span,
            value: HashMap::new(),
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

        let mut engine = Self {
            hooks,
            vars: RefCell::new(HashMap::new()),
            global_vars: RefCell::new(HashMap::new()),
        };

        parsed.assigns.into_iter().for_each(|a| {
            engine.global_assignment_eval(a);
        });

        Ok(engine)
    }

    fn resolve_ident(&self, name: &'a str, span: Span<'a>) -> Expr<'a> {
        match self.vars.borrow().get(name) {
            Some(x) => {
                match self.expr_eval(x) {
                    Expr::Integer(n) => Expr::Integer(Box::new(IntegerLiteral {
                        value: n.value,
                        span,
                    })),
                    obj @ Expr::Object(_) => obj.clone(),
                    _ => unreachable!(), // should never happen
                }
            }
            None => Expr::Integer(Box::new(IntegerLiteral { value: 0, span })),
        }
    }

    fn binary_expr_eval(&self, expr: &Box<BinaryExpr<'a>>, span: Span<'a>) -> Expr<'a> {
        let lhs = self.expr_eval(&expr.lhs);
        let rhs = self.expr_eval(&expr.rhs);
        match (lhs, rhs) {
            (Expr::Integer(l), Expr::Integer(r)) => {
                let value = match &expr.op {
                    ExprOp::Add => l.value + r.value,
                    ExprOp::Sub => l.value - r.value,
                    ExprOp::Mul => l.value * r.value,
                    ExprOp::Div => l.value / r.value,
                    ExprOp::Le => (l.value <= r.value) as i64,
                    ExprOp::Lt => (l.value < r.value) as i64,
                    ExprOp::Ge => (l.value >= r.value) as i64,
                    ExprOp::Gt => (l.value > r.value) as i64,
                    ExprOp::Eq => (l.value == r.value) as i64,
                    ExprOp::Ne => (l.value != r.value) as i64,
                    ExprOp::And => (l.value != 0 && r.value != 0) as i64,
                    ExprOp::Or => (l.value != 0 || r.value != 0) as i64,
                };
                Expr::Integer(Box::new(IntegerLiteral { value, span }))
            }
            _ => unimplemented!(), // binary expressions are not supported on other types
        }
    }

    fn expr_eval(&self, expr: &Box<Expr<'a>>) -> Expr<'a> {
        let span = expr.span();
        match &**expr {
            Expr::Identifier(ident) => self.resolve_ident(ident.name, span),
            Expr::BinaryExpr(expr) => self.binary_expr_eval(expr, span),
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
                value: hook.arg.value.clone(),
                span: hook.arg.span,
            }));

            self.vars
                .borrow_mut()
                .insert(hook.arg.name, Box::new(value));
        }
        self.block_eval(&hook.statements)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::*;

    macro_rules! assert_int_var {
        ($engine:expr, $var:expr, $expected:expr) => {
            match $engine.vars.borrow_mut().get($var) {
                Some(boxed) => match boxed.as_ref() {
                    Expr::Integer(expr) => assert_eq!(expr.value, $expected),
                    _ => panic!("variable '{}' is not an integer", $var),
                },
                _ => panic!("variable '{}' doesn't exist", $var),
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
            }
        "#,
        )
        .unwrap();

        let mut task = DispatchedTask::default();

        assert!(engine.hook_eval("monitor").is_err());
        assert!(engine.hook_eval("dequeue").is_ok());
        assert_int_var!(engine, "x", 12);
        assert_int_var!(engine, "anotherx", 13);
        assert_int_var!(engine, "y", 12);
        assert_int_var!(engine, "z", 0);
        assert_int_var!(engine, "true", 1);
        assert_int_var!(engine, "false", 0);
        assert_int_var!(engine, "cond", 99);

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
