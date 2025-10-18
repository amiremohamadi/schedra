use std::cell::RefCell;
use std::collections::HashMap;

use crate::parser::ast;
use crate::parser::{
    Assignment, BinaryExpr, Expr, ExprOp, Hook, Identifier, IntegerLiteral, Node, Statement,
};
use anyhow::{Result, anyhow};
use pest::Span;

#[derive(Debug, Default)]
pub struct DispatchedTask {
    pub pid: i32,
}

pub struct Engine<'a> {
    pub statements: Vec<Statement<'a>>,
    pub arg: Identifier<'a>,
    pub vars: RefCell<HashMap<&'a str, Box<Expr<'a>>>>,
    pub global_vars: RefCell<HashMap<&'a str, Expr<'a>>>,
}

impl<'a> Engine<'a> {
    pub fn init(buf: &'a str) -> Result<Self> {
        let parsed = ast::parse(buf)?;
        let hook = parsed
            .hooks
            .into_iter()
            .find(|h| h.attach_point.name == "dequeue")
            .ok_or_else(|| anyhow!("script should contain \"dequeue\" hook"))?;

        Ok(Self {
            statements: hook.block.statements,
            arg: hook.arg,
            vars: RefCell::new(HashMap::new()),
            global_vars: RefCell::new(HashMap::new()),
        })
    }

    fn resolve_ident(&self, name: &'a str) -> i64 {
        match self.vars.borrow().get(name) {
            Some(x) => {
                match self.expr_eval(x) {
                    Expr::Integer(n) => n.value,
                    _ => unreachable!(), // should never happen
                }
            }
            None => 0,
        }
    }

    fn binary_expr_eval(&self, expr: &Box<BinaryExpr<'a>>, span: Span<'a>) -> Expr<'a> {
        let lhs = self.expr_eval(&expr.lhs);
        let rhs = self.expr_eval(&expr.rhs);
        match (lhs, rhs) {
            (Expr::Integer(l), Expr::Integer(r)) => {
                let value = match &expr.op {
                    ExprOp::Add => r.value + l.value,
                    ExprOp::Sub => r.value - l.value,
                    ExprOp::Mul => r.value * l.value,
                    ExprOp::Div => r.value / l.value,
                    ExprOp::Le => (r.value <= l.value) as i64,
                    ExprOp::Lt => (r.value < l.value) as i64,
                    ExprOp::Ge => (r.value >= l.value) as i64,
                    ExprOp::Gt => (r.value > l.value) as i64,
                    ExprOp::Eq => (r.value == l.value) as i64,
                    ExprOp::Ne => (r.value != l.value) as i64,
                    ExprOp::And => (r.value != 0 && l.value != 0) as i64,
                    ExprOp::Or => (r.value != 0 || l.value != 0) as i64,
                };
                Expr::Integer(Box::new(IntegerLiteral { value, span }))
            }
            _ => unimplemented!(), // binary expressions are not supported on other types
        }
    }

    fn expr_eval(&self, expr: &Box<Expr<'a>>) -> Expr<'a> {
        let span = expr.span();
        match &**expr {
            Expr::Identifier(ident) => {
                let value = self.resolve_ident(ident.name);
                Expr::Integer(Box::new(IntegerLiteral { value, span }))
            }
            Expr::BinaryExpr(expr) => self.binary_expr_eval(expr, span),
            e => e.clone(),
        }
    }

    fn assignment_eval(&self, assign: &Box<Assignment<'a>>) {
        let expr = self.expr_eval(&assign.rvalue);
        self.vars
            .borrow_mut()
            .insert(assign.lvalue.name, Box::new(expr));
    }

    pub fn eval(&self, task: &mut DispatchedTask) {
        let statements = &self.statements;

        for stmt in statements.iter() {
            match stmt {
                Statement::Assignment(assign) => self.assignment_eval(assign),
                _ => {} // skip
            }
        }
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
            on dequeue(task) {
                x = 12;
                anotherx = 13;
                y = x;
                z = 1 + 2 - 1 * 3;
                true = 1 || 0;
                false = 12 && 0;
            }
        "#,
        )
        .unwrap();

        let mut task = DispatchedTask::default();

        engine.eval(&mut task);
        assert_int_var!(engine, "x", 12);
        assert_int_var!(engine, "anotherx", 13);
        assert_int_var!(engine, "y", 12);
        assert_int_var!(engine, "z", 0);
        assert_int_var!(engine, "true", 1);
        assert_int_var!(engine, "false", 0);
        assert_eq!(engine.arg.name, "task");
    }
}
