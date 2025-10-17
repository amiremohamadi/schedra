use std::cell::RefCell;
use std::collections::HashMap;

use crate::parser::ast;
use crate::parser::{Assignment, Expr, Hook, Statement};
use anyhow::{Result, anyhow};

#[derive(Debug, Default)]
pub struct DispatchedTask {
    pub pid: i32,
}

pub struct Engine<'a> {
    pub statements: Vec<Statement<'a>>,
    pub variables: RefCell<HashMap<&'a str, Expr<'a>>>,
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
            variables: RefCell::new(HashMap::new()),
        })
    }

    fn assignment_eval(&self, assign: &Box<Assignment<'a>>) {
        // FIXME: avoid cloning
        self.variables
            .borrow_mut()
            .insert(assign.lvalue.name, *assign.rvalue.clone());
    }

    pub fn eval(&self, task: &mut DispatchedTask) {
        let statements = &self.statements;

        for stmt in statements.iter() {
            match stmt {
                Statement::Assignment(assign) => self.assignment_eval(assign),
                _ => {}
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
            match $engine.variables.borrow_mut().get($var) {
                Some(Expr::Integer(expr)) => {
                    assert_eq!(expr.value, $expected);
                }
                _ => panic!("variable '{}' is not an integer", $var),
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
            }
        "#,
        )
        .unwrap();

        let mut task = DispatchedTask::default();

        engine.eval(&mut task);
        assert_int_var!(engine, "x", 12);
        assert_int_var!(engine, "anotherx", 13);
    }
}
