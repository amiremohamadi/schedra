pub mod ast;
mod test;

use crate::parser::ast::Rule;
use pest::Span;
use std::iter::FilterMap;

pub trait Node<'a> {
    fn as_node(&self) -> &dyn Node<'a>;
    fn span(&self) -> Span<'a>;

    fn as_statement(&self) -> Option<&Statement<'a>> {
        None
    }

    fn as_expr(&self) -> Option<&Expr<'a>> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct Identifier<'a> {
    pub name: &'a str,
    pub span: Span<'a>,
}

impl<'a> Node<'a> for Identifier<'a> {
    fn as_node(&self) -> &dyn Node<'a> {
        self
    }

    fn span(&self) -> Span<'a> {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteral<'a> {
    pub value: &'a str,
    pub span: Span<'a>,
}

impl<'a> Node<'a> for StringLiteral<'a> {
    fn as_node(&self) -> &dyn Node<'a> {
        self
    }

    fn span(&self) -> Span<'a> {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral<'a> {
    pub value: i64,
    pub span: Span<'a>,
}

impl<'a> Node<'a> for IntegerLiteral<'a> {
    fn as_node(&self) -> &dyn Node<'a> {
        self
    }

    fn span(&self) -> Span<'a> {
        self.span
    }
}

#[derive(Debug, Clone)]
pub enum ExprOp {
    Add,
    Sub,
    Mul,
    Div,
    Le,
    Lt,
    Ge,
    Gt,
    Eq,
    Ne,
    And,
    Or,
}

impl ExprOp {
    pub fn from(r: Rule) -> Self {
        match r {
            Rule::add => Self::Add,
            Rule::sub => Self::Sub,
            Rule::mul => Self::Mul,
            Rule::div => Self::Div,
            Rule::le => Self::Le,
            Rule::lt => Self::Lt,
            Rule::ge => Self::Ge,
            Rule::gt => Self::Gt,
            Rule::eq => Self::Eq,
            Rule::ne => Self::Ne,
            Rule::and => Self::And,
            Rule::or => Self::Or,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinaryExpr<'a> {
    pub lhs: Box<Expr<'a>>,
    pub rhs: Box<Expr<'a>>,
    pub op: ExprOp,
    pub span: Span<'a>,
}

impl<'a> Node<'a> for BinaryExpr<'a> {
    fn as_node(&self) -> &dyn Node<'a> {
        self
    }

    fn span(&self) -> Span<'a> {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct UnaryExpr<'a> {
    pub expr: Box<Expr<'a>>,
    pub span: Span<'a>,
}

impl<'a> Node<'a> for UnaryExpr<'a> {
    fn as_node(&self) -> &dyn Node<'a> {
        self
    }

    fn span(&self) -> Span<'a> {
        self.span
    }
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Identifier(Box<Identifier<'a>>),
    Integer(Box<IntegerLiteral<'a>>),
    String(Box<StringLiteral<'a>>),
    BinaryExpr(Box<BinaryExpr<'a>>),
    UnaryExpr(Box<UnaryExpr<'a>>),
}

impl<'a> Node<'a> for Expr<'a> {
    fn as_node(&self) -> &dyn Node<'a> {
        self
    }

    fn span(&self) -> Span<'a> {
        match self {
            Self::Integer(n) => n.span(),
            Self::String(s) => s.span(),
            Self::Identifier(ident) => ident.span(),
            Self::BinaryExpr(expr) => expr.span(),
            Self::UnaryExpr(expr) => expr.span(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
}

#[derive(Debug, Clone)]
pub struct Assignment<'a> {
    pub lvalue: Box<Identifier<'a>>,
    pub rvalue: Box<Expr<'a>>,
    pub span: Span<'a>,
}

impl<'a> Node<'a> for Assignment<'a> {
    fn as_node(&self) -> &dyn Node<'a> {
        self
    }

    fn span(&self) -> Span<'a> {
        self.span
    }
}

#[derive(Debug, Clone)]
pub enum Statement<'a> {
    Assignment(Box<Assignment<'a>>),
    Expr(Box<Expr<'a>>),
}

impl<'a> Node<'a> for Statement<'a> {
    fn as_node(&self) -> &dyn Node<'a> {
        self
    }

    fn span(&self) -> Span<'a> {
        match self {
            Self::Assignment(assign) => assign.span(),
            Self::Expr(e) => e.span(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Hook<'a> {
    pub attach_point: Identifier<'a>,
    pub arg: Identifier<'a>,
    pub block: Block<'a>,
    pub span: Span<'a>,
}

impl<'a> Node<'a> for Hook<'a> {
    fn as_node(&self) -> &dyn Node<'a> {
        self
    }

    fn span(&self) -> Span<'a> {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct Block<'a> {
    pub statements: Vec<Statement<'a>>,
    pub span: Span<'a>,
}

impl<'a> Node<'a> for Block<'a> {
    fn as_node(&self) -> &dyn Node<'a> {
        self
    }

    fn span(&self) -> Span<'a> {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct Program<'a> {
    pub hooks: Vec<Hook<'a>>,
    pub span: Span<'a>,
}

impl<'a> Node<'a> for Program<'a> {
    fn as_node(&self) -> &dyn Node<'a> {
        self
    }

    fn span(&self) -> Span<'a> {
        self.span
    }
}
