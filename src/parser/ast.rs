use super::*;
use anyhow::Result;
use itertools::Itertools;
use pest::{
    Parser, Span,
    iterators::Pair,
    pratt_parser::{Assoc, Op, PrattParser},
};

#[derive(pest_derive::Parser)]
#[grammar = "parser/schedra.pest"]
struct SchedraParser;

fn convert_int(pair: Pair<Rule>) -> IntegerLiteral {
    assert!(matches!(pair.as_rule(), Rule::number));
    IntegerLiteral {
        value: pair.as_str().parse().unwrap(),
        span: pair.as_span(),
    }
}

fn convert_str(pair: Pair<Rule>) -> StringLiteral {
    assert!(matches!(pair.as_rule(), Rule::string));
    let span = pair.as_span();
    StringLiteral {
        value: pair.as_str(),
        span,
    }
}

fn convert_ident(pair: Pair<Rule>) -> Identifier {
    assert!(matches!(pair.as_rule(), Rule::identifier));
    Identifier {
        name: pair.as_str(),
        span: pair.as_span(),
    }
}

fn convert_scope_access(pair: Pair<Rule>) -> ScopeAccess {
    assert!(matches!(pair.as_rule(), Rule::scope_access));
    let span = pair.as_span();

    let mut pairs = pair.into_inner();
    let name = convert_ident(pairs.next().unwrap());
    let field = convert_ident(pairs.next().unwrap());

    ScopeAccess {
        name: name.name,
        field: field.name,
        span,
    }
}

fn convert_primary_expr(pair: Pair<Rule>) -> Expr {
    assert!(matches!(pair.as_rule(), Rule::primary));
    let pair = pair.into_inner().exactly_one().unwrap();
    match pair.as_rule() {
        Rule::identifier => Expr::Identifier(Box::new(convert_ident(pair))),
        Rule::number => Expr::Integer(Box::new(convert_int(pair))),
        Rule::string => Expr::String(Box::new(convert_str(pair))),
        _ => unreachable!(),
    }
}

fn convert_expr(pair: Pair<Rule>) -> Expr {
    assert!(matches!(pair.as_rule(), Rule::expr));
    let pairs = pair.into_inner();

    let parser = PrattParser::new()
        .op(Op::infix(Rule::or, Assoc::Left))
        .op(Op::infix(Rule::and, Assoc::Left))
        .op(Op::infix(Rule::ge, Assoc::Left)
            | Op::infix(Rule::gt, Assoc::Left)
            | Op::infix(Rule::le, Assoc::Left)
            | Op::infix(Rule::lt, Assoc::Left)
            | Op::infix(Rule::eq, Assoc::Left)
            | Op::infix(Rule::ne, Assoc::Left))
        .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::sub, Assoc::Left))
        .op(Op::infix(Rule::mul, Assoc::Left) | Op::infix(Rule::div, Assoc::Left));

    parser
        .map_primary(|p| convert_primary_expr(p))
        .map_infix(|lhs, _op, rhs| {
            let span =
                Span::new(lhs.span().get_input(), lhs.span().start(), rhs.span().end()).unwrap();
            Expr::BinaryExpr(Box::new(BinaryExpr {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: ExprOp::from(_op.as_rule()),
                span,
            }))
        })
        .parse(pairs)
}

fn convert_assign_op(pair: Pair<Rule>) -> AssignOp {
    assert!(matches!(pair.as_rule(), Rule::assign_op));
    match pair.as_str() {
        "=" => AssignOp::Assign,
        _ => unreachable!(),
    }
}

fn convert_assignment(pair: Pair<Rule>) -> Assignment {
    assert!(matches!(pair.as_rule(), Rule::assignment));
    let span = pair.as_span();
    let (lvalue, op, rvalue) = pair.into_inner().collect_tuple().unwrap();

    let lvalue = match lvalue.as_rule() {
        Rule::identifier => Lvalue::Identifier(Box::new(convert_ident(lvalue))),
        Rule::scope_access => Lvalue::ScopeAccess(Box::new(convert_scope_access(lvalue))),
        _ => unreachable!(),
    };
    let _op = convert_assign_op(op);
    let rvalue = convert_expr(rvalue);

    Assignment {
        lvalue: Box::new(lvalue),
        rvalue: Box::new(rvalue),
        span,
    }
}

fn convert_cond(pair: Pair<Rule>) -> Cond {
    assert!(matches!(pair.as_rule(), Rule::condition));
    let span = pair.as_span();
    let mut pairs = pair.into_inner();

    let expr = Box::new(convert_expr(pairs.next().unwrap()));
    let body = Box::new(convert_block(pairs.next().unwrap()));

    Cond { expr, body, span }
}

fn convert_statement(pair: Pair<Rule>) -> Statement {
    assert!(matches!(pair.as_rule(), Rule::statement));
    let pair = pair.into_inner().exactly_one().unwrap();
    match pair.as_rule() {
        Rule::assignment => Statement::Assignment(Box::new(convert_assignment(pair))),
        Rule::expr => Statement::Expr(Box::new(convert_expr(pair))),
        Rule::condition => Statement::Cond(Box::new(convert_cond(pair))),
        _ => unreachable!(),
    }
}

fn convert_block(pair: Pair<Rule>) -> Block {
    assert!(matches!(pair.as_rule(), Rule::block));
    let span = pair.as_span();
    let statements = pair
        .into_inner()
        .filter_map(|p| match p.as_rule() {
            Rule::statement => Some(convert_statement(p)),
            _ => None,
        })
        .collect();
    Block { statements, span }
}

fn convert_hook(pair: Pair<Rule>) -> Hook {
    assert!(matches!(pair.as_rule(), Rule::hook));
    let span = pair.as_span();
    let mut pairs = pair.into_inner();

    let attach_point = convert_ident(pairs.next().unwrap());
    let arg = convert_ident(pairs.next().unwrap());
    let block = convert_block(pairs.next().unwrap());

    Hook {
        attach_point,
        arg,
        block,
        span,
    }
}

fn convert_prog(pair: Pair<Rule>) -> Program {
    assert!(matches!(pair.as_rule(), Rule::program));
    let span = pair.as_span();

    let mut hooks = Vec::new();
    let mut assigns = Vec::new();
    pair.into_inner().for_each(|p| match p.as_rule() {
        Rule::hook => hooks.push(convert_hook(p)),
        Rule::statement => {
            let stmt = convert_statement(p);
            match stmt {
                Statement::Assignment(a) => assigns.push(*a),
                _ => {} // TODO: handle rest of statements
            }
        }
        _ => {}
    });

    Program {
        hooks,
        assigns,
        span,
    }
}

pub fn parse(input: &str) -> Result<Program<'_>> {
    let pair = SchedraParser::parse(Rule::program, input)?
        .exactly_one()
        .map_err(|_| anyhow::anyhow!("failed to parse the program"))?;
    Ok(convert_prog(pair))
}
