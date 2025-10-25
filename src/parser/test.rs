#![cfg(test)]

use super::ast::parse;
use super::*;

fn parse_no_errors(input: &str) {
    let prog = parse(input);
    assert!(prog.is_ok(), "parse failed!");
}

#[test]
fn test_smoke() {
    parse_no_errors("");
    parse_no_errors("// this is a comment");
    parse_no_errors("on dequeue(task) {}");
    parse_no_errors(
        r#"
        on dequeue(task) {
            x = 2;
            y = 2 + 3;
            object.field_x = 69;
        }
    "#,
    );
}
