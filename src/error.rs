use crate::parser::ExprOp;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::{
    emit_to_io_write,
    termcolor::{ColorChoice, StandardStream},
};
use pest::Span;
use std::fmt;

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;

#[derive(Debug)]
pub enum Type {
    Int,
    r#String,
    Object,
}

#[derive(Debug)]
pub enum Error<'a> {
    HookNotFound(String),
    UndefinedVar(Span<'a>),
    ExpectedType(Type, Span<'a>),
    OpNotSupported(Type, ExprOp, Span<'a>),
}

impl<'a> Error<'a> {
    pub fn span(&self) -> Option<Span<'a>> {
        match self {
            Self::UndefinedVar(s) => Some(*s),
            Self::ExpectedType(_, s) => Some(*s),
            Self::OpNotSupported(_, _, s) => Some(*s),
            _ => None,
        }
    }
}

impl<'a> fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::HookNotFound(name) => write!(f, "hook '{}' not found!", name)?,
            Self::UndefinedVar(span) => write!(f, "undefined variable '{}'", span.as_str())?,
            Self::ExpectedType(ty, _) => write!(f, "expected type '{:?}'", ty)?,
            Self::OpNotSupported(ty, op, _) => {
                write!(f, "operator '{:?}' not supported on type '{:?}'", op, ty)?
            }
        }
        Ok(())
    }
}

impl<'a> std::error::Error for Error<'a> {}

pub fn emit_error<'a>(err: crate::error::Error, file_name: &'a str, buf: &'a str) {
    let mut files = SimpleFiles::new();
    let id = files.add(file_name, buf);

    let config = codespan_reporting::term::Config::default();
    let writer = StandardStream::stderr(ColorChoice::Always);

    let mut diag = Diagnostic::error().with_message(&err);
    if let Some(span) = err.span() {
        diag = diag.with_labels(vec![Label::primary(id, span.start()..span.end())]);
    }
    emit_to_io_write(&mut writer.lock(), &config, &files, &diag).unwrap();
}
