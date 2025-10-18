mod engine;
mod parser;

use anyhow::Result;
use clap::Parser;
use engine::Engine;

#[derive(Debug, Parser)]
#[clap(author, version)]
pub struct Args {
    pub file: String,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let file = match std::fs::read_to_string(&args.file) {
        Ok(c) => c,
        _ => panic!("could not find the file '{}'", &args.file),
    };

    let engine = Engine::init(&file)?;

    Ok(())
}
