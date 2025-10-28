mod bpf;
mod bpf_intf;
mod bpf_skel;
mod engine;
mod parser;
mod scheduler;

use anyhow::Result;
use clap::Parser;
use engine::Engine;
use scheduler::Scheduler;
use std::mem::MaybeUninit;

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

    let mut object = MaybeUninit::uninit();
    let mut engine = Engine::init(&file)?;
    loop {
        let mut scheduler = Scheduler::new(&mut object)?;
        if !scheduler.run(&mut engine)?.should_restart() {
            break;
        }
    }

    Ok(())
}
