use std::{
    collections::HashMap, mem::MaybeUninit, sync::Arc, sync::atomic::AtomicBool,
    sync::atomic::Ordering,
};

use anyhow::Result;
use libbpf_rs::OpenObject;
use pest::Span;
use scx_utils::UserExitInfo;

use crate::bpf::*;
use crate::bpf_skel::*;
use crate::engine::{Engine, Hook};
use crate::parser::{Expr, IntegerLiteral};

fn init_hook_arg<'a>(task: &QueuedTask, span: &Span<'a>) -> HashMap<&'a str, Expr<'a>> {
    [(
        "pid",
        Expr::Integer(Box::new(IntegerLiteral {
            value: task.pid.into(),
            span: span.clone(),
        })),
    )]
    .into_iter()
    .collect::<HashMap<_, _>>()
}

pub struct Scheduler<'a> {
    bpf_scheduler: BpfScheduler<'a>,
}

impl<'a> Scheduler<'a> {
    pub fn new(obj: &'a mut MaybeUninit<OpenObject>) -> Result<Self> {
        let bpf_scheduler = BpfScheduler::init(obj, None, 0, false, false, false, "Schedra")?;
        Ok(Self { bpf_scheduler })
    }

    fn schedule(&mut self, engine: &mut Engine) -> Result<()> {
        while let Ok(Some(task)) = self.bpf_scheduler.dequeue_task() {
            let hook = engine.get_hook("dequeue");
            *hook.arg.value.borrow_mut() = init_hook_arg(&task, &hook.arg.span);

            let arg = engine.eval(hook)?;

            let mut task = DispatchedTask::new(&task);

            for (key, val) in arg {
                match key {
                    "slice" => {}
                    _ => {}
                }
            }

            self.bpf_scheduler.dispatch_task(&task).unwrap();
        }

        self.bpf_scheduler.notify_complete(0);
        Ok(())
    }

    pub fn run(&mut self, engine: &mut Engine<'_>) -> Result<UserExitInfo> {
        println!("Running...");

        while !self.bpf_scheduler.exited() {
            if let Err(e) = self.schedule(engine) {
                break;
            }
        }
        self.bpf_scheduler.shutdown_and_report()
    }
}
