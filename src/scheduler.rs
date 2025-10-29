use std::{collections::HashMap, mem::MaybeUninit};

use anyhow::{Result, anyhow};
use libbpf_rs::OpenObject;
use pest::Span;
use scx_utils::UserExitInfo;

use crate::bpf::*;
use crate::engine::Engine;
use crate::parser::{Expr, IntegerLiteral};

macro_rules! convert_expr {
    ($expr:expr) => {{
        match $expr {
            Expr::Integer(b) => {
                let IntegerLiteral { value, .. } = *b;
                Ok(value)
            }
            _ => Err(anyhow!("expected Expr::Int")),
        }
    }};
}

fn init_hook_arg<'a>(task: &QueuedTask, span: &Span<'a>) -> HashMap<&'a str, Expr<'a>> {
    [
        (
            "pid",
            Expr::Integer(Box::new(IntegerLiteral {
                value: task.pid as _,
                span: span.clone(),
            })),
        ),
        (
            "cpu",
            Expr::Integer(Box::new(IntegerLiteral {
                value: task.cpu as _,
                span: span.clone(),
            })),
        ),
        (
            "nr_cpus_allowed",
            Expr::Integer(Box::new(IntegerLiteral {
                value: task.nr_cpus_allowed as _,
                span: span.clone(),
            })),
        ),
        (
            "flags",
            Expr::Integer(Box::new(IntegerLiteral {
                value: task.flags as _,
                span: span.clone(),
            })),
        ),
        (
            "start_ts",
            Expr::Integer(Box::new(IntegerLiteral {
                value: task.start_ts as _,
                span: span.clone(),
            })),
        ),
        (
            "stop_ts",
            Expr::Integer(Box::new(IntegerLiteral {
                value: task.stop_ts as _,
                span: span.clone(),
            })),
        ),
        (
            "exec_runtime",
            Expr::Integer(Box::new(IntegerLiteral {
                value: task.exec_runtime as _,
                span: span.clone(),
            })),
        ),
        (
            "weight",
            Expr::Integer(Box::new(IntegerLiteral {
                value: task.weight as _,
                span: span.clone(),
            })),
        ),
        (
            "vtime",
            Expr::Integer(Box::new(IntegerLiteral {
                value: task.vtime as _,
                span: span.clone(),
            })),
        ),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>()
}

pub struct Scheduler<'a> {
    pub bpf_scheduler: BpfScheduler<'a>,
}

impl<'a> Scheduler<'a> {
    pub fn new(obj: &'a mut MaybeUninit<OpenObject>) -> Result<Self> {
        let bpf_scheduler = BpfScheduler::init(obj, None, 0, false, false, false, "Schedra")?;
        Ok(Self { bpf_scheduler })
    }

    fn schedule(&mut self, engine: &mut Engine) -> Result<()> {
        self.register_builtins(engine); // FIXME: performance costs

        while let Ok(Some(task)) = self.bpf_scheduler.dequeue_task() {
            let hook = engine.get_hook("dequeue");
            *hook.arg.value.borrow_mut() = init_hook_arg(&task, &hook.arg.span);

            let arg = engine.eval(hook)?;

            let mut task = DispatchedTask::new(&task);
            let mut dispatch = true;
            for (key, val) in arg {
                match key {
                    "pid" => task.pid = convert_expr!(val)? as _,
                    "cpu" => task.cpu = convert_expr!(val)? as _,
                    "flags" => task.flags = convert_expr!(val)? as _,
                    "slice_ns" => task.slice_ns = convert_expr!(val)? as _,
                    "vtime" => task.vtime = convert_expr!(val)? as _,
                    "dispatch" => dispatch = convert_expr!(val)? != 0,
                    _ => {}
                }
            }

            if dispatch {
                self.bpf_scheduler.dispatch_task(&task).unwrap();
            }
        }

        self.bpf_scheduler.notify_complete(0);
        Ok(())
    }

    fn register_builtins(&mut self, engine: &mut Engine) {
        engine
            .builtins
            .insert("nr_queued", *self.bpf_scheduler.nr_queued_mut());
        engine
            .builtins
            .insert("nr_running", *self.bpf_scheduler.nr_running_mut());
        engine
            .builtins
            .insert("nr_online_cpus", *self.bpf_scheduler.nr_online_cpus_mut());
        engine
            .builtins
            .insert("nr_scheduled", *self.bpf_scheduler.nr_scheduled_mut());
        engine.builtins.insert(
            "nr_user_dispatches",
            *self.bpf_scheduler.nr_user_dispatches_mut(),
        );
        engine.builtins.insert(
            "nr_kernel_dispatches",
            *self.bpf_scheduler.nr_kernel_dispatches_mut(),
        );
        engine.builtins.insert(
            "nr_cancel_dispatches",
            *self.bpf_scheduler.nr_cancel_dispatches_mut(),
        );
        engine.builtins.insert(
            "nr_bounce_dispatches",
            *self.bpf_scheduler.nr_bounce_dispatches_mut(),
        );
        engine.builtins.insert(
            "nr_failed_dispatches",
            *self.bpf_scheduler.nr_failed_dispatches_mut(),
        );
        engine.builtins.insert(
            "nr_sched_congested",
            *self.bpf_scheduler.nr_sched_congested_mut(),
        );
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
