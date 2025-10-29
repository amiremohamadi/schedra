## Usage
```sh
$ schedra <FILENAME>
```
a program will continue running until ctrl-c is hit.

## Language
each script consists of one or more hooks.
hooks look like this:

```
on hook(arg) { action }
```

currently, only the dequeue hook is supported. the argument passed to dequeue represents the task being dequeued from the scheduler.

the most basic example of a schedra program:
```
on dequeue(t) {}
```

### Comments
single line comments are supported.
```
// comment1
// comment2
a = 2; // comment3
```

### Conditionals
conditional expressions are supported in the form of if statements. (right! we don't support else blocks yet.)
```
x = 0;
if (x >= 0) {
  y = 1;
}
```

### Data Types
to keep the language simple, we just provide three types:

- string literals
- int64 numbers
- objects (only for the args passed to the hook, user can't define objects yet)

any types of conversion will be applied by the schedra, if needed. you don't need to be worried about data types.

### Expressions
the following operators are available for integer arithmetic:
| operator | description          |
|-----------|----------------------|
| `+`       | integer addition     |
| `-`       | integer subtraction  |
| `*`       | integer multiplication |
| `/`       | integer division     |
| `%`       | integer modulo       |

and logical operators:

| operator | description     |
|-----------|----------------|
| `&&`      | logical and    |
| `||`      | logical or     |
| `!`       | logical not    |

relational operators:

| operator | description                                      |
|-----------|--------------------------------------------------|
| `<`       | left-hand expression is less than right-hand     |
| `<=`      | left-hand expression is less than or equal to right-hand |
| `>`       | left-hand expression is greater than right-hand  |
| `>=`      | left-hand expression is greater than or equal to right-hand |
| `==`      | left-hand expression is equal to right-hand      |
| `!=`      | left-hand expression is not equal to right-hand  |

bitwise operators:

| operator | description     |
|-----------|----------------|
| `<<`      | shift left     |
| `>>`      | shift right    |
| `&`       | bitwise and    |
| `|`       | bitwise or     |

assignment operators:

| operator | description                                      |
|-----------|--------------------------------------------------|
| `=`       | assign the right-hand expression to the left-hand variable |

```
a = 12;
b = "string";
```

field access for the objects:
```
on dequeue(task) {
  task.field1 = 69;
  if task.field2 > 10 { ... }
}
```

### Hooks
currently only `dequeue` hook is supported. it receives an object type argument with the following fields:

| field              | description                                                     |
|--------------------|-----------------------------------------------------------------|
| `pid`              | pid that uniquely identifies a task                              |
| `cpu`              | cpu previously used by the task                                  |
| `nr_cpus_allowed`  | number of cpus that the task can use                             |
| `flags`            | task's enqueue flags                                             |
| `start_ts`         | timestamp since last time the task ran on a cpu (in ns)          |
| `stop_ts`          | timestamp since last time the task released a cpu (in ns)        |
| `exec_runtime`     | total cpu time since last sleep (in ns)                          |
| `weight`           | task priority in the range [1..10000] (default is 100)           |
| `vtime`            | current task vruntime / deadline (set by the scheduler)          |

and the following fields can be modified

| field        | description                                                                                   |
|---------------|-----------------------------------------------------------------------------------------------
| `pid`         | pid that uniquely identifies a task                                                          |
| `cpu`         | target cpu selected by the scheduler (RL_CPU_ANY = dispatch on the first cpu available)      |
| `flags`       | task's enqueue flags                                                                         |
| `slice_ns`    | time slice in nanoseconds assigned to the task (0 = use default time slice)                  |
| `vtime`       | value used to send the task's vruntime or deadline directly to the dispatcher                |

example:
```
on dequeue(t) {
  if t.pid == 9 {
    t.cpu = 3;
  }
  if t.pid != 9 {
    t.cpu = RL_CPU_ANY;
  }
  t.vtime = 10;
}
```

### Builtins

| built-in variable        | description |
|--------------------------|------------|
| nr_queued                | number of tasks currently queued |
| nr_running               | number of tasks currently running |
| nr_online_cpus           | number of online cpus |
| nr_scheduled             | total number of scheduled tasks |
| nr_user_dispatches       | number of user-level dispatches |
| nr_kernel_dispatches     | number of kernel-level dispatches |
| nr_cancel_dispatches     | number of canceled dispatches |
| nr_bounce_dispatches     | number of bounced dispatches |
| nr_failed_dispatches     | number of failed dispatches |
| nr_sched_congested       | number of congested scheduler events |
