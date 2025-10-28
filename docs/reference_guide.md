## Usage
```sh
$ schedra <FILENAME>
```

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

### Builtins
TODO
