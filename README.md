# my_scheme

Simple scheme-like language written in Rust as self-educational exercise.
This is in **very early stages** of development, so the information below may be not
accurate. Not intended for serious use.

## Syntax
The goal syntax would be something like this:

- `1`, `-1`, `1.0`, `-1.0` - numeric constants,
- `"abc"` - string,
- `'(1 2 3)` - list,
- `'abc` - symbol,
- `abc` - identifier,
- `()` - procedure application, except in some special forms,
- `;` - line comment,
- `#` - pattern,
- `\` - escape.

Reserved characters, that are not allowed as part of names of variables, neither
by itself, nor in escaped form:
- `(`, `)` - used for procedure application, lists,
- `'` - used for quoting identifiers, and list construction,
- `#` - used for patterns,
- `;` - used for comments,
- `"` - used as a string delimiter,
- `\` - used to escape string delimiter in strings.

Language should support variables, anonymous procedures, calling procedures
through identifiers.

### Procedure creation
All procedures are anonymous procedures and are created with `lambda` procedure:

```
> (lambda (argument list) body)
```

`lambda` accepts list of arguments with the first set of parenthesis, the rest
expressions are treated as body, and returns a `#procedure`. For example
procedure which computes square of number `x` is defined like this:

```
> (lambda (x) (* x x))
=> #procedure
```

We can apply procedure using extra set of parentheses around it and by providing
argument:

```
> ((lambda (x) (* x x)) 8)
=> 64
```

The expression `(lambda (x) (* x x)` creates procedure with local variable `x`
that exists only inside the scope of the procedure. So if procedure
returns another procedure, which uses the `x` variable, it will be stored inside
returned procedure as value. For example:

```
> (((lambda (x) (lambda (y) (+ x y)) 1) 2)
3
```

When we apply `1` to the first lambda expression `x` will be holding the `1`
value, and will be stored inside the `#procedure` returned by `(lambda (y) (+ x
y))` as `1` thus making it `(lambda (y) (+ 1 y))`. When we apply `2` to the
resulting `#procedure` it will compute `(+ 1 2)`.

### Variable definition
Variables are defined with the `define` procedure, which takes two arguments as
an input and produces no output. This creates two variables in global scope:

```
> (define abc 123)
> (define def "def")
```

We can not use numeric constants, strings, reserved characters, and whitespace
to define variable names.

```
> (define -1 -1)
#error
> (define "-1" -1)
#error
> (define my variable 22)
#error
```

The `define` procedure is scope aware, and creates identifiers inside the scope
of the caller:

```
> (lambda (x)
    (define var 10)
    (+ x var))
> var
=> error, var undefined
```

variables can store any kind of information: values, strings, procedures,
symbols, patterns:

```
> (define var1 22)
> var1
22
> (define var2 "22")
> var2
"22"
```

When variables are holding `#procedure` those also can be used for procedure
application:

```
> (define square (lambda (x) (* x x)))
> square
#procedure
> (square 4)
16
```

## Special forms
Scheme and Lisps feature some special forms, such as `if` `let` `cond` that can
have some parentheses represent not what those usually do. Because `let` and
`cond` require additional parsing rules, so their form may be changed in order
to make parsing simpler. Some forms were added in addition, like `match` to
provide more flexible branching of the program.

### The `if` procedure
Used for branching the program. The syntax is:

```
(if <boolean>
    <true expression>
  <false expression 1>
  ...
  <false expression N>)
```

`if` procedure accepts `boolean` or as first argument. Booleans are written as
patterns: `#t` `#f`, everything that is not `#t` is treated as `#f`. For
example, comparison of two numbers returns `boolean`:

```
> (= 1 2)
#f
> (= 2 2)
#t
> (if (= 1 2)
      "wrong"
    "ok")
"ok"
```

If false arm holds several expressions, those evaluated in order, and the result
of the last one is returned:

```
> (if (= 1 2)
      "wrong"
    "okay"
    "ok")
"ok"
```

Because only one of `if` arms is evaluated this is a special form.

### The `cond` procedure
`cond` is similar to `if` but has more than two arms. `cond` checks if current arm
holds `#t` in the place of the first argument, and if it is `#t` the rest of the
code in the arm is executed, and `cond` returns the value of last executed
expression as the result. If arm is holding `#f` it doesn't get executed, and
the next arm is evaluated, thus only first arm which holds `#t` will be
evaluated. The syntax is:

```
(cond (<boolean arm 1>
       <expression 1>
       ...
       <expression N>)
      ...
      (<boolean arm 2>
       <expression 1>
       ...
       <expression N>)
```

For example:

```
> (cond ((= 1 2) "what?")
        ((not (= 1 1)) "WHAT???")
        (#t "all good"))
"all good"
```

Because `cond` uses parentheses in special way, and arms are executed only when
it holds `#t`, similarly to `if`, `cond` is a special form.

### The `match` procedure
Similarly to `cond`, `match` is used for matching patterns. But `match` takes an
identifier as it's first argument, and matches it across arms, that hold
`pattern` as a first argument:

```
(match <id> (<pattern 1>
             <expression 1>
             ...
             <expression N>)
            ...
            (<pattern N>
             <expression 1>
             ...
             <expression N>))
```

For example:

```
> (define a (lambda (x) (+ x x)))
> (match a (#procedure
            "a is procedure")
           (#any
            "a is something else"))
"a is procedure"
```

Unlike `if` or `cond`, `match` directly compares identifiers pattern to the arm
pattern, so in order to match anything, special `#any` pattern must be used.

Because `match` requires accessing identifier internal information it is a
special form.

### The `recur` procedure
In order to provide recursion one must use special form called `recur`. This
form allows calling `lambda` in terms of itself:

```
> ((lambda (x) (if (= x 0) "end" (recur (- x 1)))) 10)
"end"
```

If called in tail position should provide tail call optimized recursion without
stack overflow.

### The `let` procedure
`let` is used to create local bindings. `let` form should be equal to the `letrec`
form by default. The syntax is:

```
(let (<binding 1>
      <binding 2>
      ...
      <binding N)
  <body expression 1>
  ...
  <body expression N)
```

For example:

```
> (let ((var1 10)
        (var2 (+ var1 10)))
    (+ var1 var2))
30
    ```

## Patterns and Pattern Matching
Patterns describe what identifier is like. Those should be used for branching
with `match` special form. Patterns can be thought as types, but not quite,
because those also express some other concepts, like `true` and `false` are
being expressed through patterns, `#t` and `#f`. Identifiers that hold
procedures have the `#procedure` pattern.

Matching on a pattern can be useful to detect errors such as calling
non-procedure holding identifier as procedure:

```
> (define var "I'm not a procedure")
> (match var (#procedure
              (var 10))
             (#any
              "expected a procedure here!!"))
"expected a procedure here!!"
```

At some point it is possible that language will allow creating user-defined
patterns, and bind them to values.
