# my_scheme

Simple scheme-like language written in Rust as self-educational exercise.  This
is in **very very VERY early stages** of development, so the information below
may be not accurate. Not intended for serious use.

## Goals
I do not have CS grade, and did not studied programming language theory. I do
have some experience in various programming languages though. I've worked mainly
with C and C++, and studied or used for hobby project such languages as Rust,
Perl, POSIX shell scripting, Awk, Emacs Lisp, GNU Guile, Racket and maybe
something else.

This is my experiment in writing a language without knowing much about how to
actually write one. I'm going to explore jungle map-less. Here's what I have in
mind with this project (which actually may change over time):

- Have a small and fast language written in safe Rust;
- Be a pure functional language;
- Run all procedures in separate threads, e.g.
  when evaluating `(f1 (f2) (f3))`, `f1` spawns a thread, then `f2` spawns it's
  own thread, and `f3` spawn another one. `f1` then waits for `f2` and `f3`
  threads to finish;
- Tail call optimization. Not sure about mutual t.c.o. though.
- Pattern-based typing system.
- Less parentheses in places where we should not need ones, like `cond` or `let`
- Improve (IMO) upon various things I find strange in Scheme, like `let` `let*` and
  `letrec` really should be one thing.

Because this will be a pure language, there should be no person who would like
to actually use this language for anything practical, but if you want, consider
something else (at least for now).

## Syntax (or how do you actually call it in Lisps?)
The goal syntax would be something like this:

- `1`, `-1`, `1.0`, `-1.0` - numeric constants,
- `"abc"` - string,
- `'(1 2 3)` - list,
- `'abc` - symbol,
- `abc` - identifier,
- `()` - procedure application, except in some special forms,
- `;` - line comment,
- `#` - pattern,
- `:` - pattern expansion
- `\` - escape.

Reserved characters, that are not allowed as part of names, neither by itself,
nor in escaped form:
- `(`, `)` - used for procedure application, lists,
- `'` - used for quoting identifiers, and list construction,
- `` ` `` - used for quasiquoting,
- `,` - used for unquoting in quasiquote,
- `#` - used for patterns,
- `:` - used for accessing pattern's additional fields.
- `;` - used for comments,
- `"` - used as a string delimiter,
- `\` - used to escape string delimiter in strings.

### Procedure creation
All procedures are anonymous procedures and are created with `lambda` (or `λ`) procedure:

```
> (lambda (argument list) body)
```

`lambda` accepts list of arguments with the first set of parenthesis, the rest
expressions are treated as body with implicit `progn` around it, and returns a
`#procedure`. For example procedure which computes square of number `x` is
defined like this:

```
> (lambda (x) (* x x))
#procedure
```

We can apply procedure using extra set of parentheses around it and by providing
argument:

```
> ((lambda (x) (* x x)) 8)
64
```

The expression `(lambda (x) (* x x)` creates procedure with local name `x`
that exists only inside the scope of the procedure. So if procedure
returns another procedure, which uses the `x` name, it will be stored inside
returned procedure as value. For example:

```
> (((lambda (x) (lambda (y) (+ x y)) 1) 2)
3
```

When we apply `1` to the first lambda expression `x` will be holding the `1`
value, and will be stored inside the `#procedure` returned by `(lambda (y) (+ x
y))` as `1` thus making it `(lambda (y) (+ 1 y))`. When we apply `2` to the
resulting `#procedure` it will compute `(+ 1 2)`.

### Name definition
Names are used to hold data. Names are not variables, because those are
immutable. Since this language is pure functional, we can't assign twice to a
name without redefining it before.

Names are defined with the `define` procedure, which takes two arguments as an
input and produces `#void`. This creates two names in global scope:

```
> (define abc 123)
> (define def "def")
```

We can not use numeric constants, strings, reserved characters, and whitespace
to define name names.

```
> (define -1 -1)
;; error
> (define "-1" -1)
#error
> (define my name 22)
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

names can store any kind of information: values, strings, procedures,
symbols, patterns:

```
> (define var1 22)
> var1
22
> (define var2 "22")
> var2
"22"
```

When names are holding `#procedure` those also can be used for procedure
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
(cond <boolean arm 1>
       <expression 1>
      <boolean arm 2>
       <expression 1>)
```

For example:

```
> (cond (= 1 2) "what?"
        (not (= 1 1)) "WHAT???"
        #t "all good")
"all good"
```

Because `cond` executes its arms in order it doesn't spawn threads, and arms are
executed only when it holds `#t`, similarly to `if`, `cond` is a special form.

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
