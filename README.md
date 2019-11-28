# my_scheme

Simple scheme-like language written in Rust as self-educational exercise.
This is in very early stages of development, so the information below may be not
accurate. Not intended for serious use.

## Syntax
The goal syntax would be something like this:

- `1`, `-1`, `1.0`, `-1.0` - numeric constants,
- `"abc"` - string,
- `'(1 2 3)` - list,
- `'abc` - symbol,
- `abc` - identifier,
- `()` - procedure application,
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

`lambda` accepts list of arguments with the first set of parenthesis, and the
rest expressions are treated as body, and returns a `#procedure`. For example
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
```

when we apply `1` to the first lambda expression `x` will be holding the `1`
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

### Special forms
Scheme and Lisps feature some special forms, such as `if` `let` `cond` that can
have some syntax parsing rules adjustments and some weird semantics. Currently
the goal is:

#### `if`
Used for branching the program. Syntax:

```
(if (boolean expression)
      (true expression)
    (false expression)
    ...
    (false expression))
```

Accepts boolean or as first argument. Booleans are written as patterns: `#t`
`#f`, everything that is not `#t` is `#f`. For example, comparison of two
numbers returns boolean:

```
> (= 1 2)
#f
> (= 2 2)
#t
```

#### `let`
`let` is used to create local bindings. `let` form should be equal to the `letrec`
form by default. Syntax:

```
> (let ((a 1)
       (b a))
   (+ a b))
2
```

#### `cond`
Cond is similar to `if` but has more arms and can match boolean patterns:

```
> (cond ((= 1 2) "what?")
        ((not (= 1 1)) "WHAT???")
        (#t "all good"))
"all good"
```

only one arm is executed.

#### `match`
`match` is used for matching any patterns. Its syntax is similar to `cond`

```
> (define a (lambda (x) (+ x x)))
> (match a (#procedure
            "a is procedure")
           (#t
            "a is something else"))
"a is procedure"
```

only one arm is executed.
