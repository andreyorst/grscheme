# my_scheme

Small scheme-like language written in Rust as self educational exercise.

Currently supports only `define` to create global variables, `+`, `-`, `*`,
`/` as mathematical operators, and `=`, `<`, `<=`, `>`, `>=` as numeric comparison operators.

```
> (define a 22)
a
> (+ a 2 (- a 12) 10)
44
> (< 1 2)
#t
```
