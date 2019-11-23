# my_scheme

Small scheme-like language written in Rust as self educational exercise.

Currently supports only `define` to create global variables, and `+`, `-`, `*`,
`/` as mathematical operators.

```
> (define a 22)
a
> (+ a 2 (- a 12) 10)
44
```
