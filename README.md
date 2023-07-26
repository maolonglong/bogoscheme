# bogoscheme

A subset of [Scheme](https://www.gnu.org/software/mit-scheme/)

## Examples

```bash
# interpreter
$ dune exec ./main.exe -- ./tests/fibonacci.bs
0
1
1
2
3
5
55
9227465
12586269025

# REPL
$ dune exec ./main.exe
> (define x 10)
#u
> (print x)
10
#u
>
```

## References

- [Ocaml track: assignment 5: Implementing Scheme, part 1](http://courses.cms.caltech.edu/cs11/material/ocaml/lab5/lab5.html)
- [Ocaml track: assignment 6: Implementing Scheme, part 2](http://courses.cms.caltech.edu/cs11/material/ocaml/lab6/lab6.html)
