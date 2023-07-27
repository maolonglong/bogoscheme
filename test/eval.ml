open OUnit2
open Bogoscheme

let interp input =
  let lexbuf = Lexing.from_string input in
  let env = Env.make None in
  let rec loop env ret =
    let sexpr = Parser.parse Lexer.lex lexbuf in
    match sexpr with
    | None -> ret
    | Some s ->
      let expr = Ast.ast_of_sexpr s in
      let v = Eval.eval expr env in
      loop env (Some v)
  in
  Primitives.load env;
  match loop env None with
  | None -> assert_failure "Failed to eval"
  | Some v -> v
;;

let make_eval_test name expected_output input =
  name
  >:: fun _ -> assert_equal expected_output (interp input) ~printer:Env.string_of_value
;;

let tests =
  let open Env in
  [ make_eval_test
      "fib35"
      (Val_int 9227465)
      "(define fib-tailrec-aux\n\
      \  (lambda (n x y)\n\
      \    (if (= n 0)\n\
      \        x\n\
      \        (fib-tailrec-aux (- n 1) y (+ x y)))))\n\n\
       (define fib-tailrec\n\
      \  (lambda (n)\n\
      \    (fib-tailrec-aux n 0 1)))\n\n\
       (fib-tailrec 35)\n"
  ; make_eval_test
      "factorial"
      (Val_int 3628800)
      "(define number 10)\n\n\
       (define factorial\n\
      \  (lambda (n)\n\
      \    (if (= n 0)\n\
      \        1\n\
      \        (* n (factorial (- n 1))))))\n\n\
       (factorial number)"
  ]
;;

let _ = run_test_tt_main ("test suite for eval" >::: tests)
