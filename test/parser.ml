open OUnit2
open Bogoscheme

let ( >>= ) = Fun.flip Option.map

let ast_of_string input =
  let lexbuf = Lexing.from_string input in
  let sexpr = Parser.parse Lexer.lex lexbuf in
  sexpr >>= fun e -> Ast.ast_of_sexpr e
;;

let make_parser_test name expected_output input =
  name
  >:: fun _ ->
  assert_equal expected_output (ast_of_string input) ~printer:(fun x ->
    match x with
    | None -> "None"
    | Some e -> Ast.string_of_ast e)
;;

let tests =
  let open Ast in
  [ make_parser_test "none" None ""
  ; make_parser_test "none_comment" None ";"
  ; make_parser_test "none_comment2" None "; foo bar"
  ; make_parser_test "none_comment3" None "; foo bar \n"
  ; make_parser_test
      "define_int"
      (Some (Expr_define ("x", Expr_int 10)))
      "; foo bar \n (define x 10)"
  ; make_parser_test
      "lambda"
      (Some (Expr_lambda ([ "x" ], [ Expr_id "x" ])))
      "; identity \n (lambda (x) x)"
  ; make_parser_test
      "define_lambda"
      (Some (Expr_define ("id", Expr_lambda ([ "x" ], [ Expr_id "x" ]))))
      "(define id (lambda (x) x))"
  ; make_parser_test
      "apply_id"
      (Some (Expr_apply (Expr_id "+", [ Expr_int 1; Expr_int 2 ])))
      "(+ 1 2)"
  ; make_parser_test
      "apply_lambda"
      (Some (Expr_apply (Expr_lambda ([ "x" ], [ Expr_id "x" ]), [ Expr_int 1 ])))
      "((lambda (x) x) 1)"
  ; make_parser_test
      "if"
      (Some (Expr_if (Expr_bool true, Expr_int 1, Expr_int 0)))
      "(if #t 1 0)"
  ; make_parser_test
      "factorial"
      (Some
         (Expr_define
            ( "factorial"
            , Expr_lambda
                ( [ "n" ]
                , [ Expr_if
                      ( Expr_apply (Expr_id "=", [ Expr_id "n"; Expr_int 0 ])
                      , Expr_int 1
                      , Expr_apply
                          ( Expr_id "*"
                          , [ Expr_id "n"
                            ; Expr_apply
                                ( Expr_id "factorial"
                                , [ Expr_apply (Expr_id "-", [ Expr_id "n"; Expr_int 1 ])
                                  ] )
                            ] ) )
                  ] ) )))
      "(define factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))))"
  ]
;;

let _ = run_test_tt_main ("test suite for parser" >::: tests)
