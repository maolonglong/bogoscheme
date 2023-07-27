open OUnit2
open Bogoscheme
open Lexer
open Parser

let string_of_token = function
  | TOK_LPAREN -> "LPAREN"
  | TOK_RPAREN -> "RPAREN"
  | TOK_UNIT -> "UNIT"
  | TOK_BOOL b -> if b then "#t" else "#f"
  | TOK_INT i -> string_of_int i
  | TOK_STRING s -> Printf.sprintf "%S" s
  | TOK_ID id -> id
  | TOK_EOF -> "EOF"
;;

let string_of_token_list lst =
  "[ " ^ String.concat "; " (List.map string_of_token lst) ^ " ]"
;;

let make_lexer_test name expected_output input =
  name
  >:: fun _ ->
  let lexbuf = Lexing.from_string input in
  let rec loop acc =
    match lex lexbuf with
    | TOK_EOF -> List.rev acc
    | tok -> loop (tok :: acc)
  in
  let output = loop [] in
  assert_equal expected_output output ~printer:string_of_token_list
;;

let tests =
  [ make_lexer_test "int" [ TOK_INT 123 ] "123"
  ; make_lexer_test "neg_int" [ TOK_INT (-123) ] "-123"
  ; make_lexer_test "string" [ TOK_STRING "abc" ] "\"abc\""
  ; make_lexer_test "unit" [ TOK_UNIT ] "#u"
  ; make_lexer_test "bool_true" [ TOK_BOOL true ] "#t"
  ; make_lexer_test "bool_false" [ TOK_BOOL false ] "#f"
  ; make_lexer_test "id" [ TOK_ID "foo" ] "foo"
  ; make_lexer_test "id_with_digit" [ TOK_ID "foo2" ] "foo2"
  ; make_lexer_test
      "op_add"
      [ TOK_LPAREN; TOK_ID "+"; TOK_INT 1; TOK_INT 2; TOK_RPAREN ]
      "(+ 1 2)"
  ; make_lexer_test
      "op_sub"
      [ TOK_LPAREN; TOK_ID "-"; TOK_INT 1; TOK_INT 2; TOK_RPAREN ]
      "(- 1 2)"
  ; make_lexer_test
      "op_sub_neg"
      [ TOK_LPAREN; TOK_ID "-"; TOK_INT (-1); TOK_INT 2; TOK_RPAREN ]
      "(- -1 2)"
  ]
;;

let _ = run_test_tt_main ("test suite for lexer" >::: tests)
