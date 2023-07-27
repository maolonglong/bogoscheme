open Env

(*
 * Define the primitive functions as functions from values to a value.
 *)

(* Create an arithmetic operator mapping an arbitrary number of integers
   to an integer. *)
let make_arith_operator op init name _ values =
  let err_msg = name ^ " requires integer arguments only" in
  let rec iter rest current =
    match rest with
    | [] -> current
    | Val_int i1 :: rest' -> iter rest' (op current i1)
    | _ -> invalid_arg err_msg
  in
  Val_int (iter values init)
;;

(* Define arithmetic operators. *)
let add = make_arith_operator ( + ) 0 "+"
let mul = make_arith_operator ( * ) 1 "+"

(* Subtract two integers. *)
let sub _ values =
  match values with
  | [ Val_int i1 ] -> Val_int (-i1) (* Unary minus *)
  | [ Val_int i1; Val_int i2 ] -> Val_int (i1 - i2)
  | _ -> invalid_arg "- requires exactly one or two integer arguments"
;;

(* Divide two integers. *)
let div _ values =
  match values with
  | [ Val_int i1; Val_int i2 ] -> Val_int (i1 / i2)
  | _ -> invalid_arg "/ requires exactly two integer arguments"
;;

(* Create a boolean operator acting on two integers. *)
let make_int_int_bool_operator op name _ =
  let err_msg = name ^ " requires exactly two integer arguments" in
  function
  | [ Val_int i1; Val_int i2 ] -> Val_bool (op i1 i2)
  | _ -> invalid_arg err_msg
;;

(* Define binary operators. *)
let eq = make_int_int_bool_operator ( = ) "="
let ne = make_int_int_bool_operator ( <> ) "!="
let lt = make_int_int_bool_operator ( < ) "<"
let gt = make_int_int_bool_operator ( > ) ">"
let le = make_int_int_bool_operator ( <= ) "<="
let ge = make_int_int_bool_operator ( >= ) ">="

(* Print a value. *)
let print _ values =
  let err_msg = "print requires exactly one argument" in
  match values with
  | [ value ] ->
    Printf.printf "%s\n" (string_of_value value);
    flush stdout;
    Val_unit
  | _ -> invalid_arg err_msg
;;

let and' _ = function
  | [] -> Val_bool true
  | [ x ] -> x
  | [ Val_bool b; y ] -> if b then y else Val_bool false
  | _ -> invalid_arg "and requires less than three arguments"
;;

let or' _ = function
  | [] -> Val_bool false
  | [ x ] -> x
  | [ (Val_bool b as x); y ] -> if b then x else y
  | _ -> invalid_arg "or requires less than three arguments"
;;

let load_from_lexbuf env lexbuf =
  let rec loop () =
    match Parser.parse Lexer.lex lexbuf with
    | None -> ()
    | Some sexpr ->
      let ast = Ast.ast_of_sexpr sexpr in
      ignore @@ Eval.eval ast env;
      loop ()
  in
  loop ()
;;

let load_from_string env input =
  let lexbuf = Lexing.from_string input in
  load_from_lexbuf env lexbuf
;;

let load_from_file env = function
  | [ Val_string filename ] ->
    let inx = open_in filename in
    let lexbuf = Lexing.from_channel inx in
    let origin_cwd = Sys.getcwd () in
    Sys.chdir (Filename.dirname filename);
    load_from_lexbuf env lexbuf;
    close_in inx;
    Sys.chdir origin_cwd;
    Val_unit
  | _ -> invalid_arg "load requires exactly one string argument"
;;

(* Load the primitive functions into an environment,
   along with their names. *)

let load env =
  let ops =
    [ add, "+"
    ; sub, "-"
    ; mul, "*"
    ; div, "/"
    ; eq, "="
    ; ne, "!="
    ; lt, "<"
    ; gt, ">"
    ; le, "<="
    ; ge, ">="
    ; print, "print"
    ; load_from_file, "load"
    ; and', "and"
    ; or', "or"
    ]
  in
  List.iter (fun (op, name) -> Env.add env name (Val_prim op)) ops;
  load_from_string env Embed.primitives_dot_bs
;;
