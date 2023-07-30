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

let display _ values =
  let err_msg = "display requires exactly one argument" in
  match values with
  | [ Val_string s ] ->
    let unescaped = Scanf.unescaped s in
    print_string unescaped;
    Val_unit
  | [ value ] ->
    Printf.printf "%s" (string_of_value value);
    flush stdout;
    Val_unit
  | _ -> invalid_arg err_msg
;;

let car _ = function
  | [ Val_list (hd :: _) ] -> hd
  | _ -> invalid_arg "car must be given a pair."
;;

let cdr _ = function
  | [ Val_list (_ :: tl) ] -> Val_list tl
  | _ -> invalid_arg "cdr must be given a pair."
;;

(* FIXME: distinguish between ordinary list and dotted_list *)
let cons _ = function
  | [ x; Val_list xs ] -> Val_list (x :: xs)
  | [ x; y ] -> Val_list (x :: [ y ])
  | _ -> invalid_arg "cons must be given exactly two arguments."
;;

let is_eqv_aux v1 v2 =
  match v1, v2 with
  | Val_bool x, Val_bool y -> x = y
  | Val_id x, Val_id y -> x = y
  | Val_int x, Val_int y -> x = y
  | Val_unit, Val_unit -> true
  | _ -> false
;;

let is_eqv _ = function
  | [ v1; v2 ] -> Val_bool (is_eqv_aux v1 v2)
  | _ -> failwith "Equality comparators require exactly two arguments."
;;

let is_equal _ =
  let rec is_equal_aux v1 v2 =
    match v1, v2 with
    | Val_string x, Val_string y -> x = y
    | Val_list [], Val_list [] -> true
    | Val_list (t1 :: h1), Val_list (t2 :: h2) ->
      is_equal_aux t1 t2 && is_equal_aux (Val_list h1) (Val_list h2)
    | _ -> is_eqv_aux v1 v2
  in
  function
  | [ v1; v2 ] -> Val_bool (is_equal_aux v1 v2)
  | _ -> failwith "Equality comparators require exactly two arguments."
;;

let is_boolean _ = function
  | [ Val_bool _ ] -> Val_bool true
  | [ _ ] -> Val_bool false
  | _ -> failwith "boolean? requires exactly one argument."
;;

let is_number _ = function
  | [ Val_int _ ] -> Val_bool true
  | [ _ ] -> Val_bool false
  | _ -> failwith "number? requires exactly one argument."
;;

let is_string _ = function
  | [ Val_string _ ] -> Val_bool true
  | [ _ ] -> Val_bool false
  | _ -> failwith "string? requires exactly one argument."
;;

let is_symbol _ = function
  | [ Val_id _ ] -> Val_bool true
  | [ _ ] -> Val_bool false
  | _ -> failwith "symbol? requires exactly one argument."
;;

let is_list _ = function
  | [ Val_list _ ] -> Val_bool true
  | [ _ ] -> Val_bool false
  | _ -> failwith "list? requires exactly one argument."
;;

let is_pair _ = function
  | [ Val_list (_ :: _) ] -> Val_bool true
  | [ _ ] -> Val_bool false
  | _ -> failwith "pair? requires exactly one argument."
;;

let is_null _ = function
  | [ Val_list [] ] -> Val_bool true
  | [ _ ] -> Val_bool false
  | _ -> failwith "null? requires exactly one argument."
;;

let error _ = function
  | [ Val_string s ] -> failwith ("[ERROR] " ^ s)
  | _ -> failwith "ERROR requires exactly one string argument."
;;

let load_from_lexbuf env lexbuf =
  let is_macro id =
    try
      match Env.lookup env id with
      | Env.Val_macro _ -> true
      | _ -> false
    with
    | Failure _ -> false
  in
  let rec loop () =
    match Parser.parse Lexer.lex lexbuf with
    | None -> ()
    | Some sexpr ->
      let ast = Ast.ast_of_sexpr is_macro sexpr in
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
    ; display, "display"
    ; load_from_file, "load"
    ; car, "car"
    ; cdr, "cdr"
    ; cons, "cons"
    ; is_eqv, "eq?"
    ; is_eqv, "eqv?"
    ; is_equal, "equal?"
    ; is_boolean, "boolean?"
    ; is_number, "number?"
    ; is_string, "string?"
    ; is_symbol, "symbol?"
    ; is_list, "list?"
    ; is_pair, "pair?"
    ; is_null, "null?"
    ; error, "error"
    ]
  in
  List.iter (fun (op, name) -> Env.add env name (Val_prim op)) ops;
  load_from_string env Embed.primitives_dot_scm
;;
