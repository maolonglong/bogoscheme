open Env

(*
 * Define the primitive functions as functions from values to a value.
 *)

(* Create an arithmetic operator mapping an arbitrary number of integers
   to an integer. *)
let make_arith_operator op init name values =
  let err_msg = name ^ " requires integer arguments only" in
  let rec iter rest current =
    match rest with
    | [] -> current
    | Val_int i1 :: rest' -> iter rest' (op current i1)
    | _ -> raise (Invalid_argument err_msg)
  in
  Val_int (iter values init)
;;

(* Define arithmetic operators. *)
let add = make_arith_operator ( + ) 0 "+"
let mul = make_arith_operator ( * ) 1 "+"

(* Subtract two integers. *)
let sub values =
  match values with
  | [ Val_int i1 ] -> Val_int (-i1) (* Unary minus *)
  | [ Val_int i1; Val_int i2 ] -> Val_int (i1 - i2)
  | _ -> raise (Invalid_argument "- requires exactly one or two integer arguments")
;;

(* Divide two integers. *)
let div values =
  match values with
  | [ Val_int i1; Val_int i2 ] -> Val_int (i1 / i2)
  | _ -> raise (Invalid_argument "/ requires exactly two integer arguments")
;;

(* Create a boolean operator acting on two integers. *)
let make_binary_bool_operator op name =
  let err_msg = name ^ " requires exactly two integer arguments" in
  function
  | [ Val_int i1; Val_int i2 ] -> Val_bool (op i1 i2)
  | _ -> raise (Invalid_argument err_msg)
;;

(* Define binary operators. *)
let eq = make_binary_bool_operator ( = ) "="
let ne = make_binary_bool_operator ( <> ) "!="
let lt = make_binary_bool_operator ( < ) "<"
let gt = make_binary_bool_operator ( > ) ">"
let le = make_binary_bool_operator ( <= ) "<="
let ge = make_binary_bool_operator ( >= ) ">="

(* Print a value. *)
let print values =
  let err_msg = "print requires exactly one argument" in
  match values with
  | [ value ] ->
    Printf.printf "%s\n" (string_of_value value);
    flush stdout;
    Val_unit
  | _ -> raise (Invalid_argument err_msg)
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
    ]
  in
  List.iter (fun (op, name) -> Env.add env name (Val_prim op)) ops
;;
