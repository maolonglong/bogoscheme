type id = string

type expr =
  | Expr_unit
  | Expr_bool of bool
  | Expr_int of int
  | Expr_string of string
  | Expr_id of id
  | Expr_quoted of Sexpr.expr
  | Expr_define of id * expr
  | Expr_define_macro of id * expr
  | Expr_if of expr * expr * expr
  (* lambda: arg_ids * varg? * body *)
  | Expr_lambda of id list * id option * expr list
  | Expr_apply of expr * expr list

module StringSet = Set.Make (String)

(* FIXME *)
let macros = ref StringSet.empty

let rec ast_of_sexpr sx =
  let module S = Sexpr in
  let ast_of_atom = function
    | S.Atom_unit -> Expr_unit
    | S.Atom_bool b -> Expr_bool b
    | S.Atom_int i -> Expr_int i
    | S.Atom_string s -> Expr_string s
    | S.Atom_id id -> Expr_id id
  in
  let ast_of_lambda l =
    let id_of_sexpr = function
      | S.Expr_atom (S.Atom_id id) -> id
      | _ -> failwith "Invalid id"
    in
    match l with
    (* (lambda (x) body) *)
    | S.Expr_list operands :: sexprs ->
      Expr_lambda (List.map id_of_sexpr operands, None, List.map ast_of_sexpr sexprs)
    (* (lambda (x . y) body) *)
    | S.Expr_dotted_list (l, last) :: sexprs ->
      Expr_lambda
        (List.map id_of_sexpr l, Some (id_of_sexpr last), List.map ast_of_sexpr sexprs)
    | _ -> failwith "Invalid lambda"
  in
  let ast_of_define = function
    (* (define x y) *)
    | [ S.Expr_atom (S.Atom_id id); tl ] ->
      macros := StringSet.remove id !macros;
      Expr_define (id, ast_of_sexpr tl)
    (* (define (f x) body) *)
    | [ S.Expr_list (S.Expr_atom (S.Atom_id id) :: operands); body ] ->
      macros := StringSet.remove id !macros;
      Expr_define (id, ast_of_lambda (S.Expr_list operands :: [ body ]))
    (* (define (f x . y) body) *)
    | [ S.Expr_dotted_list (S.Expr_atom (S.Atom_id id) :: arg_ids, operands); body ] ->
      macros := StringSet.remove id !macros;
      Expr_define (id, ast_of_lambda (S.Expr_dotted_list (arg_ids, operands) :: [ body ]))
    | _ -> failwith "Invalid define"
  in
  let ast_of_macro = function
    | [ S.Expr_atom (S.Atom_id name); tl ] ->
      let body = ast_of_sexpr tl in
      (match body with
       | Expr_lambda ([ _ ], None, _) as lambda ->
         macros := StringSet.add name !macros;
         Expr_define_macro (name, lambda)
       | _ -> failwith "Invalid define-macro")
    | _ -> failwith "Invalid define-macro"
  in
  let ast_of_if = function
    | [ test_clause; then_clause; else_clause ] ->
      Expr_if
        (ast_of_sexpr test_clause, ast_of_sexpr then_clause, ast_of_sexpr else_clause)
    | _ -> failwith "Invalid if"
  in
  let ast_of_apply = function
    | func :: operands as l ->
      (match ast_of_sexpr func with
       | Expr_id id as macro_name when StringSet.mem id !macros ->
         Expr_apply (macro_name, [ Expr_quoted (S.Expr_list l) ])
       | _ -> Expr_apply (ast_of_sexpr func, List.map ast_of_sexpr operands))
    | _ -> failwith "Invalid apply"
  in
  let ast_of_quote = function
    | [ sexpr ] -> Expr_quoted sexpr
    | _ -> failwith "Invalid quote"
  in
  match sx with
  | S.Expr_atom atom -> ast_of_atom atom
  | S.Expr_list sexprs ->
    (match sexprs with
     | [] -> failwith "Invalid expression"
     | hd :: tl ->
       (match hd with
        | S.Expr_atom atom ->
          (match atom with
           | S.Atom_id "define" -> ast_of_define tl
           | S.Atom_id "define-macro" -> ast_of_macro tl
           | S.Atom_id "if" -> ast_of_if tl
           | S.Atom_id "lambda" -> ast_of_lambda tl
           | S.Atom_id "quote" -> ast_of_quote tl
           | _ -> ast_of_apply (S.Expr_atom atom :: tl))
        | S.Expr_list _ -> Expr_apply (ast_of_sexpr hd, List.map ast_of_sexpr tl)
        | S.Expr_dotted_list _ -> failwith "Invalid expression"))
  | S.Expr_dotted_list _ -> failwith "TODO"
;;

let string_of_ast ast =
  let sprintf = Printf.sprintf in
  (* to make the code cleaner *)
  let spaces n = String.make n ' ' in
  let rec string_of_ids id_lst =
    match id_lst with
    | [] -> ""
    | [ id ] -> id
    | h :: t -> h ^ " " ^ string_of_ids t
  in
  let rec iter ast indent =
    let string_of_exprs e_list =
      List.fold_left ( ^ ) "" (List.map (fun e -> "\n" ^ iter e (indent + 2)) e_list)
    in
    match ast with
    | Expr_unit -> sprintf "%sUNIT" (spaces indent)
    | Expr_bool b -> sprintf "%sBOOL[ %b ]" (spaces indent) b
    | Expr_int i -> sprintf "%sINT[ %d ]" (spaces indent) i
    | Expr_string s -> sprintf "%sSTRING[ %S ]" (spaces indent) s
    | Expr_id id -> sprintf "%sID[ %s ]" (spaces indent) id
    | Expr_quoted e -> sprintf "%sQUOTED[ %s ]" (spaces indent) (Sexpr.string_of_expr2 e)
    | Expr_define (id, e) ->
      sprintf "%sDEFINE[%s\n%s ]" (spaces indent) id (iter e (indent + 2))
    | Expr_define_macro (name, e) ->
      sprintf "%sDEFINE_MACRO[%s\n%s ]" (spaces indent) name (iter e (indent + 2))
    | Expr_if (test_clause, then_clause, else_clause) ->
      sprintf
        "%sIF[\n%s\n%s\n%s ]"
        (spaces indent)
        (iter test_clause (indent + 2))
        (iter then_clause (indent + 2))
        (iter else_clause (indent + 2))
    | Expr_lambda (ids, _, body) ->
      sprintf
        "%sLAMBDA[(%s)%s ]"
        (spaces indent)
        (string_of_ids ids)
        (string_of_exprs body)
    | Expr_apply (operator, operands) ->
      sprintf
        "%sAPPLY[\n%s%s ]"
        (spaces indent)
        (iter operator (indent + 2))
        (string_of_exprs operands)
  in
  "\n" ^ iter ast 0 ^ "\n"
;;
