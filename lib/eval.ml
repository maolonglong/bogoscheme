open Env

exception Type_error of string

let split_list n lst =
  let rec split acc n lst =
    match n, lst with
    | 0, _ -> List.rev acc, lst
    | _, [] -> List.rev acc, []
    | _, x :: xs -> split (x :: acc) (n - 1) xs
  in
  split [] n lst
;;

let rec val_of_sexpr sexpr =
  let convert_atom = function
    | Sexpr.Atom_unit -> Val_unit
    | Atom_bool b -> Val_bool b
    | Atom_int i -> Val_int i
    | Atom_string s -> Val_string s
    | Atom_id id -> Val_id id
  in
  let rec convert_list acc = function
    | [] -> List.rev acc
    | Sexpr.Expr_atom atom :: tl -> convert_list (convert_atom atom :: acc) tl
    | (Sexpr.Expr_list _ as e) :: tl -> convert_list (val_of_sexpr e :: acc) tl
    | Sexpr.Expr_dotted_list (l, last) :: tl ->
      convert_list (Val_list (convert_dotted_list (l, last)) :: acc) tl
  and convert_dotted_list (l, last) =
    match last with
    | Sexpr.Expr_list [] -> convert_list [] l
    | Sexpr.Expr_dotted_list _ as dl ->
      (match val_of_sexpr dl with
       | Val_list last_list -> convert_list [] l @ last_list
       | _ -> failwith "Invalid dotted list")
    | _ -> convert_list [] (l @ [ last ])
  in
  match sexpr with
  | Sexpr.Expr_atom atom -> convert_atom atom
  | Sexpr.Expr_list lst -> Val_list (convert_list [] lst)
  | Sexpr.Expr_dotted_list (l, last) -> Val_list (convert_dotted_list (l, last))
;;

let rec sexpr_of_val_list =
  let rec aux acc = function
    | [] -> Sexpr.Expr_list (List.rev acc)
    | Val_unit :: tl -> aux (Sexpr.Expr_atom Sexpr.Atom_unit :: acc) tl
    | Val_bool b :: tl -> aux (Sexpr.Expr_atom (Sexpr.Atom_bool b) :: acc) tl
    | Val_int i :: tl -> aux (Sexpr.Expr_atom (Sexpr.Atom_int i) :: acc) tl
    | Val_string s :: tl -> aux (Sexpr.Expr_atom (Sexpr.Atom_string s) :: acc) tl
    | Val_id id :: tl -> aux (Sexpr.Expr_atom (Sexpr.Atom_id id) :: acc) tl
    | (Val_list _ as l) :: tl -> aux (sexpr_of_val_list l :: acc) tl
    | _ -> failwith "This value cannot be converted to sexpr"
  in
  function
  | Val_list l -> aux [] l
  | Val_unit -> Sexpr.Expr_atom Sexpr.Atom_unit
  | Val_bool b -> Sexpr.Expr_atom (Sexpr.Atom_bool b)
  | Val_int i -> Sexpr.Expr_atom (Sexpr.Atom_int i)
  | Val_string s -> Sexpr.Expr_atom (Sexpr.Atom_string s)
  | Val_id id -> Sexpr.Expr_atom (Sexpr.Atom_id id)
  | _ -> failwith "This value cannot be converted to sexpr"
;;

let rec eval ast env =
  match ast with
  | Ast.Expr_unit -> Val_unit
  | Ast.Expr_bool b -> Val_bool b
  | Ast.Expr_int i -> Val_int i
  | Ast.Expr_string s -> Val_string s
  | Ast.Expr_id id -> lookup env id
  | Ast.Expr_quoted sexpr -> val_of_sexpr sexpr
  | Ast.Expr_define (id, e) ->
    let value = eval e env in
    add env id value;
    Val_unit
  | Ast.Expr_define_macro (name, expr) ->
    add env name (Val_macro expr);
    Val_unit
  | Ast.Expr_if (test_e, then_e, else_e) ->
    (match eval test_e env with
     | Val_bool true -> eval then_e env
     | Val_bool false -> eval else_e env
     | _ -> raise (Type_error "Cannot evaluate non-boolean in if statement"))
  | Ast.Expr_lambda (ids, varg, exprs) -> Val_lambda (env, ids, varg, exprs)
  | Ast.Expr_apply (e, es) ->
    (* Evaluate all the operands (all arguments except the first. *)
    let operands () = List.map (fun x -> eval x env) es in
    (* Evaluate the function argument (the first argument). *)
    let f = eval e env in
    (match f with
     | Val_prim prim_func -> prim_func env (operands ())
     | Val_macro expr ->
       (match es with
        | [ Ast.Expr_quoted _ ] -> ()
        | _ -> failwith "Invalid macro expand");
       let lst = eval (Ast.Expr_apply (expr, es)) env in
       let sexpr = sexpr_of_val_list lst in
       (match sexpr with
        | Sexpr.Expr_atom _ as atom -> val_of_sexpr atom
        | Sexpr.Expr_list _ -> eval (Ast.ast_of_sexpr sexpr) env
        | Sexpr.Expr_dotted_list _ -> failwith "Invalid macro")
     | Val_lambda (env', ids, varg, exprs) ->
       let len_ids = List.length ids in
       let len_ops = List.length (operands ()) in
       (match varg with
        | None ->
          if len_ops <> len_ids then raise (Type_error "Applied to wrong operands")
        | Some _ ->
          if len_ops < len_ids + 1 then raise (Type_error "Applied to wrong operands"));
       let parent_env = make (Some env') in
       (match varg with
        | None -> add_all parent_env ids (operands ())
        | Some last ->
          let l, r = split_list len_ids (operands ()) in
          add_all parent_env ids l;
          add parent_env last (Val_list r));
       let results = List.map (fun x -> eval x parent_env) exprs in
       List.hd (List.rev results)
     | _ -> raise (Type_error "Cannot apply non-function"))
;;
