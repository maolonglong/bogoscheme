open Env

exception Type_error of string

let rec eval ast env =
  match ast with
  | Ast.Expr_unit -> Val_unit
  | Ast.Expr_bool b -> Val_bool b
  | Ast.Expr_int i -> Val_int i
  | Ast.Expr_string s -> Val_string s
  | Ast.Expr_id id -> lookup env id
  | Ast.Expr_define (id, e) ->
    let value = eval e env in
    add env id value;
    Val_unit
  | Ast.Expr_if (test_e, then_e, else_e) ->
    (match eval test_e env with
     | Val_bool true -> eval then_e env
     | Val_bool false -> eval else_e env
     | _ -> raise (Type_error "Cannot evaluate non-boolean in if statement"))
  | Ast.Expr_lambda (ids, exprs) -> Val_lambda (env, ids, exprs)
  | Ast.Expr_apply (e, es) ->
    (* Evaluate all the operands (all arguments except the first. *)
    let operands = List.map (fun x -> eval x env) es in
    (* Evaluate the function argument (the first argument). *)
    let f = eval e env in
    (match f with
     | Val_prim prim_func -> prim_func env operands
     | Val_lambda (env', ids, exprs) ->
       if List.length ids <> List.length operands
       then raise (Type_error "Applied to wrong operands");
       let parent_env = make (Some env') in
       add_all parent_env ids operands;
       let results = List.map (fun x -> eval x parent_env) exprs in
       List.hd (List.rev results)
     | _ -> raise (Type_error "Cannot apply non-function"))
;;
