(* Types. *)

type id = string

type value =
  | Val_unit
  | Val_bool of bool
  | Val_int of int
  | Val_prim of (value list -> value) (* primitive functions *)
  | Val_lambda of env * id list * Ast.expr list

and env =
  { parent : env option
  ; bindings : (id, value) Hashtbl.t
  }

(* Values. *)

let string_of_value v =
  match v with
  | Val_unit -> "#u"
  | Val_bool true -> "#t"
  | Val_bool false -> "#f"
  | Val_int i -> string_of_int i
  | Val_prim _ -> "[primitive function]"
  | Val_lambda _ -> "[lambda expression]"
;;

(* Environments. *)

let make parent = { parent; bindings = Hashtbl.create 5 }

let rec lookup env name =
  let { parent = p; bindings = b } = env in
  match Hashtbl.find_opt b name with
  | Some v -> v
  | None ->
    (match p with
     | Some p_env -> lookup p_env name
     | None -> failwith ("Unbound value " ^ name))
;;

let add env name value =
  let { parent = _; bindings = b } = env in
  Hashtbl.add b name value
;;

let add_all env names values =
  let pairs = List.combine names values in
  List.iter (fun (x, y) -> add env x y) pairs
;;
