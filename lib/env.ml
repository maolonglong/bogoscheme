(* Types. *)

type id = string

type value =
  | Val_unit
  | Val_bool of bool
  | Val_int of int
  | Val_string of string
  | Val_id of id
  | Val_list of value list
  | Val_prim of (env -> value list -> value) (* primitive functions *)
  | Val_lambda of env * id list * id option * Ast.expr list
  | Val_macro of Ast.expr

and env =
  { parent : env option
  ; bindings : (id, value) Hashtbl.t
  }

(* Values. *)

let rec string_of_value : value -> string =
  let rec string_of_value_list : value list -> string = function
    | [] -> ""
    | [ x ] -> string_of_value x
    | x :: xs -> string_of_value x ^ " " ^ string_of_value_list xs
  in
  function
  | Val_unit -> "#u"
  | Val_bool true -> "#t"
  | Val_bool false -> "#f"
  | Val_int i -> string_of_int i
  | Val_string s -> Printf.sprintf "%S" s
  | Val_id id -> id
  | Val_list l -> "(" ^ string_of_value_list l ^ ")"
  | Val_prim _ -> "[primitive function]"
  | Val_lambda _ -> "[lambda expression]"
  | Val_macro _ -> "[macro expression]"
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
