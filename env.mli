(** Identifiers. *)
type id = string

(** Type of legal values i.e. results of evaluation. *)
type value =
  | Val_unit
  | Val_bool of bool
  | Val_int of int
  | Val_prim of (value list -> value) (* primitive functions *)
  | Val_lambda of env * id list * Ast.expr list

(** Type of environments. *)
and env =
  { parent : env option
  ; bindings : (id, value) Hashtbl.t
  }

(** Convert a value into a string. *)
val string_of_value : value -> string

(** Create an empty environment with a specified parent environment. *)
val make : env option -> env

(** Look up an identifier in an environment, returning a value.
    Raises Not_found if the value doesn't exist. *)
val lookup : env -> id -> value

(** Add a name/value binding to an environment.
    This can bind a new name or rebind it if it's already bound. *)
val add : env -> id -> value -> unit

(** Add lists of names and values to an environment.
    This is just for convenience. *)
val add_all : env -> id list -> value list -> unit
