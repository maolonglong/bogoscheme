(** Type of Scheme identifiers. *)
type id = string

(** Type of Scheme expressions. *)
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

(** Convert an S-expression into an AST expression. *)
val ast_of_sexpr : Sexpr.expr -> expr

(** Convert an AST expression into a string. *)
val string_of_ast : expr -> string
