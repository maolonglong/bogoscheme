val eval : Ast.expr -> Env.env -> Env.value

exception Type_error of string
