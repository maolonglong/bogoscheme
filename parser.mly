%token TOK_LPAREN TOK_RPAREN

%token          TOK_UNIT
%token <bool>   TOK_BOOL
%token <int>    TOK_INT
%token <string> TOK_ID
%token          TOK_EOF

%start parse
%type <Sexpr.expr option> parse
%type <Sexpr.expr>        sexpr
%type <Sexpr.atom>        atom
%type <Sexpr.expr list>   slist
%type <Sexpr.expr list>   sexpr_list

%%

parse:
  | e = sexpr { Some e }
  | TOK_EOF   { None }
  ;

sexpr:
  | a = atom  { Sexpr.Expr_atom a }
  | l = slist { Sexpr.Expr_list l }
  ;

atom:
  |      TOK_UNIT { Sexpr.Atom_unit }
  | b  = TOK_BOOL { Sexpr.Atom_bool b }
  | i  = TOK_INT  { Sexpr.Atom_int i }
  | id = TOK_ID   { Sexpr.Atom_id id }
  ;

slist:
  | TOK_LPAREN; es = sexpr_list; TOK_RPAREN { List.rev es }
  ;

sexpr_list:
  | (* empty *)                { [] }
  | es = sexpr_list; e = sexpr { e :: es }
  ;
