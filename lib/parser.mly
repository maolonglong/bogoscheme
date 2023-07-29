%token TOK_LPAREN TOK_RPAREN

%token          TOK_QUOTE
%token          TOK_DOT
%token          TOK_UNIT
%token <bool>   TOK_BOOL
%token <int>    TOK_INT
%token <string> TOK_STRING
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
  | a = atom        { Sexpr.Expr_atom a }
  | e = quoted      { Sexpr.Expr_list [ Sexpr.Expr_atom (Sexpr.Atom_id "quote"); e ] }
  | l = slist       { Sexpr.Expr_list l }
  | l = dotted_list { l }
  ;

atom:
  |      TOK_UNIT   { Sexpr.Atom_unit }
  | b  = TOK_BOOL   { Sexpr.Atom_bool b }
  | i  = TOK_INT    { Sexpr.Atom_int i }
  | s  = TOK_STRING { Sexpr.Atom_string s }
  | id = TOK_ID     { Sexpr.Atom_id id }
  ;

quoted:
  | TOK_QUOTE; e = sexpr { e }
  ;

slist:
  | TOK_LPAREN; es = sexpr_list; TOK_RPAREN { List.rev es }
  ;

dotted_list:
  | TOK_LPAREN; es = sexpr_list; TOK_DOT; e = sexpr; TOK_RPAREN { Sexpr.Expr_dotted_list (List.rev es, e) }
  ;

sexpr_list:
  | (* empty *)                { [] }
  | es = sexpr_list; e = sexpr { e :: es }
  ;
