{
open Parser
}

let whitespace = [' ' '\t' '\n']
let digit      = ['0' - '9']
let integer    = '-'? digit+
let id_chars   = ['a' - 'z' '+' '-' '*' '/' '=' '<' '>' '!']
let comment    = ';' [^ '\n']*
let identifier = id_chars (id_chars | digit)*

rule lex = parse
  | comment    { lex lexbuf }
  | whitespace { lex lexbuf }
  | '('        { TOK_LPAREN }
  | ')'        { TOK_RPAREN }
  | "#u"       { TOK_UNIT }
  | "#t"       { TOK_BOOL true }
  | "#f"       { TOK_BOOL false }
  | integer    { TOK_INT (int_of_string (Lexing.lexeme lexbuf)) }
  | identifier { TOK_ID (Lexing.lexeme lexbuf) }
  | eof        { TOK_EOF }
  | _          { failwith ("Unexpected char: " ^ Lexing.lexeme lexbuf) }
