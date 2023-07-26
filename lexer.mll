{
open Parser
}

let whitespace = [' ' '\t' '\n']
let integer    = '-'? ['0' - '9']+
let id_chars   = ['a' - 'z' '+' '-' '*' '/' '=' '<' '>' '!']+

rule lex = parse
  | ';' [^ '\n']* { lex lexbuf }
  | whitespace    { lex lexbuf }
  | '('           { TOK_LPAREN }
  | ')'           { TOK_RPAREN }
  | "#u"          { TOK_UNIT }
  | "#t"          { TOK_BOOL true }
  | "#f"          { TOK_BOOL false }
  | integer       { TOK_INT (int_of_string (Lexing.lexeme lexbuf)) }
  | id_chars      { TOK_ID (Lexing.lexeme lexbuf) }
  | eof           { TOK_EOF }
  | _             { failwith ("Unexpected char: " ^ Lexing.lexeme lexbuf) }
