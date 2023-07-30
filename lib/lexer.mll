{
open Parser
}

let whitespace = [' ' '\t' '\n']
let digit      = ['0' - '9']
let integer    = '-'? digit+
let id_chars   = ['a' - 'z' '+' '-' '*' '/' '=' '<' '>' '!']
let comment    = ';' [^ '\n']*
let identifier = id_chars (id_chars | digit)* '?'?

rule lex = parse
  | comment          { lex lexbuf }
  | whitespace       { lex lexbuf }
  | '('              { TOK_LPAREN }
  | ')'              { TOK_RPAREN }
  | '.'              { TOK_DOT }
  | '\''             { TOK_QUOTE }
  | '`'              { TOK_QUASIQUOTE }
  | ','              { TOK_UNQUOTE }
  | ",@"             { TOK_UNQUOTE_SPLICING }
  | "#u"             { TOK_UNIT }
  | "#t"             { TOK_BOOL true }
  | "#f"             { TOK_BOOL false }
  | integer          { TOK_INT (int_of_string (Lexing.lexeme lexbuf)) }
  | identifier | '.' { TOK_ID (Lexing.lexeme lexbuf) }
  | '"'              { lex_string (Buffer.create 16) lexbuf }
  | eof              { TOK_EOF }
  | _                { failwith ("Unexpected char: " ^ Lexing.lexeme lexbuf) }

and lex_string buf = parse
  | '"'          { TOK_STRING (Buffer.contents buf) }
  | '\\' '\\'    { Buffer.add_char buf '\\'; lex_string buf lexbuf }
  | '\\' 'n'    { Buffer.add_char buf '\n'; lex_string buf lexbuf }
  | '\\' '"'     { Buffer.add_char buf '"'; lex_string buf lexbuf }
  | [^ '"' '\\'] { Buffer.add_string buf (Lexing.lexeme lexbuf); lex_string buf lexbuf }
  | eof          { failwith "String is not terminated" }
  | _            { failwith ("Illegal string character: " ^ Lexing.lexeme lexbuf) }
