open Bogoscheme

let repl () =
  let interp s env =
    let lexbuf = Lexing.from_string s in
    match Parser.parse Lexer.lex lexbuf with
    | None -> ()
    | Some sexpr ->
      let ast = Ast.ast_of_sexpr sexpr in
      let v = Eval.eval ast env in
      print_endline (Env.string_of_value v)
  in
  let rec loop env =
    print_string "> ";
    flush stdout;
    try
      let s = read_line () in
      interp s env;
      loop env
    with
    | End_of_file -> ()
  in
  let env = Env.make None in
  Primitives.load env;
  loop env
;;

let run_program infile =
  let lexbuf = Lexing.from_channel infile in
  let env = Env.make None in
  let rec loop env =
    let sexpr = Parser.parse Lexer.lex lexbuf in
    match sexpr with
    | None -> ()
    | Some s ->
      let expr = Ast.ast_of_sexpr s in
      let _ = Eval.eval expr env in
      loop env
  in
  Primitives.load env;
  loop env
;;

let _ =
  let handle_exn = function
    | Failure f -> Printf.fprintf stderr "\nERROR: %s\n" f
    | Eval.Type_error s -> Printf.fprintf stderr "\nERROR: type error: %s\n" s
    | Not_found -> Printf.fprintf stderr "\nERROR: name not found\n"
    | _ -> Printf.fprintf stderr "\nERROR: unspecified\n"
  in
  let argc = Array.length Sys.argv in
  if argc = 1
  then (
    try repl () with
    | e -> handle_exn e)
  else if argc = 2
  then (
    let infile = open_in Sys.argv.(1) in
    (try run_program infile with
     | e ->
       handle_exn e;
       close_in infile;
       exit 1);
    close_in infile;
    exit 0)
  else Printf.fprintf stderr "Usage: %s [input_filename]\n" Sys.argv.(0)
;;
