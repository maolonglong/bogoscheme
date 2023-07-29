open Bogoscheme

let repl () =
  Sys.catch_break true;
  let env = Env.make None in
  let rec loop () =
    print_string "> ";
    flush stdout;
    try
      (* read *)
      let input = read_line () in
      try
        let lexbuf = Lexing.from_string input in
        match Parser.parse Lexer.lex lexbuf with
        | None -> loop ()
        | Some sexpr ->
          let ast = Ast.ast_of_sexpr sexpr in
          (* eval *)
          let v = Eval.eval ast env in
          (* print *)
          print_endline (Env.string_of_value v);
          (* loop *)
          loop ()
      with
      | e ->
        (match e with
         | Failure f -> Printf.printf "ERROR: %s\n" f
         | Invalid_argument f -> Printf.printf "ERROR: %s\n" f
         | Eval.Type_error s -> Printf.printf "ERROR: type error: %s\n" s
         | Not_found -> Printf.printf "ERROR: name not found\n"
         | Sys.Break -> print_endline "Interrupted."
         | _ -> Printf.printf "ERROR: unspecified\n");
        loop ()
    with
    | Sys.Break ->
      print_newline ();
      loop ()
    | End_of_file -> exit 0
  in
  print_endline "BogoScheme REPL (Press Ctrl-D to exit)";
  Primitives.load env;
  loop ()
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

let () =
  let usage () = Printf.fprintf stderr "Usage: ./bs [filename]\n" in
  match Sys.argv with
  | [| _ |] -> repl ()
  | [| _; "-h" |] | [| _; "--help" |] | [| _; "help" |] -> usage ()
  | [| _; filename |] ->
    let infile = open_in filename in
    Sys.chdir (Filename.dirname filename);
    (try run_program infile with
     | e ->
       (match e with
        | Failure f -> Printf.fprintf stderr "\nERROR: %s\n" f
        | Eval.Type_error s -> Printf.fprintf stderr "\nERROR: type error: %s\n" s
        | Not_found -> Printf.fprintf stderr "\nERROR: name not found\n"
        | _ -> Printf.fprintf stderr "\nERROR: unspecified\n");
       close_in infile;
       exit 1);
    close_in infile;
    exit 0
  | _ -> usage ()
;;
