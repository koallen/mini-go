(* File calc.ml *)
let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let result = Parser.prog Lexer.token lexbuf in
        Printf.printf "%s" "Parsing is successful!";
        print_newline();
        flush stdout
  with Lexer.Eof ->
    exit 0
