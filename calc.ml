(* File calc.ml *)
let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
        let result = Parser.factor Lexer.token lexbuf in
        print_int 1; print_newline(); flush stdout
    done
  with Lexer.Eof ->
    exit 0
