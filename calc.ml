(* File calc.ml *)
open Ast
open Vm

let file_to_string filename =
    let ic = open_in filename in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    (s)

let _ =
  try
    let source = file_to_string Sys.argv.(1) in
    let lexbuf = Lexing.from_string source in
    let prog = Parser.prog Lexer.token lexbuf in
    if Checker.typeCheckProgram prog then
        (*(Printf.printf "%s\n" (string_of_prog prog);*)
        (let translatedProg = Icgenerator.translateProg prog (Environ (None, [])) in
        (match translatedProg with
        | (code, place) -> let vmCode = Vmgenerator.translateCmd code [] [] in
        Vm.run (vmCode@[Halt])))
    else
        failwith "Type error!";
    flush stdout
  with Parsing.Parse_error ->
      Printf.printf "%s" "Syntax error!\n";
  | Invalid_argument error_message ->
      Printf.printf "%s" "Usage: ./calc main.go\n";
      Printf.printf "%s" error_message;
      exit 0
  | Failure error_message ->
          Printf.printf "%s" error_message;


        (*Icgenerator.translateProg (Normalization.normalizeProg prog))*)
