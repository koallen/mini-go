(* File calc.ml *)
open Ast
open Vm
open Icgenerator

let file_to_string filename =
    let ic = open_in filename in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    (s)

let _ =
  try
    (*Printf.printf "Starting\n";*)
    let source = file_to_string Sys.argv.(1) in
    (*Printf.printf "Program compiling\n";*)
    let lexbuf = Lexing.from_string source in
    (*Printf.printf "Lexed\n";*)
    let prog = Parser.prog Lexer.token lexbuf in
    (*Printf.printf "Parsed\n";*)
    if Checker.typeCheckProgram prog then (
        let translatedProg = Icgenerator.translateProg prog in
        (match translatedProg with
        | IRC (procs, main) -> let mainVMCode = (Vmgenerator.translateMain main) @ [Halt] in
                               let vmCodeHalt = Vmgenerator.translateAllFuncs mainVMCode procs in
                               let updatedVmCode = Vmgenerator.updateJumps vmCodeHalt [] vmCodeHalt in
                               (*Vm.printVMList updatedVmCode 0;*)
                               Vm.run updatedVmCode
        )
    )
    else
        failwith "Type error!";
    flush stdout
  with Parsing.Parse_error ->
      Printf.printf "%s" "Syntax error!\n";
  (*| Invalid_argument error_message ->*)
      (*Printf.printf "%s" "Usage: ./calc main.go\n";*)
      (*Printf.printf "%s" error_message;*)
      (*exit 0*)
  | Failure error_message ->
          Printf.printf "%s\n" error_message;


        (*Icgenerator.translateProg (Normalization.normalizeProg prog))*)
