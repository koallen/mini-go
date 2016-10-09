(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
open Char
exception Eof
}
rule token = parse
    [' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
  | "int"          { INT }
  | "bool"         { BOOL }
  | "chan int"     { CHANINT }
  | "true"         { TRUE }
  | "false"        { FALSE }
  | ['a'-'z']+ as lxm { LETTER lxm }
  | ['0'-'9']+ as lxm { DIGIT lxm }
  | "<-"           { CHANOP }
  | '>'            { GREATER }
  | "=="           { EQUAL }
  | "&&"           { AND }
  | '!'            { NOT }
  | ','            { COMMA }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | eof            { EOF }
