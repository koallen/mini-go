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
  | "while"        { WHILE }
  | "if"           { IF }
  | "else"         { ELSE }
  | "return"       { RETURN }
  | "print"        { PRINT }
  | "func"         { FUNC }
  | "go"           { GO }
  | "newChannel"   { NEWCHAN }
  | ['a'-'z']+ as lxm { LETTER lxm }
  | ['0'-'9']+ as lxm { DIGIT lxm }
  | "<-"           { CHANOP }
  | '>'            { GREATER }
  | "=="           { EQUAL }
  | ":="           { DECLAR }
  | '='            { ASSIGN }
  | "&&"           { AND }
  | '!'            { NOT }
  | ';'            { SEMI }
  | ','            { COMMA }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '{'            { LCURLY }
  | '}'            { RCURLY }
  | eof            { EOF }
