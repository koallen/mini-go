/* File parser.mly */
%{
    open Ast
%}
%token <string> LETTER
%token <string> DIGIT
%token INT BOOL CHANINT
%token PLUS MINUS TIMES DIV
%token GREATER
%token EQUAL AND NOT
%token LPAREN RPAREN
%token LCURLY RCURLY
%token FUNC
%token GO
%token DECLAR ASSIGN
%token WHILE IF ELSE
%token RETURN
%token PRINT
%token TRUE FALSE
%token CHANOP NEWCHAN
%token SEMI
%token COMMA
%token EOF
%start prog             /* the entry point */
%type <Ast.prog> prog
%%
prog:
    proclist block { Prog ($1, $2) }
;
proclist:
    proc proclist { $1::$2 }
  |               { [] }
;
proc:
    FUNC name LPAREN RPAREN block { Proc ($2, [], None, $5) }
  | FUNC name LPAREN param RPAREN block { Proc ($2, $4, None, $6) }
  | FUNC name LPAREN RPAREN types block { Proc ($2, [], Some $5, $6) }
  | FUNC name LPAREN param RPAREN types block { Proc ($2, $4, Some $6, $7) }
;
param:
    vars types COMMA param  {($1, $2)::$4}
  | vars types              {($1, $2)::[]}
;
statement:
    statement SEMI statement { Seq ($1, $3) }
  | GO block                 { Go $2 }
  | vars CHANOP aexp         { match $1 with
                               | Var s -> Transmit (s, $3)}
  | CHANOP vars              { match $2 with
                               | Var s -> RcvStmt s}
  | vars DECLAR bexp         { match $1 with
                               | Var s -> Decl (s, $3)}
  | vars DECLAR NEWCHAN      { match $1 with
                               | Var s -> DeclChan s}
  | vars ASSIGN bexp         { match $1 with
                               | Var s -> Assign (s, $3)}
  | WHILE bexp block         { While ($2, $3) }
  | IF bexp block ELSE block { ITE ($2, $3, $5) }
  | RETURN bexp              { Return $2 }
  | name LPAREN arg RPAREN   { FuncCall ($1, $3) }
  | PRINT bexp               { Print $2 }
;
block:
    LCURLY statement RCURLY { $2 }
;
bexp:
    cexp          { $1 }
  | cexp AND bexp { And ($1, $3) }
;
cexp:
    cterm         { $1 }
  | cterm EQUAL cterm { Eq ($1, $3) }
;
cterm:
    aexp          { $1 }
  | aexp GREATER aexp { Gt ($1, $3) }
;
aexp:
    term          { $1 }
  | term PLUS aexp    { Plus ($1, $3) }
  | term MINUS aexp   { Minus ($1, $3) }
;
term:
    factor   { $1 }
  | factor TIMES term { Times ($1, $3) }
  | factor DIV term   { Division ($1, $3) }
;
factor:
    ints    { $1 }
  | bools   { $1 }
  | vars    { $1 }
  | CHANOP vars { match $2 with
                  | Var s -> RcvExp s }
  | NOT factor { Not $2 }
  | LPAREN bexp RPAREN { $2 }
  | name LPAREN arg RPAREN { FuncExp ($1, $3) }
;
arg:
    bexp COMMA arg  {$1::$3}
  | bexp            {$1::[]}
;
bools:
    TRUE        { BConst true }
  | FALSE       { BConst false }
;
ints:
    DIGIT       { IConst (int_of_string $1) }
;
name:
    LETTER      { $1 }
;
vars:
    LETTER { Var $1 }
  | LETTER digits_or_letters { Var ($1 ^ $2) }
;
digits_or_letters:
    LETTER digits_or_letters { $1 ^ $2 }
  | DIGIT digits_or_letters { $1 ^ $2 }
  | LETTER { $1 }
  | DIGIT { $1 }
;
types:
    INT     { TyInt }
  | BOOL    { TyBool }
  | CHANINT { TyChan TyInt }
;
