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
%token TRUE FALSE
%token CHANOP
%token SEMI
%token COMMA
%token EOF
%start factor             /* the entry point */
%type <Ast.exp> factor
%%
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
