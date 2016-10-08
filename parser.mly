/* File parser.mly */
%token <int> INT
%token <string> VARS
%token <string> NAME
%token <string> TYPE
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token LCURLY RCURLY
%token SEMI
%token COMMA
%token EOL
%start param             /* the entry point */
%type <int> param
%%
param:
    VARS TYPE COMMA param {1}
  | VARS TYPE EOL {1}
;
