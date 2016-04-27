%{
  open Cmdtypes
%}

%token TRUE FALSE
%token <int> INT
%token <float> FLOAT
%token <string> STRING

%start main
%type <Cmdtypes.input list> main

%%

main:
  | expr
    { [$1] }
  | expr main
    { $1::$2 }

expr:
  | TRUE
    { `Bool true }
  | FALSE
    { `Bool false }
  | INT
    { `Int $1 }
  | FLOAT
    { `Float $1 }
  | STRING
    { `String $1 }
