%{
	open Printf
	open Interpreter
    open List
%}	

%token ENDOFCLAUSE
%token LBR RBR COMMA ASSIGN
%token EOF
%token <string> VARIABLE
%token <string> ATOM
%start goal
%type <Interpreter.formula> goal

%%
goal:
    formula ENDOFCLAUSE {$1}

formula:
    ATOM LBR termlist RBR {($1,$3)}
    ;

termlist:
    term    {[$1]}
    | termlist COMMA term   {$1 @ [$3]}
    ;

term:
    VARIABLE    {Var($1)}
    | ATOM  {Const($1)}
    | formula   {Node($1)}
    /* ;

comma:
    COMMA   {true} */
