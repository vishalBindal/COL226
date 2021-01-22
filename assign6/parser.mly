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
%start program
%type <Interpreter.program> program

%%
program:
	clause ENDOFCLAUSE  {[$1]}
	| program clause ENDOFCLAUSE	{$1 @ [$2]}
    ;

clause:
	formula    {Fact($1)}   /*fact*/
	| formula ASSIGN body  {Rule($1,$3)}   /*rule*/
    ;

body:
    formula {[$1]}
    | body COMMA formula    {$1 @ [$3]}
    ;

formula:
    ATOM LBR termlist RBR   {($1,$3)}
    ;

termlist:
    term    {[$1]}
    | termlist COMMA term   {$1 @ [$3]}
    ;

term:
    VARIABLE    {Var($1)}
    | ATOM  {Const($1)}
    | formula   {Node($1)}




