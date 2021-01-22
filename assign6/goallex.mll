{
  open Goalparser
  exception Eof
  open Printf
}
let atom = ['a'-'z']+ ['A'-'Z' 'a'-'z']* | '\'' [^ '\n' '\''] '\'' 
let variable = ['A'-'Z' '_'] ['0'-'9' 'a'-'z' 'A'-'Z' '_']*
let spaces = [' ' '\t']*		(*spaces or tabs*)

rule token = parse
		'.' {ENDOFCLAUSE}
	| '(' {LBR}
	| ')'	{RBR}
	| ',' {COMMA}
	| ':' '-' {ASSIGN}
	| atom as at {ATOM(at)}
	| variable as var {VARIABLE(var)}
	|	[' ' '\t' '\n']	{token lexbuf} (*skip spaces, tabs and newlines*)
	| eof 	{raise Eof}

