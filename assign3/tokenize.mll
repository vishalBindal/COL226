{
  open Parser
  exception Eof
(*for printing , (for testing) *)
  open Printf
}
let digit = ['0'-'9']
let num = '0' | ['1'-'9'] digit*
let int = ['+' '-']? num
let sp = [' ' '\t']*		(*spaces or tabs*)

rule token = parse
		[' ' '\t']	{token lexbuf} (*skip space and tab*)
	|	'\n'	{EOL}
	|	'['	{LPAREN}
	|	']'	{RPAREN}
	|	'('	{LBR}
	|	')'	{RBR}
	|	','	{COMMA}
	|	':'	{COLON}
	|	":="	{ASSIGN}
	|	';'	{SEMICOLON}
	|	['+' '-']? num ('.' digit* ['1'-'9'])?	as flt {FLOAT(float_of_string flt)}	(*float without extra leading or trailing zeroes NOTE: 2.0 is invalid, 2 is valid*)
	|	'[' sp (int as i1) sp ',' sp (int as i2) sp ']'	{I(int_of_string i1, int_of_string i2)}
	|	'('sp ('[' sp (int as i1) sp ',' sp (int as i2) sp ']') sp ':' sp ('[' sp (int as i3) sp ',' sp (int as i4) sp ']') sp ')'	{R((int_of_string i1, int_of_string i2),(int_of_string i3, int_of_string i4))}
	|	"SUM"	{SUM}
	|"AVG"	{AVG}
	|"MIN"	{MIN}
	|"MAX"	{MAX}
	|"COUNT"	{COUNT}
	|"ROWCOUNT"	{ROWCOUNT}
	|"COLCOUNT"	{COLCOUNT}
	|"ROWSUM"	{ROWSUM}
	|"COLSUM"	{COLSUM}
	|"ROWAVG"	{ROWAVG}
	|"COLAVG"	{COLAVG}
	|"ROWMIN"	{ROWMIN}
	|"COLMIN"	{COLMIN}
	|"ROWMAX"	{ROWMAX}
	|"COLMAX"	{COLMAX}
	|	"ADD"	{ADD}
	|"SUBT"	{SUBT}
	|"MULT"	{MULT}
	|"DIV"	{DIV}
	| eof 	{raise Eof}

