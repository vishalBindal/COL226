{
  exception Eof
(*for printing , (for testing) *)
	open Printf
(*defining tokens*)
	type token = 
	| EOL (*end of line*)
	| LPAREN (*left parenthesis [*)
	| RPAREN (*right parenthesis ]*)
	| LBR (*left bracket ( *)
	| RBR	 (*right bracket ) *)
	| COMMA
	| COLON
	| ASSIGN	(* := *)
	| SEMICOLON
	| FLOAT of float
	| I of int*int	(*indices*)
	| R of (int*int)*(int*int)	(*range*)
	| SUM
	| AVG
	| MIN
	| MAX
	| COUNT
	| ROWCOUNT
	| COLCOUNT
	| ROWSUM
	| COLSUM
	| ROWAVG
	| COLAVG
	| ROWMIN
	| COLMIN
	| ROWMAX
	| COLMAX
	| ADD
	| SUBT
	| MULT
	| DIV
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
	|	'('sp ('[' sp (int as i1) sp ',' sp (int as i2) sp ']') sp ',' sp ('[' sp (int as i3) sp ',' sp (int as i4) sp ']') sp ')'	{R((int_of_string i1, int_of_string i2),(int_of_string i3, int_of_string i4))}
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

{
	(* Scanning from stdin and printing to console for testing *)
	let main() = begin
		try
			let lexbuf = Lexing.from_channel stdin in
			while true do
				let result = token lexbuf in
				match result with
					| EOL -> printf "EOL\n"
					| LPAREN -> printf "LPAREN "
					| RPAREN-> printf "RPAREN "
					| LBR -> printf "LBR "
					| RBR -> printf "RBR "
					| COMMA -> printf "COMMA "
					| COLON -> printf "COLON "
					| ASSIGN -> printf "ASSIGN "
					| SEMICOLON -> printf "SEMICOLON "
					| FLOAT(f) -> printf "FLOAT(%f) " f
					| I(i1,i2) -> printf "I(%d,%d) " i1 i2
					| R((i1,i2),(i3,i4)) -> printf "R((%d,%d),(%d,%d)) " i1 i2 i3 i4
					| SUM -> printf "SUM "
					| AVG -> printf "AVG "
					| MIN -> printf "MIN "
					| MAX -> printf "MAX "
					| COUNT -> printf "COUNT "
					| ROWCOUNT -> printf "ROWCOUNT "
					| COLCOUNT -> printf "COLCOUNT "
					| ROWSUM -> printf "ROWSUM "
					| COLSUM -> printf "COLSUM "
					| ROWAVG -> printf "ROWAVG "
					| COLAVG -> printf "COLAVG "
					| ROWMIN -> printf "ROWMIN "
					| COLMIN -> printf "COLMIN "
					| ROWMAX -> printf "ROWMAX "
					| COLMAX -> printf "COLMAX "
					| ADD -> printf "ADD "
					| SUBT -> printf "SUBT "
					| MULT -> printf "MULT "
					| DIV -> printf "DIV "
			done
		with Eof -> exit 0
		end ;;
		main () ;;
}



