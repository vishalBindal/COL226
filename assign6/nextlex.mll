{
  open Nextparser
}

rule token = parse
		';' {NEXT}
	| '.' {END}
  | ['\n' ' ' '\t']  {token lexbuf}
	| _	{UNKNOWN}
  
