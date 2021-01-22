open Printf
open List
open Interpreter

(* Program containing list of clauses *)
let pro = 
  let file = open_in (Sys.argv.(1)) in
  let lexbuf = Lexing.from_channel file in
  Parser.program Tokenize.token lexbuf;;

let pro = setUniqueVars pro 0;;

(* Printing list of solutions one by one *)
let rec print_sub_list_onebyone subls = 
  let lexbuf = Lexing.from_channel stdin in
  match subls with 
  | []::[] -> printf "yes.\n\n"
  | sub::[] -> print_sub sub; printf ".\n\n";
  | sub::tl -> 
  (
    print_sub sub; flush stdout;
    let rec sig_response signal tl=
      if signal=1 then (print_sub_list_onebyone tl)
      else if signal=0 then (printf "\n"; flush stdout;)
      else (printf "Unknown action!\nAction? "; flush stdout; 
      let signal = Nextparser.signal Nextlex.token lexbuf in 
      sig_response signal tl) in
    let signal = Nextparser.signal Nextlex.token lexbuf in 
    sig_response signal tl
  )
  | _ -> raise UNEXPECTED;;

(* Reading and printing user queries from the console *)
let _ = 
  try
    let lexbuf = Lexing.from_channel stdin in
    printf "?-" ; flush stdout;
    while true do
      let goal = Goalparser.goal Goallex.token lexbuf in
      let found, ansls = solve_goal goal pro in
      (if found then print_sub_list_onebyone ansls
      else printf "no.\n\n");
      printf "?-"; flush stdout
    done
  with Goallex.Eof ->
    printf "\nExiting!\n";
    exit 0;;

