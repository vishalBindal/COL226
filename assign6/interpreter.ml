open List;;
open Printf;;

(* Exceptions *)
exception NOT_UNIFIABLE;; 
exception UNEXPECTED;;

(* Type definitions *)
type variable = string;;
type atom = string;;
type term = Var of variable | Const of atom | Node of (atom* (term list));;
type formula = atom * (term list);;
type rule = formula * (formula list);; (* head*body *)
type fact = formula;; (* head *)
type clause = Fact of fact | Rule of rule ;;
type program = clause list;;
type substitution = (term* term) list;; 

(* Generic functions *)

(* Check if y belongs to list s *)
let rec belongs y s = match s with [] -> false
| x::ls -> if x=y then true else belongs y ls;;

(* Remove duplicates of any element present in substitution s1, from substitution s2 *)
let rec remove_duplicates s1 s2 = 
  let rec removefromset sub s = match s with [] -> []
  | (a,b)::tl -> match sub with (x,y) -> (if ((x=a && y=b)||(x=b && y=a)) then tl else (a,b)::(removefromset sub tl)) in
  match s1 with [] -> s2 | x::ls -> remove_duplicates ls (removefromset x s2);;

(* Print functions *)
let rec print_term t = match t with
  Var(v) -> print_string v
  | Const(c) -> print_string c
  | Node(sym,ls) -> print_string sym; printf("("); print_term_list ls; printf(")")
  
and print_term_list ls = match ls with
  [] -> printf ""
  | x::[] -> print_term x
  | x::tl -> print_term x; printf ", ";print_term_list tl;;

let rec print_sub sub= 
  match sub with [] -> printf "yes"
  | x::[] -> (match x with (t1, t2) -> print_term t1; printf " = ";print_term t2;)
  | x::ls -> match x with (t1, t2) -> print_term t1; printf " = ";print_term t2; printf ",\n";print_sub ls;;

let rec print_sub_list subls = match subls with 
  | []::[] -> printf "yes.\n\n"
  | sub::[] -> print_sub sub; printf ".\n\n";
  | sub::tl -> print_sub sub; printf " ;\n"; print_sub_list tl
  | _ -> raise UNEXPECTED;;

(* Substitution: for each variable in term t, search if there is a substitution in s, and perform*)
let rec subst (s:substitution) t = 
  let rec findsub s t = match s with [] -> t 
    | (x,y)::ls -> if x=t then y else findsub ls t in
  match t with Var(var) -> findsub s t
  | Node(sym,ls) -> Node(sym, (map (subst s) ls))
  | Const(c) -> t;;

(* compose 2 valid substitutions s1 and s2 *)
let compose s1 s2 = 
  let rec compose_distinct (s1:substitution) (s2:substitution) :substitution = 
    let replace x y = match (y,x) with ((a,b),(b',c)) -> 
      if b=b' then (a,c) else y in
    match s2 with [] -> s1
    | x::ls -> compose_distinct (x::(map (replace x) s1)) ls in
  compose_distinct s1 (remove_duplicates s1 s2);;

(* Most general unifier of 2 terms t1 and t2 *)
let rec mgu t1 t2 =
  if t1=Var("_") || t2=Var("_") then [] else 
  match t1 with 
  | Var(x) -> 
    (match t2 with
      | Var(y) -> if x=y then [] else [(t1,t2)]
      | Const(c) -> [(t1,t2)]
      | Node(_, ls) -> if (belongs t1 ls) then raise NOT_UNIFIABLE else [(t1,t2)])
  | Const(c) ->
    (match t2 with
      Const(d) -> if c=d then [] else raise NOT_UNIFIABLE
      | Var(x) -> [(t2,t1)]
      | Node(sym,ls) -> if ls=[] && sym=c then [] else raise NOT_UNIFIABLE )
  | Node(sym,ls) ->
    (match t2 with
      Node(sym2,ls2) -> if (sym=sym2 && (length ls)=(length ls2)) then (findsigma ls ls2 []) else raise NOT_UNIFIABLE
      | Var(x) -> if (belongs t2 ls) then raise NOT_UNIFIABLE else [(t2,t1)]
      | Const(c) -> if ls=[] && c=sym then [] else raise NOT_UNIFIABLE)
(* given sprev = sigma1.sigma2....sigma(k-1) and ls1 = [tk,t(k+1)...tn] and ls2 = [t'k,t'(k+1)...t'n], 1<=k<=n+1
find sigma1.sigma2...sigma(n) *)
and findsigma ls1 ls2 sprev = match (ls1,ls2) with
  ([],[]) -> sprev
  | (t1::nx1, t2::nx2) -> findsigma nx1 nx2 (compose sprev (mgu (subst sprev t1) (subst sprev t2)))
  | _ -> raise UNEXPECTED;;


(* Functions for setting unique variables for each rule*)

(* Check if a variable is an internal representation
i.e. is of the form $ruleno$varno, e.g. $1$2, or of the form '_' *)
let isInterVar v = (v.[0] == '$') || (v="_");;

(* Check if a term contains an 'internal' variable (one which is stored in the program
and not entered by the user in the query) *)
let rec containsInterVar term =
  let getor a b = (a || b) in
  match term with Var(v) -> isInterVar v
  | Const(c) -> false
  | Node(c, tls) -> fold_left getor false (map containsInterVar tls);; 

(* Check if a tuple tup (in a substitution list) has terms containing only user-entered
 variables in the current query, i.e. does not contain any 'internal'-defined variables.
 Such tuples are not printed on the console in the answer *)
let userSub tup = match tup with (t1,t2) ->
  not (containsInterVar t1 || containsInterVar t2);;

(* Search term t for any user-entered variable *)
let rec search_term t = match t with
  Var(v) -> if not (isInterVar v) then v else ""
  | Const(c) -> ""
  | Node(_, tls) -> search_term_list tls
and search_term_list tls = match tls with [] -> ""
  | x::ls -> let res = search_term x in 
    if res <> "" then res else search_term_list ls;; 
  
(* Substitute user variables with internal variables for a given clause number clid, in
a given term list tls.
vid is the variable no for each user variable in a given clause*)
let rec setTermList tls clid vid= 
  let v = search_term_list tls in
  if v="" then tls else 
    setTermList (map (subst [(Var(v), Var("$"^(string_of_int clid)^"$"^(string_of_int vid)))]) tls) clid (vid+1);;

(* Convert formula to term *)
let nodify f = Node(f);;
(* Convert term to formula *)
let denodify t = match t with Node(f) -> f | _ -> raise UNEXPECTED;;

(* substitute user variables with internal variables in a given clause with id clid *)
let setClauseVars clause clid = match clause with 
  Fact(f) -> 
  (
    match f with (at,tls) -> 
      Fact((at, (setTermList tls clid 0) ))
  )
  | Rule(r) -> 
  (
    match r with (f, fls) -> 
    match (map denodify (setTermList (map nodify (f::fls)) clid 0)) with 
    x::ls ->  Rule(x,ls) | [] -> raise UNEXPECTED
  );;

(* Set unique internal variables for a given program pro, starting with clause no. clid *)
let rec setUniqueVars pro clid = match pro with [] -> []
  | clause::ls -> (setClauseVars clause clid)::(setUniqueVars ls (clid+1));;


(* Functions to solve query *)

(* Try to unify formula form with formula f and update curfound, curansls if unified*)
let rec matchfact form f curfound curansls = 
  try
    let sub = (mgu (Node(form)) (Node(f))) in
    true, sub::curansls
  with NOT_UNIFIABLE ->
    curfound, curansls;;
  
(* Try to unify a formula list fls, with substitution lastsub already found to be applied to all terms *)
let rec match_formula_ls fls lastsub pro =
  match fls with [] -> true, [lastsub]
  | form::tl -> 
  (
    let curfound, curansls = (
      let termsubbed = subst lastsub (Node(form)) in
      let foundn, anslsn = solve (denodify termsubbed) pro pro false [] in
      foundn, (map (compose lastsub) anslsn)
    ) in
    if tl=[] then curfound, curansls 
    else if curfound then
      match_fls_multiple curansls tl pro false []
    else false, []
  )
(* Try to unify a formula list fls with the program, with a 
list of substitutions cursubls already found to be applied to all terms.
All substitutions in cursubls are tried one by one to find solutions, and 
found and curansls are updated *)
and match_fls_multiple cursubls fls pro found curansls=
  match cursubls with [] -> found, curansls
  | sub::tl -> 
  let foundnew, ansls = match_formula_ls fls sub pro in
  if foundnew then match_fls_multiple tl fls pro true (curansls@ansls)
  else match_fls_multiple tl fls pro found curansls

(* Try to unify a rule r with a formula form, and update curfound, curansls 
if solution found *)
and matchrule form r pro curfound curansls =
  try
    match r with (fhead, fbody) ->
      let found, ansls = match_formula_ls fbody (mgu (Node(form)) (Node(fhead))) pro in
      if found then true, (curansls@ansls)
      else curfound, curansls
  with NOT_UNIFIABLE ->
    curfound, curansls

(* Find a unification for formula form in the remaining program pro, 
with pro0 being the original program. 
found denotes whether a solution has been found
ansls denotes list of substitutions in the answer *)
and solve form pro pro0 found ansls=
    match pro with [] -> (found, ansls)
    | clause::protl -> 
    (
      match clause with Rule(r) -> (
      let newfound, newansls = matchrule form r pro0 found ansls in
        solve form protl pro0 newfound newansls
      )
      | Fact(f) -> (
        let newfound, newansls = matchfact form f found ansls in
          solve form protl pro0 newfound newansls
        )
    );;

(* Find a solution for user-entered formula 'goal' in the given program 'pro' *)
let solve_goal goal pro = 
  let found, ansls = solve goal pro pro false [] in
  found, map (filter userSub) ansls;;