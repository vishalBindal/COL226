open List;;
exception NOT_UNIFIABLE;;
exception UNEXPECTED;;

type variable = string;;
type symbol = string;;
type term = V of variable | Node of symbol * (term list);;
type signature = (symbol*int) list;;
type substitution = (term* term) list;; 

(*Quick-sort for signature (i.e. (symbol*int) list) by sorting on basis of lexicographical order of string of symbol*)
let rec qsort s = 
  let gt x tup = match tup with (y,_) -> y > x in (*returns true if first element of tup > x*)
  let lte x tup = not (gt x tup) in (* returns true if first element of tup <= x *)
  match s with [] -> []
  | [x] -> [x]
  | (x,arity)::ls -> (qsort (filter (lte x) ls)) @ ((x,arity)::(qsort (filter (gt x) ls)));;

(* check_sig checks 
(1) arity >= 0 for all symbols
(2) no repetition of symbols
(3) symbols <> "" *)
  let check_sig (sign:signature) = 
  (* check_sig_sorted assumes the signature 'sign' is sorted acc to lexicographic order of symbol*)
  (* prev_sym = symbol of previous (sym,arity) tuple in the signature list *)
  let rec check_sig_sorted sign prev_sym = match sign with [] -> true 
    | (sym, arity)::ls -> if (arity >= 0 && sym <> prev_sym && sym <> "") 
      then check_sig_sorted ls sym else false in
check_sig_sorted (qsort sign) "";;

(* wfterm checks
(1) all symbols belong to the given signature
(2) no of elements in list of each symbol equals to its arity *)
let rec wfterm (sign:signature) preterm=
  let rec issym sym sign = match sign with [] -> false  (*returns true if symbol sym belongs to signature sign*)
  | (x,_)::ls -> if sym=x then true else issym sym ls in
  let rec getarity sym sign = match sign with [] -> -1  (*get arity of symbol sym in signature sign*)
  | (s,ar)::ls -> if s=sym then ar else getarity sym ls in
  let and1 a b = a && b in (*and of a and b*)
  match preterm with V(var) -> not (issym var sign) (* for variable, just check if it is not a symbol*)
  | Node(sym, ls) -> if (issym sym sign && length ls = getarity sym sign) then 
    (fold_left and1 true (map (wfterm sign) ls)) else false;; 

(* ht(variable) = 0, ht(f(t1,t2,..tk)) = 1 + max(-1,ht(t1),ht(t2)..,ht(tk)), k>=0 *)
let rec ht preterm = match preterm with V(var) -> 0 
  | Node(_,ls) -> 1 + fold_left max (-1) (map ht ls);;

(* size(variable) = 1, size(f(t1,t2,...tk)) = 1 + size(t1) + size(t2) + ... size(tk), k>=0 *)
let rec size preterm = 
  let add x y = x+y in
  match preterm with V(var) -> 1
  | Node(_,ls) -> fold_left add 1 (map size ls);; 

(* set(var) = [var], set(f(t1,t2,..tk)) = combinesets (set(t1) set(t2) .. set(tk)), k>=0 *)
let rec set preterm = 
  let rec addtoset set x = match set with [] -> [x] (*add element x to set, x may or may not already be in set*)
  | y::ls -> if x=y then set else y::(addtoset ls x) in
  let rec combinesets set1 set2 = match set2 with [] -> set1 (*combine 2 sets set1 and set2*)
  | x::ls -> combinesets (addtoset set1 x) ls in
  match preterm with V(var) -> [var] 
  | Node(_,ls) -> fold_left combinesets [] (map set ls);;

(* for each variable in term t, search if there is a substitution in s*)
let rec subst (s:substitution) t = 
  let rec findsub s t = match s with [] -> t 
    | (x,y)::ls -> if x=t then y else findsub ls t in
  match t with V(var) -> findsub s t
  | Node(sym,ls) -> Node(sym, (map (subst s) ls));;

(* compose 2 valid substitutions s1 and s2 *)
let rec compose (s1:substitution) (s2:substitution) :substitution = 
  let replace x y = match (y,x) with ((a,b),(b',c)) -> 
    if b=b' then (a,c) else y in
  match s2 with [] -> s1
  | x::ls -> compose (x::(map (replace x) s1)) ls;;
  
(* mgu of 2 well-formed terms t1 and t2 *)
let rec mgu t1 t2 = 
  let rec belongs y s = match s with [] -> false
    | x::ls -> if x=y then true else belongs y ls in  
  match t1 with 
  V(x) -> 
    (match t2 with
      | V(y) -> if x=y then [] else [(t1,t2)]
      | Node(sym, ls) -> if (belongs t1 ls) then raise NOT_UNIFIABLE else [(t1,t2)])
  | Node(sym,[]) ->
    (match t2 with
      Node(sym2,_) -> if sym=sym2 then [] else raise NOT_UNIFIABLE 
      | V(x) -> [(t2,t1)])
  | Node(sym,ls) ->
    (match t2 with
      Node(sym2,ls2) -> if sym=sym2 then (findsigma ls ls2 []) else raise NOT_UNIFIABLE
      | V(x) -> if (belongs t2 ls) then raise NOT_UNIFIABLE else [(t2,t1)])
(* given sprev = sigma1.sigma2....sigma(k-1) and ls1 = [tk,t(k+1)...tn] and ls2 = [t'k,t'(k+1)...t'n], 1<=k<=n+1
find sigma1.sigma2...sigma(n) *)
and findsigma ls1 ls2 sprev = match (ls1,ls2) with
  ([],[]) -> sprev
  | (t1::nx1, t2::nx2) -> findsigma nx1 nx2 (compose sprev (mgu (subst sprev t1) (subst sprev t2)))
  | (_::_,[]) -> raise UNEXPECTED | ([],_::_) -> raise UNEXPECTED;;
  

(*
(* Test cases *)

(* Test for check_sig *)
let s1 = [("Zero",0);("One",0);("Add",2);("Inc",1);("Some",5)];;
check_sig s1;;
let s2 = [("A",-1);("B",1)];;
check_sig s2;;
let s3 = [("Abc",1);("F",2);("Abc",4)];;
check_sig s3;;

(* Test for wfterm *)
let x = V("x");;
let y = V("y");;
let z = Node("Zero",[]);;
let o = Node("One",[]);;
let term = Node("Inc",[Node("Add", [x;Node("Some",[x;z;o;y;Node("Inc", [x])])])]);;
wfterm s1 term;;
let term' = Node("Inc", [x;y]);;
wfterm s1 term';;
let term'' = Node("ZeroX", []);;
wfterm s1 term'';;

(* Test for ht,size,set *)
ht term;;
size term;;
set term;;

(* Test for subst *)
let sub1 = [(V("x"), Node("Zero",[]));(V("y"),Node("Add",[z;o]))];;
subst sub1 term;;

(* Test for mgu *)
let t1 = x;;
let t2 = z;;
let t3 = Node("Add",[x;x]);;
let t4 = Node("Add",[y;z]);;
mgu t3 t4;;
let term1 = Node("Some",[t1;t2;t3;t4;Node("Inc",[o])]);;
let term2 = Node("Some", [z;x;y;Node("Add",[Node("Add", [z;z]);z]);Node("Inc",[o])]);;
mgu term1 term2;;
*)
