exception UNEXPECTED;;

type exp = Var of string | Lambda of string * exp | Apply of exp * exp;;

type table = (string * answer) list
  and answer = Int of int | Bool of bool | Vclos of table * exp;;

type clos = I of int | B of bool | Clos of table * exp

type stack = clos list;;

(* returns gamma[x->d'], where d' is the answer type equivalent of d*)
let rec bind (x: string) (d : clos) (gamma : table) :table =
  match gamma with 
  (y,d')::ls -> if(x<>y) then (y,d')::(bind x d ls)
  else (
    match d with 
    I(i) ->  (x,Int(i))::ls
    | B(b) -> (x, Bool(b))::ls
    | Clos(g, e) -> (x, Vclos(g, e))::ls
  )
  | [] -> 
  (
    match d with 
    I(i) ->  [(x,Int(i))]
    | B(b) -> [(x, Bool(b))]
    | Clos(g, e) -> [(x, Vclos(g, e))]
  );;

(* Returns c' (clos type equivalent of c) if (x,c) present in gamma *)
let rec lookup (gamma:table) (x:string) : clos = 
  match gamma with 
  (y,a)::ls -> if(x<>y) then lookup ls x
  else (
    match a with 
    Int(i) -> I(i)
    | Bool(b) -> B(b)
    | Vclos(gamma, e) -> Clos(gamma, e)
  )
  | [] -> raise UNEXPECTED;;

(* Krivine machine *)
let rec krivine (c:clos) (s:stack) :answer= 
  match (c,s) with
  (I(i), _) -> Int(i)
  | (B(b), _) -> Bool(b)
  | (Clos(gamma, Var(x)), s) -> krivine (lookup gamma x) s
  | (Clos(gamma, Lambda(x, e)), d::s') -> krivine (Clos(bind x d gamma, e)) s'
  | (Clos(gamma, Apply(e1, e2)), s) -> krivine (Clos(gamma, e1)) (Clos(gamma, e2)::s)
  | _ -> raise UNEXPECTED;;  

let eval e rho = krivine (Clos(rho,e)) [];;