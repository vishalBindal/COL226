exception UNEXPECTED;;

type exp = B of bool | I of int | Var of string 
	| Lambda of string * exp | Apply of exp * exp
	| Add of exp * exp | Mult of exp * exp | Gtz of exp;; 
(* Gtz : greater than zero *)

type opcode = LOOKUP of string | LAMBDA of string * (opcode list) | APPLY 
	| INT of int | BOOL of bool | ADD | MULT | GTZ;;

type table = (string * answer) list
	and answer = Bool of bool | Int of int | String of string 
				| Vcl of table*string*(opcode list);;

type dump = ((answer list) * table * (opcode list)) list;;

let rec compile (e:exp) : opcode list = 
	match e with 
	Var(x) -> [LOOKUP(x)] 
	| Lambda(x, e1) -> [LAMBDA(x, (compile e1))]
	| Apply(e1, e2) -> (compile e1)@(compile e2)@[APPLY]
	| I(i) -> [INT(i)]
	| B(b) -> [BOOL(b)]
	| Add(e1, e2) -> (compile e1)@(compile e2)@[ADD]
	| Mult(e1, e2) -> (compile e1)@(compile e2)@[MULT]
	| Gtz(e1) -> (compile e1)@[GTZ];;

(* Returns e[x->a] *)
let rec bind (x: string) (a : answer) (e : table) :table =
	match e with 
	(y,b)::ls -> if(x=y) then (x,a)::ls else (y,b)::(bind x a ls)
	| [] -> [(x,a)];;

(* Returns a if (x,a) present in e *)
let rec lookup (e:table) (x:string) : answer = 
	match e with 
	(y,a)::ls -> if(x=y) then a else lookup ls x
	| [] -> raise UNEXPECTED;;

(* SECD machine *)
let rec secd (s: answer list) (e: table) (c: opcode list) (d: dump) : answer = 
	match (s,e,c,d) with
	([a], _, [], []) -> a
	| (s, _, [], (s',e',c')::d') -> secd (s@s') e' c' d'
	| (s, e, LOOKUP(x)::c', d) -> secd ((lookup e x)::s) e c' d
	| (s, e, LAMBDA(x, c1)::c', d) -> secd (Vcl(e, x, c1)::s) e c' d
	| (a::(Vcl(e', x, c1)::s'), e, APPLY::c', d) -> secd [] (bind x a e') c1 ((s',e,c')::d)
	| (s, e, INT(i)::c', d) -> secd (Int(i)::s) e c' d
	| (s, e, BOOL(b)::c', d) -> secd (Bool(b)::s) e c' d
	| ((Int(a))::((Int(b))::s'), e, ADD::c', d) -> secd ((Int(a+b))::s') e c' d
	| ((Int(a))::((Int(b))::s'), e, MULT::c', d) -> secd ((Int(a*b))::s') e c' d
	| (Int(a)::s', e, GTZ::c', d) -> if a>0 then secd ((Bool(true))::s') e c' d
										else secd ((Bool(false))::s') e c' d
	| _ -> raise UNEXPECTED;; 

let eval (e:exp) (rho:table) = secd [] rho (compile e) [];; 
