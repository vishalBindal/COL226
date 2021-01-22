exception InvalidInput;;
exception UnequalVectorSize;;
exception UnequalMatrixShape;;
exception IncompatibleMatrixShape;;
exception SingularMatrix;;

(* General list functions *)
let hd ls = match ls with [] -> raise InvalidInput | x::xs -> x;;
let tl ls = match ls with [] -> raise InvalidInput | x::xs -> xs;;
let rec map f ls = match ls with [] -> [] | x::xs -> (f x)::(map f xs);;
let rec length ls = match ls with [] -> 0 | x::xs -> 1+(length xs);;
(* Remove (i)th element of a list, i>=1 *)
let rec removei i ls = match ls with
	[]-> [] | x::xs ->
	(if i=1 then xs else x::(removei (i-1) xs));;

(*General tuple functions*)
let first tup = match tup with (x,y) -> x;;
let second tup = match tup with (x,y) -> y;;

(* Vector type def*)
type vector = float list;;

(* Dimension of vector *)
let vdim (vector:vector) = 
	let rec vdim_tr vector dim= 
		match vector with
		[] -> dim | x::xs -> (vdim_tr xs (dim+1)) in 
	vdim_tr vector 0;;

(* Make zero vector of given dimension*)
let mkzerov dim = 
	let rec mkzerov_tr dim (vector:vector) =  
		if dim=0 then vector
		else (mkzerov_tr (dim-1) (0.0::vector)) in
	if(dim<=0) then raise InvalidInput else mkzerov_tr dim [];;

(* Check if zero vector *)
let rec isvzerov (vector:vector) = 
	match vector with
	[] -> true | x::xs -> (if x=0.0 then (isvzerov xs) else false);;

(* Add 2 vectors *)
let rec addv (v1:vector) (v2:vector) :vector= 
	if not ((vdim v1)=(vdim v2)) then raise UnequalVectorSize else
	match v1 with
	[] -> [] | x1::xs1 -> (match v2 with 
		[] -> [] | x2::xs2 -> (x1 +. x2)::(addv xs1 xs2));;

(* Multiply scalar with vector *)
let rec scalarmultv scalar (vector:vector) :vector=
	match vector with 
	[]-> [] | x::xs -> (scalar *. x)::(scalarmultv scalar xs);;

(* Dot product of 2 vectors *)
let dotprodv (v1:vector) (v2:vector) =
	let rec dotprodv_tr (v1:vector) (v2:vector) dot = 
		match v1 with
		[]-> dot | x1::xs1 -> (match v2 with [] -> dot | x2::xs2 -> (dotprodv_tr xs1 xs2 (dot +. (x1*. x2)))) in
	if not ((vdim v1)=(vdim v2)) then raise UnequalVectorSize else dotprodv_tr v1 v2 0.0;;

(* Type matrix definition*)
type matrix = float list list;;

(* Check if given matrix is valid : Each float list has the same dimension *)
let validmatrix (matrix:matrix) =
	let rec validmatrix_tr matrix lastl = match matrix with
		[] -> true | x::xs -> 
		(if ((length x) = lastl || lastl = -1) then (validmatrix_tr xs (length x)) else false) in
	validmatrix_tr matrix (-1);;

(* Find dimensions of given matrix (m,n) *)
let rec mdim (matrix:matrix) = 
	if not (validmatrix matrix) then raise InvalidInput else
    match matrix with
	[] -> (0,0) | x::xs -> ((length matrix), (length x));;

(* Make zero matrix of dimensions (rows, cols) *)
let mkzerom rows cols :matrix= 
	let rec mkm rows cols (matrix:matrix)=
		if (rows=0 || cols=0) then matrix  
		else mkm (rows-1) cols ((mkzerov cols)::matrix) in
	if not(rows>0 && cols>0) then raise InvalidInput else mkm rows cols [];;

(* Check if given matrix is zero matrix *)
let rec iszerom (matrix:matrix) = 
	if not (validmatrix matrix) then raise InvalidInput else
	match matrix with
	[] -> true | x::xs -> (if (isvzerov x) then (iszerom xs) else false);;

(* Make unit matrix of given dimension *)
let mkunitm (dim:int) :matrix = 
	let rec idrow (i:int) (n:int) : float list = if n<=0 then [] 
		else (if i=0 then 1.0::(idrow (i-1) (n-1)) else 0.0::(idrow (i-1) (n-1))) in
	let rec mkrows (i:int) (n:int) : matrix = if i>=n then [] else (idrow i n)::(mkrows (i+1) n) in
	if(dim<=0) then raise InvalidInput else (mkrows 0 dim);;

(* Check if given matrix is unit matrix *)
let isunitm (matrix:matrix) = 
	let rec issquare matrix = 
		match (mdim matrix) with (x,y) -> if (x=y) then true else false in
	let rec iscorrow ls i = match ls with [] -> true 
		| x::xs -> (if i=0 then (if x=1.0 then (iscorrow xs (i-1)) else false) else 
		(if x=0.0 then (iscorrow xs (i-1)) else false)) in
	let rec isidentity m i= match m with []->true | x::xs -> if(iscorrow x i) then (isidentity xs (i+1)) else false in
	if not (validmatrix matrix) then raise InvalidInput else if((issquare matrix) && (isidentity matrix 0)) then true else false;;

(* Add 2 matrices *)
let rec addm (m1:matrix) (m2:matrix) :matrix= 
	if not ((validmatrix m1) && (validmatrix m2)) then raise InvalidInput else 
	if ((mdim m1)<>(mdim m2)) then raise UnequalMatrixShape else match m1 with 
	[] -> [] | x1::xs1 -> (match m2 with []-> [] | x2::xs2 -> (addv x1 x2)::(addm xs1 xs2));;

(* Multiply matrix with scalar *)
let rec scalarmultm scalar (matrix:matrix) :matrix =
	if not (validmatrix matrix) then raise InvalidInput else match matrix with
	[] -> [] | x::xs -> (scalarmultv scalar x)::(scalarmultm scalar xs);;

(* Transpose *)
let rec transm (matrix:matrix) :matrix= 
	if not (validmatrix matrix) then raise InvalidInput else
	match matrix with
	[] -> [] | []::rows -> (transm rows) | (x::xs)::rows ->
		(x::(map hd rows))::(transm (xs::(map tl rows)));;

(* Determinant *)
let rec detm (m:matrix) :float = 
	if not (validmatrix m) then raise InvalidInput else
	match m with
	[] -> 0.0 | [[x]] -> x | x::xs -> iterm m x 1.0 1 0.0  
	and term (m:matrix) (sign:float) (count:int) (elem:float) :float= match m with [] -> 0. |
	x::xs -> elem *. (sign *. (detm (map (removei count) xs)))
	and iterm (m:matrix) (ls:float list) (sign:float) (count:int) (ans:float) :float= match ls with 
	[] ->  ans | x::xs -> iterm m xs (sign*.(-1.0)) (count+1) (ans+.(term m sign count x));; 

(* Multiplication of 2 matrices *)
let multm (m1:matrix) (m2:matrix) :matrix = 
	let rec term (ls:float list) (m:matrix) = match ls with [] -> [] | y::ys ->
		(match m with [] -> [] | x::xs -> (dotprodv ls x)::(term ls xs)) in
	let rec multhelper (m1:matrix) (m2:matrix) :matrix = 
		match m1 with [] -> [] | x::xs -> (term x m2)::(multhelper xs m2) in
	if not ((validmatrix m1) && (validmatrix m2)) then raise InvalidInput else 
	if ((second(mdim m1))<>(first (mdim m2))) then raise IncompatibleMatrixShape else
	multhelper m1 (transm m2);;

(* Cofactor of given matrix *)
let rec cofact m = 
	let rec cofactlshelper ls mwols sign count= match ls with
		[] -> [] | x::xs -> 
		((detm (map (removei count) mwols))*.sign)::(cofactlshelper xs mwols (sign *. (-1.0)) (count+1)) in
	let rec cofactls ls mwols countrow= if(countrow mod 2=0) then (cofactlshelper ls mwols (-1.) 1) else
		(cofactlshelper ls mwols 1. 1) in
	let rec cofacthelp mcur m count= match mcur with []-> [] 
		| x::xs -> (cofactls x (removei count m) count)::(cofacthelp xs m (count+1)) in
	cofacthelp m m 1;;
	
(* Inverse *)
let invm m = 
	if not (validmatrix m) then raise InvalidInput
	else let det = detm m in
	if ((det)=0.) then raise SingularMatrix else scalarmultm (1./.(det)) (transm (cofact m));;

(* A term of the cross product vector *)
let rec termcrossprod count m dim sign= if count>dim then [] 
	else (sign*.(detm (map (removei count) m)))::(termcrossprod (count+1) m dim (sign*.(-1.0)));;

(* Cross product of 2 vectors (in 3 dimensions) *)
let crossprodv (v1:vector) (v2:vector) :vector =
	let m = v1::(v2::[]) in
	if not ((vdim v1)=3 && (vdim v2)=3) then raise UnequalVectorSize else termcrossprod 1 m 3 1.;;

(* Cross product of n vectors *)
let rec crossprodgen (vls : vector list) :vector = 
	if not (validmatrix vls) then raise UnequalVectorSize else 
	match (mdim vls) with (x,y) -> if not(x+1=y) then raise InvalidInput
	else termcrossprod 1 vls (vdim (hd vls)) 1.;;


(* Test cases: *)

(*
let a = mkunitm 3;;
let b =  scalarmultm 2. a;;
mdim a;;
let c = mkzerom 2 3;;
mdim c;;
iszerom c;;
iszerom a;;
isunitm a;;
isunitm c;;
addm a b;;
multm a b;;
transm c;;
detm b;;
invm b;;

let a = mkzerov 5;;
let b = [1.;2.;3.;4.;5.];;
vdim a;;
isvzerov a;;
isvzerov b;;
addv b b;;
dotprodv b b;;
scalarmultv 3. b;;
let c = [1.;0.;0.];;
let d = [0.;0.;1.];;
crossprodv c d;;
let a = [[1.;2.;3.;4.];[0.;0.;1.;2.];[5.;5.;9.;11.]];;
crossprodgen a;;
*)