exception InvalidInput;; (*When given index or range is invalid*)
exception Uninitialised;; (*When any element in given range is Empty (not raised in COUNT queries)*)
exception UnequalRange;;  (*When 2 given ranges are not of equal dimensions *)
exception DivisionByZero;;

type element = Value of float | Empty;;
type sheet = element array array;;
type index = int*int;;
type range = (int*int)*(int*int);;

(* Creating a new sheet of dimensions m X n and instantiating it with Empty elements*)
let newsheet m n :sheet= 
    Array.make_matrix m n Empty ;;

let m = int_of_string Sys.argv.(2);;
let n = int_of_string Sys.argv.(3);;
let csvpath = Sys.argv.(1);;

(* The sheet *)
let mysheet = newsheet m n;;

(* To check if given index is valid *)
let validindex (i:index) = match i with (x,y) -> 
    if (x>=0 && x<m && y>=0 && y<n) then true else false;;

(* To check if given range is valid *)
let validrange (r:range) = match r with ((x1,y1),(x2,y2)) ->
    if ((validindex (x1,y1)) && (validindex (x2,y2)) && x2>=x1 && y2>=y1) then true else false;;

(* Increment an element *)
let increment count = match count with Empty -> Empty | Value(x) -> Value(x +. 1.);;

(* operations on 2 elements : add, subtract, multiply, divide, less than*)
let add elem1 elem2 = match elem1 with Empty -> raise Uninitialised 
    | Value(x) -> match elem2 with Empty -> raise Uninitialised 
    | Value(y) -> Value(x+.y);;
let subt elem1 elem2 = match elem1 with Empty -> raise Uninitialised 
    | Value(x) -> match elem2 with Empty -> raise Uninitialised 
    | Value(y) -> Value(x-.y);;
let mult elem1 elem2 = match elem1 with Empty -> raise Uninitialised 
    | Value(x) -> match elem2 with Empty -> raise Uninitialised 
    | Value(y) -> Value(x*.y);;
let div elem1 elem2 = match elem1 with Empty -> raise Uninitialised 
    | Value(x) -> match elem2 with Empty -> raise Uninitialised 
    | Value(y) -> if y=0. then raise DivisionByZero else Value(x/.y);;
let lessthan elem1 elem2 = match elem1 with Empty -> raise Uninitialised
    | Value(x) -> match elem2 with Empty -> raise Uninitialised
    | Value(y) -> x<y;;

(* To implement a function for a ROW given a function for a full range 
e.g. ROWSUM can be implemented using SUM*)
let row_func (sheet:sheet) (r:range) (i:index) (full_func:sheet->range->index->sheet) :sheet= 
    if (not(validindex i) || not(validrange r))
    then raise InvalidInput
    else
        let rec call sheet row xm yi ym x y = 
            if row>xm then sheet 
            else let sheet = full_func sheet ((row,yi),(row,ym)) (x,y) in
                    call sheet (row+1) xm yi ym (x+1) y in 
        match r with ((x1,y1),(x2,y2)) -> match i with (x,y) ->
        call sheet x1 x2 y1 y2 x y;;
    
(*To implement a function for a COLUMN given a function for a full range*)
let col_func (sheet:sheet) (r:range) (i:index) (full_func:sheet->range->index->sheet) :sheet= 
    if (not(validindex i) || not(validrange r))
    then raise InvalidInput
    else
        let rec call sheet xi xm col ym x y = 
            if col>ym then sheet 
            else let sheet = full_func sheet ((xi,col),(xm,col)) (x,y) in
                    call sheet xi xm (col+1) ym x (y+1) in 
        match r with ((x1,y1),(x2,y2)) -> match i with (x,y) ->
        call sheet x1 x2 y1 y2 x y;;

let full_count (sheet:sheet) (r:range) (i:index) :sheet= 
    if (not (validindex i) || not (validrange r)) 
    then raise InvalidInput
    else 
        let rec fc sheet i j xm yi ym count = 
            if j>ym then (if i<xm then (fc sheet (i+1) yi xm yi ym count) else count) else
            if (sheet.(i)).(j) = Empty then (fc sheet i (j+1) xm yi ym count) 
            else (fc sheet i (j+1) xm yi ym (increment count)) in 
        match r with ((x1,y1),(x2,y2)) -> match i with (x,y) -> 
        (sheet.(x)).(y) <- (fc sheet x1 y1 x2 y1 y2 (Value(0.)));
        sheet;;

let row_count (sheet:sheet) (r:range) (i:index) :sheet=
    row_func sheet r i full_count;;

let col_count (sheet:sheet) (r:range) (i:index) :sheet= 
    col_func sheet r i full_count;;

(*Check if no cell in the given range is empty*)
let initialised (sheet:sheet) (r:range)=
    let rec fc sheet i j xm yi ym = 
        if j>ym then (if i<xm then (fc sheet (i+1) yi xm yi ym) else true) else
        if (sheet.(i)).(j) = Empty then false 
        else (fc sheet i (j+1) xm yi ym) in
    (*Assert: r is valid*)
    match r with ((x1,y1),(x2,y2)) -> 
    fc sheet x1 y1 x2 y1 y2;; 
    

let full_sum (sheet:sheet) (r:range) (i:index) :sheet= 
    if (not (validindex i) || not (validrange r)) 
    then raise InvalidInput
    else if not (initialised sheet r) then raise Uninitialised
    else 
        let rec fc sheet i j xm yi ym count = 
            if j>ym then 
                (if i<xm then 
                    (fc sheet (i+1) yi xm yi ym count) 
                else count)
            else
                fc sheet i (j+1) xm yi ym (add count (sheet.(i)).(j)) in 
        match r with ((x1,y1),(x2,y2)) -> match i with (x,y) -> 
        (sheet.(x)).(y) <- (fc sheet x1 y1 x2 y1 y2 (Value(0.)));
        sheet;;

let row_sum (sheet:sheet) (r:range) (i:index) :sheet= 
    if not(initialised sheet r) then raise Uninitialised else
    row_func sheet r i full_sum;;

let col_sum (sheet:sheet) (r:range) (i:index) :sheet=
    if not(initialised sheet r) then raise Uninitialised else
    col_func sheet r i full_sum;;

let full_avg (sheet:sheet) (r:range) (i:index) :sheet = 
    if (not (validindex i) || not (validrange r)) 
    then raise InvalidInput
    else if not (initialised sheet r) then raise Uninitialised
    else 
    let sheet = full_sum sheet r i in
    match r with ((x1,y1),(x2,y2)) -> match i with (x,y) -> match (sheet.(x)).(y) with
    Empty -> raise Uninitialised
    | Value(sum) -> (sheet.(x)).(y) <- Value(sum /. ( float_of_int ((y2-y1+1)*(x2-x1+1)) ));
    sheet;;

let row_avg (sheet:sheet) (r:range) (i:index) :sheet = 
    if not (initialised sheet r) then raise Uninitialised else
    row_func sheet r i full_avg;;

let col_avg (sheet:sheet) (r:range) (i:index) :sheet = 
    if not (initialised sheet r) then raise Uninitialised else
    col_func sheet r i full_avg;;
 
let full_min (sheet:sheet) (r:range) (i:index) :sheet= 
    if (not (validindex i) || not (validrange r)) 
    then raise InvalidInput
    else if not (initialised sheet r) then raise Uninitialised
    else 
        let rec fc sheet i j xm yi ym minelem = 
            if j>ym then 
                (if i<xm then (fc sheet (i+1) yi xm yi ym minelem) 
                else minelem)
            else if lessthan (sheet.(i)).(j) minelem then 
                fc sheet i (j+1) xm yi ym ((sheet.(i)).(j)) 
            else
                fc sheet i (j+1) xm yi ym minelem in 
        match r with ((x1,y1),(x2,y2)) -> match i with (x,y) -> 
        (sheet.(x)).(y) <- (fc sheet x1 y1 x2 y1 y2 (Value(infinity)));
        sheet;;

let row_min (sheet:sheet) (r:range) (i:index) :sheet = 
    if not (initialised sheet r) then raise Uninitialised else
    row_func sheet r i full_min;;

let col_min (sheet:sheet) (r:range) (i:index) :sheet = 
    if not (initialised sheet r) then raise Uninitialised else
    col_func sheet r i full_min;;

let full_max (sheet:sheet) (r:range) (i:index) :sheet= 
    if (not (validindex i) || not (validrange r)) 
    then raise InvalidInput
    else if not (initialised sheet r) then raise Uninitialised
    else 
        let rec fc sheet i j xm yi ym maxelem = 
            if j>ym then 
                (if i<xm then (fc sheet (i+1) yi xm yi ym maxelem) 
                else maxelem)
            else if lessthan maxelem (sheet.(i)).(j) then 
                fc sheet i (j+1) xm yi ym ((sheet.(i)).(j)) 
            else
                fc sheet i (j+1) xm yi ym maxelem in 
        match r with ((x1,y1),(x2,y2)) -> match i with (x,y) -> 
        (sheet.(x)).(y) <- (fc sheet x1 y1 x2 y1 y2 (Value(neg_infinity)));
        sheet;;

let row_max (sheet:sheet) (r:range) (i:index) :sheet = 
    if not (initialised sheet r) then raise Uninitialised else
    row_func sheet r i full_max;;

let col_max (sheet:sheet) (r:range) (i:index) :sheet = 
    if not (initialised sheet r) then raise Uninitialised else
    col_func sheet r i full_max;;

(*Check if 2 ranges are compatible, i.e. of same dimensions*)
let eqrange (r1:range) (r2:range) = 
    match r1 with ((a1,b1),(c1,d1)) -> match r2 with ((a2,b2),(c2,d2)) ->
    c1-a1 = c2-a2 && d1-b1 = d2 - b2 ;;

(*Making a common function for add_range, subt_range, mult_range and div_range
where op is the operation to be performed on corresponding elements in the 2 ranges
op = add, mult, subt, div*)
let op_range (sheet:sheet) (r1:range) (r2:range) (i:index) op :sheet=
    if not ((validindex i) && (validrange r1) && (validrange r2)) 
        then raise InvalidInput
    else if not ((initialised sheet r1) && (initialised sheet r2)) 
        then raise Uninitialised
    else if not (eqrange r1 r2) 
        then raise UnequalRange
    else
        let rec fc sheet i j xm yi ym dx1 dy1 dx2 dy2 op= 
            if j>ym then 
                (if i<xm then (fc sheet (i+1) yi xm yi ym dx1 dy1 dx2 dy2 op) 
                else sheet)
            else 
                let sheet = (sheet.(i)).(j) <- op ((sheet.(i+dx1)).(j+dy1)) ((sheet.(i+dx2)).(j+dy2)); sheet in
                fc sheet i (j+1) xm yi ym dx1 dy1 dx2 dy2 op in 
        match i with (x,y) -> match r1 with ((a1, b1), (c1, d1)) -> 
        match r2 with ((a2, b2), _ ) ->
        fc sheet x y (x+c1-a1) y (y+d1-b1) (a1-x) (b1-y) (a2-x) (b2-y) op;;

let add_range (sheet:sheet) (r1:range) (r2:range) (i:index) : sheet = 
    op_range sheet r1 r2 i add;;

let subt_range (sheet:sheet) (r1:range) (r2:range) (i:index) : sheet = 
    op_range sheet r1 r2 i subt;;

let mult_range (sheet:sheet) (r1:range) (r2:range) (i:index) : sheet = 
    op_range sheet r1 r2 i mult;;

let div_range (sheet:sheet) (r1:range) (r2:range) (i:index) : sheet = 
    op_range sheet r1 r2 i div;;

(*Making a common function for add_const, subt_const, mult_const and div_const
where op is the operation to be performed between the range element and constant float f
op = add, mult, subt, div*)
let op_const (sheet:sheet) (r:range) f (i:index) op :sheet=
    if not ((validindex i) && (validrange r)) 
        then raise InvalidInput
    else if not (initialised sheet r) 
        then raise Uninitialised
    else
        let rec fc sheet i j xm yi ym dx dy f op= 
            if j>ym then 
                (if i<xm then (fc sheet (i+1) yi xm yi ym dx dy f op) 
                else sheet)
            else 
                let sheet = (sheet.(i)).(j) <- op ((sheet.(i+dx)).(j+dy)) (Value(f)); sheet in
                fc sheet i (j+1) xm yi ym dx dy f op in 
        match i with (x,y) -> match r with ((a, b), (c, d)) -> 
        fc sheet x y (x+c-a) y (y+d-b) (a-x) (b-y) f op;;

let add_const (sheet:sheet) (r:range) f (i:index) : sheet = 
    op_const sheet r f i add;;

let subt_const (sheet:sheet) (r:range) f (i:index) : sheet = 
    op_const sheet r f i subt;;

let mult_const (sheet:sheet) (r:range) f (i:index) : sheet = 
    op_const sheet r f i mult;;

let div_const (sheet:sheet) (r:range) f (i:index) : sheet = 
    op_const sheet r f i div;;

(* For adding element in given index to all elements of a given range *)
let add_index (sheet:sheet) (r:range) (i1:index) (i:index) : sheet = 
    if not (validindex i1) then raise InvalidInput
    else match i1 with (x,y) -> match (sheet.(x)).(y) with 
    Empty -> raise Uninitialised | Value(f) ->
    op_const sheet r f i add;;

let subt_index (sheet:sheet) (r:range) (i1:index) (i:index) : sheet = 
    if not (validindex i1) then raise InvalidInput
    else match i1 with (x,y) -> match (sheet.(x)).(y) with 
    Empty -> raise Uninitialised | Value(f) ->
    op_const sheet r f i subt;;

let mult_index (sheet:sheet) (r:range) (i1:index) (i:index) : sheet = 
    if not (validindex i1) then raise InvalidInput
    else match i1 with (x,y) -> match (sheet.(x)).(y) with 
    Empty -> raise Uninitialised | Value(f) ->
    op_const sheet r f i mult;;

let div_index (sheet:sheet) (r:range) (i1:index) (i:index) : sheet = 
    if not (validindex i1) then raise InvalidInput
    else match i1 with (x,y) -> match (sheet.(x)).(y) with 
    Empty -> raise Uninitialised | Value(f) ->
    op_const sheet r f i div;;

(*Helper function for printing the sheet*)
let rec printhelper (sheet:sheet) i j xm yi ym = 
    if j>ym then (if i<xm then (print_string "\n"; printhelper sheet (i+1) yi xm yi ym) else print_string "\n\n") else
    match (sheet.(i)).(j) with Empty -> print_string " __ " ; (printhelper sheet i (j+1) xm yi ym) 
    | Value(f) -> print_string " "; print_float f; print_string " ";
        (printhelper sheet i (j+1) xm yi ym);;

(*Current instruction (formula) no*)
let insno = ref 1;;

(* Printing the sheet and incrementing insno *)
let printsheet (sheet:sheet) =
    begin
    Printf.printf "Sheet after formula [%d]:\n" !insno;
    incr insno;
    printhelper sheet 0 0 (m-1) 0 (n-1)
    end;;

(* Printing initial sheet *)
let printsheetinitial (sheet:sheet) = 
    Printf.printf "Initial sheet:\n" ;
    printhelper sheet 0 0 (m-1) 0 (n-1);;

(* Converting string s to element *)
let stringtoelement s = 
    if s="" then Empty
    else Value(float_of_string s);;

(* Reading input from .csv file *)
let _ =
    try
    let in_stream = open_in csvpath in
        for i=0 to (m-1) do
            let line = input_line in_stream in
            let split = Str.split_delim (Str.regexp ",") in
            let values = split line in
            mysheet.(i) <- Array.of_list (List.map stringtoelement values);
        done;
        close_in in_stream;
        printsheetinitial mysheet; 
    with e ->
    Printf.printf "File not found!";
    raise e
