let init n f = if n<0 then raise(Invalid_argument "init") else 
	let rec aux i l = if i>=n then List.rev l 
			else aux (i+1) (f i::l)
	in aux 0 [];;

let rec fromto m n = init (n-m+1) (function x->x+m);;

let rlist_t r n =
	let rec aux r n acc=
		if n<=0 then acc
		else aux r (n-1) (Random.int r::acc)
	in aux r n [];;

let rec insert x = function 
  [] -> [x]
| h::t -> if x <= h then x::h::t
          else h::insert x t;;
          
let rec isort = function
  [] -> []
| h::t -> insert h (isort t);;

let insert_t x l = 
	let rec aux a = function 
	[] -> List.rev (x::a)
	|h::t -> if x <= h then List.rev_append a (x::h::t)
		else aux (h::a) t
	in aux [] l;;

let isort_t l = 
	let rec aux a = function
	[] -> a
	|h::t -> aux (insert_t h a) t
	in aux [] (List.rev l);;

let rec divide = function
  h1::h2::t -> let t1,t2 = divide t in h1::t1, h2::t2
| l -> l,[];;

let rec merge l1 l2 = match l1,l2 with
  [], l | l, [] -> l
| h1::t1, h2::t2 -> if h1 <= h2 then h1::merge t1 l2
                    else h2::merge l1 t2;;

let rec msort l = match l with
  [] | [_] -> l
| _ -> let l1,l2 = divide l in merge (msort l1) (msort l2);;

let divide_t l =
	let rec aux (x,y) = function 
	[] -> (x,y)
	|h1::t -> aux (y,h1::x) t
	in aux ([],[]) (List.rev l);;

let merge_t ls1 ls2 = 
	let rec aux a l1 l2 = match l1,l2 with 
	[],l|l,[] -> List.rev_append a l
	|h1::t1,h2::t2 -> if h1<=h2 then aux (h1::a) t1 l2
			else aux (h2::a) l1 t2
	in aux [] ls1 ls2;;

let rec msort_qt l = match l with 
	[]|[_] -> l 
	|_ -> let l1,l2 = divide_t l in merge_t (msort_qt l1) (msort_qt l2);;

let rec qsort l = match l with
  [] | [_] -> l
| h::t -> let l1,l2 = List.partition ((<=) h) t in
          qsort l2 @ (h::qsort l1);;

let qsort_qt l = 
	let rec aux a = function
	[] -> a
	|[x] -> x::a 
	|h::t -> let l1,l2 = List.partition ((<=) h) t 
		in let der = aux a l1 
		in aux (h::der) l2
	in aux [] l;;

let crono f x =
  let t = Sys.time () in
  let _ = f x in 
  Sys.time () -. t;;	
	
let rec insert_gen p x = function 
  [] -> [x]
| h::t -> if p x h then x::h::t
          else h::insert_gen p x t;;
          
let rec isort_gen p = function
  [] -> []
| h::t -> insert_gen p h (isort_gen p t);;

let insert_t_gen p x l = 
	let rec aux a = function 
	[] -> List.rev (x::a)
	|h::t -> if p x h then List.rev_append a (x::h::t)
		else aux (h::a) t
	in aux [] l;;  

let isort_t_gen p l = 
	let rec aux a = function
	[] -> a
	|h::t -> aux (insert_t_gen p h a) t
	in aux [] (List.rev l);;

let rec merge_gen p l1 l2 = match l1,l2 with
  [], l | l, [] -> l
| h1::t1, h2::t2 -> if p h1 h2 then h1::merge_gen p t1 l2
                    else h2::merge_gen p l1 t2;;
                    
let rec msort_gen p l = match l with
	  [] | [_] -> l
	| h::t -> let l1,l2 = List.partition (p h) t 
	in merge_gen p (h::msort_gen p l1) (msort_gen p l2);;
	
let merge_t_gen p ls1 ls2 = 
	let rec aux a l1 l2 = match l1,l2 with 
	[],l|l,[] -> List.rev_append a l
	|h1::t1,h2::t2 -> if p h1 h2 then aux (h1::a) t1 l2
			else aux (h2::a) l1 t2
	in aux [] ls1 ls2;;
		
let rec msort_qt_gen p l = match l with 
	[]|[_] -> l 
	| h::t -> let l1,l2 = List.partition (p h) t
	in merge_t_gen p (h::msort_qt_gen p l1) (msort_qt_gen p l2);;
	
let rec qsort_gen p l = match l with
  [] | [_] -> l
| h::t -> let l1,l2 = List.partition (p h) t in
          qsort_gen p l2 @ (h::qsort_gen p l1);;

let qsort_qt_gen p l = 
	let rec aux a = function
	[] -> a
	|[x] -> x::a 
	|h::t -> let l1,l2 = List.partition (p h) t 
		in let der = aux a l1 
		in aux (h::der) l2
	in aux [] l;;
