type log_exp =
    Const of bool
  | Var of string
  | Neg of log_exp
  | Disj of log_exp * log_exp
  | Conj of log_exp * log_exp
  | Cond of log_exp * log_exp
  | BiCond of log_exp * log_exp;;
  
let rec eval ctx = function 
    Const b -> b
  | Var s -> List.assoc s ctx
  | Neg e -> not (eval ctx e)   
  | Disj (e1, e2) -> (eval ctx e1) || (eval ctx e2)
  | Conj (e1, e2) -> (eval ctx e1) && (eval ctx e2)
  | Cond (e1, e2) -> (not (eval ctx e1)) || (eval ctx e2)
  | BiCond (e1, e2) -> (eval ctx e1) = (eval ctx e2);;

type oper = Not;;

type biOper = Or | And | If | Iff;;

type prop =
    C of bool
  | V of string
  | Op of oper * prop
  | BiOp of biOper * prop * prop;;

let rec prop_of_log_exp = function
	Const x -> C x
	|Var x -> V x
	|Neg x -> Op (Not,prop_of_log_exp x)
	|Disj (x,y) -> BiOp (Or,prop_of_log_exp x,prop_of_log_exp y)
	|Conj (x,y) -> BiOp (And,prop_of_log_exp x,prop_of_log_exp y)
  	|Cond (x,y) -> BiOp (If,prop_of_log_exp x,prop_of_log_exp y)
  	|BiCond (x,y) -> BiOp (Iff,prop_of_log_exp x,prop_of_log_exp y);;
  	
let rec log_exp_of_prop = function 
	C x -> Const x
	|V x -> Var x
	|Op (Not,x) -> Neg (log_exp_of_prop x)
	|BiOp (Or,x,y) -> Disj (log_exp_of_prop x,log_exp_of_prop y)
	|BiOp (And,x,y) -> Conj (log_exp_of_prop x,log_exp_of_prop y)
  	|BiOp (If,x,y) -> Cond (log_exp_of_prop x,log_exp_of_prop y)
  	|BiOp (Iff,x,y) -> BiCond (log_exp_of_prop x,log_exp_of_prop y);;
  	
let opval = function 
    Not -> (not);;
    
let biopval = function
    Or -> (||)
  | And -> (&&)
  | If -> (fun x y -> (not x) || y)
  | Iff -> (=);;
  
let rec peval ctx = function
	C b -> b
	|V s -> List.assoc s ctx
	|Op (x,y) -> (opval x) (peval ctx y)   
 	|BiOp (x,y,z) -> (biopval x) (peval ctx y) (peval ctx z);;
 
let rec it_bool = function
	h::t -> if h then false::it_bool t else true::t
	|[]->raise(Invalid_argument "it_bool");;
 
let rec del_rep = function
	[] -> []
	|h::t -> if (List.mem h t) then del_rep t else h::del_rep t;;

let rec comp_list = function
	[] -> true 
	|h::t -> h && comp_list t;;

let init n f = if n<0 then raise(Invalid_argument "init") else 
	let rec aux i l = if i>=n then List.rev l 
			else aux (i+1) (f i::l)
	in aux 0 [];;

let rec comp l1 l2 = match l1,l2 with
	[],[] -> []
	|h1::t1,h2::t2 -> (h1,h2)::comp t1 t2
	|(_::_, [])->raise(Invalid_argument "comp")
	|([], _::_)->raise(Invalid_argument "comp");;
let is_tau p = 
	let rec aux a = function
		C b -> []
		|V s -> [s]
		|Op (x,y) -> aux a y   
 		|BiOp (x,y,z) -> aux a y @ aux a z
 	in let temp = del_rep (aux [] p) in 
 	let l = ref (init (List.length temp) (fun _ -> false)) in
 	let res = ref true in
 	while !res && not (comp_list !l) do
 		res := peval (comp temp !l) p;
 		l := it_bool !l	
 	done;
 	if !res = true then true else false;;
  
  
  
  
  
