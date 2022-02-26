type 'a bin_tree =
     Empty
   | Node of 'a * 'a bin_tree * 'a bin_tree
   
exception No_branches

let empty = Empty;;
let comp x y = Node (x,(fst y),(snd y));;
let root = function 
	Empty -> raise No_branches
	|Node (x,_,_) -> x;; 
let branches = function 
	Empty -> raise No_branches
	|Node (_,x,y) -> x,y;;
let is_empty t = (t = Empty);;
let left = function 
	Empty -> raise No_branches
	|Node (_,x,_) -> x;;
let right = function 
	Empty -> raise No_branches
	|Node (_,_,x) -> x;;
let rec size = function 
	Empty -> 0
	|Node (_,x,y) -> 1 + size x + size y;;  
let rec height = function 
	Empty -> 0
	|Node (_,x,y) -> 1 + max (height x) (height y);;
let pre_order tr = 
	let rec aux a t = if is_empty t then a 
			else root t::aux a (left t) @ aux a (right t)  
	in aux [] tr;;
let post_order tr = 
	let rec aux a t = if is_empty t then a 
			else aux a (left t) @ aux a (right t) @ root t::[] 
	in aux [] tr;;
let in_order tr = 
	let rec aux a t = if is_empty t then a 
			else aux a (left t) @ root t::aux a (right t)  
	in aux [] tr;;
let leaves tr = 
	let rec aux a t = if is_empty t then a 
			else let x,y = left t,right t 
			in match x,y with
				Empty,Empty -> (root t)::a
				|_ -> aux a x @ aux a y  
	in aux [] tr;;
let rec mirror t = if is_empty t then Empty 
		else let y,x = left t,right t 
			in match x,y with
				Empty,Empty -> t
				|_ -> comp (root t) (mirror x,mirror y);; 
let rec tree_map f t = if is_empty t then Empty 
		else let x,y = left t,right t 
			in match x,y with
				Empty,Empty -> comp (f (root t)) (Empty,Empty)
				|_ -> comp (f (root t)) (tree_map f x,tree_map f y);; 
	
	
	
	
