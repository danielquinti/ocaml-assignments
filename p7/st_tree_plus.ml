open St_tree;;

let is_single t = 
    try let _ = branches t in false 
    with No_branches -> true;;
let left t = fst(branches t);;
let right t = snd(branches t);;
let rec size t = if is_single t then 1 
		else 1 + size (left t) + size (right t);;
let rec height t = if is_single t then 1 
		else 1 + max (height (left t)) (height (right t));;
let pre_order tr = 
	let rec aux a t = if is_single t then List.rev (root t::a) 
			else root t::aux a (left t) @ aux a (right t)
	in aux [] tr;;
let post_order tr = 
	let rec aux a t = if is_single t then root t::a
			else aux a (left t) @ aux a (right t) @ root t::[]
	in aux [] tr;;
let in_order tr = 
	let rec aux a t = if is_single t then root t::a
			else aux a (left t) @ root t::aux a (right t) 
	in aux [] tr;;
let leaves tr =
	let rec aux a t = if is_single t then root t::a 
			else aux a (left t) @ aux a (right t)
	in aux [] tr;; 
let rec mirror t = if is_single t then t 
		else let y,x = branches t 
		in comp (root t) (mirror x,mirror y);;
let rec tree_map f t = if is_single t then single (f (root t))
		else let x,y = branches t
		in comp (f (root t)) (tree_map f x,tree_map f y);;	
	
	
	
	
		
	
