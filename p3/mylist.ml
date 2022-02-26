let hd = function
  [] -> failwith "hd"
| a::_ -> a;;

let tl = function
  [] -> failwith "tl"
  | _::tl -> tl;;

let rec length = function
  [] -> 0
| a::b -> 1 + length b;;

let rec nth l n = match l with
  [] -> failwith "nth"
| h::t when n < 0 -> failwith "nth"
| h::_ when n = 0 -> h
| a::b -> nth b (n-1);;

let rec append h1 h2 = match h1 with
[] -> h2
| a::b -> a::(append b h2);;

let rec rev = function
[] -> []
| a::b -> append (rev b) (a::[]);;

let rev_append h1 h2 = append (rev h1) h2;;

let rec concat = function
  [] -> []
  | h::t -> append h (concat t);;

let flatten = concat;;

let rec map f = function
  [] -> []
| a::l -> let r = f a in r:: map f l;;

let rec map2 f a1 a2 = match(a1,a2) with
  ([],[]) -> []
  |(_::_, [])->failwith "map2"
  |([], _::_)->failwith "map2"
  |(h1::t1,h2::t2)-> (f h1 h2)::(map2 f t1 t2);;

let rec fold_left f i l= match l with
    []->i
    |a::b->fold_left f (f i a) b;;

let rec fold_right f l i = match l with
    []->i
    |a::b->f a (fold_right f b i);;

let rec find f l=match l with
    []-> failwith "find"
    |a::b-> if f a then a else find f b;;

let rec for_all f l= match l with
    []-> true
    |a::[]->f a
    |a::b-> f a && for_all f b;;

let rec exists f l= match l with
    []-> false
    |a::[]->f a
    |a::b-> f a || exists f b;;

let mem a l = exists(function x->x=a) l

let rec filter p l = match l with
[]->[]
|h::t when p h -> h::filter p t
|h::t -> filter p t;;

let find_all p l = 
filter p l;;

let comp g f x = g(f x);;

let partition p l =(filter p l, filter (comp not p) l);;

let rec split= function
[]->([],[])
| (a1,a2)::t-> let (t1,t2) = split t in (a1::t1,a2::t2);;

let split l= (map fst l, map snd l);;

let rec combine l1 l2= match(l1,l2) with
([],[])->[]
|(h1::t1,[])->raise (Invalid_argument "List.combine")
|([],h1::t1)-> raise (Invalid_argument "List.combine")
|(h1::t1,h2::t2)-> let t = combine t1 t2 in (h1,h2)::t;;
