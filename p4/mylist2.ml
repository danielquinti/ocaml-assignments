let hd = function
  [] -> failwith "hd"
| a::_ -> a;;

let tl = function
  [] -> failwith "tl"
  | _::tl -> tl;;

let rec nth l n = match l with
  [] -> failwith "nth"
| h::t when n < 0 -> failwith "nth"
| h::_ when n = 0 -> h
| a::b -> nth b (n-1);;

let rec append h1 h2 = match h1 with
[] -> h2
| a::b -> a::(append b h2);;

let rec rev_append l1 l2 =match l1 with
[]->l2
|h::t -> rev_append t (h::l2);;

let rev l =rev_append l [];;

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

let length l =fold_left (fun s _ -> s+1) 0 l;;

let rec fold_right f l i = match l with
    []->i
    |a::b->f a (fold_right f b i);;

let rec find f l=match l with
    []-> failwith "find"
    |a::b-> if f a then a else find f b;;

let rec for_all f l= match l with
    []-> true
    |a::b when f a ->for_all f b
    |a::b->false;;



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

let rec remove n l= match l with
[]->[]
|a::b->if n=a then b else a::(remove n b);;

let rec remove_all n l= match l with
[]->[]
|a::b->if n=a then remove_all n b else a::(remove n b);;

let rec ldif l1 l2= match l2 with
    []->l1
    |(h::t)->ldif(remove_all h l1) t;; 


let rec lprod l1 l2=
let rec aux x l2= match l2 with
  []->[]
  |c::d->(x,c)::aux x d
  in match l1 with
  []->[]
|a::b->(aux a l2) @ lprod b l2;;

let divide l=
    let rec aux acc=
        let acc1,acc2=acc in function
        |h1::h2::t->aux(h1::acc1,h2::acc2) t
        |h::[] ->(h::acc1,acc2)
        |[]->acc
    in let f,s = aux ([],[]) l in List.rev f,List.rev s;;
