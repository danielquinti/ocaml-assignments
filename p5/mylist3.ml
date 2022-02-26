let init len f=
    let rec aux acc i len f=
        if i>=len then acc
        else aux ((f i):: acc) (i+1) len f
        in
            if len<0 then invalid_arg "len"
            else List.rev (aux [] 0 len f);;

let suml l=
    let rec aux acc l= match l with
        []->acc
        |h::t->aux(acc+h) t
        in aux 0 l;;

let maxl l = 
	if l=[] then raise(Failure "maxl") 
	else List.fold_left max (List.hd l) (List.tl l);;

let to0from n=
    let rec aux acc n= match n with
        -1->acc
        |_->n:: aux acc (n-1)
        in
            if (n<0) then []
            else aux [] n;;

let fromto m n=
  let rec aux i l=
    if i<m then l
      else aux(i-1)(i::l)
    in aux n [];;

let from1to n =
  let rec aux i l =
      if i < 1 then l
      else aux (i-1) (i::l)
      in aux n [];;

let append l1 l2=
  List.rev_append (List.rev l1) l2;;

let concat l=
  let rec aux acc l= match l with
    []->List.rev acc
    |a::b->aux (List.rev_append a acc) b
  in aux [] l;;

let map f l=
  let rec aux acc f l= match l with
    []->List.rev acc
    |a::b->aux (f a::acc) f b
  in aux [] f l;;

let power x y=
  let rec aux acc x y= match y with
    0->acc
    |_->aux (acc*x) x (y-1)
  in
    if y>=0 then aux 1 x y
    else invalid_arg "power";;

let fib n=
  let rec aux i a b=
    if i= n then a
    else aux (i+1) b (a+b) in
  aux 0 0 1;;

let fact n =
    let rec innerfact n a =
        if n = 0 then a
        else innerfact (n-1) (a *. float n)
    in 
    if n >= 0 then innerfact n 1.
    else invalid_arg "fact";;

let incseg l=
  let rec aux accn accl l =match l with
    []->List.rev accl
    |a::b->aux (accn+a) ((accn+a)::accl) b
  in aux 0 [] l;;

let multicomp l x=
  let rec aux l acc x =match l with
    []->acc
    |a::b->aux b (a acc) x
  in aux l x x;;

let rec insert x l=match l with
	[]->[x]
 |h::t when x<=h-> x::l
 |h::t-> insert h (insert x t);;

let rec insert_gen f x l=match l with
	[]->[x]
 |h::t when f x h-> x::l
 |h::t-> insert h (insert x t);;
