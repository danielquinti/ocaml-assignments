let u = 2 + 4 * 3 - 6 mod 2;;
(*val u : int = 14*)

let v = fst (3.0 , 4.5) +. 2.3;;
(*val v : float = 5.3*)

let w = char_of_int (u + 40);;
(*val w : char = '6'*)

let x = true && u < 12;;
(*val x : bool = false*)

let y = if 10 > 4 then "bien" else "mal"
(*val y : string = "bien";;*)

let z = int_of_char 'Y'+12, int_of_char 'O'+45;;
(*val z : int * int = (101, 124)*)
