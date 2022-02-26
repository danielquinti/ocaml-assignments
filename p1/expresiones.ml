( );;
(*- : unit =()*)

2 + 5 * 3;;
(*- : int = 17*)

1.0;;
(*- : float = 1.*)

(*1.0 * 2;;*)
(*Error de tipo: El operador '*' corresponde a la multiplicacion entre
valores de tipo int, pero el 1.0 es de tipo float*)
1 * 2;;
(*- : int = 2*)

(*2 - 2.0;;*)
(*Error de tipo: El operador '-' corresponde a la resta entre
valores de tipo int, pero el 2.0 es de tipo float*)
2-2;;
(*- : int = 0*)

(*3.0 + 2.0;;*)
(*Error de tipo: El operador '+' corresponde a la resta entre
valores de tipo int, pero ambos valores son de tipo float*)
3.0 +. 2.0;;
(*- : float = 5.*)

5 / 3;;
(*- : int = 1*)

5 mod 3;;
(*- : int = 2*)

3.0 *. 2.0 ** 3.0;;
(*- : float=24.*)

3.0 = float_of_int 3;;
(*- : bool = true*)

(*sqrt 4;;*)
(*Error de tipo: El operador "sqrt" corresponde a la raiz cuadrada de un valor
de tipo float, pero se introdujo un valor de tipo int*)
sqrt 4.0;;
(*- : float = 2.*)


int_of_float 2.1 + int_of_float (-2.9);;
(*- : int = 0*)

truncate 2.1 + truncate (-2.9);;
(*- : int = 0*)

floor 2.1 +. floor (-2.9);;
(*- : float = -1.*)

(*ceil 2.1 +. ceil -2.9;;*)
(*Error de tipo: el operador "+." corresponde a la suma de valores de tipo float, pero ceil es de tipo float->float (a falta de parentesis antes del 2.9, el compilador reconoce ceil como un valor independiente en vez de como una operacion unaria)*)
ceil 2.1 +. ceil (-2.9);;
(*- : float = 1.*)

'B';;
(*- : char = 'B'*)

int_of_char 'A';;
(*- : int = 65*)

char_of_int 66;;
(*- : char 'B'*)

Char.code 'B';;
(*- : int = 66*)

Char.chr 67;;
(*- : char = 'C'*)

'\067';;
(*- : char = 'C'*)

Char.chr(Char.code 'a' - Char.code 'A' + Char.code 'Ñ');;
(*- : char = '\241'*)

Char.uppercase 'ñ';;
(*- : char = '\209'*)

Char.lowercase 'O';;
(*-: char = 'o'*)

"this is a string";;
(*- : string ="This is a string"*)

String.length "longitud";;
(*- : int = 8*)

(*"1999" + "1";;*)
(*Error de tipo: el operador "+." corresponde a la suma de valores de tipo float, pero ambos valores son de tipo string*)
1999 + 1;;
(*- : int = 2000*)

"1999" ^ "1";;
(*- : string = "19991"*)

int_of_string "1999" + 1;;
(*- : int = 2000*)

"\064\065";;
(*- : string "@A"*)

string_of_int 010;;
(*- : string = "10"*)

not true;;
(*- : bool = false*)

true && false;;
(*- : bool = false*)

true || false;;
(*- : bool = true*)

(1 < 2) = false;;
(*- : bool = false*)

"1" < "2";;
(*- : bool = true*)

2 < 12;;
(*- : bool = true*)

"2" < "12";;
(*- : bool = false*)

"uno" < "dos";;
(*- : bool = false*)

2,5;;
(*- : int * int = (2, 5)*)

"hola", "adios";;
(*- : string * string = ("hola", "adios")*)

0, 0.0;;
(*- : int * float = (0, 0.)*)

fst ('a',0);;
(*- : char = 'a'*)

snd (false, true);;
(*- : bool = true*)

(1,2,3);;
(*- : int * int * int = (1, 2, 3)*)

(1,2),3;;
(*- : (int * int) * int = ((1, 2), 3)*)

fst ((1,2),3);;
(*- : int * int = (1, 2)*)

if 3 = 4 then 0 else 4;;
(*- : int = 4*)

if 3 = 4 then "0" else "4";;
(*- : string = "4"*)

(*if 3 = 4 then 0 else "4";;*)
(*Error de tipo: la estructura "if then else" exige que el valor escogido para usar en caso de que la condicion no se cumpla sea del mismo tipo que el escogido para usar en caso de que la condicion se cumpla, pero 0 es de tipo int y "4" es de tipo string*)
if 3 = 4 then 0 else 4;;
(*- : int = 4;;*)

(if 3 < 5 then 8 else 10) + 4;;
(*- : int = 12*)

let pi = 2.0 *. asin 1.0;;
(*val pi : float = 3.14159265358979312*)

sin (pi /. 2.);;
(*- : float = 1.*)
