(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(* 1 *)
val only_capitals = List.filter (fn str => Char.isUpper(String.sub(str, 0)))
(* 2 *)
val longest_string1 = List.foldl (fn (x,init) => if String.size x > String.size init then x else init) ""
(* 3 *)
val longest_string2 = List.foldl (fn (x,init) => if String.size x >= String.size init then x else init) ""
(* 4 *)
fun longest_string_helper f strs = List.foldl (fn (x,init) => if f(String.size x, String.size init) then x else init) "" strs
val longest_string3 = longest_string_helper (fn (x,y) => x > y)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)
(* 5 *)
val longest_capitalized = longest_string1 o only_capitals
(* 6 *)
val rev_string = String.implode o List.rev o String.explode
(* 7 *)
fun first_answer f xs = 
	case xs of 
		[] => raise NoAnswer
		| x::xs' => case f x of 
					SOME v => v
					| _ => first_answer f xs'
(* 8 *)
(* ('a -> 'b list option) -> 'a list -> 'b list option *)
fun all_answers f l = 
	let fun helper(xs, acc) = 
		case xs of 
			[] => SOME acc
			| x::xs' => case f x of 
					NONE => NONE
					| SOME lst => helper(xs', acc @ lst)
	in
		helper(l, [])
	end
(* 9(a) *)
val count_wildcards = g (fn () => 1) (fn _ => 0)
(* 9(b) *)
val count_wild_and_variable_lengths = g (fn () => 1) String.size
(* 9(c) *)
fun count_some_var (str, p) = g (fn _ => 0) (fn v => if v = str then 1 else 0) p
(* 10 *)
val check_pat = 
	let 
		(* pattern -> string list *)
		fun get_all_vars p = 
			case p of 
				Variable v => [v]
				| ConstructorP(_,p') => get_all_vars p'
				| TupleP ps => List.foldl (fn (x, acc) => get_all_vars x @ acc) [] ps
				| _ => []
		(* string list -> bool *)
		fun has_duplicate strs = 
			case strs of 
				[] => false
				| x::xs => List.exists (fn v => v = x) xs orelse has_duplicate xs
	in
		not o has_duplicate o get_all_vars
	end
(* 11 *)
fun match (valu, p) = 
	case (valu, p) of 
		 (_, Wildcard) => SOME []
		| (Unit, UnitP) => SOME []
		| (Const c, ConstP cp) => if c = cp then SOME [] else NONE
		| (valu, Variable v) => SOME [(v, valu)]
		| (Constructor(s1, valu), ConstructorP(s2, p)) => if s1 = s2 then match (valu, p) else NONE
		(* zip: 'a list * 'b list -> ('a * 'b) list *)
		| (Tuple vs, TupleP ps) => if List.length vs = List.length ps then all_answers match (ListPair.zip (vs, ps)) else NONE
		| _ => NONE
(* 12 *)
fun first_match valu ps = 
	SOME (first_answer (fn p => match (valu, p)) ps) 
	handle NoAnswer => NONE