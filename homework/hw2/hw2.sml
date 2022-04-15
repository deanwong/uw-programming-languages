(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str : string, strs: string list) =
    case strs of
	[] => NONE
      | x::xs => case same_string(str, x) of
		     true => SOME(xs)
		   | false => case all_except_option(str, xs) of
				  NONE => NONE
				| SOME ys => SOME(x::ys)

fun get_substitutions1 (subs : string list list, s: string) =
    case subs of
	[] => []
      | x::xs => case all_except_option(s, x) of
		     NONE => get_substitutions1(xs, s)
		   | SOME ys => ys @ get_substitutions1(xs, s)

fun get_substitutions2 (subs : string list list, s: string) =
    let fun aux (subs, acc) =
	    case subs of
		[] => acc
	      | x::xs => case all_except_option(s, x) of
			     NONE => aux(xs, acc)
			   | SOME ys => aux(xs, acc @ ys)
    in
	aux(subs, [])
    end

type tname = {first: string, middle: string, last: string}

fun similar_names (subs: string list list, name : tname) =
    let fun aux (subs, acc) =
	    case subs of
		[] => acc
	      | x::xs =>  aux(xs, acc @ [{first=x, middle=(#middle name), last=(#last name)}])
    in
	aux(get_substitutions2(subs, #first name), [name])
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (s: suit, r: rank) =
    case s of
	Clubs => Black
      | Spades => Black
      | Diamonds => Red
      | Hearts => Red

fun card_value (s: suit, r: rank) =
    case r of
	Ace => 11
      | Jack => 10
      | Queen => 10
      | King => 10
      | Num i => i

fun remove_card (cs: card list, c: card, e) =
    case cs of
	[] => raise e
      | x::xs => case c = x of
		     true => xs
		   | false => case remove_card(xs, c, e) of
				  [] => [x]
			       | y::ys => x::y::ys

fun all_same_color (cs: card list) =
    case cs of
	[] => true
      | x::[] => true
      | x::x'::xs => case card_color(x) = card_color(x') of
			 false => false
		      | true => all_same_color(x'::xs) 

fun sum_cards (cs: card list) =
    let fun aux(cs, acc) =
	    case cs of
		[] => acc
	      | x::xs => aux(xs, acc + card_value(x))
    in
	aux(cs, 0)
    end

fun score (cs: card list, goal: int) =
    let fun pre_score(cs) =
	    case (sum_cards(cs), goal) of
		(sum, goal) => case sum > goal of
				   true => (sum - goal) * 3
				 | false => goal -sum
    in
	case all_same_color(cs) of
	    true => pre_score(cs) div 2
	  | false => pre_score(cs)
    end

fun officiate (cs: card list, ms: move list, goal: int) =
    let fun state (cs, ms, held) =
	    case ms of
		[] => held
	      | x::xs => case x of
			     Discard card => state(cs, xs, remove_card(held, card, IllegalMove))
			   | Draw => case cs of
					 [] => held
				      | y::ys => case sum_cards(y::held) > goal of
						    true => y::held
						  | false => state(remove_card(cs, y, IllegalMove ), xs, y::held)
    in
	score(state(cs, ms, []), goal)
    end
