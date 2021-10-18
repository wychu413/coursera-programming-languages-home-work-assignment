(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* Q1a string not found -> NONE, otherwise SOME(list) without the string *)
fun all_except_option(s, xs) =
    (* check if any thing has been removed, if no return NONE otherwise 
     return SOME. *)
    let fun strRemoval xs =
	    case xs of
		[] => []
	      | (x::xs') => if same_string(x,s)
                            then strRemoval(xs')
			    else x::strRemoval(xs')
	val lst = strRemoval xs
    in if xs = lst then NONE else SOME lst
    end
	
(* Q1b return list of string that contains in the list list string excluding string *)
fun get_substitutions1([], _) = []
  | get_substitutions1(xs::xss, s) =
    case all_except_option(s, xs) of
	NONE => get_substitutions1(xss, s)
      | SOME xs => xs @ get_substitutions1(xss, s) 

(* Q1c tail recurion version of Q1b *)
fun get_substitutions2(xss, s) =
    let fun helper([], acc) = acc
	  | helper(xs::xss, acc) =
	    case all_except_option(s, xs) of
		NONE => helper(xss, acc)
	      | SOME xs' => helper(xss, xs' @ acc) 
    in helper(xss, [])
    end

(* Q1d return a list of record *)
fun similar_names(substitutions, {first=fst, middle=mid, last=lst}) =
    let val possibleNames = get_substitutions1(substitutions, fst)
	fun makeNameList names =
	    case names of
		[] => []
	      | (name::names') =>
		{first=name, middle=mid, last=lst}::makeNameList names'
    in {first=fst, middle=mid, last=lst}::makeNameList possibleNames
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

(* Q2a Given a card return its color *)
fun card_color (suit, _) = 
    case suit of
	(Clubs | Spades) => Black
      | (Diamonds | Hearts) => Red
				   
(* Q2b Given a card return its value *)
fun card_value (_, rank) =
    case rank of
	Num n => n
      | Ace => 11
      | _ => 10

(* Q2c *)
fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | (c'::cs') => if c = c'
		    then cs'
		    else c'::remove_card(cs', c, e)

(* Q2d *)
fun all_same_color cs =
    case cs of
	[] => true
      | _::[] => true 
      | c1::c2::cs' => card_color c1 = card_color c2
			 andalso all_same_color (c2::cs')
		  
(* Q2e *)		
fun sum_cards cs =
    let fun helper (cs , acc) =
	    case cs of
		[] => acc
	      | c::cs' => helper (cs', acc + card_value c) 
    in helper(cs, 0)
    end

(* Q2f *)
fun score (cs, goal) =
    let
	val sum = sum_cards cs
	val preliminary = if sum > goal then 3 * (sum - goal) else (goal - sum)
    in
	if all_same_color cs then preliminary div 2 else preliminary 
    end

(* Q2g cs: card list, ml: move list, hcs: held cards(list) *)
fun officiate (cs, ml, goal) =
    let fun helper (_, hcs, [], cur_score) = cur_score (* no more move *)
	  | helper (cs, hcs, mv::ml, cur_score) =
	    if sum_cards hcs > goal
	    then cur_score
	    else case mv of
		     Draw => (case cs of
				  [] => cur_score (* draw, but no more card *)
				| c::cs' =>  helper (cs', c::hcs, ml, score (c::hcs, goal)))
		   | Discard c => let val hcs' = remove_card(hcs, c, IllegalMove)
				  in helper (cs, hcs', ml, score (hcs', goal)) 
				  end
    in helper (cs, [], ml, 0)
    end
