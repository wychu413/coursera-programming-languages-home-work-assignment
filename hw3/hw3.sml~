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

(* Q1 take a list of string and return the string that is starat with capital letter *)
val only_capitals = List.filter (fn x => Char.isUpper (String.sub(x,0)))

(* Q2 take a list of string return the longest string, return "" if list is empty *)
val longest_string1 =
    foldl (fn (x,y) => if String.size x > String.size y then x else y) ""

(* Q3 *)
val longest_string2 =
    foldl (fn (x,y) => if String.size x >= String.size y then x else y) ""

(* Q4 *)
fun longest_string_helper f = foldl (fn (x,y) => if f(String.size x, String.size y) then x else y) ""

val longest_string3 = longest_string_helper (fn (x,y) => x > y)

val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

(* Q5 *)
val longest_capitalized = longest_string1 o only_capitals					    
(* Q6 *)
val rev_string = String.implode o rev o String.explode					

(* Q7 *)
fun first_answer _ [] = raise NoAnswer
  | first_answer f (x::xs) =
    case f x of
	NONE => first_answer f xs
      | SOME xs' => xs'

(* Q8 readibility trades performance
This function iterate over the entire list. Therefore, it should be less
efficient. 
fun all_answers f xs =
    let val lst = map f xs
    in if List.exists (fn x => x = NONE) lst
       then NONE
       else SOME lst
    end
*)
fun all_answers f xs =
    let fun loop (acc,xs) =
        case xs of
	    [] => SOME acc
	  | x::xs' => case f x of 
                          NONE => NONE
              		| SOME y => loop((y @ acc), xs')
    in loop ([],xs) end

	
(* Q9 a *)
val count_wildcards = g (fn x => 1) (fn x => 0)
    
(* Q9 b *)
val count_wild_and_variable_lengths = g (fn x => 1) String.size

(* Q9 c *)
fun count_some_var (s,p) = g (fn x => 0) (fn x => if s = x then 1 else 0) p

(* Q10 *)
fun check_pat p =
    let fun variableNames p =
	    case p of
		Variable x => [x]
	      | TupleP ps  => foldl (fn (p,l) => (variableNames p) @ l) [] ps
	      | ConstructorP(_,p) => variableNames p
	      | _ => []
	fun hasDuplicate xs =
	    case xs of
		[] => false
	      | (x::xs') => List.exists (fn x' => x' = x) xs' orelse hasDuplicate xs'
    in not (hasDuplicate (variableNames p))
    end

(* Q11 *)
fun match (v,p) =
    case (p,v) of
        (Wildcard, _) => SOME []
      | (Variable s, v) => SOME [(s, v)]
      | (UnitP, Unit) => SOME []
      | (ConstP i', Const i) => if i' = i then SOME [] else NONE
      | (TupleP ps, Tuple vs) => if List.length ps = List.length vs
				 then all_answers (match) (ListPair.zip (vs,ps))
				 else NONE 
      | (ConstructorP(s1,p'), Constructor(s2,q)) => if s1 = s2 then match (q,p') else NONE 
      | _ => NONE

(* Q12 *)
fun first_match valu patlst =
    SOME (first_answer (fn pat => match (valu,pat)) patlst)
    handle NoAnswer => NONE
