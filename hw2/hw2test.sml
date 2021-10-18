(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw2.sml";

val test1_1 = all_except_option("string", ["string"]) = SOME [];
val test1_2 = all_except_option("string", ["not related1", "not related2", "string"]) = SOME ["not related1", "not related2"];
val test1_3 = all_except_option("string", ["not related1", "not related2"]) = NONE;
val test1_4 = all_except_option("string", []) = NONE;
val test1_5 = all_except_option("", []) = NONE;

val test2_1 = get_substitutions1([["foo"],["there"]], "foo") = [];
val test2_2 = get_substitutions1([["foo", "bar"], ["foo"]], "foo") = ["bar"];
val test2_3 = get_substitutions1([], "foo") = [];
val test2_4 = get_substitutions1([], "") = [];

val test3_1 = get_substitutions2 ([["foo"],["there"]], "foo") = [];
val test3_2 = get_substitutions1([["foo", "bar"], ["foo"]], "foo") = ["bar"];
val test3_3 = get_substitutions1([], "foo") = [];
val test3_4 = get_substitutions1([], "") = [];
								  
val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}];

val test5_1 = card_color (Clubs, Num 2) = Black;
val test5_2 = card_color (Spades, Jack) = Black;
val test5_3 = card_color (Hearts, King) = Red;
val test5_4 = card_color (Diamonds, Queen) = Red;

val test6_1 = card_value (Clubs, Num 2) = 2;
val test6_2 = card_value (Clubs, Ace) = 11;
val test6_3 = card_value (Clubs, King) = 10;
					    
val test7_1 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = [];
(*val test7_2 = remove_card ([(Diamonds, Ace)], (Hearts, Ace), IllegalMove) = IllegalMove;*)
val test7_3 = remove_card ([(Hearts, Ace), (Diamonds, Ace)], (Hearts, Ace), IllegalMove) = [(Diamonds, Ace)];

val test8_1 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true;
val test8_2 = all_same_color [(Hearts , 2), (Diamonds, 3), (Hearts, 4)] = true;
val test8_3 = all_same_color [(Spades, 2), (Diamonds, 3)] = false;
									      
val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4;

val test10_1 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4;
val test10_2 = score ([(Hearts, Num 2), (Hearts, Num 4)], 10) = 2;
							       
val test11_1 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6;

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
