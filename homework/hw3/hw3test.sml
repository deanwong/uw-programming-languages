(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw3.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true
val test10_1 = check_pat (TupleP([(Variable("x")),(Variable("x"))])) = false

val test11 = match (Const(1), UnitP) = NONE
val test11_1 = match (Const(1), ConstP(1)) = SOME []
val test11_2 = match (Unit, UnitP) = SOME []
val test11_3 = match (Unit, Wildcard) = SOME []
val test11_4 = match (Constructor("_", Unit), Variable("x")) = SOME [("x", Constructor("_", Unit))]
val test11_5 = match (Constructor("_", Constructor("x", Const(1))), ConstructorP("_", Variable("x"))) = SOME [("x", Constructor("x", Const(1)))]
val test11_6 = match (Tuple([(Constructor("_", Unit)), (Constructor("_", Constructor("x", Const(1))))]),TupleP([(Variable("x")),(ConstructorP("_", Variable("x")))])) = SOME [("x", Constructor("_", Unit)),("x", Constructor("x", Const(1)))]

val test12 = first_match Unit [UnitP] = SOME []
val test12_1 = first_match Unit [] = NONE
val test12_2 = first_match (Constructor("_", Unit)) [(Variable("x"))] = SOME [("x",Constructor ("_",Unit))]
