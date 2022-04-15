use "hw1.sml";

(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

val is_older1 = is_older((1,2,3),(1,2,3)) = false
val is_older2 = is_older((1,2,3),(2,3,4)) = true
val is_older3 = is_older((5,6,7), (4,5,6)) = false

val number_in_month1 = number_in_month([(2021,12,31),(2021,4,2),(2020,4,30)], 4) = 2
val number_in_month2 = number_in_month([], 4) = 0

val number_in_months1 = number_in_months([], []) = 0
val number_in_months2 = number_in_months([(2021,12,31),(2021,4,2),(2020,4,30)], [4,12,1]) = 3

val dates_in_month1 = dates_in_month([(2021,12,31),(2021,4,2),(2020,4,30)], 4) = [(2021,4,2),(2020,4,30)]

val dates_in_months1 = dates_in_months([(2021,12,31),(2021,4,2),(2020,4,30)], [4,12,1]) = [(2021,4,2),(2020,4,30),(2021,12,31)]

val get_nth1 = get_nth(["a","b","c"], 2) = "b"

val date_to_string1 = date_to_string((2021,12,31)) = "December 31, 2021"

val number_before_reaching_sum1 = number_before_reaching_sum(1, [1,2,3]) = 0
val number_before_reaching_sum2 = number_before_reaching_sum(5, [2,2,1,4]) = 2
										 
val what_month1 = what_month(1) = 1
val what_month2 = what_month(365) = 12

val oldest1 = oldest([]) = NONE
val oldest2 = oldest([(2021,12,31),(2021,4,2),(2020,4,30)]) = SOME (2021,4,2)

val unique_list1 = unique_list([1,2,3,1,4,3,5,6,9,8,8]) = [1,2,3,4,5,6,9,8]

val number_in_months_challenge1 = number_in_months_challenge([(2021,12,31),(2021,4,2),(2020,4,30)], [4,12,4,1]) = 3
val dates_in_months_challenge1 = dates_in_months_challenge([(2021,12,31),(2021,4,2),(2020,4,30)], [4,12,4,1]) = [(2021,4,2),(2020,4,30),(2021,12,31)]

val leap_year1 = leap_year(2000) = false
val leap_year2 = leap_year(2004) = true
val leap_year3 = leap_year(1995) = false

val days_in_month1 = days_in_month(2004, 2) = 29

val reasonable_date1 = reasonable_date((2021,12,31)) = true
val reasonable_date2 = reasonable_date((2000,2,29)) = false
val reasonable_date3 = reasonable_date((2021,9,31)) = false
val reasonable_date4 =  reasonable_date(2004,2,29) =  true
