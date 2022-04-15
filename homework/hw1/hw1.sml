(**
Write a function is_older that takes two dates and evaluates to true or false.  It evaluates to true ifthe first argument is a date that comes before the second argument.  (If the two dates are the same,the result is false 
*)
fun is_older (date: int * int * int, date2: int * int * int) =
    #1 date < #1 date2
    orelse #1 date = #1 date2 andalso #2 date < #2 date2
    orelse #2 date = #2 date2 andalso #2 date = #2 date2 andalso #3 date < #3 date2

(**
Write a function number_in_month that takes a list of dates and a month (i.e., anint) and returns how many dates in the list are in the given month
*)
fun number_in_month (dates: (int * int * int) list, month: int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

(**
Write a function number_in_months that takes a list of dates and a list of months (i.e., anint list)and returns the number of dates in the list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated.Hint:  Use your answer to the previous problem
*)
fun number_in_months (dates: (int * int * int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(**
Write a function dates_in_month that takes a list of dates and a month (i.e., anint) and returns a list holding the dates from the argument list of dates that are in the month.  The returned list should contain dates in the order they were originally given
*)
fun dates_in_month (dates: (int * int * int) list, month: int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

(**
Write a function dates_in_months that takes a list of dates and a list of months (i.e., anint list)and returns a list holding the dates from the argument list of dates that are in any of the months inthe list of months.Assume the list of months has no number repeated.Hint:  Use your answer to theprevious problem and SML’s list-append operator
*)
fun dates_in_months (dates: (int * int * int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(**
Write a function get_nth that takes a list of strings and an int n and returns the nth element of the list where the head of the list is 1st.  Do not worry about the case where the list has too few elements:your function may apply hd or tl to the empty list in this case, which is okay
*)							   
fun get_nth (strs: string list, n: int) =
    if n = 1
    then hd strs
    else get_nth(tl strs, n - 1)

(**
Write a function date_to_string that takes a date and returns a string of the form January 20, 2013 (for example).  Use the operator ^ for concatenating strings and the library function Int.toString for converting an int to astring.  For producing the month part, do not use a bunch of conditionals. Instead, use a list holding 12 strings and your answer to the previous problem.  For consistency, put acomma following the day and use capitalized English month names:  January, February, March, April,May, June, July, August, September, October, November, December.		
*)
fun date_to_string (date: int * int * int) =
    let
	val month_strs = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(month_strs, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

(**
Write a function number_before_reaching_sum that takes an int called sum, which you can assume is positive, and an int list, which you can assume contains all positive numbers, and returns an int.You should return an int n such that the first n elements of the list add to less than sum, but the first n+ 1 elements of the list add to sum or more.  Assume the entire list sums to more than the passed in value; it is okay for an exception to occur if this is not the case
*)
fun number_before_reaching_sum (sum: int, nums: int list) =
    if hd nums >= sum
    then 0
    else 1 + number_before_reaching_sum(sum - hd nums, tl nums)
				       
							 
(**
Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns what month that day is in (1 for January, 2 for February, etc.).  Use a list holding 12 integers and your answer to the previous problem
*)
fun what_month (day : int) =
    let
	val days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	number_before_reaching_sum(day, days_in_month) + 1
    end

(**
Write a function month_range that takes two days of the year day1 and day2 and returns an int list[m1,m2,...,mn]where m1 is the month of day1,m2 is the month of day1+1, ..., and mn is the month of day day2.  Note the result will have length day2 - day1 + 1 or length 0 if day1>day2
*)
fun month_range (day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

(**
Write a function oldest that takes  a  list  of  dates  and  evaluates  to  an(int*int*int) option.   It evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list
*)					
fun oldest (dates: (int * int * int) list) =
    if null dates
    then NONE
    else
	let
	    fun find_oldest(dates: (int * int * int) list) =
		if null (tl dates)
		then hd dates
		else
		    let
				val tl_date = find_oldest(tl dates)
		    in
				if is_older(hd dates, tl_date)
				then hd dates
				else tl_date
		    end
	in
	    SOME(find_oldest(dates))
	end


(**
Challenge Problem:Write functionsnumber_in_months_challengeanddates_in_months_challengethat are like your solutions to problems 3 and 5 except having a month in the second argument multipletimes has no more effect than having it once.  (Hint:  Remove duplicates, then use previous work.)
*)
fun unique_list (nums: int list) =
    let
	fun remove_duplicate(n: int, nums: int list) =
	    if null nums
	    then []
	    else
		let
		    val tl_nums = remove_duplicate(n, tl nums)
		in 				  
		    if (hd nums) = n
		    then tl_nums
		    else (hd nums) :: tl_nums
		end
    in
	if null nums
	then []
	else (hd nums) :: unique_list(remove_duplicate(hd nums, tl nums))
    end

fun number_in_months_challenge (dates: (int*int*int) list, months: int list) =
    number_in_months(dates, unique_list(months))

fun dates_in_months_challenge (dates: (int*int*int) list, months: int list) =
    dates_in_months(dates, unique_list(months))

(**
Challenge Problem:Write  a  functionreasonable_datethat  takes  a  date  and  determines  if  itdescribes a real date in the common era.  A “real date” has a positive year (year 0 did not exist), amonth between 1 and 12, and a day appropriate for the month.  Solutions should properly handle leapyears.  Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100
*)		   

fun leap_year (year: int) =
    (year mod 400 = 0 orelse year mod 4 = 0) andalso year mod 100 <> 0
									 
fun days_in_month (year: int, mon: int) =
    if mon < 1 orelse mon > 12
    then 0
    else
	let
	    fun get_nth (nums: int list, n: int) =
		if n = 1
		then hd nums
		else get_nth(tl nums, n - 1)
	    val days_in_month =  get_nth([31,28,31,30,31,30,31,31,30,31,30,31], mon)
	in
	    if leap_year(year)
	    then days_in_month + 1
	    else days_in_month
	end

fun reasonable_date (date: (int*int*int)) =
    let
	fun valid_year (year: int) =
	    year > 0
	fun valid_month (mon: int) =
	    mon >= 1 andalso mon <= 12
	fun valid_day (year: int, mon: int, day: int) =
	    day > 0 andalso day <= days_in_month(year, mon)
    in
	valid_year(#1 date) andalso valid_month(#2 date) andalso valid_day(#1 date, #2 date, #3 date)
    end
