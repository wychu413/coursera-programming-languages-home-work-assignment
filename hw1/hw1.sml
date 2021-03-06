(* Q1: evaluates to true if date1 is older than date2 *)
fun is_older(date1: int*int*int, date2: int*int*int) =
    (* comparison order: year -> month -> date *)
    let val comparables = [(#1 date1, #1 date2),
			   (#2 date1, #2 date2),
			   (#3 date1, #3 date2)]
	fun compare(comparables: (int*int) list): bool =
	    if null comparables
	    then false
	    else if #1 (hd comparables) < #2 (hd comparables)
	    then true
	    else if #1 (hd comparables) > #2 (hd comparables)
	    then false
	    else compare(tl comparables)
    in compare(comparables)
    end

(* Q2: Given a list of dates and a month, count the number of dates in month *)
fun number_in_month(dates: (int*int*int) list, month: int) =
    (* use iteration to enhance performance *)
    let fun iter_method(dates: (int*int*int) list, count: int) =
	    if null dates
	    then count
	    else if #2 (hd dates) = month
	    then iter_method(tl dates, count+1)
	    else iter_method(tl dates, count)
    in iter_method(dates, 0)
    end

(* Q3 Use Q2 but instead of month, check for each month inside list of months *)
fun number_in_months(dates: (int*int*int) list, months: int list) =
    (* use iteration to enhance performance *)
    let fun iter_method(months: int list, count: int): int =
	    if null months
	    then count
	    else iter_method(tl months, count + number_in_month(dates, hd months))
    in iter_method(months, 0)
    end

(* Q4 return the list of dates in the given month *)
fun dates_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

(* Q5 return list of dates in the given months *)
fun dates_in_months(dates: (int*int*int) list, months: int list) = 
    if null months
    then []
    else dates_in_month(dates, hd months)@dates_in_months(dates, tl months)

(* Q6  *)
fun get_nth(str: string list, n: int) =
    if n <= 1
    then hd str
    else get_nth(tl str, n - 1)

(* Q7 *)
fun date_to_string(date: int*int*int) =
    let val months = ["January", "February", "March",
		      "April", "May", "June",
		      "July", "August", "September",
		      "October", "November", "December"]
	val year = #1 date
	val month = #2 date
	val day = #3 date
    in get_nth(months, month)^" "^Int.toString(day)^", "^Int.toString(year)
    end

(* Q8 return n that sum of element[1..n+1] is just greater than sum *)
fun number_before_reaching_sum(sum: int, elements: int list) =
    (* Since the Assumption is that the elements list is
     always sum to larger than the variable sum *)
    if null elements orelse sum - hd elements <= 0 
    then 0
    else 1 + number_before_reaching_sum(sum - hd elements,
					tl elements)
	   
(* Q9 take a day and return the month... assume the day is 1 - 365 *)
fun what_month(day: int) =
    (*  *)
    let val total_days_in_months = [31,28,31,30,31,30,
				    31,31,30,31,30,31]
    in number_before_reaching_sum(day, total_days_in_months) + 1
    end

(* Q10 *)
fun month_range(day1: int, day2:int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1, day2)

(* Q11 *)
fun oldest(dates: (int*int*int) list) =
    (* function in Q1 will be reuse to 
     compare two dates *)
    if null dates
    then NONE
    else
	let fun non_empty_oldest(dates: (int*int*int) list) =
		if null (tl dates)
		then hd dates
		else let val cached_oldest = non_empty_oldest(tl dates)
		     in if is_older(hd dates, cached_oldest)
			then hd dates
			else cached_oldest
		     end    
	in SOME(non_empty_oldest dates)
	end

(* Q12 The way I use in Q3 Q5 is iteration
This time try memoization *)
fun number_in_months_challenge() = 0

fun dates_in_months_challenge()= 0


	    
	   
