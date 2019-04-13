(* 1. Compare two dates and return true if the first date is older than the other, else false *)
fun is_older (a: (int * int * int), b: (int * int * int)) =
    if (#1 a < #1 b)
    then true
    else if (#1 a = #1 b) andalso (#2 a < #2 b)
    then true
    else if (#1 a = #1 b) andalso (#2 a = #2 b) andalso (#3 a < #3 b)
    then true
    else false

(* 2. Count the number of dates in a list that are in a given month *)
fun number_in_month (dates: (int * int * int) list, month: int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

(* 3. Count the number of dates that are in a list of given months *)
fun number_in_months (dates: (int * int * int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* 4. Given a list of dates, find the dates that falls in a given month and return it in a list *)
fun dates_in_month (dates: (int * int * int) list, month: int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then hd dates :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

(* 5. Given a list of dates, find the dates that falls in any month in a list of months, then return it in a list *)
fun dates_in_months (dates: (int * int * int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* 6. Finds and returns the nth element of a string list *)
fun get_nth (xs: string list, n: int) =
    if n = 1
    then hd xs
    else get_nth(tl xs, n - 1)

(* 7. Converts a date of format YYYY,MM,DD to a string of MONTH_NAME DD, YYYY *)
fun date_to_string (date: int * int * int) =
    let
        val month_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(month_names, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

(* 8. Finds the maximum number of elements in a list that needs to be added sequentially that results in the highest value that is lower than a given value, sum *)
fun number_before_reaching_sum (sum: int, xs: int list) =
    if hd xs < sum
    then 1 + number_before_reaching_sum(sum - hd xs, tl xs)
    else 0

(* 9. Returns a month give a day in a year *)
fun what_month (day: int) =
    let
        val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day, days_in_months) + 1
    end

(* 10. Generate a list of months mapping to a range of days of a year between two given days of a year *)
fun month_range (d1: int, d2: int) =
    if d1 > d2
    then []
    else what_month(d1) :: month_range(d1 + 1, d2)

(* 11. Find the oldest date in a list of dates *)
fun oldest (dates: (int * int * int) list) =
    if null dates
    then NONE
    else if null (tl dates)
    then SOME (hd dates)
    else
        let val current_oldest = oldest(tl dates)
        in
            if isSome current_oldest andalso is_older(hd dates, valOf current_oldest)
            then SOME (hd dates)
            else current_oldest
        end