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

(* 9. Returns a month given a day in a year *)
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

(* 12. number_in_months and dates_in_months that can deal with duplicate months argument *)
fun contains (xs: int list, item: int) =
    if null xs
    then false
    else if hd xs = item
    then true orelse contains(tl xs, item)
    else false orelse contains(tl xs, item)

fun remove_duplicates (xs: int list) =
    if null xs
    then []
    else
        let val clean_list = remove_duplicates(tl xs) 
        in
            if contains(clean_list, hd xs)
            then clean_list
            else hd xs :: clean_list
        end

fun number_in_months_challenge (dates: (int * int * int) list, months: int list) =
    number_in_months(dates, remove_duplicates months)

fun dates_in_months_challenge (dates: (int * int * int) list, months: int list) =
    dates_in_months(dates, remove_duplicates months)

(* 13. Validates if a given date is a real date in the common era *)
fun reasonable_date (date: int * int * int) =
    let
        val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val leap_year_days_in_months = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

        fun is_leap_year (year: int) =
            let
                val divisible_by_400 = (year = ((year div 400) * 400))
                val divisible_by_4 = (year = ((year div 4) * 4))
                val divisible_by_100 = (year = ((year div 100) * 100))
            in
                if (divisible_by_400) orelse (divisible_by_4 andalso not divisible_by_100)
                then true
                else false
            end

        fun get_nth_int (xs: int list, n: int) =
            if n = 1
            then hd xs
            else get_nth_int(tl xs, n - 1)
    in
        if (#1 date < 1) orelse (#2 date < 1) orelse (#2 date > 12) 
            orelse (#3 date < 1) orelse (#3 date > 31)
        then false
        else if is_leap_year(#1 date)
        then (#3 date) <= get_nth_int(leap_year_days_in_months, #2 date)
        else (#3 date) <= get_nth_int(days_in_months, #2 date)
    end