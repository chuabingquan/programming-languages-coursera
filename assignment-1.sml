(* Compare two dates and return true if the first date is older than the other, else false *)
fun is_older (a: (int * int * int), b: (int * int * int)) =
    if (#1 a < #1 b)
    then true
    else if (#1 a = #1 b) andalso (#2 a < #2 b)
    then true
    else if (#1 a = #1 b) andalso (#2 a = #2 b) andalso (#3 a < #3 b)
    then true
    else false

(* Count the number of dates in a list that are in a given month *)
fun number_in_month (dates: (int * int * int) list, month: int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

(* Count the number of dates that are in a list of given months *)
fun number_in_months (dates: (int * int * int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* Given a list of dates, find the dates that falls in a given month and return it in a list *)
fun dates_in_month (dates: (int * int * int) list, month: int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then hd dates :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)