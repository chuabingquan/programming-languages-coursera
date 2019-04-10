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

(* fun number_in_months (dates: (int * int * int) list, months: int list) =
    if null dates orelse null months
    then 0
    else if #2 (hd dates) = hd months
    then 1 + number_in_months() *)

