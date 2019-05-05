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
(* 1. *)
val only_capitals = List.filter (fn x => Char.isUpper(String.sub (x, 0)))

(* 2. *)
val longest_string1 = foldl (fn (x, y) => if String.size x > String.size y then x else y) ""

(* 3. *)
val longest_string2 = foldl (fn (x, y) => if String.size x < String.size y then y else x) ""

(* 4. *)
fun longest_string_helper f xs = foldl (fn (x, y) => if f(String.size x, String.size y) then x else y) "" xs
val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => not (x < y))

(* 5. *)
val longest_capitalized = longest_string3 o only_capitals

(* 6. *)
val rev_string = String.implode o List.rev o String.explode

(* 7. *)
fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
      | x :: xs' => case f x of
                        NONE => first_answer f xs'
                      | SOME v => v

(* 8. *)
fun all_answers f xs =
    let
        fun iterator ys acc =
            case ys of
                [] => SOME acc
              | y :: ys' => case f y of
                                NONE => NONE
                              | SOME v => iterator ys' (acc @ v)
    in
        iterator xs []
    end

(* 9a. *)
val count_wildcards = g (fn () => 1) (fn _ => 0)

(* 9b. *)
val count_wild_and_variable_lengths = g (fn () => 1) (fn s => String.size s)

(* 9c. *)
fun count_some_var (str, pattern) =
	g (fn () => 0) (fn name => if name = str then 1 else 0) pattern

(* 10. *)
fun check_pat pattern =
    let
        fun extract_variable_names p =
            case p of
                Variable name => [name]
              | TupleP ps => List.foldl (fn (p, acc) => extract_variable_names p @ acc) [] ps
              | ConstructorP (_, p) => extract_variable_names p
              | _ => []

        fun has_repetition xs acc =
            case xs of
                [] => acc
              | x :: xs' => has_repetition xs' ((List.exists (fn y => x = y) xs') orelse acc)
    in
        not(has_repetition (extract_variable_names pattern) false)
    end