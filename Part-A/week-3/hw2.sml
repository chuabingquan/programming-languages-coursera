(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* 1a. *)
fun all_except_option (x, xs) =
    let fun remove_from_list (ys) =
        case ys of
            [] => []
          | y :: ys' => if same_string(x, y)
                        then remove_from_list(ys')
                        else y :: remove_from_list(ys')
        val lst = remove_from_list xs
    in
        if lst = xs then NONE else SOME lst
    end

(* 1b. *)
fun get_substitutions1 (substitutions, s) =
    case substitutions of
        [] => []
      | x :: xs => case all_except_option(s, x) of
                        NONE => get_substitutions1(xs, s)
                      | SOME substitutes => substitutes @ get_substitutions1(xs, s)

(* 1c. *)
fun get_substitutions2 (substitutions, s) =
    let fun get_sub_tail_rec (subs, acc) =
        case subs of
            [] => acc
          | x :: xs => case all_except_option(s, x) of
                            NONE => get_sub_tail_rec(xs, acc)
                          | SOME substitutes => get_sub_tail_rec(xs, acc @ substitutes)
    in
        get_sub_tail_rec(substitutions, [])
    end

(* 1d. *)
fun similar_names (substitutions, fullname) =
    let val { first = first, middle = middle, last = last } = fullname
        val similar_first_names = get_substitutions2(substitutions, first)
        fun generate_names (first_names, acc) =
            case first_names of
                [] => acc
              | x :: xs => generate_names(xs, {first = x, middle = middle, last = last} :: acc)
    in
        fullname :: generate_names(similar_first_names, [])
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* 2a. *)
fun card_color (suit, _) =
    case suit of
        Clubs => Black
      | Spades => Black
      | Diamonds => Red
      | Hearts => Red

(* 2b. *)
fun card_value (_, rank) =
    case rank of
        Num n => n
      | Ace => 11
      | _ => 10

(* 2c. *)
fun remove_card (cs, c, e) =
    let
        fun remove_card_helper (cards) =
            case cards of
                [] => []
              | card :: remaining_cards => if card = c
                                           then remaining_cards
                                           else card :: remove_card_helper(remaining_cards)
        val filtered = remove_card_helper(cs)
    in
        if filtered = cs 
        then raise e
        else filtered
    end

(* 2d. *)
fun all_same_color (cs) =
    case cs of
        [] => true
      | card :: [] => true
      | card :: card' :: remaining_cards => if card_color(card) = card_color(card')
                                            then all_same_color(card' :: remaining_cards)
                                            else false

(* 2e. *)
fun sum_cards (cs) =
    let
        fun sum_cards_tail_rec (cards, acc) =
            case cards of
                [] => acc
              | card :: remaining_cards => sum_cards_tail_rec(remaining_cards, acc + card_value(card))
    in
        sum_cards_tail_rec(cs, 0)
    end

(* 2f. *)
fun score (cs, goal) =
    let
        val sum = sum_cards cs
        val preliminary_score = if sum > goal then 3 * (sum - goal) else goal - sum
    in
        if all_same_color cs
        then preliminary_score div 2
        else preliminary_score
    end

(* 2g. *)
fun officiate (deck_list, move_list, goal) = 
    let
        fun proceed_game (deck, holding, moves) =
            case moves of
                [] => holding
              | Discard card :: remaining_moves => proceed_game(deck, remove_card(holding, card, IllegalMove), remaining_moves)
              | Draw :: remaining_moves => case deck of
                                            [] => holding
                                          | card :: remaining_deck => if sum_cards(card :: holding) > goal
                                                                      then card :: holding
                                                                      else proceed_game(remaining_deck, card :: holding, remaining_moves)
    in
        score(proceed_game(deck_list, [], move_list), goal)
    end