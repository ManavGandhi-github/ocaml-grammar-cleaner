(* Type Definitions *)
type ('nonterminal, 'terminal) symbol = N of 'nonterminal | T of 'terminal
type ('nonterminal, 'terminal) rule = 'nonterminal * ('nonterminal, 'terminal) symbol list
type ('nonterminal, 'terminal) grammar = 'nonterminal * ('nonterminal, 'terminal) rule list

(* Subset Function *)
let rec subset a b = match a with
    | [] -> true
    | head :: tail -> List.mem head b && subset tail b

(* Equal Sets Function *)
let equal_sets a b = subset a b && subset b a

(* Set Union Function *)
let set_union a b = a @ b

(* Set All Union Function *)
let rec set_all_union a = match a with
    | [] -> []
    | head :: tail -> set_union head (set_all_union tail)

(* Self Member Function - Theoretical *)
let self_member s = false  
(* Russell's Paradox arises when considering a set that contains all sets that do not contain themselves. 
   Does such a set contain itself? If it does, by definition it should not. If it doesn't, by definition it should.
   This paradox makes it impossible to implement a function in OCaml (or any language) that correctly determines 
   whether a set is a member of itself without leading to a contradiction. Thus, the function is unimplementable 
   and returns false by default. *)


(* Computed Fixed Point Function *)
let rec computed_fixed_point eq f x =
    let fx = f x in
    if eq fx x then x
    else computed_fixed_point eq f fx

(* Computed Periodic Point Function *)
let rec computed_periodic_point eq f p x =
    let rec apply_f_p_times f p current_x =
        if p = 0 then current_x
        else apply_f_p_times f (p - 1) (f current_x) in
    let rec find_periodic_point current_x =
        let fxp = apply_f_p_times f p current_x in
        if eq fxp current_x then current_x
        else find_periodic_point (f current_x) in
    find_periodic_point x

(* Whileseq Function *)
let rec whileseq s p x =
    if p x then x :: whileseq s p (s x)
    else []



(* Checks if an element is part of a list *)
let rec is_present_in_list item list = 
    match list with
    | [] -> false
    | head::tail -> (head = item) || is_present_in_list item tail

(* Determines if a grammar symbol leads to a terminal *)
let rec terminal_production_check symbol productive_list = 
    match symbol with 
    | T _ -> true
    | N non_terminal -> is_present_in_list (N non_terminal) productive_list

(* Validates if all symbols in a rule's RHS can produce a terminal *)
let rec all_symbols_terminal_producing rhs productive_list = 
    match rhs with 
    | [] -> true
    | symbol::remaining_symbols -> 
        terminal_production_check symbol productive_list && 
        all_symbols_terminal_producing remaining_symbols productive_list

(* Updates the list of symbols that lead to terminals *)
let rec update_productive_symbol_list grammar_rules known_productive = 
    match grammar_rules with
    | (sym, rhs)::remaining_rules -> 
        let new_productive = 
            if all_symbols_terminal_producing rhs known_productive 
            then N sym :: known_productive 
            else known_productive 
        in update_productive_symbol_list remaining_rules new_productive
    | [] -> known_productive

(* Wrapper for fixed point calculation *)
let productive_symbols_accumulator (rules, productive) = 
    (rules, update_productive_symbol_list rules productive)

(* Compute fixed point to find all productive symbols *)
let compute_productive_symbols grammar =
    let init_state = (grammar, [])
    in let (_, productive_syms) = computed_fixed_point 
        (fun (_, lst1) (_, lst2) -> equal_sets lst1 lst2) 
        productive_symbols_accumulator init_state
    in productive_syms

(* Filters out rules that don't lead to terminal symbols *)
let filter_non_terminal_rules rules productive_symbols =
    List.filter (fun (sym, rhs) ->
        is_present_in_list (N sym) productive_symbols && 
        all_symbols_terminal_producing rhs productive_symbols
    ) rules

(* Main function to eliminate non-productive rules *)
let filter_blind_alleys (start_symbol, rules) =
    let productive_symbols = compute_productive_symbols rules in
    (start_symbol, filter_non_terminal_rules rules productive_symbols)
