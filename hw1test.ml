(* hw1test.ml *)

(* Subset Function Test Cases *)
let subset_test0 = subset [] [1;2;3]
let subset_test1 = subset [3;1;3] [1;2;3]
let subset_test2 = not (subset [1;3;7] [4;1;3])

(* Equal Sets Function Test Cases *)
let equal_sets_test0 = equal_sets [1;3] [3;1;3]
let equal_sets_test1 = not (equal_sets [1;3;4] [3;1;3])

(* Set Union Function Test Cases *)
let set_union_test0 = equal_sets (set_union [] [1;2;3]) [1;2;3]
let set_union_test1 = equal_sets (set_union [3;1;3] [1;2;3]) [1;2;3]
let set_union_test2 = equal_sets (set_union [] []) []

(* Set All Union Function Test Cases *)
let set_all_union_test0 = equal_sets (set_all_union []) []
let set_all_union_test1 = equal_sets (set_all_union [[3;1;3]; [4]; [1;2;3]]) [1;2;3;4]
let set_all_union_test2 = equal_sets (set_all_union [[5;2]; []; [5;2]; [3;5;7]]) [2;3;5;7]

(* Computed Fixed Point Function Test Cases *)
let computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x / 2) 1000000000 = 0
let computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity
let computed_fixed_point_test2 = computed_fixed_point (=) sqrt 10. = 1.
let computed_fixed_point_test3 = (computed_fixed_point (fun x y -> abs_float (x -. y) < 1.) (fun x -> x /. 2.) 10.) = 1.25

(* Computed Periodic Point Function Test Cases *)
let computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> x / 2) 0 (-1) = -1
let computed_periodic_point_test1 = computed_periodic_point (=) (fun x -> x *. x -. 1.) 2 0.5 = -1.

(* Filter Blind Alleys Function Test Cases *)
(* Note: Define awksub_grammar and giant_grammar before these test cases as they depend on them *)
(* Grammar Definitions for Filter Blind Alleys Test Cases *)
type awksub_nonterminals = Expr | Lvalue | Incrop | Binop | Num
let awksub_rules =
  [Expr, [T "("; N Expr; T ")"];
   Expr, [N Num];
   Expr, [N Expr; N Binop; N Expr];
   Expr, [N Lvalue];
   Expr, [N Incrop; N Lvalue];
   Expr, [N Lvalue; N Incrop];
   Lvalue, [T "$"; N Expr];
   Incrop, [T "++"];
   Incrop, [T "--"];
   Binop, [T "+"];
   Binop, [T "-"];
   Num, [T "0"];
   Num, [T "1"];
   Num, [T "2"];
   Num, [T "3"];
   Num, [T "4"];
   Num, [T "5"];
   Num, [T "6"];
   Num, [T "7"];
   Num, [T "8"];
   Num, [T "9"]]
let awksub_grammar = Expr, awksub_rules

type giant_nonterminals = Conversation | Sentence | Grunt | Snore | Shout | Quiet
let giant_grammar =
  Conversation,
  [Snore, [T "ZZZ"];
   Quiet, [];
   Grunt, [T "khrgh"];
   Shout, [T "aooogah!"];
   Sentence, [N Quiet];
   Sentence, [N Grunt];
   Sentence, [N Shout];
   Conversation, [N Snore];
   Conversation, [N Sentence; T ","; N Conversation]]

let awksub_test0 = filter_blind_alleys awksub_grammar = awksub_grammar
let awksub_test1 = filter_blind_alleys (Expr, List.tl awksub_rules) = (Expr, List.tl awksub_rules)
let awksub_test2 = filter_blind_alleys (Expr, [Expr, [N Num]; Expr, [N Lvalue]; Expr, [N Expr; N Lvalue]; Expr, [N Lvalue; N Expr]; Expr, [N Expr; N Binop; N Expr]; Lvalue, [N Lvalue; N Expr]; Lvalue, [N Expr; N Lvalue]; Lvalue, [N Incrop; N Lvalue]; Lvalue, [N Lvalue; N Incrop]; Incrop, [T "++"]; Incrop, [T "--"]; Binop, [T "+"]; Binop, [T "-"]; Num, [T "0"]; Num, [T "1"]; Num, [T "2"]; Num, [T "3"]; Num, [T "4"]; Num, [T "5"]; Num, [T "6"]; Num, [T "7"]; Num, [T "8"]; Num, [T "9"]]) = (Expr, [Expr, [N Num]; Expr, [N Expr; N Binop; N Expr]; Incrop, [T "++"]; Incrop, [T "--"]; Binop, [T "+"]; Binop, [T "-"]; Num, [T "0"]; Num, [T "1"]; Num, [T "2"]; Num, [T "3"]; Num, [T "4"]; Num, [T "5"]; Num, [T "6"]; Num, [T "7"]; Num, [T "8"]; Num, [T "9"]])
let awksub_test3 = filter_blind_alleys (Expr, List.tl (List.tl (List.tl awksub_rules))) = filter_blind_alleys (Expr, List.tl (List.tl awksub_rules))

let giant_test0 = filter_blind_alleys giant_grammar = giant_grammar
let giant_test1 = filter_blind_alleys (Sentence, List.tl (snd giant_grammar)) = (Sentence, [Quiet, []; Grunt, [T "khrgh"]; Shout, [T "aooogah!"]; Sentence, [N Quiet]; Sentence, [N Grunt]; Sentence, [N Shout]])
let giant_test2 = filter_blind_alleys (Sentence, List.tl (List.tl (snd giant_grammar))) = (Sentence, [Grunt, [T "khrgh"]; Shout, [T "aooogah!"]; Sentence, [N Grunt]; Sentence, [N Shout]])

(* Additional Subset Function Test Cases *)
let my_subset_test0 = subset [1; 2] [1; 2; 3]
let my_subset_test1 = not (subset [1; 4] [1; 2; 3])

(* Additional Equal Sets Function Test Cases *)
let my_equal_sets_test0 = not (equal_sets [1; 2; 3] [3; 4; 5])
let my_equal_sets_test1 = equal_sets [2; 2; 3] [3; 2]

(* Additional Set Union Function Test Cases *)
let my_set_union_test0 = equal_sets (set_union [1; 2] [3; 4]) [1; 2; 3; 4]
let my_set_union_test1 = equal_sets (set_union [] [1; 2]) [1; 2]

(* Additional Set All Union Function Test Cases *)
let my_set_all_union_test0 = equal_sets (set_all_union [[1; 2]; [2; 3]; [3; 4]]) [1; 2; 3; 4]
let my_set_all_union_test1 = equal_sets (set_all_union [[1]; []; [2]]) [1; 2]

(* Additional Computed Fixed Point Function Test Cases *)
let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x + 1) 0 = 0
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x mod 7) 10 = 3

(* Additional Computed Periodic Point Function Test Cases *)
let my_computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> x - 1) 2 5 = 4
let my_computed_periodic_point_test1 = computed_periodic_point (=) (fun x -> -x) 1 10 = -10

(* Additional Filter Blind Alleys Function Test Cases *)
let my_filter_blind_alleys_test0 =
  let grammar = Expr, [Expr, [T "1"]; Lvalue, [N Expr; T "+"; N Expr]]
  in filter_blind_alleys grammar = grammar

let my_filter_blind_alleys_test1 =
  let grammar = Expr, [Expr, [N Lvalue; T "2"]; Lvalue, [N Expr]]
  in filter_blind_alleys grammar = (Expr, [])

(* Additional Test Cases for Newly Implemented Functions *)
let my_terminal_production_check_test0 = terminal_production_check (T "a") ["a"; "b"; "c"]
let my_terminal_production_check_test1 = not (terminal_production_check (N Expr) ["a"; "b"])

let my_all_symbols_terminal_producing_test0 = all_symbols_terminal_producing [T "a"; N Expr] [Expr]
let my_all_symbols_terminal_producing_test1 = not (all_symbols_terminal_producing [N Expr; N Lvalue] [Expr])

let my_update_productive_symbol_list_test0 = 
  update_productive_symbol_list [Expr, [N Num]; Num, [T "1"]] [Num] = [Expr; Num]

let my_productive_symbols_accumulator_test0 = 
  productive_symbols_accumulator ([Expr, [N Num]; Num, [T "1"]], [Num]) = ([Expr, [N Num]; Num, [T "1"]], [Expr; Num])

let my_compute_productive_symbols_test0 = 
  compute_productive_symbols [Expr, [N Num]; Num, [T "1"]] = [Expr; Num]

let my_filter_non_terminal_rules_test0 = 
  filter_non_terminal_rules [Expr, [N Num]; Num, [T "1"]] [Expr; Num] = [Expr, [N Num]; Num, [T "1"]]

