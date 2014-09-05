type symbol = string
type literal = symbol * bool
type clause = literal list
type cnf = clause list
type assignment = symbol * bool
type model = assignment list

open List

(* utility method for finding the assignment of a symbol within a model *)
let rec find_assignment (sym:symbol) (ml:model) : bool option =
  match ml with
      [] -> None
    | (sym', b)::tl -> if (sym = sym') then Some b else find_assignment sym tl

(* utility method for checking if a model satisfies a clause *)
let is_clause_sat (cl:clause) (ml:model) : bool =
  List.fold_left (fun acc lit -> let (sym, b) = lit in
   acc || (Some b = find_assignment sym ml))
   false cl

(* utility method for checking if a model satisfies a cnf *)
let is_cnf_sat (cnf:cnf) (ml:model) : bool =
  List.fold_left (fun acc cl -> acc && is_clause_sat cl ml) true cnf

(* utility method for extracting set of symbols in a cnf *)
let symbols_in_cnf (cnf:cnf) : symbol list =
  let contains (sym:symbol) (syms:symbol list) : bool =
    List.fold_left (fun acc nxt -> acc || nxt = sym) false syms in
  List.fold_left (fun all_syms nxt ->
                      List.fold_left (fun acc nxt ->
    let (sym, _) = nxt in if not (contains sym acc) then sym::acc else acc)
     all_syms nxt)
    [] cnf

(* method to print model *)
let print_model (ml:model) =
  List.fold_left
    (fun _ (sym, b) ->
      print_string sym;
      print_string " = ";
      if b then print_string "true" else print_string "false";
      print_newline ()) () ml

(* method to print cnf *)
let print_cnf (cnf:cnf) =
  List.fold_left
    (fun _ clause ->
      print_string "---CLAUSE---";
      print_newline ();
      print_model clause) () cnf
