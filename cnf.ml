type symbol = string ;;
type literal = symbol * bool ;;
type clause = literal list ;;
type cnf = clause list ;;
type assignment = symbol * bool ;;
type model = assignment list ;;

open List ;;

(* utility method for finding the assignment of a symbol within a model *)
let rec find_assignment (sym:symbol) (ml:model) : bool =
  match ml with
      [] -> failwith "literal not contained in model"
    | (sym', b)::tl -> if (sym = sym') then b else find_assignment sym tl
;;

(* utility method for checking if a model satisfies a clause *)
let is_clause_sat (cl:clause) (ml:model) : bool =
  List.fold_left (fun acc lit -> let (sym, b) = lit in
				 acc || (b = find_assignment sym ml))
				 false ml
;;

(* utility method for checking if a model satisfies a cnf *)
let is_cnf_sat (cnf:cnf) (ml:model) : bool =
  List.fold_left (fun acc cl -> acc && is_clause_sat cl ml) true cnf
;;

let symbols_in_cnf (cnf:cnf) : symbol list =
  let contains (sym:symbol) (syms:symbol list) : bool =
    List.fold_left (fun acc nxt -> acc || nxt = sym) false syms in
  List.fold_left (fun all_syms nxt ->
                      List.fold_left (fun acc nxt ->
			  let (sym, _) = nxt in if not (contains sym acc) then sym::acc else acc)
		   all_syms nxt)
    [] cnf
;;
