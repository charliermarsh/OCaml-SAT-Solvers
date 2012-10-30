type symbol = string ;;
type literal = symbol * boolean ;;
type clause = literal list ;;
type cnf = clause list ;;
type assignment = symbol * boolean ;;
type model = assignment list ;;

open List ;;

(* utility method for finding the assignment of a symbol within a model *)
let rec find_assignment (sym:symbol) (ml:model) : boolean =
  match ml with
      [] -> failwith "literal not contained in model"
    | (sym', bool)::tl -> if (sym = sym') then bool else find_assignment sym tl
;;

(* utility method for checking if a model satisfies a clause *)
let is_clause_sat (cl:clause) (ml:model) : boolean =
  List.fold_left (fun acc lit -> let (sym, bool) = lit in
				 acc || (bool = find_assignment sym ml))
				 false ml
;;

(* utility method for checking if a model satisfies a cnf *)
let is_cnf_sat (cnf:cnf) (ml:model) : boolean =
  List.fold_left (fun acc cl -> acc && is_clause_sat cl ml) true cnf
;;
