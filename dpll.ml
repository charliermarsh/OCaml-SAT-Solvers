let find_pure_symbol (syms:symbol list) (cnf:cnf) (ml:model) : assignment option ;;

let find_unit_clause (cnf:cnf) (ml:model) : assignment option ;;


let dpll_sat (cnf:cnf) : boolean =
  
