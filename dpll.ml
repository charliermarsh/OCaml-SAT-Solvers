open Cnf

(* extracts a literal that appears in pure form in a given cnf *)
let find_pure_symbol (cnf:cnf) : assignment option =
  let matches_assignment (lit:literal) (lit_lst:literal list) : bool option =
    List.fold_left (fun acc nxt ->
                       match acc with
			   None -> let (sym, b) = lit in
				   let (sym', b') = nxt in
				   if (sym = sym') then Some (b = b')
				   else None
			 | _ -> acc) None lit_lst in
  let pure_lits = List.fold_left (fun pure_lits nxt_clause ->
                     (List.fold_left
			(fun lits nxt_lit ->
			  match (matches_assignment nxt_lit pure_lits) with
			    None -> nxt_lit::lits
			  | Some false -> let (sym, _) = nxt_lit in List.filter (fun (sym', _) -> not (sym = sym')) lits
			  | _ -> lits)
			pure_lits nxt_clause)) [] cnf in
  match pure_lits with
      [] -> None
    | hd::_ -> Some hd

(* extracts a literal that appears as a unit clause in a given cnf *)
let find_unit_clause (cnf:cnf) : assignment option =
  List.fold_left (fun acc nxt -> if (acc = None) then
                                    match nxt with hd::[] -> Some hd | _ -> None
                                 else acc) None cnf

(* returns a cnf with instances of a symbol and satisfied clauses removed *)
let cleanup (cnf:cnf) (sym:symbol) : cnf =
  List.fold_left
    (fun new_cnf clause ->
      let new_clause =
         (List.fold_left (fun new_clause lit ->
	   let (lit_sym, _) = lit in
	   if (lit_sym = sym) then new_clause else lit::new_clause) [] clause) in
    match new_clause with [] -> new_cnf | _ -> new_clause::new_cnf) [] cnf

(* main method to run dpll algorithm: returns (bool, model) where bool
 * = 'is cnf satisfied by model' *)
let dpll (cnf:cnf) : bool*model =
  let rec dpll_sat (cnf:cnf) (syms:symbol list) (ml:model) : bool*model =
    if (is_cnf_sat cnf ml) then (true, ml)
    else
      let unit = find_unit_clause cnf in
      let pure = if (unit = None) then find_pure_symbol cnf else None in
      match (unit, pure) with
	  ((Some asg, _) | (_, Some asg)) ->
	    let (sym, _) = asg in
	    let syms = List.filter (fun sym' -> not (sym = sym')) syms in
	    dpll_sat (cleanup cnf sym) syms (asg::ml)
	| _ -> match syms with
	         [] -> (false, ml)
               | hd::tl ->
		 let cnf' = cleanup cnf hd in
		 let asg1 = (hd, true) in
		 let (b1, m1) = dpll_sat cnf' tl (asg1::ml) in
		 if b1 then (b1, m1) else
		   let asg2 = (hd, false) in
		   dpll_sat cnf' tl (asg2::ml) in
  dpll_sat cnf (symbols_in_cnf cnf) []
