open Cnf
open Dpll

(* generates a randomized cnf of length num_clauses in which each clause
 * has num_literals literals and the total number of distinct symbols is
 * bounded by num_syms *)
let generate_cnf (num_syms:int) (num_clauses:int) (num_literals:int) : cnf =
  (* utility method to generate clause of length num_literals *)
  let generate_clause () =
    let rec add_literal cnt cl =
      match cnt with
	  0 -> cl
	| _ -> let sym = "P" ^ string_of_int (Random.int num_syms) in
	       add_literal (cnt-1) ((sym, Random.bool ())::cl) in
    add_literal num_literals [] in
  let rec add_clause cnt cnf =
    match cnt with
	0 -> cnf
  | _ -> add_clause (cnt-1) (generate_clause ()::cnf) in
  add_clause num_clauses []

let gen_solve_print (num_syms:int) (num_clauses:int) (num_literals:int) =
  let cnf = generate_cnf num_syms num_clauses num_literals in
  print_cnf cnf;
  print_newline ();
  let (b, ml) = dpll cnf in
  if b then print_string "Solved" else print_string "Unsolved";
  print_newline ();
  print_model ml
