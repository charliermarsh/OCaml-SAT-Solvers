A fully functional set of OCaml modules to solve satisfiability problems represented in conjunctive normal form using only the OCaml list library. Specifically, uses the DPLL algorithm to solve (or attempt to solve) satisfiability problems. Written by Charles Marsh.

cnf.ml: contains representations for literals, symbols, cnf statements, clauses, etc.; also includes utility methods for generalized operations on cnf statements, including satisfiability checkers.

dpll.ml: contains an OCaml list-only implementation of the Davis-Putnam-Logemann-Loveland algorithm for solving satisfiability problems.

random_generator.ml: contains methods for generating random cnf statements of a specified size, as well as a method to print and solve said statements, all with a single call.