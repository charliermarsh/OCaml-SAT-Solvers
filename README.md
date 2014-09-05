# OCaml-SAT-Solvers

A fully functional set of OCaml modules to solve satisfiability problems represented in conjunctive normal form (CNF) using only the OCaml list library and the DPLL algorithm.

## File Structure

The repository is broken down into several files:

- `cnf.ml`: contains representations for literals, symbols, cnf statements, clauses, etc., as well as utility methods for generalized operations on CNF statements, including satisfiability checkers.

- `dpll.ml`: contains an OCaml list-only implementation of the Davis-Putnam-Logemann-Loveland (DPLL) algorithm for solving satisfiability problems.

- `generator.ml`: contains methods for generating random cnf statements of a specified size, as well as a method to print and solve said statements, all with a single function call.

- `test.ml`: contains a sample test case.
