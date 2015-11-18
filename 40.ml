(* === Logic and Codes === *)
(*
Let us define a small "language" for boolean expressions containing variables:

# type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;
type bool_expr =
    Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
A logical expression in two variables can then be written in prefix notation. For example, (a ∨ b) ∧ (a ∧ b) is written:

# And(Or(Var "a", Var "b"), And(Var "a", Var "b"));;
- : bool_expr = And (Or (Var "a", Var "b"), And (Var "a", Var "b")) *)


(* Truth tables for logical expressions (2 variables). (medium)

Define a function, table2 which returns the truth table of a given logical expression in two variables (specified as arguments). The return value must be a list of triples containing (value_of_a, balue_of_b, value_of_expr). *)
(*
	# table2 "a" "b" (And(Var "a", Or(Var "a", Var "b")));;
	- : (bool * bool * bool) list =
	[(true, true, true); (true, false, true); (false, true, false);
	 (false, false, false)]
*)

type bool_expr =
	| Var of string
	| Not of bool_expr
	| And of bool_expr * bool_expr
	| Or  of bool_expr * bool_expr;;

let rec eval vars = function
	| Var s          -> Hashtbl.find vars s
	| Not expr       -> not (eval vars expr)
	| And (lhs, rhs) -> (eval vars lhs) && (eval vars rhs)
	| Or  (lhs, rhs) -> (eval vars lhs) || (eval vars rhs);;

let table2 a b expr =
	let aux a b aval bval expr =
		let vars = Hashtbl.create 2 in
		let _ =
			Hashtbl.add vars a aval;
			Hashtbl.add vars b bval; () in
		(aval, bval, eval vars expr) in
	(aux a b true  true  expr) ::
	(aux a b true  false expr) ::
	(aux a b false true  expr) ::
	(aux a b false false expr) ::
	[];;

table2 "a" "b" (And (Var "a", Var "b"));;
table2 "a" "b" (And (Not (Var "a"), Var "b"));;
table2 "a" "b" (Or (And (Var "a", Var "b"), And (Not (Var "a"), Not (Var "b"))));;

(*
# table2 "a" "b" (And (Var "a", Var "b"));;
- : (bool * bool * bool) list =
[(true, true, true); (true, false, false); (false, true, false);
 (false, false, false)]
# table2 "a" "b" (And (Not (Var "a"), Var "b"));;
- : (bool * bool * bool) list =
[(true, true, false); (true, false, false); (false, true, true);
 (false, false, false)]
# table2 "a" "b" (Or (And (Var "a", Var "b"), And (Not (Var "a"), Not (Var "b"))));;
- : (bool * bool * bool) list =
[(true, true, true); (true, false, false); (false, true, false);
 (false, false, true)]
*)
