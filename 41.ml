(*
Truth tables for logical expressions. (medium)

Generalize the previous problem in such a way that the logical expression may contain any number of logical variables. Define table in a way that table variables expr returns the truth table for the expression expr, which contains the logical variables enumerated in variables.
*)
(*
	# table ["a"; "b"] (And(Var "a", Or(Var "a", Var "b")));;
	- : ((string * bool) list * bool) list =
	[([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
	 ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)]
	# let a = Var "a" and b = Var "b" and c = Var "c" in
	  table ["a"; "b"; "c"] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c))));;
	- : ((string * bool) list * bool) list =
	[([("a", true); ("b", true); ("c", true)], true);
	 ([("a", true); ("b", true); ("c", false)], true);
	 ([("a", true); ("b", false); ("c", true)], true);
	 ([("a", true); ("b", false); ("c", false)], false);
	 ([("a", false); ("b", true); ("c", true)], false);
	 ([("a", false); ("b", true); ("c", false)], false);
	 ([("a", false); ("b", false); ("c", true)], false);
	 ([("a", false); ("b", false); ("c", false)], false)]
*)

type bool_expr =
	| Var of string
	| Not of bool_expr
	| And of bool_expr * bool_expr
	| Or  of bool_expr * bool_expr;;

let rec eval vars = function
	| Var s          -> List.assoc s vars
	| Not expr       -> not (eval vars expr)
	| And (lhs, rhs) -> (eval vars lhs) && (eval vars rhs)
	| Or  (lhs, rhs) -> (eval vars lhs) || (eval vars rhs);;

let table vars expr =
	let rec next_perm acc = function
		| []          -> List.rev acc
		| true  :: xs -> next_perm (false::acc) xs
		| false :: xs -> List.rev_append (true::acc) xs in
	let all_perms start =
		let rec aux start acc next =	
			if next = start then acc
			else aux start (next::acc) (next_perm [] next) in
		List.map List.rev (start :: (aux start [] (next_perm [] start))) in
	let all_vals = all_perms (List.map (fun _ -> true) vars) in
	let compute = List.map (fun vals -> List.combine vars vals) all_vals in
	List.map (fun vars -> (vars, eval vars expr)) compute;;

table ["a"; "b"] (And(Var "a", Or(Var "a", Var "b")));;
let a = Var "a" and b = Var "b" and c = Var "c" in
	table ["a"; "b"; "c"] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c))));;
let a = Var "a" and b = Var "b" and c = Var "c" in
	table ["a"; "b"; "c"] (Or(a, And(b, c)));;
