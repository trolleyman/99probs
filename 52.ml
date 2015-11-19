(* Collect the internal nodes of a binary tree in a list. (easy)

An internal node of a binary tree has either one or two non-empty successors. Write a function internals to collect them in a list. *)
(*
	# internals (Node('a', Empty, Empty));;
	- : char list = []
	# internals example_tree;;
	- : char list = ['b'; 'a'; 'c'; 'f']
*)

type 'a binary_tree =
	| Empty
	| Node of 'a * 'a binary_tree * 'a binary_tree;;

let example_tree =
	Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
		Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)));;

let internals t =
	let rec aux acc = function
		| Empty -> List.rev acc
		| Node (_, Empty, Empty) -> List.rev acc
		| Node (x, n1, n2)       -> aux (aux (x::acc) n2) n1 in
	aux [] t;;

internals (Node('a', Empty, Empty));;
internals example_tree;;
internals (Node('a', Node('b', Empty, Empty), Empty));;
