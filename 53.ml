(* Collect the nodes at a given level in a list. (easy)

A node of a binary tree is at level N if the path from the root to the node has length N-1. The root node is at level 1. Write a function at_level t l to collect all nodes of the tree t at level l in a list. *)
(*
	# at_level example_tree 2;;
	- : char list = ['b'; 'c']
	# at_level example_tree 5;;
	- : char list = []
*)

type 'a binary_tree =
	| Empty
	| Node of 'a * 'a binary_tree * 'a binary_tree;;

let example_tree =
	Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
		Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)));;

let at_level t level =
	let rec aux acc level = function
		| _ when level < 1 -> raise (Invalid_argument "level must be > 1.")
		| Empty -> acc
		| Node (x, _, _) when level = 1 -> x::acc
		| Node (x, n1, n2)              -> aux (aux acc (level-1) n2) (level-1) n1 in
	aux [] level t;;

at_level example_tree 1;;
at_level example_tree 2;;
at_level example_tree 3;;
at_level example_tree 4;;
at_level example_tree 5;;
