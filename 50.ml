(* Count the leaves of a binary tree. (easy)

A leaf is a node with no successors. Write a function count_leaves to count them.*)
(*
	# count_leaves Empty;;
	- : int = 0
	# count_leaves example_tree;;
	- : int = 3
*)

type 'a binary_tree =
	| Empty
	| Node of 'a * 'a binary_tree * 'a binary_tree;;

let example_tree =
	Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
		Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)));;

let count_leaves n =
	let rec aux acc = function
		| Empty -> acc
		| Node (_, Empty, Empty) -> acc + 1
		| Node (_, n1, n2)       -> aux (aux acc n1) n2 in
	aux 0 n;;

count_leaves Empty;;
count_leaves (Node(0, Node(1, Empty, Empty), Node(2, Empty, Empty)));;
count_leaves example_tree;;
