(*Collect the leaves of a binary tree in a list. (easy)

A leaf is a node with no successors. Write a function leaves to collect them in a list.*)
(*
	# leaves Empty;;
	- : 'a list = []
	# leaves example_tree;;
	- : char list = ['d'; 'e'; 'g']
*)

type 'a binary_tree =
	| Empty
	| Node of 'a * 'a binary_tree * 'a binary_tree;;

let example_tree =
	Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
		Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)));;

let leaves n =
	let rec aux acc = function
		| Empty -> acc
		| Node (x, Empty, Empty) -> x::acc
		| Node (_, n1, n2)       -> aux (aux acc n2) n1 in
	aux [] n;;

leaves Empty;;
leaves (Node(0, Node(1, Empty, Empty), Node(2, Empty, Empty)));;
leaves example_tree;;
