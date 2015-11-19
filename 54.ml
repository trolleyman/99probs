(* Construct a complete binary tree. (medium)

A complete binary tree with height H is defined as follows: The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2i-1 at the level i, note that we start counting the levels from 1 at the root). In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted". This means that in a levelorder tree traversal all internal nodes come first, the leaves come second, and empty successors (the nil's which are not really nodes!) come last.

Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.

We can assign an address number to each node in a complete binary tree by enumerating the nodes in levelorder, starting at the root with number 1. In doing so, we realize that for every node X with address A the following property holds: The address of X's left and right successors are 2*A and 2*A+1, respectively, supposed the successors do exist. This fact can be used to elegantly construct a complete binary tree structure. Write a function is_complete_binary_tree with the following specification: is_complete_binary_tree n t returns true iff t is a complete binary tree with n nodes. *)
(*
	# complete_binary_tree [1;2;3;4;5;6];;
	- : int binary_tree =
	Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)),
	 Node (3, Node (6, Empty, Empty), Empty))
*)

type 'a binary_tree =
	| Empty
	| Node of 'a * 'a binary_tree * 'a binary_tree;;

let pow_2 x =
	let rec aux acc = function
		| n when n < 0 -> raise (Invalid_argument "positive numbers only")
		| 0 -> acc
		| n -> aux (acc*2) (n-1) in
	aux 1 x;;

let largest_pow_2 n =
	let rec aux acc i prev = function
		| n when n < acc -> i-1
		| n when n = acc -> i
		| n              -> aux (acc*2) (i+1) acc n in
	aux 2 1 1 n;;

let complete_binary_tree xs =
	let create_complete_tree i xs =
		let rec aux i xs = match i with
			| 1 -> (Node (List.hd xs, Empty, Empty), List.tl xs)
			| i ->
				let x, xs = List.hd xs, List.tl xs in
				let n1, xs = aux (i-1) xs in
				let n2, xs = aux (i-1) xs in
				(Node (x, n1, n2), xs) in
		aux i xs in
	let add_to t xs =
		let rec aux t xs = match t with
			| Empty -> (match xs with
				| [] -> (Empty, xs)
				| x :: xs -> (Node (x, Empty, Empty), xs))
			| Node (x, n1, n2) ->
				let (n1, xs) = aux n1 xs in
				let (n2, xs) = aux n2 xs in
				(Node (x, n1, n2), xs) in
		aux t xs in
	let len = List.length xs in
	let i = largest_pow_2 (len + 1) in
	let (t, rest) = create_complete_tree i xs in
	fst (add_to t rest);;

let a = complete_binary_tree [1;2]
let b = complete_binary_tree [1;2;3]
let c = complete_binary_tree [1;2;3;4]
let d = complete_binary_tree [1;2;3;4;5]
let e = complete_binary_tree [1;2;3;4;5;6]
let f = complete_binary_tree [1;2;3;4;5;6;7];;

let is_complete_binary_tree n t =
	let rec aux n = function
		| Empty -> n = 0
		| Node (_, Empty, Empty) -> n = 1
		| Node (_, n1, n2)       -> aux (n-1) n1 && aux (n-1) n2 in
	aux n t;;

is_complete_binary_tree 1 (complete_binary_tree [1]);; (*true*)
is_complete_binary_tree 1 a;;
is_complete_binary_tree 2 a;;
is_complete_binary_tree 2 b;; (*true*)
is_complete_binary_tree 3 c;;
is_complete_binary_tree 3 d;;
is_complete_binary_tree 3 e;;
is_complete_binary_tree 3 f;; (*true*)
