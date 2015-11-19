(*Layout a binary tree (1). (medium)

As a preparation for drawing the tree, a layout algorithm is required to determine the position of each node in a rectangular grid. Several layout methods are conceivable, one of them is shown in the illustration.

Binary Tree Grid

In this layout strategy, the position of a node v is obtained by the following two rules:

x(v) is equal to the position of the node v in the inorder sequence;
y(v) is equal to the depth of the node v in the tree.
In order to store the position of the nodes, we redefine the OCaml type representing a node (and its successors) as follows:

# type 'a pos_binary_tree =
    | E (* represents the empty tree *)
    | N of 'a * int * int * 'a pos_binary_tree * 'a pos_binary_tree;;
type 'a pos_binary_tree =
    E
  | N of 'a * int * int * 'a pos_binary_tree * 'a pos_binary_tree
N(w,x,y,l,r) represents a (non-empty) binary tree with root w "positioned" at (x,y), and subtrees l and r. Write a function layout_binary_tree_1 with the following specification: layout_binary_tree_1 t returns the "positioned" binary tree obtained from the binary tree t.

The tree pictured above is

# let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node('n', Node('k', Node('c', leaf 'a',
                             Node('h', Node('g', leaf 'e',Empty), Empty)),
                   leaf 'm'),
         Node('u', Node('p', Empty, Node('s', leaf 'q', Empty)), Empty));;
val example_layout_tree : char binary_tree =
  Node ('n',
   Node ('k',
    Node ('c', Node ('a', Empty, Empty),
     Node ('h', Node ('g', Node ('e', Empty, Empty), Empty), Empty)),
    Node ('m', Empty, Empty)),
   Node ('u', Node ('p', Empty, Node ('s', Node ('q', Empty, Empty), Empty)),
    Empty)) *)
(*
	# layout_binary_tree_1 example_layout_tree;;
	- : char pos_binary_tree =
	N ('n', 8, 1,
	 N ('k', 6, 2,
	  N ('c', 2, 3, N ('a', 1, 4, E, E),
	   N ('h', 5, 4, N ('g', 4, 5, N ('e', 3, 6, E, E), E), E)),
	  N ('m', 7, 3, E, E)),
	 N ('u', 12, 2, N ('p', 9, 3, E, N ('s', 11, 4, N ('q', 10, 5, E, E), E)), E))
*)

type 'a binary_tree =
	| Empty
	| Node of 'a * 'a binary_tree * 'a binary_tree;;

type 'a pos_binary_tree =
	| E (* represents the empty tree *)
	| N of 'a * int * int * 'a pos_binary_tree * 'a pos_binary_tree;;

let example_layout_tree =
	let leaf x = Node (x, Empty, Empty) in
	Node('n', Node('k', Node('c', leaf 'a',
							Node('h', Node('g', leaf 'e', Empty), Empty)),
					leaf 'm'),
		Node('u', Node('p', Empty, Node('s', leaf 'q', Empty)), Empty));;

let layout_binary_tree_1 t =
	let width t =
		let rec aux acc = function
			| Empty -> acc
			| Node (_, n1, n2) -> aux (aux (acc+1) n2) n1 in
		aux 0 t in
	let rec aux t y x_min x_max = match t with
		| Empty -> E
		| Node (n, Empty, Empty) -> N (n, x_min, y, E, E)
		| Node (n, l, r) ->
			let lw = width l in
			let rw = width r in
			let l = aux l (y+1) x_min (x_max - rw - 1) in
			let r = aux r (y+1) (x_min + lw + 1) x_max in
			N (n, x_min + lw, y, l, r) in
	aux t 1 1 (width t);;

layout_binary_tree_1 example_layout_tree;;
layout_binary_tree_1 (Node('a', Node('b', Empty, Empty), Node('c', Empty, Empty)));;
