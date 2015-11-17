 (* Modified run-length encoding. (easy) *)

(* Modify the result of the previous problem in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Since OCaml lists are homogeneous, one needs to define a type to hold both single elements and sub-lists. *)

type 'a rle =
	| One of 'a
	| Many of int * 'a

let rec encode xs = match xs with
	| [] -> []
	| x :: xs ->
		let rec aux acc n c xs = match xs with
			| [] -> acc @ [Many (n, c)]
			| x :: xs ->
				if x = c then aux acc (n+1) c xs
				else if n = 1 then aux (acc @ [One c]) 1 x xs
				else aux (acc @ [Many (n, c)]) 1 x xs
		in aux [] 1 x xs;;

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
encode [];;
