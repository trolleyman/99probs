(* Run-length encoding of a list. (easy) *)
(*
	# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
	- : (int * string) list =
	[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
*)


let rec encode xs = match xs with
	| [] -> []
	| x :: xs ->
		let rec aux acc n c xs = match xs with
			| [] -> acc @ [(n, c)]
			| x :: xs ->
				if x = c then aux acc (n+1) c xs
				else aux (acc @ [(n, c)]) 1 x xs
		in aux [] 1 x xs;;

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
encode [];;