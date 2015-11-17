(* Insert an element at a given position into a list. (easy) *)
(* Start counting list elements with 0. If the position is larger or equal to the length of the list, insert the element at the end.
(The behavior is unspecified if the position is negative.) *)

(*
	# insert_at "alfa" 1 ["a";"b";"c";"d"];;
	- : string list = ["a"; "alfa"; "b"; "c"; "d"]
	# insert_at "alfa" 3 ["a";"b";"c";"d"];;
	- : string list = ["a"; "b"; "c"; "alfa"; "d"]
	# insert_at "alfa" 4 ["a";"b";"c";"d"];;
	- : string list = ["a"; "b"; "c"; "d"; "alfa"]
*)

let insert_at x i xs =
	let rec aux acc x i = function
		| [] -> (List.rev (x :: acc))
		| h :: t ->
			if i = 0 then (List.rev (h :: x :: acc)) @ t
			else aux (h :: acc) x (i-1) t
	in aux [] x i xs;;

insert_at "alfa" 1 ["a";"b";"c";"d"];;
insert_at "alfa" 3 ["a";"b";"c";"d"];;
insert_at "alfa" 4 ["a";"b";"c";"d"];;