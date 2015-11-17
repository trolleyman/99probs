(* Remove the K'th element from a list. (easy) *)
(*
	# remove_at 1 ["a";"b";"c";"d"];;
	- : string list = ["a"; "c"; "d"]
*)

let remove_at n xs =
	let rec aux acc n = function
		| [] -> failwith "remove_at"
		| x :: xs ->
			if n = 0 then (List.rev acc) @ xs
			else aux (x :: acc) (n-1) xs
	in aux [] n xs;;

remove_at 1 ["a";"b";"c";"d"];;
remove_at 4 ["a";"b";"c";"d"];;
remove_at (-1) ["a";"b";"c";"d"];;