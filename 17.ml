(* Split a list into two parts; the length of the first part is given. (easy) *)
(*
	# split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
	- : string list * string list =
	(["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
	# split ["a";"b";"c";"d"] 5;;
	- : string list * string list = (["a"; "b"; "c"; "d"], [])
*)

let split xs n =
	let rec aux acc proc n = match proc with
		| [] -> (acc, [])
		| x :: xs ->
			if n = 1 then (x :: acc, xs)
			else aux (x :: acc) xs (n-1)
	in let (l1, l2) = aux [] xs n
	in (List.rev l1, l2);;

split [0;1;2;3;4;5] 3;;
split [0;1;2;3;4;5] 5;;
split [0;1;2;3;4;5] 6;;
split [0;1;2;3;4;5] 7;;

split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
split ["a";"b";"c";"d"] 5;;
