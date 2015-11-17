(* Extract a given number of randomly selected elements from a list. (medium) *)
(* The selected items shall be returned in a list. We use the Random module but do not initialize it with Random.self_init for reproducibility. *)
(*
	# rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;
	- : string list = ["g"; "d"; "a"]
*)

let rand_select xs n =
	let rec extract xs acc n = match xs with
		| [] -> failwith "rand_select"
		| h :: t -> if n = 0 then (h, acc @ t) else extract t (h :: acc) (n-1)
	in let rec aux xs acc n =
		if n = 0 then acc
		else
			let (x, xs) = extract xs [] (Random.int (List.length xs)) in
			aux xs (x::acc) (n-1)
	in aux xs [] n;;

rand_select [0;1;2;3;4;5] 2;;
rand_select [0;1;2;3;4;5] 2;;
rand_select [0;1;2;3;4;5] 2;;
rand_select [0;1;2;3;4;5] 2;;
rand_select [0;1;2;3;4;5] 2;;
rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;
rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;
rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;
rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;
rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;
rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;
