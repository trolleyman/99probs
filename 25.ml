(* Generate a random permutation of the elements of a list. (easy) *)
(*
	# permutation ["a"; "b"; "c"; "d"; "e"; "f"];;
	- : string list = ["a"; "e"; "f"; "b"; "d"; "c"]
*)

let permutation xs =
	let rec extract xs acc n = match xs with
		| [] -> failwith "extract"
		| h :: t -> if n = 0 then (h, acc @ t) else extract t (h :: acc) (n-1) in
	let extract_rand xs len =
		extract xs [] len in
	let rec aux acc xs len =
		if len = 0 then acc
		else let (picked, rest) = extract_rand xs len in
		aux (picked::acc) rest (len-1) in
	aux [] xs (List.length xs);;
