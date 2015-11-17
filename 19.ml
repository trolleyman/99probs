(* Rotate a list N places to the left. (medium) *)
(*
	# rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
	- : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
	# rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
	- : string list = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
*)

let rotate xs n =
	let split xs n =
		let rec aux acc proc n = match proc with
			| [] -> (acc, [])
			| x :: xs ->
				if n = 1 then (x :: acc, xs)
				else aux (x :: acc) xs (n-1) in
		let (l1, l2) = aux [] xs n in
		(List.rev l1, l2) in
	if n = 0 then xs
	else if n < 0 then let (l1, l2) = split xs ((List.length xs) + n) in l2 @ l1
	else (*n > 0*) let (l1, l2) = split xs n in l2 @ l1;;


rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
rotate [] 0;;
rotate [0;1;2] 0;;
rotate [0;1;2] 1;;
rotate [0;1;2] 2;;