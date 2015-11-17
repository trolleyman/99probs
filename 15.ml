(* Replicate the elements of a list a given number of times. (medium) *)

(*
	# replicate ["a";"b";"c"] 3;;
	- : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
*)

let rec replicate xs n =
	let rec rep n x =
		if n < 0 then failwith "rep"
		else if n = 0 then []
		else if n = 1 then x :: []
		else x :: rep (n-1) x
	in match xs with
		| [] -> []
		| x :: xs -> (rep n x) @ (replicate xs n);;

replicate ["a";"b";"c"] 3;;
