(* Lotto: Draw N different random numbers from the set 1..M. (easy) *)
(* The selected numbers shall be returned in a list. *)
(*
	# lotto_select 6 49;;
	- : int list = [10; 20; 44; 22; 41; 2]
*)

let lotto_select n m =
	let range a b =
		let rec aux a b step =
			if a = b then [a]
			else a :: aux (a+step) b step
		in if a < b then aux a b 1 else aux a b (-1) in
	let rand_select xs n =
		let rec extract xs acc n = match xs with
			| [] -> failwith "rand_select"
			| h :: t -> if n = 0 then (h, acc @ t) else extract t (h :: acc) (n-1)
		in let rec aux xs acc n =
			if n = 0 then acc
			else
				let (x, xs) = extract xs [] (Random.int (List.length xs)) in
				aux xs (x::acc) (n-1)
		in aux xs [] n in
	rand_select (range 1 m) n;;