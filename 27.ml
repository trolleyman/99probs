(*Group the elements of a set into disjoint subsets. (medium)

In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.
Generalize the above function in a way that we can specify a list of group sizes and the function will return a list of groups.*)

(*
	# group ["a";"b";"c";"d"] [2;1];;
	- : string list list list =
	[[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
	 [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
	 [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
	 [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]]
*)

let group xs ns =
	let extract_remove n xs =
		let rec each base remacc acc xs n = match xs with
			| [] -> acc
			| x :: xs ->
				if n = 1 then each base      (x::remacc) ((x::base, remacc @ xs)::acc) xs n
				else          each (x::base) remacc      (each base (x::remacc) acc xs n) xs (n-1) in
		List.map (fun y -> (List.rev (fst y), snd y)) (each [] [] [] xs n) in
	let rec prepend_all acc base news = match news with
		| [] -> acc
		| (nbase, ntodo) :: news -> prepend_all ((base @ [nbase], ntodo)::acc) base news in
	let rec expand acc xs n = match xs with
		| [] -> acc
		| (base, todo) :: xs ->
			let news = extract_remove n todo in
			expand (prepend_all acc base news) xs n in
	let rec aux acc ns = match ns with
		| [] -> acc
		| n :: ns -> aux (expand [] acc n) ns in
	List.fold_left (fun l x -> let (r, _) = x in r::l) [] (aux [([], xs)] ns);;

group ["a";"b";"c";"d"] [2;2];;

(*
abcd

takes ([[a;b]; [c]], [d])

([[a;b]; [c]], todo)
[[a;b]; [d]]
[[a;c]; [b]]
[[a;c]; [d]]
ad b
ad c


[([], [a;b;c;d])]
(* 2 *)
[([[a;d]], [c;b]); ([[a;c]], [b;d]);
 ([[a;b]], [c;d]); ([[b;d]], [c;a]);
 ([[b;c]], [a;d]); ([[c;d]], [b;a])]
(* 1 *)
[([[a;d];[c]], [b]); ([[a;c];b], [d]);
 ([[a;b];[c]], [d]); ([[b;d];c], [a]);
 ([[b;c];[a]], [d]); ([[c;d];b], [a]);
 ([[a;d];[b]], [c]); ([[a;c];d], [b]);
 ([[a;b];[d]], [c]); ([[b;d];a], [c]);
 ([[b;c];[d]], [a]); ([[c;d];a], [b])]*)