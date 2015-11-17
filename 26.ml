(* Generate the combinations of K distinct objects chosen from the N elements of a list. (medium)

In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list. *)
(*
	# extract 2 ["a";"b";"c";"d"];;
	- : string list list =
	[["c"; "d"]; ["b"; "d"]; ["b"; "c"]; ["a"; "d"]; ["a"; "c"]; ["a"; "b"]]
*)

(*let extract n xs =
	let listify xs =
		let rec aux acc xs = match xs with
			| [] -> acc
			| x :: xs -> aux ([x]::acc) xs
		in aux [] xs
	in
	let add e xs =
		let rec aux acc e xs = match xs with
			| [] -> acc
			| x :: xs -> aux ((e::x)::acc) e xs
		in aux [] e xs
	in
	let rec aux acc n xs = match xs with
		| [] -> acc
		| x :: xs ->
			if n >= 0 then aux ((add x (listify xs)) @ acc) (n-1) xs
			else acc
	in aux [] n xs;;*)

let extract n xs =
	let rec each base acc xs n = match xs with
		| [] -> acc
		| x :: xs ->
			if n = 1 then each base      ((x::base)::acc) xs n
			else          each (x::base) (each base acc xs n) xs (n-1)
	in List.map List.rev (each [] [] xs n);;

extract 2 ["a";"b";"c";"d"];;
extract 3 ["a";"b";"c";"d"];;
extract 4 ["a";"b";"c";"d"];;
extract 4 ["a";"b";"c";"d";"e";"f"];;

extract 3 [1;2;3;4;5;6;7;8;9;10;11;12];;

let alphabet = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'];;
