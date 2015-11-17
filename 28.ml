(*Sorting a list of lists according to length of sublists. (medium)

1. We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

2. Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.*)

(*
	# length_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
	                ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ];;
	- : string list list =
	[["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; ["f"; "g"; "h"];
	 ["i"; "j"; "k"; "l"]]
	# frequency_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
	                   ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ];;
	- : string list list =
	[["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["d"; "e"];
	 ["d"; "e"]; ["m"; "n"]]
*)

let length_sort xs =
	let lengthify xs =
		let rec aux acc = function
			| [] -> List.rev acc
			| x :: xs -> aux ((List.length x, x)::acc) xs in
		aux [] xs in
	let delengthify xs =
		let rec aux acc = function
			| [] -> List.rev acc
			| (_, x) :: xs -> aux (x::acc) xs in
		aux [] xs in
	delengthify (List.sort (fun (x, _) (y, _) -> x - y) (lengthify xs));;

length_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"]; ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ];;

let frequency_sort xs =
	let lengthify xs =
		let rec aux acc = function
			| [] -> List.rev acc
			| x :: xs -> aux ((List.length x, x)::acc) xs in
		aux [] xs in
	let delengthify xs =
		let rec aux acc = function
			| [] -> List.rev acc
			| (_, x) :: xs -> aux (x::acc) xs in
		aux [] xs in
	let count xs =
		let rec add_to acc y xs = match xs with
			| [] -> (y, 1)::acc
			| (x, n) :: xs when x = y -> ((x, n+1)::acc) @ xs
			| x :: xs                 -> add_to (x::acc) y xs in
		let rec aux acc xs = match xs with
			| [] -> acc
			| (x, _) :: xs -> aux (add_to [] x acc) xs in
		aux [] xs in
	let rec find y xs = match xs with
		| [] -> failwith "find"
		| (x, f) :: xs when x = y -> f
		| (x, _) :: xs            -> find y xs in
	let len = lengthify xs in
	let cnt = count len in
	delengthify (List.sort (fun (x, _) (y, _) -> (find x cnt) - (find y cnt)) len);;

frequency_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"]; ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ];;