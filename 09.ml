(* # pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
- : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]] *)

let rec pack ls =
	let rec aux packed acc ls = match ls with
		| [] -> packed
		| x :: [] -> packed @ [x::acc]
		| x :: y :: xs ->
			if x = y then aux packed (x::acc) (y::xs)
			else aux (packed @ [x::acc]) [] (y::xs)
	in aux [] [] ls;;

pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
pack [];;
pack ['a';'a';'b';'b';'b';'a';'c';'c'];;
pack ['a'];;
