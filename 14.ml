(* Duplicate the elements of a list. (easy) *)

(*
	# duplicate ["a";"b";"c";"c";"d"];;
	- : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
*)

let rec duplicate = function
	| [] -> []
	| x :: xs -> x :: x :: duplicate xs;;

duplicate ['a'];;
duplicate ['a'; 'b'; 'c'];;
duplicate ["a";"b";"c";"c";"d"];;
