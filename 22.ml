(* Create a list containing all integers within a given range. (easy)

If first argument is smaller than second, produce a list in decreasing order. *)

(*
	# range 4 9;;
	- : int list = [4; 5; 6; 7; 8; 9]
	# range 9 4;;
	- : int list = [9; 8; 7; 6; 5; 4]
*)

let range a b =
	let rec aux a b step =
		if a = b then [a]
		else a :: aux (a+step) b step
	in if a < b then aux a b 1 else aux a b (-1);;

range 9 4;;
range 4 9;;
