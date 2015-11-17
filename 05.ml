(* Reverse a list. *)

let rec rev ls = match ls with
	| [] -> []
	| x :: xs -> (rev xs) @ [x];;

rev [0;1;2];;
rev [0;1;2;3;4;5];;
rev [];;
