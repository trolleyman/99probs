(* # compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
  : string list = ["a"; "b"; "c"; "a"; "d"; "e"] *)

let rec compress xs = match xs with
	| [] -> []
	| x :: y :: xs -> if x = y then compress (x :: xs) else x :: (compress (y :: xs))
	| x :: [] -> x :: [];;

compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
