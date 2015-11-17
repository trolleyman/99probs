(* Extract a slice from a list. (medium) *)
(* Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 0 (this is the way the List module numbers elements). *)

(*
	# slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
	- : string list = ["c"; "d"; "e"; "f"; "g"]
*)

let slice xs i k =
	let rec aux acc proc i k = match proc with
		| [] -> if not (i = 0) && not (k = 0) then failwith "slice" else acc
		| x :: xs ->
			if i <= 0 && k > 0 then aux (x::acc) xs (i-1) (k-1)
			else if k = 0 then (x::acc)
			else (*if i > 0 then*) aux acc xs (i-1) (k-1)
	in List.rev (aux [] xs i k);;

slice [0;1;2] 0 1;;
slice [0;1;2] 0 2;;
slice [0;1;2;3;4;5;6;7;8;9] 2 6;;
slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 1 1;;
