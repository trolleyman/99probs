(* Drop every N'th element from a list. (medium) *)

let rec drop xs n =
	let rec aux acc i = match acc with
		| [] -> []
		| x :: xs ->
			if i = 1 then aux xs n
			else x :: aux xs (i-1)
	in aux xs n;;

drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
drop [0;1;2;3;4;5;6;7;8;9] 2;;
