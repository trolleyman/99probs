let rec last_two xs = match xs with
	| []  -> None
	| [_] -> None
	| [x; y] -> Some (x, y)
	| _ :: xs -> last_two xs;;

last_two ['a'];;
last_two ['a';'b'];;
last_two ['a';'b';'c'];;
