(* Decode a run-length encoded list. (medium)

Given a run-length code list generated as specified in the previous problem, construct its uncompressed version.*)

type 'a rle =
	| One of 'a
	| Many of int * 'a

let decode xs =
	let rec rep n x = match n with
		| 0 -> []
		| 1 -> [x]
		| _ -> x :: rep (n-1) x
	in let rec aux acc = match acc with
		| [] -> []
		| x :: xs -> match x with
			| One c -> c :: (aux xs)
			| Many (n, c) -> (rep n c) @ (aux xs)
	in aux xs;;

decode [Many (4, 'a'); One 'b'; Many (12, 'e'); One 'f'];;
decode [One '2'];;
decode [Many (3, '3')];;
decode [];;
