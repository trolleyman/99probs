(* Gray code. (medium)

An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,

n = 1: C(1) = ['0','1'].
n = 2: C(2) = ['00','01','11','10'].
n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
Find out the construction rules and write a function with the following specification: gray n returns the n-bit Gray code. *)


let gray n =
	let prepend_all x ys =
		let rec aux acc x = function
			| [] -> List.rev acc
			| y :: ys -> aux ((x ^ y)::acc) x ys in
		aux [] x ys in
	let gray_step xs =
		let rev = List.rev xs in
		let r = prepend_all "1" rev in
		let l_rev = prepend_all "0" rev in
		List.rev_append l_rev r in
	let rec aux acc n = match n with
		| 1 -> acc
		| n -> aux (gray_step acc) (n-1) in
	aux ["0"; "1"] n;;

gray 1;;
gray 2;;
gray 3;;
gray 4;;
gray 8;;
