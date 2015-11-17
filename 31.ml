(* Determine whether two positive integer numbers are coprime. (easy)

Two numbers are coprime if their greatest common divisor equals 1. *)
(*
	# coprime 13 27;;
	- : bool = true
	# not (coprime 20536 7826);;
	- : bool = true
*)

let coprime a b =
	let rec gcd a = function
		| 0 -> a
		| b -> gcd b (a mod b) in
	(gcd a b) = 1;;

coprime 13 27;;
coprime 20536 7826;;