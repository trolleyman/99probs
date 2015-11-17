(*Determine the greatest common divisor of two positive integer numbers. (medium)

Use Euclid's algorithm.*)
(*
	function gcd(a, b)
		if b = 0
			return a; 
		else
			return gcd(b, a mod b); 
*)

(*
	# gcd 13 27;;
	- : int = 1
	# gcd 20536 7826;;
	- : int = 2
*)

let rec gcd a = function
	| 0 -> a
	| b -> gcd b (a mod b);;

gcd 13 27;;
gcd 20536 7826;;
gcd 54 12;; (* = 6 *)
