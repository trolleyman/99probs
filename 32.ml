(*Calculate Euler's totient function φ(m). (medium)

Euler's so-called totient function φ(m) is defined as the number of positive integers r (1 ≤ r < m) that are coprime to m. We let φ(1) = 1.

Find out what the value of φ(m) is if m is a prime number. Euler's totient function plays an important role in one of the most widely used public key cryptography methods (RSA). In this exercise you should use the most primitive method to calculate this function (there are smarter ways that we shall discuss later).*)
(*
	# phi 10;;
	- : int = 4
	# phi 13;;
	- : int = 12
*)

let phi =
	let count_if_true f x0 x1 =
		let rec aux acc f x0 x1 = match x0 with
			| x0 when x0 >= x1 -> acc
			| x0 when f x0 -> aux (acc+1) f (x0+1) x1
			| x0           -> aux acc     f (x0+1) x1 in
		aux 0 f x0 x1 in
	let coprime a b =
		let rec gcd a = function
			| 0 -> a
			| b -> gcd b (a mod b) in
		(gcd a b) = 1 in
	function
		| m when m < 1 -> failwith "phi"
		| 1 -> 1
		| m -> count_if_true (coprime m) 1 m;;

phi 10;;
phi 13;;