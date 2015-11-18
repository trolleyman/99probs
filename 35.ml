(*Calculate Euler's totient function φ(m) (improved). (medium)

See problem "Calculate Euler's totient function φ(m)" for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of the previous problem then the function phi(m) can be efficiently calculated as follows: Let [(p1, m1); (p2, m2); (p3, m3); ...] be the list of prime factors (and their multiplicities) of a given number m. Then φ(m) can be calculated with the following formula:

φ(m) = (p1 - 1) × p1^(m1 - 1) × (p2 - 1) × p2^(m2 - 1) × (p3 - 1) × p3^(m3 - 1) × ⋯*)
(*
	# phi_improved 10;;
	- : int = 4
	# phi_improved 13;;
	- : int = 12
*)

let sieve n =
	let arr = Array.make (n+1) true in
	Array.set arr 0 false;
	Array.set arr 1 false;
	let rec mult_to_false arr x0 m n =
		if x0 > n then ()
		else (Array.set arr x0 false; mult_to_false arr (x0+m) m n) in
	let rec aux arr x n m =
		if x > n then ()
		else if Array.get arr x then (mult_to_false arr (x*2) x m; aux arr (x+1) n m)
		else aux arr (x+1) n m in
	aux arr 2 (n/2) n;
	arr;;

let primes_from_sieve s =
	List.rev (snd (Array.fold_left (fun (n, l) x -> if x then (n+1, n::l) else (n+1, l)) (0, []) s));;

let primes n =
	let s = sieve n in
	primes_from_sieve s;;

let factors n =
	let rle xs = match xs with
		| [] -> []
		| x :: xs ->
			let rec aux acc y n xs = match xs with
				| [] -> List.rev ((y,n)::acc)
				| x :: xs ->
					if x = y then aux acc           y (n+1) xs
					else          aux ((y, n)::acc) x 1     xs in
			aux [] x 1 xs in
	let is_prime s n = Array.get s n in
	let rec find_divisor ps n = match ps with
			| [] -> 1
			| p :: ps when n mod p = 0 -> p
			| p :: ps                  -> find_divisor ps n in
	let rec aux s ps acc n =
		if n = 1 then acc
		else if is_prime s n then n::acc
		else (let div = find_divisor ps n in
			if div = 1 then failwith "???"
			else aux s ps (div::acc) (n/div)
		) in
	let s = sieve (n+1/2) in
	let ps = primes_from_sieve s in
	rle (List.rev (aux s ps [] n));;

let phi_improved n =
	let rec pow a = function
		| 0 -> 1
		| 1 -> a
		| n -> 
			let b = pow a (n / 2) in
			b * b * (if n mod 2 = 0 then 1 else a) in
	let rec aux acc = function
		| [] -> acc
		| (f, m) :: fs -> aux (acc * (f-1) * (pow f (m-1))) fs in
	aux 1 (factors n);;