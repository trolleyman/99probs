(*Compare the two methods of calculating Euler's totient function. (easy)

Use the solutions of problems "Calculate Euler's totient function φ(m)" and "Calculate Euler's totient function φ(m) (improved)" to compare the algorithms. Take the number of logical inferences as a measure for efficiency. Try to calculate φ(10090) as an example.*)
(*
	# timeit phi 10090;;
	- : float = 0.00364589691162109375
	# timeit phi_improved 10090;;
	- : float = 5.41210174560546875e-05
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

let timeit f x =
	let t0 = Sys.time() in
	let _ = f x in
	let t1 = Sys.time() in
	t1 -. t0;;

timeit phi 5;;
timeit phi_improved 5;;

timeit phi 100000;;
timeit phi_improved 100000;;

timeit phi 10000000;;
timeit phi_improved 10000000;;