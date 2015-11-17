(*Determine the prime factors of a given positive integer (2). (medium)

Construct a list containing the prime factors and their multiplicity. Hint: The problem is similar to problem Run-length encoding of a list (direct solution).*)

(*# factors 315;;
- : (int * int) list = [(3, 2); (5, 1); (7, 1)]*)

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

factors 315;;
factors 10000;;