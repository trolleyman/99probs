(*Huffman code (hard)

First of all, consult a good book on discrete mathematics or algorithms for a detailed description of Huffman codes (you can start with the Wikipedia page)!

We consider a set of symbols with their frequencies. For example, if the alphabet is "a",..., "f" (represented as the positions 0,...5) and respective frequencies are 45, 13, 12, 16, 9, 5:

# let fs = [ ("a", 45); ("b", 13); ("c", 12); ("d", 16);
             ("e", 9); ("f", 5) ];;
val fs : (string * int) list =
  [("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5)]
Our objective is to construct the Huffman code c word for all symbols s. In our example, the result could be hs = [("a", "0"); ("b", "101"); ("c", "100"); ("d", "111"); ("e", "1101"); ("f", "1100")] (or hs = [ ("a", "1");...]). The task shall be performed by the function huffman defined as follows: huffman(fs) returns the Huffman code table for the frequency table fs *)
(*
	# huffman fs;;
	- : (string * string) list =
	[("a", "0"); ("c", "100"); ("b", "101"); ("f", "1100"); ("e", "1101");
	 ("d", "111")]
	
	# huffman ["a", 10;  "b", 15;  "c", 30;  "d", 16;  "e", 29];;
	- : (string * string) list =
	[("d", "00"); ("a", "010"); ("b", "011"); ("e", "10"); ("c", "11")]
*)

let fs = [ ("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5) ];;

type 'a node =
	| Leaf of int * 'a node * 'a node
	| Node of 'a * int;;

let huffman fs =
	let freq = function
		| Leaf (f, _, _) -> f
		| Node (_, f) -> f in
	let remove_first_two xs = match xs with
		| x :: y :: xs -> (x, y, xs)
		| _ -> failwith "remove_first_two" in
	let combine a b = Leaf ((freq a) + (freq b), a, b) in
	let tree fs =
		let rec aux = function
			| [] -> failwith "huffman"
			| n :: [] -> n
			| n :: ns ->
				let ns = List.sort (fun x y -> (freq x) - (freq y)) (n::ns) in
				let (a, b, ns) = remove_first_two ns in
				aux ((combine a b)::ns) in
		aux (List.map (fun (x, f) -> Node (x, f)) fs) in
	let expand_tree t =
		let rec aux acc base = function
			| Leaf (_, ln, rn) -> aux (aux acc (base ^ "1") rn) (base ^ "0") ln
			| Node (c, _)      -> (c, base)::acc in
		aux [] "" t in
	expand_tree (tree fs);;

huffman fs;;
huffman ["a", 10;  "b", 15;  "c", 30;  "d", 16;  "e", 29];;
