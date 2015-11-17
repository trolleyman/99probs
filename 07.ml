type 'a node =
    | One of 'a 
    | Many of 'a node list;;

let rec flatten_node nd = match nd with
		| One x -> [x]
		| Many xs -> flatten_nodes xs
	and flatten_nodes xs = match xs with
		| [] -> []
		| x :: xs -> (flatten_node x) @ (flatten_nodes xs);;

let flatten = flatten_nodes;

flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
