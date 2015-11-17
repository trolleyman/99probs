let rec at n xs = match xs with
          | [] -> None
          | x :: xs -> if n < 0 then None else if n == 0 then Some x else at (n-1) xs;;
val at : int -> 'a list -> 'a option = <fun>
at 2 ['a'];;
at 1 ['a'];;
at 0 ['a'];;
at (-1) ['a'];;
at 2 ['a', 'b', 'c'];;

# Tail-recursive version
let length list =
    let rec aux n = function
      | [] -> n
      | _::t -> a
    in aux 0 list;;
