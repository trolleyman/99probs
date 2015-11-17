let rec last xs = match xs with
        | [] -> None
        | x :: [] -> Some (x)
        | _ :: xs -> last xs;;

last ['a'; 'b'; 'c'];;
last [];;
