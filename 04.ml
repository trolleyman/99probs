let rec len xs = match xs with
        | [] -> 0
        | _ :: xs -> 1 + len xs;;

len [0;1;2];;
len [];;
len [0;1;2;3;4;5;6;7;8;9];;
