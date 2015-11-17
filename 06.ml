(* Find out whether a list is a palindrome. *)

let is_palindrome xs =
	List.rev xs = xs;;

is_palindrome [0;1;0];;
is_palindrome [0;1;1;0];;
is_palindrome [0];;
is_palindrome [];;
is_palindrome [0;1];;
is_palindrome [0;4;1;4];;
