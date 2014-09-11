
(*let rec is_mon_inc (x:int list) : bool =
	match x with 
	| [] -> true
	| [_] -> true
	| h::t -> if h <= List.hd(t) then is_mon_inc t else false;;

let rec is_unimodel (x:int list) : bool = 
	match x with
	| [] -> true
	| [_] -> true
	| h::t -> if h <= List.hd(t) then is_unimodel t else is_mon_inc(List.rev(t));;

let rec powerset (x:int list) :  int list list =
	match x with 
	| [] -> [[]]
	| [y] -> [[y]]
	| h::t -> let rec f (head:int) (xx:int list list) : int list list  = 
			      match xx with
			      | [] -> [[]]
			      | hd::tl -> List.append ([head]::[hd]) (f head tl)
			  in f h (powerset(t))

let rec rev_int (x:int) : int =
	match x with
	| 
	


let rec unflatten (k: int) (lst: list) : list list = 
	if k <= 0 then None else 
	let rec take_apart count acc rem lst_acc = 
		match rem with Some
		[] - > lst_acc @ acc
		| h::t -> if count = k then helper 0
		[] rem (lst_acc@acc) else helper(count + 1) (acc@h) t list_acc*)





(* 4. The function rev_int takes an integer first_num and returns an
*integer whose digits are the reverse of first_num. The sign remains
*unchanged.*)

let rev_int (first_num : int) : int = 
	let rec into_list (number: int) : int list =
		match number with
		| 0 -> []
		| _ -> ((number mod 10) :: []) @ into_list (number/10) in 
	let rec list_to_string (lst:int list) : string =
		match lst with 
		| [] -> ""
		| h::t -> string_of_int(h) ^ list_to_string t
	in int_of_string(list_to_string(into_list(first_num)));;