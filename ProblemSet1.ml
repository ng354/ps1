
(*1. Function is_mon_inc takes an integer list and returns whether or not
*that list is monotonically increasing.*)
let rec is_mon_inc (x:int list) : bool =
	match x with 
	| [] -> true
	| h::t -> match t with
			  | [] -> true
			  | hd::tl -> if h <= hd then is_mon_inc t else false

	

(*2. Function is_unimodel takes an integer list and returns whether that list
*monotonically increases to some maximum value and then monotonically decreases
*after that value. Either or both segments can be empty.*)
let rec is_unimodel (x:int list) : bool = 
	match x with
	| [] -> true
	| h::t -> match t with 
			  | [] -> true
			  | hd::tl -> if h <= hd then is_unimodel t else  is_mon_inc(List.rev(t))


	

(*3. Function powerset takes a set represented as a list x and returns the powerset
*of that set. The powerset of a set S is the set of all subsets of S.*)
let rec powerset (x:int list) :  int list list =
	match x with 
	| [] -> [[]]
	| [y] -> [[y]]
	| h::t -> let rec f (head:int) (xx:int list list) : int list list  = 
			      match xx with
			      | [] -> [[]]
			      | hd::tl -> [head]::[hd] @ (f head tl)
			  in f h (powerset(t))

let powerset1 (lst :int list) : int list list =
	let rec temp_iterator (head : int) (tail : int list) (tmp1 : int list list) : int list list =
		match tmp1 with
		| [] -> [head]::tmp1
		| hd::tl -> if tl = [] 
		then [head::hd] 
		else [head::hd] @ (temp_iterator head tail tl) in
	let rec powerset_helper (lst : int list) (tmp : int list list) : int list list =
		match lst with
		| [] -> [[]]
		| h::t -> powerset_helper t (temp_iterator h t tmp)
		in powerset_helper 

(*if tl = [] then powerset_helper tail ([head::hd] @ tmp1)  
						else powerset_helper tail ([head::hd] @ (f head tail lst1 tl))*)


(* 4. The function rev_int takes an integer first_num and returns an
*integer whose digits are the reverse of first_num. The sign remains
*unchanged. If rev_int x is larger than max_int, the behavior is undefined.*)

let rev_int (first_num : int) : int = 
	if first_num = 0 then 0 else

	let rec into_list (number: int) : int list =
		match number with
		| 0 -> []
		| _ -> ((number mod 10) :: []) @ into_list (number/10) in 
	let rec list_to_string (lst:int list) : string =
		match lst with 
		| [] -> ""
		| h::t -> string_of_int(h) ^ list_to_string t
	in if first_num < 0 then -1 * int_of_string(list_to_string(into_list(abs(first_num)))) else int_of_string(list_to_string(into_list(first_num)))

(*let rev_int1 (num : int) : int =
	let rec accum (x : int) (acc : int) : int =
		if (x > -10 && x < 10) then x else
		if acc = 0 then accum (x/10) ((x mod 10)*10) else
			(acc + x mod 10)*10 + accum (x/10) () 
	in accum num 0

	match num with
	| 0 -> acc
	| x -> if check then (x mod 10)*10 + rev_int1 x/10  




	if (num = 0) then acc else
		let f double = acc2 in f (acc*10);;
		if check then  rev_int1 (num/10) ((num mod 10)*10) (false) else 
			rev_int1 (num/10) ((acc2 + num mod 10)*10) (false)*)


(*5. Function unflatten takes an integer k and a list lst and breaks it up into a
*list of lists that are of size k. If the length of lst is not a multiple of k, the
*last list is allowed to be a size less than k. If k<=0 then return None.*)
(*let rec unflatten (k: int) (lst: list) : list list = 
	if k <= 0 then None else 
	let rec take_apart count acc rem lst_acc = 
		match rem with Some
		[] - > lst_acc @ acc
		| h::t -> if count = k then helper 0
		[] rem (lst_acc@acc) else helper(count + 1) (acc@h) t list_acc*)