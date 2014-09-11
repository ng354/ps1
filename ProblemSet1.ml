
(*1. Function is_mon_inc takes an integer list and returns whether or not
*that list is monotonically increasing (never decreasing)*)

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
              | hd::tl -> if h <= hd then is_unimodel t 
                          else is_mon_inc(List.rev(t))


(*3. Function powerset takes in a list of ints representing a set of numbers
*and returns a list of int lists representing all subsets of the original 
*set of numbers*)

let powerset (lst : int list) : int list list =
    let rec iterator (lst1 : int list) (tmp : int list list) = 
        match lst1 with
        | [] -> tmp
        | h::t -> iterator t ((List.map (fun x -> [h] @ x) tmp) @ tmp)
    in iterator lst [[]];; 


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
    in if first_num < 0 then 
          -1 * int_of_string(list_to_string(into_list(abs(first_num)))) 
       else 
          int_of_string(list_to_string(into_list(first_num)))



(*5. Function unflatten takes an integer k and a list lst and breaks it up 
*into a list of lists that are of size k. If the length of lst is not a 
*multiple of k, the last list is allowed to be a size less than k. 
*If k<=0 then return None.*)

(*let rec unflatten (k: int) (lst: list) : list list = 
	if k <= 0 then None else 
	let rec take_apart count acc rem lst_acc = 
		match rem with Some
		[] - > lst_acc @ acc
		| h::t -> if count = k then helper 0
		[] rem (lst_acc@acc) else helper(count + 1) (acc@h) t list_acc*)