
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


let rec unflatten (k : int) (lst : 'a list) : 'a list list option' = 
	let rec take_apart (k : int) (lst : 'a list) (lst_acc : 'a list list) : 'a list list option = 
		if (k <=0) then None else
		match lst with
		| [] -> Some []
		| h::t -> take_apart (b-1) 'lst (a::lst_acc)
in take_apart k lst []









let unflatten (k : int) (lst : 'a list) : 'a list list option = 
	if (k <=0) then None else 
	let rec take_apart (b : int) (lst' : 'a list) (lst_acc : 'a list list) (temp_lst : 'a list): 'a list list =  
		match lst with 
		| [] -> begin match temp_lst with 
					  | _::_ -> temp_lst::lst_acc 
					  | [] -> lst_acc
				end 
		| h::t -> if b = 1 
					then let new_temp = h::temp_lst in take_apart k t (new_temp@lst')
					else take_apart (b-1) t (lst_acc (h::temp_lst)) in Some (take_apart k lst)


(*6. given a list of Roman numerals as inputs, int_of_roman returns the integer
*value of the Roman numerals *)

type numeral = I | V| X | L | C | D | M 
type roman = numeral list
let rec int_of_roman (r: roman) : int = 
	let int_of_numeral = function
	  	| I -> 1
	  	| V -> 5
	  	| X -> 10
	  	| L -> 50
	  	| C -> 100
	  	| D -> 500
	  	| M -> 1000 in 

	let rec roman_tolist (r: roman): int list =
		match r with
		|[] -> []
		|h::t -> int_of_numeral(h)::roman_tolist(t) in

	let rec sum_ints (r :int list) (x: int) : int= 
		match r with
		|[] -> x
		|h::[] -> (x+h)
		|h1::h2::t -> if h1 < h2 then sum_ints (h2::t) (x-h1) else
		sum_ints (h2::t) (h1+x) 

	in sum_ints (roman_tolist r) 0 
