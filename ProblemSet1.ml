let rec is_mon_inc (x:int list) : bool =
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
	





