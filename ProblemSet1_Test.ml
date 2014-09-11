open ProblemSet1
open Assertions

(*TEST_UNIT "test_is_mon_inc1" = assert_true (is_mon_inc [1;2;3])
TEST_UNIT "test_is_mon_inc2" = assert_true (is_mon_inc [1;1;1])
TEST_UNIT "test_is_mon_inc3" = assert_true (is_mon_inc [1])
TEST_UNIT "test_is_mon_inc4" = assert_true (is_mon_inc [])

TEST_UNIT "test_is_unimodel1" = assert_true (is_unimodel [1;2;3])
TEST_UNIT "test_is_unimodel2" = assert_true (is_unimodel [])
TEST_UNIT "test_is_unimodel3" = assert_true (is_unimodel [1;2;3;2;1])*)

TEST_UNIT "test_powerset1" = assert_true (powerset [1;2;3] =
 [[1];[2];[3];[1;2];[1;3];[2;3];[1;2;3]])

(*let rec print_list_list x = 
	let rec print_list y = 
		match y with
		| [] -> ()
		| e::l -> print_int e ; print_string " " ; print_list l
	match x with
	| [] -> ()
	| h::t -> print_list h ; print_string " " ; print_list_list t
in print_list_list [[2];[3]]*)



let () = Pa_ounit_lib.Runtime.summarize()

