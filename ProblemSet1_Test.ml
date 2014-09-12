open ProblemSet1
open Assertions

TEST_UNIT "test_is_mon_inc1" = assert_true (is_mon_inc [1;2;3])
TEST_UNIT "test_is_mon_inc2" = assert_true (is_mon_inc [1;1;1])
TEST_UNIT "test_is_mon_inc3" = assert_true (is_mon_inc [1])
TEST_UNIT "test_is_mon_inc4" = assert_true (is_mon_inc [])
TEST_UNIT "test_is_mon_inc4" = assert_false (is_mon_inc [5;4])
TEST_UNIT "test_is_mon_inc4" = assert_false (is_mon_inc [1;3;6;4])

TEST_UNIT "test_is_unimodel1" = assert_true (is_unimodel [1;2;3])
TEST_UNIT "test_is_unimodel2" = assert_true (is_unimodel [])
TEST_UNIT "test_is_unimodel3" = assert_true (is_unimodel [1;2;3;2;1])
TEST_UNIT "test_is_unimodel3" = assert_true (is_unimodel [1;2;3;3;3;2])
TEST_UNIT "test_is_unimodel3" = assert_true (is_unimodel [1;1;1])
TEST_UNIT "test_is_unimodel3" = assert_true (is_unimodel [4;1])
TEST_UNIT "test_is_unimodel3" = assert_false (is_unimodel [1;2;3;2;3])
TEST_UNIT "test_is_unimodel3" = assert_false (is_unimodel [4;3;5])


TEST_UNIT "test_powerset1" = assert_true (powerset [1;2;3] =
 [[1];[2];[3];[1;2];[1;3];[2;3];[1;2;3]])

TEST_UNIT  "test_rev_int1" = assert_true (rev_int 123= 321)
TEST_UNIT  "test_rev_int2" = assert_true (rev_int 123678 = 876321)
TEST_UNIT  "test_rev_int3" = assert_true (rev_int 0  = 0)
TEST_UNIT  "test_rev_int4" = assert_true (rev_int 5  = 5)
TEST_UNIT  "test_rev_int5" = assert_true (rev_int(-1234)  = -4321)
TEST_UNIT  "test_rev_int6" = assert_true (rev_int(-1)  = -1)


TEST_UNIT  "int_of_roman1" = assert_true (int_of_roman [I;I;I]= 3)
TEST_UNIT  "int_of_roman2" = assert_true (int_of_roman [I;V]= 4)
TEST_UNIT  "int_of_roman3" = assert_true (int_of_roman [M;C;M;X;C;I;X]= 1999)
TEST_UNIT  "int_of_roman4" = assert_false (int_of_roman [I;X]= 11)






let () = Pa_ounit_lib.Runtime.summarize()

