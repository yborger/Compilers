(** This file should be used to write your tests of your other code. *)

open Batteries;;

open OUnit2;;

open TestUtils;;

let all_tests =
  [
    test_success "test_code/4.bird" "4";
    test_success "test_code/after_4.bird" "5";
    test_success "test_code/test_ELet_1.bird" "2";
    test_success "test_code/BinOp_test_1.bird" "39";
    test_success "test_code/BinOp_simple_test_1.bird" "3";
    test_success "test_code/BinOp_simple_test_2.bird" "7";
    test_success "test_code/BinOp_test_3.bird" "44";
    test_success "test_code/ELet_test_2.bird" "10";
    test_success "test_code/UnaryOp_test_1.bird" "8";
    test_success "test_code/UnaryOp_arith_test_1.bird" "96";
    test_success "test_code/arithmetic.bird" "-9";
    test_success "test_code/minus3.bird" "2";
    test_success "test_code/times2.bird" "15";
  ];;

let suite = "compiler test suite" >::: all_tests;;

run_test_tt_main suite;;
