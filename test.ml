open OUnit2

let _ =
  "Test" >:::
  [ Test_list_op.suite
  ; Test_prime.suite
  ; Test_segtree.suite ]
  |> run_test_tt_main
