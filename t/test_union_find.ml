open OUnit2
open Union_find

let test =
  "uf" >::
  (fun _ ->
     let tbl = init 10 in

     assert_equal false (same tbl 0 1);
     assert_equal false (same tbl 1 2);
     assert_equal true (same tbl 1 1);

     union tbl 1 2;
     union tbl 2 3;

     assert_equal true (same tbl 1 2);
     assert_equal true (same tbl 2 3);
     assert_equal true (same tbl 3 1);
     assert_equal false (same tbl 3 5))

let test_many =
  "uf_many" >::
  (fun _ ->
     let tbl = init 1_000_000 in
     for i = 0 to 1_000_000 - 2 do
       union tbl i (i + 1)
     done;
     for i = 0 to 1_000_000 - 2 do
       assert_equal true (same tbl i (i + 1))
     done)

let suite =
  "union_find" >:::
  [ test
  ; test_many
  ]

