open OUnit2
open Segtree

let test_mrq =
  "mrq" >::
  (fun _ ->
     let tbl = MRQ.init () in

     MRQ.update tbl 0 10;
     MRQ.update tbl 1 20;
     MRQ.update tbl 3 15;
     MRQ.update tbl 4 12;

     assert_equal 10 (MRQ.query tbl 0 4);
     assert_equal 15 (MRQ.query tbl 1 4);
     assert_equal 12 (MRQ.query tbl 1 5);

     MRQ.update tbl 8 2;

     assert_equal 2 (MRQ.query tbl 0 10);

     MRQ.update tbl 6 9;

     assert_equal 9 (MRQ.query tbl 0 7))

let test_mrq2 =
  "mrq" >::
  (fun _ ->
     let tbl = MRQ.init () in

     for i = 0 to 1_000_000 do
       MRQ.update tbl i i
     done;

     assert_equal 0 (MRQ.query tbl 0 1_000_000);

     for i = 0 to 1_000_000 do
       MRQ.update tbl i (i + 100)
     done;

     assert_equal 100 (MRQ.query tbl 0 1_000_000))

let suite =
  "segtree" >:::
  [ test_mrq
  ; test_mrq2
  ]
