open OUnit2
open List_op

let id x = x

let l1_000_000 = init 1_000_000 id

let test_init =
  "init" >::
  (fun _ ->
     assert_equal
       [0;1;2;3;4;5;6;7;8;9]
       (init 10 id);
     assert_equal
       500_000
       (List.nth l1_000_000 500_000))

let test_map =
  "map" >::
  (fun _ ->
     assert_equal
       (500_000 * 2)
       (l1_000_000
        |> map (fun x -> x * 2)
        |> fun l -> List.nth l 500_000))

let test_map2 =
  "map2" >::
  (fun _ ->
     assert_equal
       [5;12;21]
       (map2 (fun x y -> x * y) [1;2;3] [5;6;7]);
     assert_equal
       (500_000 * 3)
       (map2
         (fun x y -> x + y)
         l1_000_000
         (map (fun x -> x * 2) l1_000_000)
        |> fun l -> List.nth l 500_000))

let test_mapi =
  "mapi" >::
  (fun _ ->
     assert_equal
       ["0a";"1b";"2c"]
       (mapi (fun i x -> string_of_int i ^ x) ["a";"b";"c"]);
     assert_equal
       (500_000 * 2)
       (mapi (fun i x -> i + x) l1_000_000
        |> fun l -> List.nth l 500_000))

let test_append =
  "append" >::
  (fun _ ->
     assert_equal
       [1;2;3;5;6;7]
       (append [1;2;3] [5;6;7]);
     assert_equal
       (1_000_000 * 2)
       (append l1_000_000 l1_000_000 |> List.length))

let test_fold_right =
  "fold_right" >::
  (fun _ ->
     assert_equal
       [1;2;3]
       (fold_right (fun a acc -> a :: acc) [1;2;3] []);
     assert_equal
       1_000_000
       (fold_right (fun a acc -> a :: acc) l1_000_000 [] |> List.length))

let test_fold_right2 =
  "fold_right2" >::
  (fun _ ->
     assert_equal
       [1,"a";2,"b";3,"c"]
       (fold_right2 (fun a b acc -> (a, b) :: acc) [1;2;3] ["a";"b";"c"] []);
     assert_equal
       1_000_000
       (fold_right2 (fun a b acc -> (a, b) :: acc) l1_000_000 l1_000_000 []
        |> List.length))

let test_combine =
  "combine" >::
  (fun _ ->
     assert_equal
       [1,"a";2,"b";3,"c"]
       (combine [1;2;3] ["a";"b";"c"]))

let test_split =
  "split" >::
  (fun _ ->
     assert_equal
       ([1;2;3], ["a";"b";"c"])
       (split [1,"a";2,"b";3,"c"]))

let test_concat =
  "concat" >::
  (fun _ ->
     assert_equal
       [1;2;3;2;3;4;5;6;7]
       (concat [[1;2;3];[2;3;4];[5;6;7]]);
     assert_equal
       (1_000_000 * 2)
       (concat [l1_000_000; l1_000_000] |> List.length))

let suite =
  "list_op" >:::
  [ test_init
  ; test_map
  ; test_map2
  ; test_mapi
  ; test_append
  ; test_fold_right
  ; test_fold_right2
  ; test_combine
  ; test_split
  ; test_concat
  ]
          
