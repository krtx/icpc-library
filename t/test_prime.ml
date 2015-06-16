
open OUnit2

let test_make_primes =
  let (primes,isp) = Prime.make_primes () in
  "make_primes" >::
  (fun _ ->
     assert_equal 2 primes.(0);
     assert_equal 3 primes.(1);
     assert_equal 5 primes.(2);
     assert_equal 7 primes.(3);
     assert_equal 11 primes.(4);
     assert_equal 13 primes.(5);
     assert_equal 17 primes.(6);
     assert_equal 19 primes.(7);
     assert_equal 541 primes.(99);
     assert_equal 104729 primes.(9999);
     
     assert_equal false isp.(0);
     assert_equal false isp.(1);
     assert_equal true  isp.(2);
     assert_equal true  isp.(3);
     assert_equal false isp.(4);
     assert_equal true  isp.(5);
     assert_equal false isp.(6);
     assert_equal true  isp.(7);
     assert_equal true  isp.(99991);
     assert_equal false isp.(999982);
     assert_equal true  isp.(999983);
     assert_equal false isp.(999984))

let suite =
  "prime" >:::
  [ test_make_primes
  ]
