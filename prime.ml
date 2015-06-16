
let make_primes ?(size=1_000_000) () : int array * bool array =
  let isp = Array.make size true in
  let tbl =
    Array.make
      (2 * int_of_float (float_of_int size /. log (float_of_int size)))
      0
  in
  isp.(0) <- false;
  isp.(1) <- false;
  let rec set_false i prime =
    if i < size
    then (isp.(i) <- false; set_false (i + prime) prime)
  in
  let rec seive i count =
    if i >= size
    then ()
    else if isp.(i)
    then
      begin
        set_false (2 * i) i;
        tbl.(count) <- i;
        seive (i + 1) (count + 1)
      end
    else seive (i + 1) count
  in
  seive 2 0;
  (tbl, isp)
