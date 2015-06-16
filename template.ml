
open Num
let ni = num_of_int

let (|>) x f = f x
let ($) f g = fun x -> f (g x)

(* read integers in the same line *)
let read_ints ?(sep=" ") () =
  read_line ()
  |> Str.split (Str.regexp sep)
  |> List.map int_of_string
