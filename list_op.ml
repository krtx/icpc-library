let init n f =
  assert (n >= 0);
  let rec loop acc = function
    | 0 -> acc
    | n -> loop (f (n - 1) :: acc) (n - 1)
  in
  loop [] n

let map f l = List.rev (List.rev_map f l)

let map2 f l1 l2 = List.rev (List.rev_map2 f l1 l2)

let rev_mapi f l =
  let rec loop count acc = function
    | [] -> acc
    | x :: rest -> loop (count + 1) (f count x :: acc) rest
  in
  loop 0 [] l

let mapi f l = List.rev (rev_mapi f l)

let append l1 l2 =
  List.rev_append (List.rev l1) l2

let fold_right f l init =
  List.fold_left (fun a b -> f b a) init (List.rev l)

let fold_right2 f l1 l2 init =
  List.fold_left2 (fun acc b c -> f b c acc) init (List.rev l1) (List.rev l2)

let combine l1 l2 =
  fold_right2 (fun b c acc -> (b, c) :: acc) l1 l2 []

let split l =
  let rec loop acc1 acc2 = function
    | []             -> (acc1, acc2)
    | (a, b) :: rest -> loop (a :: acc1) (b :: acc2) rest
  in
  loop [] [] (List.rev l)

let concat ls =
  fold_right (fun l acc -> append l acc) ls []
