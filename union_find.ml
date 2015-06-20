
module Union_find = struct
  let init size = Array.init size (fun i -> i)

  let rec find tbl x =
    if tbl.(x) = x
    then x
    else
      let y = find tbl tbl.(x) in
      tbl.(x) <- y;
      y

  let same tbl x y = find tbl x = find tbl y

  let union tbl x y =
    let px = find tbl x
    and py = find tbl y
    in
    if px <> py then tbl.(px) <- py
end
