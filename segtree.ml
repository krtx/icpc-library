
module type Element = sig
  type t                        (* type of stored value *)
  type raw                      (* type of raw value *)

  val max_value : t
  val min       : t -> t -> t
  val of_raw    : int -> raw -> t
  val to_raw    : t -> raw
end

module SegTree(F : Element) = struct
  let size = 2_097_152 (* 1 << 21, table size is allowed upto 10^6 *)

  let init () = Array.make size F.max_value

  let update tbl idx value =
    let rec loc i =
      let i = (i - 1) / 2 in
      tbl.(i) <- F.min tbl.(i * 2 + 1) tbl.(i * 2 + 2);
      if i > 0 then loc i
    in
    let i' = (size / 2) + idx - 1 in
    tbl.(i') <- F.of_raw idx value;
    loc i'

  (* [a, b) *)
  let query tbl a b =
    let rec loc k l r =
      if r <= a || b <= l
      then F.max_value
      else if a <= l && r <= b
      then tbl.(k)
      else
        let vl = loc (k * 2 + 1) l ((l + r) / 2)
        and vr = loc (k * 2 + 2) ((l + r) / 2) r
        in F.min vl vr
    in
    F.to_raw (loc 0 0 (size / 2))
end

(* Minimum Range Query *)
module MRQ = SegTree(struct
    type t = int * int
    type raw = int

    let max_value = (max_int, 0)
    let min = min
    let of_raw i x = (x, i)
    let to_raw = fst
  end)
