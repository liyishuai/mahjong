open Base

module TileType = struct
  type t =
    | M1 | M2 | M3 | M4 | M5 | M6 | M7 | M8 | M9
    | P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8 | P9
    | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9
    | EW | SW | WW | NW
    | WD | GD | RD
  [@@deriving sexp, compare, hash, equal]

  let to_int = function
    | M1 -> 0 | M2 -> 1 | M3 -> 2 | M4 -> 3 | M5 -> 4 | M6 -> 5 | M7 -> 6 | M8 -> 7 | M9 -> 8
    | P1 -> 9 | P2 -> 10 | P3 -> 11 | P4 -> 12 | P5 -> 13 | P6 -> 14 | P7 -> 15 | P8 -> 16 | P9 -> 17
    | S1 -> 18 | S2 -> 19 | S3 -> 20 | S4 -> 21 | S5 -> 22 | S6 -> 23 | S7 -> 24 | S8 -> 25 | S9 -> 26
    | EW -> 27 | SW -> 28 | WW -> 29 | NW -> 30
    | WD -> 31 | GD -> 32 | RD -> 33

  let of_int = function
    | 0 -> Some M1 | 1 -> Some M2 | 2 -> Some M3 | 3 -> Some M4 | 4 -> Some M5 | 5 -> Some M6 | 6 -> Some M7 | 7 -> Some M8 | 8 -> Some M9
    | 9 -> Some P1 | 10 -> Some P2 | 11 -> Some P3 | 12 -> Some P4 | 13 -> Some P5 | 14 -> Some P6 | 15 -> Some P7 | 16 -> Some P8 | 17 -> Some P9
    | 18 -> Some S1 | 19 -> Some S2 | 20 -> Some S3 | 21 -> Some S4 | 22 -> Some S5 | 23 -> Some S6 | 24 -> Some S7 | 25 -> Some S8 | 26 -> Some S9
    | 27 -> Some EW | 28 -> Some SW | 29 -> Some WW | 30 -> Some NW
    | 31 -> Some WD | 32 -> Some GD | 33 -> Some RD
    | _ -> None
end

module T = struct
  type t = int [@@deriving sexp, compare, hash]
end

include T
include Comparable.Make (T)

let of_int i = if i >= 0 && i < 136 then Some i else None
let to_int i = i

let create type_ ~offset =
  assert (offset >= 0 && offset <= 3);
  (TileType.to_int type_ * 4) + offset

let type_ t =
  match TileType.of_int (t / 4) with
  | Some x -> x
  | None -> failwith "Invalid tile"

let offset t = t % 4

let is_red_five t =
  t = 16 || t = 52 || t = 88

let num t =
  let tt = type_ t in
  (TileType.to_int tt) % 9 + 1

let to_string t =
  let suffix = match type_ t with
    | M1 -> "m1" | M2 -> "m2" | M3 -> "m3" | M4 -> "m4" | M5 -> "m5" | M6 -> "m6" | M7 -> "m7" | M8 -> "m8" | M9 -> "m9"
    | P1 -> "p1" | P2 -> "p2" | P3 -> "p3" | P4 -> "p4" | P5 -> "p5" | P6 -> "p6" | P7 -> "p7" | P8 -> "p8" | P9 -> "p9"
    | S1 -> "s1" | S2 -> "s2" | S3 -> "s3" | S4 -> "s4" | S5 -> "s5" | S6 -> "s6" | S7 -> "s7" | S8 -> "s8" | S9 -> "s9"
    | EW -> "ew" | SW -> "sw" | WW -> "ww" | NW -> "nw"
    | WD -> "wd" | GD -> "gd" | RD -> "rd"
  in
  Printf.sprintf "%s(%d)" suffix (offset t)

let to_char t =
  match type_ t with
  | M1 -> "一" | M2 -> "二" | M3 -> "三" | M4 -> "四" | M5 -> "五" | M6 -> "六" | M7 -> "七" | M8 -> "八" | M9 -> "九"
  | P1 -> "①" | P2 -> "②" | P3 -> "③" | P4 -> "④" | P5 -> "⑤" | P6 -> "⑥" | P7 -> "⑦" | P8 -> "⑧" | P9 -> "⑨"
  | S1 -> "１" | S2 -> "２" | S3 -> "３" | S4 -> "４" | S5 -> "５" | S6 -> "６" | S7 -> "７" | S8 -> "８" | S9 -> "９"
  | EW -> "東" | SW -> "南" | WW -> "西" | NW -> "北"
  | WD -> "白" | GD -> "發" | RD -> "中"

let to_unicode t =
  match type_ t with
  | M1 -> "\u{1F007}" | M2 -> "\u{1F008}" | M3 -> "\u{1F009}" | M4 -> "\u{1F00A}" | M5 -> "\u{1F00B}" | M6 -> "\u{1F00C}" | M7 -> "\u{1F00D}" | M8 -> "\u{1F00E}" | M9 -> "\u{1F00F}"
  | P1 -> "\u{1F019}" | P2 -> "\u{1F01A}" | P3 -> "\u{1F01B}" | P4 -> "\u{1F01C}" | P5 -> "\u{1F01D}" | P6 -> "\u{1F01E}" | P7 -> "\u{1F01F}" | P8 -> "\u{1F020}" | P9 -> "\u{1F021}"
  | S1 -> "\u{1F010}" | S2 -> "\u{1F011}" | S3 -> "\u{1F012}" | S4 -> "\u{1F013}" | S5 -> "\u{1F014}" | S6 -> "\u{1F015}" | S7 -> "\u{1F016}" | S8 -> "\u{1F017}" | S9 -> "\u{1F018}"
  | EW -> "\u{1F000}" | SW -> "\u{1F001}" | WW -> "\u{1F002}" | NW -> "\u{1F003}"
  | WD -> "\u{1F006}" | GD -> "\u{1F005}" | RD -> "\u{1F004}\u{FE0E}"
