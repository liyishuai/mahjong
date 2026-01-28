open Base

module TileType : sig
  type t =
    | M1 | M2 | M3 | M4 | M5 | M6 | M7 | M8 | M9
    | P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8 | P9
    | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9
    | EW | SW | WW | NW
    | WD | GD | RD
  [@@deriving sexp, compare, hash, equal]

  val to_int : t -> int
  val of_int : int -> t option
end

type t = int [@@deriving sexp, compare, hash, equal]

val of_int : int -> t option
val to_int : t -> int

val create : TileType.t -> offset:int -> t
val type_ : t -> TileType.t
val offset : t -> int
val is_red_five : t -> bool
val num : t -> int

val to_string : t -> string
val to_char : t -> string
val to_unicode : t -> string

type comparator_witness
val comparator : (t, comparator_witness) Comparator.t