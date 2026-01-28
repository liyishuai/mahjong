open Base

module RelativePos : sig
  type t =
    | Self
    | Right
    | Mid
    | Left
  [@@deriving sexp, compare, equal]

  val to_int : t -> int
  val of_int : int -> t option
end

module OpenType : sig
  type t =
    | Chi
    | Pon
    | KanOpened
    | KanClosed
    | KanAdded
  [@@deriving sexp, compare, equal]
end

type t [@@deriving sexp, compare, equal]

val create_chi : tiles:Tile.t list -> stolen:Tile.t -> t
val create_pon : stolen:Tile.t -> unused:Tile.t -> from:RelativePos.t -> t
val create_kan_opened : stolen:Tile.t -> from:RelativePos.t -> t
val create_kan_closed : tile:Tile.t -> t
val create_kan_added : pon:t -> t

val type_ : t -> OpenType.t
val from : t -> RelativePos.t
val at : t -> int -> Tile.t
val size : t -> int
val tiles : t -> Tile.t list
val tiles_from_hand : t -> Tile.t list
val stolen_tile : t -> Tile.t
val last_tile : t -> Tile.t
val undiscardable_tile_types : t -> Tile.TileType.t list
val to_string : t -> verbose:bool -> string
val to_bits : t -> int
val of_bits : int -> t
