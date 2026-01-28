open Base

module HandStage : sig
  type t =
    | AfterDiscards
    | AfterDraw
    | AfterDrawAfterKan
    | AfterRiichi
    | AfterTsumo
    | AfterTsumoAfterKan
    | AfterRon
    | AfterChi
    | AfterPon
    | AfterKanOpened
    | AfterKanClosed
    | AfterKanAdded
  [@@deriving sexp, compare, equal]
end

type t

val create : Tile.t list -> t
val draw : t -> Tile.t -> t
val discard : t -> Tile.t -> t * bool (* returns (tile, tsumogiri) *)
val apply_open : t -> Open.t -> t

val stage : t -> HandStage.t
val closed_tiles : t -> Tile.t list
val opens : t -> Open.t list
val last_tile_added : t -> Tile.t option
val is_under_riichi : t -> bool
val is_menzen : t -> bool

val to_string : t -> verbose:bool -> string
val size : t -> int
val size_closed : t -> int
val size_opened : t -> int