open Base

(** Wall type representing the mahjong deck *)
type t = private {
  round : int;
  game_seed : int list;
  tiles : Tile.t array;
  mutable draw_ix : int;
  mutable num_kan_draw : int;
  mutable num_kan_dora : int;
}

(** Create a new wall from round, honba, and game_seed *)
val create : round:int -> honba:int -> game_seed:int list -> t

(** Create a wall from round and tiles (for replay from human data) *)
val from_tiles : round:int -> Tile.t list -> t

(** Get initial hand tiles (13 tiles) for a player *)
val initial_hand_tiles : t -> pos:Action.AbsolutePos.t -> Tile.t list

(** Check if normal draws are available *)
val has_draw_left : t -> bool

(** Check if next player draw is available *)
val has_next_draw_left : t -> bool

(** Normal draw *)
val draw : t -> Tile.t

(** Kan draw - uses special positions at the end *)
val kan_draw : t -> Tile.t

(** Add kan dora indicator - flip new dora
    @return (kan_dora_indicator, ura_kan_dora_indicator) *)
val add_kan_dora : t -> Tile.t * Tile.t

(** Get dora indicators (visible) *)
val dora_indicators : t -> Tile.t list

(** Get ura dora indicators (hidden until end) *)
val ura_dora_indicators : t -> Tile.t list

(** Convert dora indicator to dora tile type *)
val indicator_to_dora : Tile.TileType.t -> Tile.TileType.t

(** Get dora count as an association list (tile_type -> count) *)
val dora_count : t -> (Tile.TileType.t * int) list

(** Get ura dora count as an association list (tile_type -> count) *)
val ura_dora_count : t -> (Tile.TileType.t * int) list

(** Get all tiles (for testing/debugging) *)
val tiles : t -> Tile.t list

(** Get game seed *)
val game_seed : t -> int list

(** Get number of kan draws *)
val num_kan_draw : t -> int

(** Get number of kan dora indicators *)
val num_kan_dora : t -> int

(** Get round number *)
val round : t -> int
