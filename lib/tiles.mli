(** Tile representation for Riichi Mahjong.

    Tiles are represented as integers 0-37:
    - 0-8: 1m-9m (manzu/characters)
    - 9-17: 1p-9p (pinzu/circles)
    - 18-26: 1s-9s (souzu/bamboos)
    - 27-30: E,S,W,N (winds)
    - 31-33: P,F,C (dragons: White/Green/Red)
    - 34-36: 5mr,5pr,5sr (red fives/aka dora)
    - 37: ? (unknown tile) *)

(** Chi sequence types for chii (sequence meld) representations.

    These types represent which position a tile was taken from when forming
    a chii (sequence meld of three consecutive tiles). *)
type chi_type =
  | Low (** Lower sequence: ABC from ABC - called with the leftmost tile *)
  | Mid (** Middle sequence: ABC from A[BC]D - called with the middle tile *)
  | High (** Upper sequence: ABC from [AB]CD - called with the rightmost tile *)

(** Tile type - integer 0-37 representing tile IDs *)
type tile = int

(** Error type for invalid tile conversions *)
type invalid_tile =
  | Number of int (** Invalid numeric ID *)
  | String of string (** Invalid string representation *)

(** {1 Conversion functions} *)

(** Convert an integer to a tile.
    @return [Ok tile] if the integer is in range [0,37], [Error] otherwise *)
val tile_of_int : int -> (tile, invalid_tile) result

(** Convert an integer to a tile, raising an exception if invalid.
    @raise Invalid_argument if the integer is not in range [0,37] *)
val tile_of_int_exn : int -> tile

(** Convert an MJAI-style string to a tile.
    @return [Ok tile] if the string is valid (e.g., "1m", "5mr", "E", "?"), [Error] otherwise *)
val tile_of_string : string -> (tile, invalid_tile) result

(** Convert a tile to its underlying integer representation *)
val int_of_tile : tile -> int

(** Convert a tile to its MJAI-style string representation *)
val string_of_tile : tile -> string

(** {1 Tile properties} *)

(** Remove red variant (aka) from a tile, converting 5mr->5m, 5pr->5p, 5sr->5s *)
val deaka : tile -> tile

(** Convert a 5-tile to its red variant: 5m->5mr, 5p->5pr, 5s->5sr *)
val akaize : tile -> tile

(** Check if a tile is a red five (aka dora) *)
val is_aka : tile -> bool

(** Check if a tile is an honor tile (wind or dragon) *)
val is_jihai : tile -> bool

(** Check if a tile is a terminal (1 or 9 of suits) or honor tile *)
val is_yaokyuu : tile -> bool

(** Check if a tile is the unknown tile placeholder *)
val is_unknown : tile -> bool

(** {1 Navigation} *)

(** Get the next tile in sequence (wraps around within suit) *)
val next : tile -> tile

(** Get the previous tile in sequence (wraps around within suit) *)
val prev : tile -> tile

(** Swap manzu and pinzu for data augmentation (m<->p, others unchanged) *)
val augment : tile -> tile

(** Compare two tiles by discard priority.
    Returns negative if t1 should be discarded before t2,
    positive if t2 should be discarded before t1,
    zero if they have equal priority. *)
val cmp_discard_priority : tile -> tile -> int

(** {1 Constants} *)

(** Tile ID constants for pattern matching and convenience *)
val tile_id_1m : int

val tile_id_2m : int
val tile_id_3m : int
val tile_id_4m : int
val tile_id_5m : int
val tile_id_6m : int
val tile_id_7m : int
val tile_id_8m : int
val tile_id_9m : int
val tile_id_1p : int
val tile_id_2p : int
val tile_id_3p : int
val tile_id_4p : int
val tile_id_5p : int
val tile_id_6p : int
val tile_id_7p : int
val tile_id_8p : int
val tile_id_9p : int
val tile_id_1s : int
val tile_id_2s : int
val tile_id_3s : int
val tile_id_4s : int
val tile_id_5s : int
val tile_id_6s : int
val tile_id_7s : int
val tile_id_8s : int
val tile_id_9s : int
val tile_id_E : int
val tile_id_S : int
val tile_id_W : int
val tile_id_N : int
val tile_id_P : int
val tile_id_F : int
val tile_id_C : int
val tile_id_5mr : int
val tile_id_5pr : int
val tile_id_5sr : int
val tile_id_unknown : int

(** Array mapping tile IDs to their string representations *)
val mjai_pai_strings : string array

(** Array of discard priorities for each tile ID *)
val discard_priorities : int array

(** Create a default (unknown) tile *)
val default : unit -> tile

(** Format an invalid tile error as a string *)
val string_of_invalid_tile : invalid_tile -> string
