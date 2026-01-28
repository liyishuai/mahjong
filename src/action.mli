open Base

(** Absolute player positions *)
module AbsolutePos : sig
  type t =
    | InitEast  (* 0 - 起家 *)
    | InitSouth (* 1 *)
    | InitWest  (* 2 *)
    | InitNorth (* 3 - ラス親 *)
  [@@deriving sexp, compare, equal, enumerate]

  val to_int : t -> int
  val of_int : int -> t option
end

(** Action types *)
module ActionType : sig
  type t =
    | Discard
    | Tsumogiri
    | Riichi
    | Tsumo
    | Ron
    | Chi
    | Pon
    | KanOpened
    | KanClosed
    | KanAdded
    | No
    | NineTiles
    | Dummy
  [@@deriving sexp, compare, equal, enumerate]
end

(** Action type representing a player action *)
type t = private {
  type_ : ActionType.t;
  who : AbsolutePos.t;
  tile : Tile.t option;
  open_ : Open.t option;
}
[@@deriving sexp, equal]

(** Creation functions *)
val create_discard : who:AbsolutePos.t -> tile:Tile.t -> t
val create_tsumogiri : who:AbsolutePos.t -> tile:Tile.t -> t
val create_riichi : who:AbsolutePos.t -> t
val create_tsumo : who:AbsolutePos.t -> tile:Tile.t -> t
val create_ron : who:AbsolutePos.t -> tile:Tile.t -> t
val create_open : who:AbsolutePos.t -> open_:Open.t -> t
val create_no : who:AbsolutePos.t -> t
val create_nine_tiles : who:AbsolutePos.t -> t
val create_dummy : who:AbsolutePos.t -> t

(** Accessors *)
val type_ : t -> ActionType.t
val who : t -> AbsolutePos.t
val tile : t -> Tile.t option
val open_ : t -> Open.t option

(** Validation *)
val is_valid : t -> bool

(** {b Encode} encodes an action to a 0-180 code:
    - 0~33: Discard m1~rd
    - 34,35,36: Discard m5(red), p5(red), s5(red)
    - 37~70: Tsumogiri m1~rd
    - 71,72,73: Tsumogiri m5(red), p5(red), s5(red)
    - 74~94: Chi m1m2m3 ~ s7s8s9
    - 95,96,97: Chi m3m4m5(red), m4m5(red)m6, m5(red)m6m7
    - 98,99,100: Chi p3p4p5(red), p4p5(red)p6, p5(red)p6p7
    - 101,102,103: Chi s3s4s5(red), s4s5(red)s6, s5(red)s6s7
    - 104~137: Pon m1~rd
    - 138,139,140: Pon m5(w/ red), s5(w/ red), p5(w/ red)
    - 141~174: Kan m1~rd
    - 175: Tsumo
    - 176: Ron
    - 177: Riichi
    - 178: Kyuushu (NineTiles)
    - 179: No
    - 180: Dummy *)
val encode : t -> int

(** {b Decode} decodes a code using legal actions list *)
val decode : code:int -> legal_actions:t list -> t

(** Equality check *)
val equal : t -> t -> bool
