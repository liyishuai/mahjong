open Base

module EventType : sig
  type t =
    | Discard
    | Tsumogiri
    | Riichi
    | ClosedKan
    | AddedKan
    | Tsumo
    | AbortiveDrawNineTerminals
    | Chi
    | Pon
    | OpenKan
    | Ron
    | Draw
    | RiichiScoreChange
    | NewDora
    | AbortiveDrawFourRiichis
    | AbortiveDrawThreeRons
    | AbortiveDrawFourKans
    | AbortiveDrawFourWinds
    | ExhaustiveDrawNormal
    | ExhaustiveDrawNagashiMangan
  [@@deriving sexp, compare, equal, enumerate]
end

(** Event type representing a game event *)
type t =
  { type_ : EventType.t
  ; who : int option
  ; tile : Tile.t option
  ; open_ : Open.t option
  }

(** Create DRAW event *)
val create_draw : who:Action.AbsolutePos.t -> t

(** Create DISCARD event *)
val create_discard : who:Action.AbsolutePos.t -> discard:Tile.t -> tsumogiri:bool -> t

(** Create RIICHI event *)
val create_riichi : who:Action.AbsolutePos.t -> t

(** Create OPEN event (Chi, Pon, Kan) *)
val create_open : who:Action.AbsolutePos.t -> open_:Open.t -> t

(** Create NEW_DORA event *)
val create_new_dora : dora_indicator:Tile.t -> t

(** Create RIICHI_SCORE_CHANGE event *)
val create_riichi_score_change : who:Action.AbsolutePos.t -> t

(** Create TSUMO event *)
val create_tsumo : who:Action.AbsolutePos.t -> tile:Tile.t -> t

(** Create RON event *)
val create_ron : who:Action.AbsolutePos.t -> tile:Tile.t -> t

(** Create ABORTIVE_DRAW_NINE_TERMINALS event *)
val create_abortive_draw_nine_terminals : who:Action.AbsolutePos.t -> t

(** Create ABORTIVE_DRAW_FOUR_RIICHIS event *)
val create_abortive_draw_four_riichis : unit -> t

(** Create ABORTIVE_DRAW_THREE_RONS event *)
val create_abortive_draw_three_rons : unit -> t

(** Create ABORTIVE_DRAW_FOUR_KANS event *)
val create_abortive_draw_four_kans : unit -> t

(** Create ABORTIVE_DRAW_FOUR_WINDS event *)
val create_abortive_draw_four_winds : unit -> t

(** Create EXHAUSTIVE_DRAW_NORMAL event *)
val create_exhaustive_draw_normal : unit -> t

(** Create EXHAUSTIVE_DRAW_NAGASHI_MANGAN event *)
val create_exhaustive_draw_nagashi_mangan : unit -> t

(** Check if an event is valid *)
val is_valid : t -> bool

(** Accessor: get type *)
val type_ : t -> EventType.t

(** Accessor: get who *)
val who : t -> int option

(** Accessor: get tile *)
val tile : t -> Tile.t option

(** Accessor: get open *)
val open_ : t -> Open.t option
