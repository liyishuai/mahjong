open Base

module EventType = struct
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

type t =
  { type_ : EventType.t
  ; who : int option
  ; tile : Tile.t option
  ; open_ : Open.t option
  }

(** Convert Open type to EventType *)
let open_type_to_event_type = function
  | Open.OpenType.Chi -> EventType.Chi
  | Open.OpenType.Pon -> EventType.Pon
  | Open.OpenType.KanClosed -> EventType.ClosedKan
  | Open.OpenType.KanOpened -> EventType.OpenKan
  | Open.OpenType.KanAdded -> EventType.AddedKan

(** Create DRAW event *)
let create_draw ~who =
  { type_ = EventType.Draw
  ; who = Some (Action.AbsolutePos.to_int who)
  ; tile = None
  ; open_ = None
  }

(** Create DISCARD event *)
let create_discard ~who ~discard ~tsumogiri =
  { type_ = if tsumogiri then EventType.Tsumogiri else EventType.Discard
  ; who = Some (Action.AbsolutePos.to_int who)
  ; tile = Some discard
  ; open_ = None
  }

(** Create RIICHI event *)
let create_riichi ~who =
  { type_ = EventType.Riichi
  ; who = Some (Action.AbsolutePos.to_int who)
  ; tile = None
  ; open_ = None
  }

(** Create OPEN event (Chi, Pon, Kan) *)
let create_open ~who ~open_ =
  { type_ = open_type_to_event_type (Open.type_ open_)
  ; who = Some (Action.AbsolutePos.to_int who)
  ; tile = None
  ; open_ = Some open_
  }

(** Create NEW_DORA event *)
let create_new_dora ~dora_indicator =
  { type_ = EventType.NewDora
  ; who = None
  ; tile = Some dora_indicator
  ; open_ = None
  }

(** Create RIICHI_SCORE_CHANGE event *)
let create_riichi_score_change ~who =
  { type_ = EventType.RiichiScoreChange
  ; who = Some (Action.AbsolutePos.to_int who)
  ; tile = None
  ; open_ = None
  }

(** Create TSUMO event *)
let create_tsumo ~who ~tile =
  { type_ = EventType.Tsumo
  ; who = Some (Action.AbsolutePos.to_int who)
  ; tile = Some tile
  ; open_ = None
  }

(** Create RON event *)
let create_ron ~who ~tile =
  { type_ = EventType.Ron
  ; who = Some (Action.AbsolutePos.to_int who)
  ; tile = Some tile
  ; open_ = None
  }

(** Create ABORTIVE_DRAW_NINE_TERMINALS event *)
let create_abortive_draw_nine_terminals ~who =
  { type_ = EventType.AbortiveDrawNineTerminals
  ; who = Some (Action.AbsolutePos.to_int who)
  ; tile = None
  ; open_ = None
  }

(** Create ABORTIVE_DRAW_FOUR_RIICHIS event *)
let create_abortive_draw_four_riichis () =
  { type_ = EventType.AbortiveDrawFourRiichis
  ; who = None
  ; tile = None
  ; open_ = None
  }

(** Create ABORTIVE_DRAW_THREE_RONS event *)
let create_abortive_draw_three_rons () =
  { type_ = EventType.AbortiveDrawThreeRons
  ; who = None
  ; tile = None
  ; open_ = None
  }

(** Create ABORTIVE_DRAW_FOUR_KANS event *)
let create_abortive_draw_four_kans () =
  { type_ = EventType.AbortiveDrawFourKans
  ; who = None
  ; tile = None
  ; open_ = None
  }

(** Create ABORTIVE_DRAW_FOUR_WINDS event *)
let create_abortive_draw_four_winds () =
  { type_ = EventType.AbortiveDrawFourWinds
  ; who = None
  ; tile = None
  ; open_ = None
  }

(** Create EXHAUSTIVE_DRAW_NORMAL event *)
let create_exhaustive_draw_normal () =
  { type_ = EventType.ExhaustiveDrawNormal
  ; who = None
  ; tile = None
  ; open_ = None
  }

(** Create EXHAUSTIVE_DRAW_NAGASHI_MANGAN event *)
let create_exhaustive_draw_nagashi_mangan () =
  { type_ = EventType.ExhaustiveDrawNagashiMangan
  ; who = None
  ; tile = None
  ; open_ = None
  }

(** Check if an event is valid *)
let is_valid t =
  match t.type_ with
  | EventType.Draw ->
      (match t.who with
       | Some who -> 0 <= who && who <= 3
       | None -> false)
      && Option.is_none t.tile
      && Option.is_none t.open_

  | EventType.Discard
  | EventType.Tsumogiri ->
      (match t.who with
       | Some who -> 0 <= who && who <= 3
       | None -> false)
      &&
      (match t.tile with
       | Some tile -> 0 <= tile && tile < 136
       | None -> false)
      && Option.is_none t.open_

  | EventType.Riichi
  | EventType.RiichiScoreChange ->
      (match t.who with
       | Some who -> 0 <= who && who <= 3
       | None -> false)
      && Option.is_none t.tile
      && Option.is_none t.open_

  | EventType.Tsumo
  | EventType.Ron ->
      (match t.who with
       | Some who -> 0 <= who && who <= 3
       | None -> false)
      &&
      (match t.tile with
       | Some tile -> 0 <= tile && tile < 136
       | None -> false)
      && Option.is_none t.open_

  | EventType.Chi
  | EventType.Pon
  | EventType.ClosedKan
  | EventType.OpenKan
  | EventType.AddedKan ->
      (match t.who with
       | Some who -> 0 <= who && who <= 3
       | None -> false)
      && Option.is_none t.tile
      (* open could be zero for closed kan *)

  | EventType.NewDora ->
      Option.is_none t.who
      &&
      (match t.tile with
       | Some tile -> 0 <= tile && tile < 136
       | None -> false)
      && Option.is_none t.open_

  | EventType.AbortiveDrawNineTerminals ->
      (match t.who with
       | Some who -> 0 <= who && who <= 3
       | None -> false)
      && Option.is_none t.tile
      && Option.is_none t.open_

  | EventType.AbortiveDrawFourRiichis
  | EventType.AbortiveDrawThreeRons
  | EventType.AbortiveDrawFourKans
  | EventType.AbortiveDrawFourWinds
  | EventType.ExhaustiveDrawNormal
  | EventType.ExhaustiveDrawNagashiMangan ->
      Option.is_none t.who && Option.is_none t.tile && Option.is_none t.open_

(** Accessor: get type *)
let type_ t = t.type_

(** Accessor: get who *)
let who t = t.who

(** Accessor: get tile *)
let tile t = t.tile

(** Accessor: get open *)
let open_ t = t.open_
