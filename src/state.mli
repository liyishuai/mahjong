open Base

module PlayerId : sig
  type t = string [@@deriving sexp, compare, equal]
end

module ScoreInfo : sig
  type t = {
    player_ids : PlayerId.t list;
    game_seed : int64;
    round : int;
    honba : int;
    riichi : int;
    tens : int array;
  }
end

module GameResult : sig
  type t = {
    game_seed : int64;
    rankings : (PlayerId.t, int) Hashtbl.t;
    tens : (PlayerId.t, int) Hashtbl.t;
  }
end

module Observation : sig
  type t
end

type t

val create : ScoreInfo.t -> t
val to_json : t -> string
val update : t -> Mjx.Reader.Action.t list -> unit
val create_observations : t -> (PlayerId.t, Observation.t) Hashtbl.t
val is_round_over : t -> bool
val is_game_over : t -> bool
val result : t -> GameResult.t
val next : t -> ScoreInfo.t
val hand : t -> int -> Hand.t
val round : t -> int
val honba : t -> int
val riichi : t -> int
val tens : t -> int array