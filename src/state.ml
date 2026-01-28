open Base

module PlayerId = struct
  type t = string [@@deriving sexp, compare, equal, hash]
end

module ScoreInfo = struct
  type t = {
    player_ids : PlayerId.t list;
    game_seed : int64;
    round : int;
    honba : int;
    riichi : int;
    tens : int array;
  }
end

module GameResult = struct
  type t = {
    game_seed : int64;
    rankings : (PlayerId.t, int) Hashtbl.t;
    tens : (PlayerId.t, int) Hashtbl.t;
  }
end

module Observation = struct
  type t
end

type player = {
  player_id : PlayerId.t;
  position : int; (* 0-3 *)
  mutable hand : Hand.t;
  mutable machi : int list; (* TileType indices *)
  mutable discards : int list; (* TileType indices *)
  mutable missed_tiles : int list;
  mutable is_ippatsu : bool;
  mutable has_nm : bool; (* Nagashi Mangan potential *)
}

type t = {
  mutable score_info : ScoreInfo.t;
  players : player array;
  mutable wall : int list; (* Placeholder for Wall module *)
  mutable dora_markers : Tile.t list;
  mutable ura_dora_markers : Tile.t list;
  (* mutable events : Mjx.Event.t list; *)
  (* TODO: fully implement Wall module logic *)
}

let create (score_info : ScoreInfo.t) =
  (* Initialize players *)
  let players = Array.init 4 ~f:(fun i ->
    let pid = List.nth_exn score_info.player_ids i in
    let tiles = List.init 13 ~f:(fun _ -> Tile.create Tile.TileType.M1 ~offset:0) in (* Dummy hand init *)
    {
      player_id = pid;
      position = i;
      hand = Hand.create tiles;
      machi = [];
      discards = [];
      missed_tiles = [];
      is_ippatsu = false;
      has_nm = true;
    }
  ) in
  {
    score_info;
    players;
    wall = []; (* Todo *)
    dora_markers = [];
    ura_dora_markers = [];
  }

let to_json _ = "{}" (* TODO: implement json serialization if needed *)

let update _t _actions =
  (* Placeholder: implement state transition logic *)
  ()

let create_observations _t =
  (* Placeholder *)
  Hashtbl.create (module PlayerId)

let is_round_over _t = false (* TODO *)
let is_game_over _t = false (* TODO *)

let result t =
  {
    GameResult.game_seed = t.score_info.game_seed;
    rankings = Hashtbl.create (module PlayerId);
    tens = Hashtbl.create (module PlayerId);
  }

let next t = t.score_info

let hand t who = t.players.(who).hand
let round t = t.score_info.round
let honba t = t.score_info.honba
let riichi t = t.score_info.riichi
let tens t = t.score_info.tens