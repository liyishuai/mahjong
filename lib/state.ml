(** Game state management for Mahjong simulation *)

open Tiles
open Hand
open Rules

(** Wind position *)
type wind = East | South | West | North

(** Player's river (discards) *)
type river_tile =
  { tile : tile
  ; is_riichi : bool  (** This discard declared riichi *)
  ; is_tsumogiri : bool  (** Discarded immediately after draw *)
  ; is_called : bool  (** Tile was called by another player *)
  }

(** Player state *)
type player_state =
  { seat : wind  (** Player's seat wind *)
  ; hand : hand  (** Current hand *)
  ; river : river_tile list  (** Discards, newest first *)
  ; points : int  (** Current points *)
  ; is_riichi : bool  (** Has declared riichi *)
  ; riichi_turn : int option  (** Turn number when riichi was declared *)
  ; is_double_riichi : bool  (** Declared on first turn *)
  ; ippatsu : bool  (** Can still get ippatsu *)
  ; is_furiten : bool  (** In temporary furiten state *)
  ; permanent_furiten : bool  (** In permanent furiten state *)
  ; draws : int  (** Number of draws this round *)
  }

(** Round state *)
type round_state =
  { round_wind : wind  (** Current round wind (場風) *)
  ; round_num : int  (** Round number (1-based within wind) *)
  ; honba : int  (** Repeat counter *)
  ; riichi_sticks : int  (** Riichi sticks on table *)
  ; wall : tile array  (** Remaining wall tiles *)
  ; wall_index : int  (** Current position in wall *)
  ; dora_indicators : tile list  (** Revealed dora indicator tiles *)
  ; ura_dora_indicators : tile list  (** Ura dora indicators (hidden until win) *)
  ; kan_count : int  (** Number of kans called this round *)
  ; turn : int  (** Current turn number *)
  ; current_player : int  (** Index of current player (0-based) *)
  ; last_discard : (int * tile) option  (** Last discarded tile and player index *)
  ; is_first_turn : bool  (** True until first discard or call *)
  ; rinshan : bool  (** Current draw is from rinshan *)
  }

(** Game result for a round *)
type round_result =
  | Tsumo of int * int list * int  (** Winner idx, score from each player, han *)
  | Ron of int * int * int * int  (** Winner idx, loser idx, score, han *)
  | DoubleRon of (int * int * int) * (int * int * int)  (** Two winners *)
  | TripleRon  (** Three players ron - usually abortive draw *)
  | Draw of draw_type  (** Abortive or exhaustive draw *)

and draw_type =
  | Exhaustive of int list * int list  (** Tenpai players, noten players *)
  | NineTerminals of int  (** Player who declared *)
  | FourWinds  (** Four same wind discards on first turn *)
  | FourKans  (** Four kans by different players *)
  | FourRiichi  (** All players declared riichi *)
  | TripleRonDraw  (** Three ron on same discard *)

(** Complete game state *)
type game_state =
  { rules : rules
  ; players : player_state array
  ; round : round_state
  ; game_log : game_event list  (** Events, newest first *)
  ; rng_state : int array  (** Random state for reproducibility *)
  }

and game_event =
  | DrawEvent of int * tile
  | DiscardEvent of int * tile * bool  (** Player, tile, tsumogiri *)
  | ChiEvent of int * int * chi  (** Caller, target, chi *)
  | PonEvent of int * int * tile  (** Caller, target, tile *)
  | KanEvent of int * kan_type
  | RiichiEvent of int * bool  (** Player, is_double *)
  | TsumoEvent of int * scoring_info
  | RonEvent of int * int * scoring_info  (** Winner, loser, scoring *)
  | DrawGameEvent of draw_type
  | RoundEndEvent of round_result

and kan_type =
  | Ankan of tile  (** Closed kan *)
  | Minkan of int * tile  (** Called kan, target player *)
  | Kakan of tile  (** Added kan *)

and scoring_info =
  { base_han : int
  ; dora : int
  ; ura_dora : int
  ; aka_dora : int
  ; fu : int
  ; total_points : int
  ; yaku : string list
  }

(** Initial player state *)
let init_player_state (seat : wind) (start_points : int) : player_state =
  { seat
  ; hand = { tiles = [||]; furos = [||]; ankan = [||] }
  ; river = []
  ; points = start_points
  ; is_riichi = false
  ; riichi_turn = None
  ; is_double_riichi = false
  ; ippatsu = false
  ; is_furiten = false
  ; permanent_furiten = false
  ; draws = 0
  }

(** Get seat wind for player index in round *)
let seat_wind_for_player (_round_wind : wind) (dealer : int) (player_idx : int) (num_players : int) : wind =
  let offset = (player_idx - dealer + num_players) mod num_players in
  match num_players with
  | 3 ->
      (match offset with
       | 0 -> East
       | 1 -> South
       | _ -> West)
  | _ ->
      (match offset with
       | 0 -> East
       | 1 -> South
       | 2 -> West
       | _ -> North)

(** Wind after a given wind (cycles) *)
let next_wind (w : wind) (num_players : int) : wind =
  match w with
  | East -> South
  | South -> if num_players = 3 then West else West
  | West -> if num_players = 3 then East else North
  | North -> East

(** String representation of wind *)
let string_of_wind (w : wind) : string =
  match w with
  | East -> "東"
  | South -> "南"
  | West -> "西"
  | North -> "北"

(** Initialize game state *)
let init_game_state (rules : rules) (seed : int array) : game_state =
  let n = num_players rules in
  let seats = [|East; South; West; North|] in
  let players = Array.init n (fun i -> 
    init_player_state seats.(i) rules.points.start_points
  ) in
  let wall = make_wall rules seed in
  let round =
    { round_wind = East
    ; round_num = 1
    ; honba = 0
    ; riichi_sticks = 0
    ; wall
    ; wall_index = 0
    ; dora_indicators = []
    ; ura_dora_indicators = []
    ; kan_count = 0
    ; turn = 0
    ; current_player = 0
    ; last_discard = None
    ; is_first_turn = true
    ; rinshan = false
    }
  in
  { rules
  ; players
  ; round
  ; game_log = []
  ; rng_state = seed
  }

(** Get remaining tiles in wall *)
let remaining_wall_tiles (state : game_state) : int =
  Array.length state.round.wall - state.round.wall_index - (14 + 4 * state.round.kan_count)

(** Check if wall is exhausted *)
let is_wall_exhausted (state : game_state) : bool =
  remaining_wall_tiles state <= 0

(** Get current player state *)
let current_player (state : game_state) : player_state =
  state.players.(state.round.current_player)

(** Update player at index *)
let update_player (state : game_state) (idx : int) (player : player_state) : game_state =
  let new_players = Array.copy state.players in
  new_players.(idx) <- player;
  { state with players = new_players }

(** Advance to next player *)
let advance_player (state : game_state) : game_state =
  let n = num_players state.rules in
  let next_idx = (state.round.current_player + 1) mod n in
  let new_round = { state.round with current_player = next_idx; turn = state.round.turn + 1 } in
  { state with round = new_round }

(** Check if player is dealer *)
let is_dealer (state : game_state) (player_idx : int) : bool =
  let dealer = (state.round.round_num - 1) mod (num_players state.rules) in
  player_idx = dealer

(** Get dealer index for current round *)
let dealer_index (state : game_state) : int =
  (state.round.round_num - 1) mod (num_players state.rules)
