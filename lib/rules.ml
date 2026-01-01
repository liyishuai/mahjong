(** Game rules configuration for Mahjong variants *)

open Tiles

(** Number of players: 3 (sanma) or 4 *)
type player_count = Three | Four

(** Wind rounds: East only (東風) or East+South (半荘) *)
type wind_rounds = EastOnly | HalfGame

(** Red dora configuration *)
type aka_dora = { man5 : int; so5 : int; pin5 : int }

(** Scoring rules *)
type scoring_rules =
  { kiriage_mangan : bool  (** Round up to mangan at 1920/2000 *)
  ; kazoe_yakuman : bool   (** 13+ han is yakuman *)
  ; multiple_yakuman : bool  (** Allow multiple yakuman *)
  ; pao_rule : bool  (** Responsibility payment for specific yakuman *)
  }

(** Riichi rules *)
type riichi_rules =
  { double_riichi : bool  (** Allow double riichi *)
  ; open_riichi : bool  (** Allow open riichi *)
  ; riichi_bet : int  (** Riichi bet amount (typically 1000) *)
  }

(** Dora rules *)
type dora_rules =
  { aka_dora : aka_dora option  (** Red dora tiles *)
  ; ura_dora : bool  (** Reveal ura dora on riichi win *)
  ; kan_dora : bool  (** Reveal new dora on kan *)
  ; kan_ura_dora : bool  (** Reveal kan ura dora on riichi win *)
  }

(** Uma (placement bonus) *)
type uma =
  { first : int
  ; second : int
  ; third : int
  ; fourth : int
  }

(** Starting points and oka *)
type points_rules =
  { start_points : int  (** Starting points (e.g., 25000) *)
  ; return_points : int  (** Points for calculating final score (e.g., 30000) *)
  ; oka : int  (** Bonus for 1st place from each player *)
  ; uma : uma  (** Placement bonus/penalty *)
  }

(** Game ending conditions *)
type ending_rules =
  { tobi : bool  (** Game ends when player goes below 0 *)
  ; agariyame : bool  (** Dealer can end game if in 1st place in final round *)
  ; enchousen : int option  (** Extension threshold (e.g., 30000) *)
  }

(** Three-player (sanma) specific rules *)
type sanma_rules =
  { removed_tiles : number list  (** Typically Man 2-8 removed *)
  ; north_as_dora : bool  (** North wind is always dora *)
  ; tsumo_loss : bool  (** On tsumo, non-winners pay half each *)
  }

(** Complete rule configuration *)
type rules =
  { player_count : player_count
  ; wind_rounds : wind_rounds
  ; scoring : scoring_rules
  ; riichi : riichi_rules
  ; dora : dora_rules
  ; points : points_rules
  ; ending : ending_rules
  ; sanma : sanma_rules option  (** Only for 3-player games *)
  }

(** Standard 4-player half-game rules *)
let default_four_player : rules =
  { player_count = Four
  ; wind_rounds = HalfGame
  ; scoring =
      { kiriage_mangan = false
      ; kazoe_yakuman = true
      ; multiple_yakuman = true
      ; pao_rule = true
      }
  ; riichi =
      { double_riichi = true
      ; open_riichi = false
      ; riichi_bet = 1000
      }
  ; dora =
      { aka_dora = Some { man5 = 1; so5 = 1; pin5 = 1 }
      ; ura_dora = true
      ; kan_dora = true
      ; kan_ura_dora = true
      }
  ; points =
      { start_points = 25000
      ; return_points = 30000
      ; oka = 0
      ; uma = { first = 20; second = 10; third = -10; fourth = -20 }
      }
  ; ending =
      { tobi = true
      ; agariyame = true
      ; enchousen = Some 30000
      }
  ; sanma = None
  }

(** Standard 4-player East only (東風) rules *)
let default_four_player_east : rules =
  { default_four_player with
    wind_rounds = EastOnly
  }

(** Standard 3-player (sanma) rules *)
let default_three_player : rules =
  { player_count = Three
  ; wind_rounds = HalfGame
  ; scoring =
      { kiriage_mangan = true
      ; kazoe_yakuman = true
      ; multiple_yakuman = true
      ; pao_rule = true
      }
  ; riichi =
      { double_riichi = true
      ; open_riichi = false
      ; riichi_bet = 1000
      }
  ; dora =
      { aka_dora = Some { man5 = 1; so5 = 1; pin5 = 1 }
      ; ura_dora = true
      ; kan_dora = true
      ; kan_ura_dora = true
      }
  ; points =
      { start_points = 35000
      ; return_points = 40000
      ; oka = 0
      ; uma = { first = 20; second = 0; third = -20; fourth = 0 }
      }
  ; ending =
      { tobi = true
      ; agariyame = true
      ; enchousen = Some 40000
      }
  ; sanma = Some
      { removed_tiles = [Two; Three; Four; Five; Six; Seven; Eight]
      ; north_as_dora = true
      ; tsumo_loss = false
      }
  }

(** Standard 3-player East only (東風) rules *)
let default_three_player_east : rules =
  { default_three_player with
    wind_rounds = EastOnly
  }

(** Get number of players as int *)
let num_players (r : rules) : int =
  match r.player_count with
  | Three -> 3
  | Four -> 4

(** Get number of wind rounds *)
let num_wind_rounds (r : rules) : int =
  match r.wind_rounds with
  | EastOnly -> 1
  | HalfGame -> 2

(** Calculate total number of regular rounds *)
let total_rounds (r : rules) : int =
  num_players r * num_wind_rounds r

(** Check if a tile should be removed in sanma *)
let is_removed_tile (r : rules) (t : tile) : bool =
  match r.sanma with
  | None -> false
  | Some sanma ->
      match t with
      | Man n -> 
          (* Aka is a red 5, so if Five is removed, Aka should be too *)
          List.mem n sanma.removed_tiles || (n = Aka && List.mem Five sanma.removed_tiles)
      | _ -> false

(** Generate initial wall for the rules *)
let make_wall (r : rules) (seed : int array) : tile array =
  let full_set = random_set seed in
  match r.sanma with
  | None -> full_set
  | Some _ -> Array.of_list (List.filter (fun t -> not (is_removed_tile r t)) (Array.to_list full_set))
