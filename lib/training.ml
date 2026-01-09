(** Training infrastructure for Mahjong AI *)

open Tiles
open Hand
open Rules
open State
open Simulation

(** Feature vector for ML training *)
type features =
  { (* Hand features *)
    hand_tiles : float array  (** One-hot encoding of tiles in hand *)
  ; hand_shanten : float  (** Shanten count *)
  ; is_tenpai : float  (** 1.0 if tenpai *)
  ; has_yaku : float  (** 1.0 if current hand has yaku *)
  
  (* Call features *)
  ; num_furos : float  (** Number of open melds *)
  ; furos_type : float array  (** Encoding of furo types *)
  
  (* River features *)
  ; own_river : float array  (** One-hot of own discards *)
  ; others_rivers : float array  (** Combined rivers of opponents *)
  ; genbutsu : float array  (** Safe tiles against riichi players *)
  
  (* Round features *)
  ; round_wind : float array  (** One-hot encoding [E,S,W,N] *)
  ; seat_wind : float array  (** One-hot encoding [E,S,W,N] *)
  ; round_num : float  (** Normalized round number *)
  ; honba : float  (** Normalized honba count *)
  ; remaining_tiles : float  (** Normalized remaining wall tiles *)
  
  (* Score features *)
  ; own_points : float  (** Normalized own points *)
  ; others_points : float array  (** Normalized points of others *)
  ; rank : float  (** Current rank (1-4) normalized *)
  ; points_diff_to_first : float  (** Normalized point difference to 1st *)
  
  (* Riichi features *)
  ; is_riichi : float  (** 1.0 if in riichi *)
  ; others_riichi : float array  (** 1.0 for each opponent in riichi *)
  ; riichi_sticks : float  (** Number of riichi sticks on table *)
  
  (* Dora features *)
  ; dora_in_hand : float  (** Count of dora tiles in hand *)
  ; dora_indicators : float array  (** One-hot of dora indicators *)
  }

(** Action value pair for training *)
type training_sample =
  { state_features : features
  ; action_taken : action
  ; reward : float  (** Outcome-based reward *)
  }

(** Training configuration *)
type training_config =
  { num_games : int  (** Number of games to simulate *)
  ; batch_size : int  (** Batch size for training *)
  ; learning_rate : float  (** Learning rate *)
  ; discount_factor : float  (** Discount factor for future rewards *)
  ; exploration_rate : float  (** Epsilon for exploration *)
  ; rules : rules  (** Game rules to use *)
  ; num_threads : int  (** Number of parallel threads (for MacBook) *)
  ; checkpoint_interval : int  (** Save checkpoint every N games *)
  ; output_dir : string  (** Directory for saving results *)
  ; policy : game_state -> action list -> action  (** Policy function for action selection *)
  }

(** Training statistics *)
type training_stats =
  { games_played : int
  ; total_rounds : int
  ; wins_by_player : int array
  ; avg_score : float array
  ; avg_game_length : float
  ; tsumo_rate : float
  ; ron_rate : float
  ; riichi_rate : float
  ; avg_han : float
  }

(** Default training configuration *)
let default_config : training_config =
  { num_games = 10000
  ; batch_size = 64
  ; learning_rate = 0.001
  ; discount_factor = 0.99
  ; exploration_rate = 0.1
  ; rules = default_four_player
  ; num_threads = 4  (* Reasonable for MacBook *)
  ; checkpoint_interval = 1000
  ; output_dir = "training_output"
  ; policy = random_action
  }

(** Create empty feature vector *)
let empty_features () : features =
  { hand_tiles = Array.make 136 0.0
  ; hand_shanten = 8.0
  ; is_tenpai = 0.0
  ; has_yaku = 0.0
  ; num_furos = 0.0
  ; furos_type = Array.make 16 0.0
  ; own_river = Array.make 136 0.0
  ; others_rivers = Array.make 136 0.0
  ; genbutsu = Array.make 34 0.0
  ; round_wind = Array.make 4 0.0
  ; seat_wind = Array.make 4 0.0
  ; round_num = 0.0
  ; honba = 0.0
  ; remaining_tiles = 1.0
  ; own_points = 0.5
  ; others_points = Array.make 3 0.5
  ; rank = 0.0
  ; points_diff_to_first = 0.0
  ; is_riichi = 0.0
  ; others_riichi = Array.make 3 0.0
  ; riichi_sticks = 0.0
  ; dora_in_hand = 0.0
  ; dora_indicators = Array.make 136 0.0
  }

(** Convert tile to index (0-33 for unique tile types) *)
let tile_type_index (t : tile) : int =
  match t with
  | Man n -> int_of_number n - 1
  | So n -> 9 + int_of_number n - 1
  | Pin n -> 18 + int_of_number n - 1
  | Honor h -> 27 + int_of_honor h - 1

(** Convert tile to one-hot index (0-135 for all tiles) *)
let tile_instance_index (t : tile) (instance : int) : int =
  tile_type_index t * 4 + instance

(** Wind to index *)
let wind_index (w : wind) : int =
  match w with
  | East -> 0
  | South -> 1
  | West -> 2
  | North -> 3

(** Extract features from game state for a player *)
let extract_features (state : game_state) (player_idx : int) : features =
  let player = state.players.(player_idx) in
  let features = empty_features () in
  let n = num_players state.rules in
  
  (* Hand tiles *)
  let tile_counts = Array.make 34 0 in
  Array.iter (fun t ->
    let idx = tile_type_index t in
    let instance = tile_counts.(idx) in
    tile_counts.(idx) <- instance + 1;
    if instance < 4 then
      features.hand_tiles.(tile_instance_index t instance) <- 1.0
  ) player.hand.tiles;
  
  (* Own river *)
  let river_counts = Array.make 34 0 in
  List.iter (fun rt ->
    let idx = tile_type_index rt.tile in
    let instance = river_counts.(idx) in
    river_counts.(idx) <- instance + 1;
    if instance < 4 then
      features.own_river.(tile_instance_index rt.tile instance) <- 1.0
  ) player.river;
  
  (* Others' rivers *)
  let others_river_counts = Array.make 34 0 in
  Array.iteri (fun i p ->
    if i <> player_idx then
      List.iter (fun rt ->
        let idx = tile_type_index rt.tile in
        let instance = others_river_counts.(idx) in
        others_river_counts.(idx) <- instance + 1;
        if instance < 4 then
          features.others_rivers.(tile_instance_index rt.tile instance) <- 1.0
      ) p.river
  ) state.players;
  
  (* Round features *)
  features.round_wind.(wind_index state.round.round_wind) <- 1.0;
  features.seat_wind.(wind_index player.seat) <- 1.0;
  
  let total = total_rounds state.rules in
  let current = (state.round.round_num - 1) * n + (wind_index state.round.round_wind) in
  { features with
    round_num = float_of_int current /. float_of_int total
  ; honba = float_of_int state.round.honba /. 10.0
  ; remaining_tiles = float_of_int (remaining_wall_tiles state) /. 70.0
  ; num_furos = float_of_int (Array.length player.hand.furos)
  ; is_riichi = if player.is_riichi then 1.0 else 0.0
  ; riichi_sticks = float_of_int state.round.riichi_sticks
  ; own_points = float_of_int player.points /. 50000.0
  }

(** Calculate reward from round result *)
let calculate_reward (result : round_result) (player_idx : int) (state : game_state) : float =
  let n = num_players state.rules in
  match result with
  | Tsumo (winner, scores, _han) ->
      if winner = player_idx then
        1.0 +. float_of_int (List.fold_left (+) 0 scores) /. 10000.0
      else
        let loss = List.nth scores player_idx in
        -. float_of_int loss /. 10000.0
  | Ron (winner, loser, score, _han) ->
      if winner = player_idx then
        1.0 +. float_of_int score /. 10000.0
      else if loser = player_idx then
        -1.0 -. float_of_int score /. 10000.0
      else
        0.0
  | DoubleRon ((w1, _, s1), (w2, _, s2)) ->
      if player_idx = w1 then float_of_int s1 /. 10000.0
      else if player_idx = w2 then float_of_int s2 /. 10000.0
      else -0.5
  | TripleRon -> 0.0
  | Draw (Exhaustive (tenpai, _noten)) ->
      let is_tenpai = List.mem player_idx tenpai in
      let tenpai_count = List.length tenpai in
      if is_tenpai then
        (match tenpai_count with
         | 1 -> 3000.0 /. 10000.0
         | 2 -> 1500.0 /. 10000.0
         | 3 -> 1000.0 /. 10000.0
         | _ -> 0.0)
      else
        (match n - tenpai_count with
         | 1 -> -3000.0 /. 10000.0
         | 2 -> -1500.0 /. 10000.0
         | 3 -> -1000.0 /. 10000.0
         | _ -> 0.0)
  | Draw _ -> 0.0

(** Simple heuristic policy for action selection.
    
    This policy assigns weights to each action based on the current game state
    and features, then uses weighted random sampling to select an action.
    
    Weight assignments:
    - Winning actions (Tsumo/Ron): Highest priority (100.0) - always take a win
    - Draw action: Standard weight (1.0)
    - Discard: Slightly lower (0.8) unless in riichi (forced tsumogiri)
    - Riichi: High weight (10.0) if tenpai and sufficient points, else low (0.1)
    - Calls (Pon/Chi): Moderate weight (0.2-0.3) if few open melds, lower otherwise
      to maintain hand flexibility and yaku options
    - Pass: Medium weight (0.5) to balance between calling and keeping closed
    - Kyuushukyuuhai: Low weight (0.1) as it ends the round without scoring
    
    This is a baseline policy for data generation. For actual gameplay,
    use the neural network interface with a trained model.
*)
let policy_action (features : features) (actions : action list) : action =
  (* Weight actions based on features *)
  let weights = List.map (fun action ->
    match action with
    | DrawAction -> 1.0
    | DiscardAction _ -> 
        if features.is_riichi > 0.0 then 1.0 else 0.8
    | DeclareTsumo -> 100.0  (* High priority for winning *)
    | DeclareRon -> 100.0
    | DeclareRiichi _ ->
        if features.is_tenpai > 0.0 && features.own_points > 0.02 then 10.0 else 0.1
    | CallPon _ -> if features.num_furos < 2.0 then 0.3 else 0.1
    | CallChi _ -> if features.num_furos < 2.0 then 0.2 else 0.05
    | CallKan _ -> 0.2
    | Pass -> 0.5
    | DeclareKyuushuKyuuhai -> 0.1
  ) actions in
  
  let total = List.fold_left (+.) 0.0 weights in
  let r = Random.float total in
  let rec select weights actions acc =
    match (weights, actions) with
    | (w :: ws, a :: as_) ->
        let new_acc = acc +. w in
        if r < new_acc then a
        else select ws as_ new_acc
    | _ -> List.hd actions  (* Fallback *)
  in
  select weights actions 0.0

(** Collect training samples from a game *)
let collect_samples (state : game_state) (choose_action : game_state -> action list -> action) : training_sample list =
  let samples = ref [] in
  let rec loop state =
    let actions = valid_actions state in
    if List.length actions = 0 then !samples
    else
      let player_idx = state.round.current_player in
      let features = extract_features state player_idx in
      let action = choose_action state actions in
      match apply_action state action with
      | Continue new_state ->
          samples := { state_features = features; action_taken = action; reward = 0.0 } :: !samples;
          loop new_state
      | RoundEnd (_new_state, result) ->
          let reward = calculate_reward result player_idx state in
          samples := { state_features = features; action_taken = action; reward } :: !samples;
          (* Update previous samples with discounted reward *)
          let discount = default_config.discount_factor in
          let rec update_rewards lst discount_factor =
            match lst with
            | [] -> []
            | sample :: rest ->
                { sample with reward = sample.reward +. reward *. discount_factor } ::
                update_rewards rest (discount_factor *. discount)
          in
          samples := update_rewards !samples discount;
          !samples
      | GameEnd _ -> !samples
      | Invalid _ -> loop state
  in
  loop state

(** Train for one epoch *)
let train_epoch (config : training_config) : training_stats =
  let wins = Array.make (num_players config.rules) 0 in
  let scores = Array.make (num_players config.rules) 0 in
  let total_rounds = ref 0 in
  
  for _ = 1 to config.num_games do
    let seed = Array.init 17 (fun _ -> Random.bits ()) in
    let final_state = simulate_game config.rules seed config.policy in
    incr total_rounds;
    
    (* Track scores *)
    Array.iteri (fun i p ->
      scores.(i) <- scores.(i) + p.points
    ) final_state.players;
    
    (* Track wins (highest score) *)
    let max_score = Array.fold_left max min_int (Array.map (fun p -> p.points) final_state.players) in
    Array.iteri (fun i p ->
      if p.points = max_score then wins.(i) <- wins.(i) + 1
    ) final_state.players;
  done;
  
  let _n = num_players config.rules in
  let games_f = float_of_int config.num_games in
  
  { games_played = config.num_games
  ; total_rounds = !total_rounds
  ; wins_by_player = wins
  ; avg_score = Array.map (fun s -> float_of_int s /. games_f) scores
  ; avg_game_length = float_of_int !total_rounds /. games_f
  ; tsumo_rate = 0.0  (* Would track in actual implementation *)
  ; ron_rate = 0.0
  ; riichi_rate = 0.0
  ; avg_han = 0.0
  }

(** Save training statistics *)
let save_stats (stats : training_stats) (filename : string) : unit =
  let oc = open_out filename in
  Printf.fprintf oc "Games played: %d\n" stats.games_played;
  Printf.fprintf oc "Total rounds: %d\n" stats.total_rounds;
  Printf.fprintf oc "Avg game length: %.2f\n" stats.avg_game_length;
  Printf.fprintf oc "Wins: %s\n" (String.concat ", " (Array.to_list (Array.map string_of_int stats.wins_by_player)));
  Printf.fprintf oc "Avg scores: %s\n" (String.concat ", " (Array.to_list (Array.map (Printf.sprintf "%.0f") stats.avg_score)));
  close_out oc

(** Main training loop *)
let train (config : training_config) : unit =
  Printf.printf "Starting training with %d games\n" config.num_games;
  Printf.printf "Rules: %d players, %s\n" 
    (num_players config.rules)
    (match config.rules.wind_rounds with EastOnly -> "East only" | HalfGame -> "Half game");
  
  let stats = train_epoch config in
  
  Printf.printf "Training complete.\n";
  Printf.printf "Games: %d, Avg game length: %.2f rounds\n" stats.games_played stats.avg_game_length;
  
  save_stats stats (config.output_dir ^ "/stats.txt")
