(** Game simulation engine for Mahjong *)

open Tiles
open Hand
open Rules
open State

(** Player action *)
type action =
  | DrawAction  (** Draw from wall *)
  | DiscardAction of int  (** Discard tile at index in hand *)
  | CallChi of chi  (** Chi call *)
  | CallPon of tile  (** Pon call *)
  | CallKan of kan_type  (** Kan call *)
  | DeclareRiichi of int  (** Declare riichi and discard tile at index *)
  | DeclareTsumo  (** Declare tsumo win *)
  | DeclareRon  (** Declare ron win *)
  | Pass  (** Pass on call opportunity *)
  | DeclareKyuushuKyuuhai  (** Declare nine terminals draw *)

(** Result of applying an action *)
type action_result =
  | Continue of game_state  (** Game continues *)
  | RoundEnd of game_state * round_result  (** Round ended *)
  | GameEnd of game_state * int array  (** Game ended, final scores *)
  | Invalid of string  (** Invalid action *)

(** Draw a tile from the wall *)
let draw_tile (state : game_state) : (game_state * tile) option =
  if is_wall_exhausted state then None
  else
    let tile = state.round.wall.(state.round.wall_index) in
    let new_round = { state.round with wall_index = state.round.wall_index + 1 } in
    let player = current_player state in
    let new_hand = { player.hand with tiles = Array.append player.hand.tiles [|tile|] } in
    let new_player = { player with hand = new_hand; draws = player.draws + 1 } in
    let new_state = update_player { state with round = new_round } state.round.current_player new_player in
    let event = DrawEvent (state.round.current_player, tile) in
    Some ({ new_state with game_log = event :: new_state.game_log }, tile)

(** Discard a tile from hand *)
let discard_tile (state : game_state) (tile_idx : int) : game_state option =
  let player = current_player state in
  let tiles = player.hand.tiles in
  if tile_idx < 0 || tile_idx >= Array.length tiles then None
  else
    let tile = tiles.(tile_idx) in
    let new_tiles = Array.concat [
      Array.sub tiles 0 tile_idx;
      Array.sub tiles (tile_idx + 1) (Array.length tiles - tile_idx - 1)
    ] in
    let is_tsumogiri = tile_idx = Array.length tiles - 1 in
    let river_tile = { tile; is_riichi = false; is_tsumogiri; is_called = false } in
    let new_hand = { player.hand with tiles = new_tiles } in
    let new_player = { player with 
      hand = new_hand; 
      river = river_tile :: player.river;
      ippatsu = false
    } in
    let new_round = { state.round with 
      last_discard = Some (state.round.current_player, tile);
      is_first_turn = false
    } in
    let event = DiscardEvent (state.round.current_player, tile, is_tsumogiri) in
    let new_state = update_player { state with round = new_round } state.round.current_player new_player in
    Some { new_state with game_log = event :: new_state.game_log }

(** Count tiles in hand matching predicate *)
let count_tiles (tiles : tile array) (pred : tile -> bool) : int =
  Array.fold_left (fun acc t -> if pred t then acc + 1 else acc) 0 tiles

(** Check if two tiles match (ignoring aka) *)
let tiles_match (t1 : tile) (t2 : tile) : bool =
  match (t1, t2) with
  | (Man n1, Man n2) -> int_of_number n1 = int_of_number n2
  | (So n1, So n2) -> int_of_number n1 = int_of_number n2
  | (Pin n1, Pin n2) -> int_of_number n1 = int_of_number n2
  | (Honor h1, Honor h2) -> h1 = h2
  | _ -> false

(** Check if player can pon a tile *)
let can_pon (player : player_state) (tile : tile) : bool =
  count_tiles player.hand.tiles (tiles_match tile) >= 2

(** Check if player can minkan a tile *)
let can_minkan (player : player_state) (tile : tile) : bool =
  count_tiles player.hand.tiles (tiles_match tile) >= 3

(** Check if player can ankan a tile *)
let can_ankan (player : player_state) (tile : tile) : bool =
  count_tiles player.hand.tiles (tiles_match tile) >= 4

(** Calculate score for a tsumo win *)
let calculate_tsumo_score (state : game_state) (_winner_idx : int) (_han : int) (_fu : int) : int array =
  (* Simplified scoring - full implementation would be complex *)
  let n = num_players state.rules in
  Array.make n 0

(** Calculate score for a ron win *)
let calculate_ron_score (_state : game_state) (_winner_idx : int) (_loser_idx : int) (_han : int) (_fu : int) : int =
  (* Simplified scoring - full implementation would be complex *)
  0

(** Simple win detection - checks for 14 tiles with valid structure *)
let is_winning_hand (_hand : hand) : bool =
  (* Placeholder - full implementation requires complex meld detection *)
  false

(** Check for tenpai (waiting for win) *)
let is_tenpai (_hand : hand) : bool =
  (* Placeholder - requires checking all possible waits *)
  false

(** Apply action to game state *)
let apply_action (state : game_state) (action : action) : action_result =
  match action with
  | DrawAction ->
      (match draw_tile state with
       | None -> RoundEnd (state, Draw (Exhaustive ([], [])))
       | Some (new_state, _tile) -> Continue new_state)
  
  | DiscardAction idx ->
      (match discard_tile state idx with
       | None -> Invalid "Invalid tile index"
       | Some new_state -> Continue (advance_player new_state))
  
  | DeclareTsumo ->
      let player = current_player state in
      if is_winning_hand player.hand then
        let scores = calculate_tsumo_score state state.round.current_player 0 0 in
        let han = 0 in (* Would calculate actual han *)
        RoundEnd (state, Tsumo (state.round.current_player, Array.to_list scores, han))
      else
        Invalid "Not a winning hand"
  
  | DeclareRon ->
      (match state.round.last_discard with
       | None -> Invalid "No discard to ron"
       | Some (loser_idx, _tile) ->
           let player = current_player state in
           if is_winning_hand player.hand then
             let score = calculate_ron_score state state.round.current_player loser_idx 0 0 in
             RoundEnd (state, Ron (state.round.current_player, loser_idx, score, 0))
           else
             Invalid "Not a winning hand")
  
  | DeclareRiichi idx ->
      let player = current_player state in
      if player.is_riichi then Invalid "Already in riichi"
      else if player.points < state.rules.riichi.riichi_bet then Invalid "Not enough points"
      else if Array.length player.hand.furos > 0 then Invalid "Cannot riichi with open hand"
      else
        (match discard_tile state idx with
         | None -> Invalid "Invalid tile index"
         | Some new_state ->
             let new_player = { (new_state.players.(state.round.current_player)) with
               is_riichi = true;
               riichi_turn = Some state.round.turn;
               is_double_riichi = state.round.is_first_turn;
               ippatsu = true;
               points = new_state.players.(state.round.current_player).points - state.rules.riichi.riichi_bet
             } in
             let new_state = update_player new_state state.round.current_player new_player in
             let new_round = { new_state.round with riichi_sticks = new_state.round.riichi_sticks + 1 } in
             Continue (advance_player { new_state with round = new_round }))
  
  | CallPon tile ->
      (match state.round.last_discard with
       | None -> Invalid "No discard to pon"
       | Some (target_idx, discard_tile) ->
           if not (tiles_match tile discard_tile) then Invalid "Tile doesn't match discard"
           else
             let player = current_player state in
             if not (can_pon player tile) then Invalid "Cannot pon"
             else
               (* Remove 2 matching tiles from hand, add pon to furos *)
               let n = num_players state.rules in
               let source = 
                 let diff = (state.round.current_player - target_idx + n) mod n in
                 match diff with
                 | 1 -> Kami
                 | 2 -> Toimen
                 | _ -> Shimo
               in
               let new_furo = Pon (tile, source) in
               let new_furos = Array.append player.hand.furos [|new_furo|] in
               (* Simplified: just remove 2 tiles - proper impl would track which ones *)
               let remaining = ref 2 in
               let new_tiles = Array.of_list (List.filter (fun t ->
                 if !remaining > 0 && tiles_match t tile then begin
                   remaining := !remaining - 1;
                   false
                 end else true
               ) (Array.to_list player.hand.tiles)) in
               let new_hand = { player.hand with tiles = new_tiles; furos = new_furos } in
               let new_player = { player with hand = new_hand; ippatsu = false } in
               let new_state = update_player state state.round.current_player new_player in
               let new_round = { new_state.round with 
                 last_discard = None;
                 is_first_turn = false
               } in
               Continue { new_state with round = new_round })
  
  | Pass ->
      (* Pass on call opportunity - advance to next player *)
      Continue (advance_player state)
  
  | DeclareKyuushuKyuuhai ->
      if state.round.turn = 0 && state.round.is_first_turn then
        let player = current_player state in
        (* Count terminal and honor tiles *)
        let terminal_count = count_tiles player.hand.tiles (fun t ->
          match t with
          | Man One | Man Nine | So One | So Nine | Pin One | Pin Nine -> true
          | Honor _ -> true
          | _ -> false
        ) in
        if terminal_count >= 9 then
          RoundEnd (state, Draw (NineTerminals state.round.current_player))
        else
          Invalid "Not enough terminals/honors"
      else
        Invalid "Cannot declare after first turn"
  
  | CallChi _ -> Invalid "Chi not fully implemented"
  | CallKan _ -> Invalid "Kan not fully implemented"

(** Get valid actions for current player *)
let valid_actions (state : game_state) : action list =
  let player = current_player state in
  let actions = ref [] in

  let hand_size = Array.length player.hand.tiles in
  let num_furos = Array.length player.hand.furos in
  let base_hand_size = 13 - (3 * num_furos) in

  (* Draw if hand is at or below base size (initial deal or turn draw) *)
  if not (is_wall_exhausted state) && hand_size <= base_hand_size then
    actions := DrawAction :: !actions;

  (* Discard if hand has tiles and is above base size (i.e., just drew) *)
  if hand_size > base_hand_size then begin
    for i = 0 to hand_size - 1 do
      if not player.is_riichi || i = hand_size - 1 then
        actions := DiscardAction i :: !actions
    done
  end;
  
  (* Riichi if possible *)
  if not player.is_riichi && 
     Array.length player.hand.furos = 0 && 
     player.points >= state.rules.riichi.riichi_bet then begin
    for i = 0 to hand_size - 1 do
      actions := DeclareRiichi i :: !actions
    done
  end;
  
  (* Check for calls on last discard *)
  (match state.round.last_discard with
   | Some (_idx, tile) when state.round.current_player <> _idx ->
       (* Can pass on call opportunities *)
       actions := Pass :: !actions;
       if can_pon player tile then
         actions := CallPon tile :: !actions;
       if can_minkan player tile then
         actions := CallKan (Minkan (_idx, tile)) :: !actions
   | _ -> ());
  
  (* Ankan *)
  Array.iter (fun tile ->
    if can_ankan player tile && not player.is_riichi then
      actions := CallKan (Ankan tile) :: !actions
  ) player.hand.tiles;
  
  !actions

(** Maximum number of invalid action retries before fallback *)
let max_invalid_retries = 100

(** Simulate a single round *)
let rec simulate_round_impl (state : game_state) (choose_action : game_state -> action list -> action) (invalid_count : int) : game_state * round_result =
  if invalid_count >= max_invalid_retries then
    (* Fallback: force draw game if too many invalid actions *)
    (state, Draw (Exhaustive ([], [])))
  else
    let actions = valid_actions state in
    if List.length actions = 0 then
      (* No valid actions - draw game *)
      (state, Draw (Exhaustive ([], [])))
    else
      let action = choose_action state actions in
      match apply_action state action with
      | Continue new_state -> simulate_round_impl new_state choose_action 0
      | RoundEnd (new_state, result) -> (new_state, result)
      | GameEnd (new_state, scores) -> 
          (new_state, Draw (Exhaustive ([], Array.to_list scores)))
      | Invalid msg -> 
          Printf.printf "Invalid action: %s (retry %d)\n" msg invalid_count;
          simulate_round_impl state choose_action (invalid_count + 1)

(** Simulate a single round *)
let simulate_round (state : game_state) (choose_action : game_state -> action list -> action) : game_state * round_result =
  simulate_round_impl state choose_action 0

(** Random action selection (for basic simulation) *)
let random_action (_state : game_state) (actions : action list) : action =
  let idx = Random.int (List.length actions) in
  List.nth actions idx

(** Simulate a complete game *)
let simulate_game (rules : rules) (seed : int array) (choose_action : game_state -> action list -> action) : game_state =
  let state = init_game_state rules seed in
  let rec loop state rounds_played =
    if rounds_played >= total_rounds rules then state
    else
      let (new_state, _result) = simulate_round state choose_action in
      (* Advance to next round - simplified *)
      let new_round = { new_state.round with 
        round_num = new_state.round.round_num + 1;
        turn = 0;
        wall_index = 0
      } in
      loop { new_state with round = new_round } (rounds_played + 1)
  in
  loop state 0
