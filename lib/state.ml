(** Game state management and action validation.

    This module provides core state management for tracking game state
    and validating player actions based on MJAI events.

    Uses mutable state for performance (matches Rust libriichi implementation).
*)

(** Action candidate representing possible actions from current state *)
type action_candidate = {
  can_discard : bool;
  can_chi_low : bool;
  can_chi_mid : bool;
  can_chi_high : bool;
  can_pon : bool;
  can_daiminkan : bool;
  can_kakan : bool;
  can_ankan : bool;
  can_riichi : bool;
  can_tsumo_agari : bool;
  can_ron_agari : bool;
  can_ryukyoku : bool;
  target_actor : int;
}

(** Default action candidate (no actions available) *)
let default_action_candidate : action_candidate = {
  can_discard = false;
  can_chi_low = false;
  can_chi_mid = false;
  can_chi_high = false;
  can_pon = false;
  can_daiminkan = false;
  can_kakan = false;
  can_ankan = false;
  can_riichi = false;
  can_tsumo_agari = false;
  can_ron_agari = false;
  can_ryukyoku = false;
  target_actor = 0;
}

(** Check if any chi action is available *)
let can_chi (cans : action_candidate) : bool =
  cans.can_chi_low || cans.can_chi_mid || cans.can_chi_high

(** Check if any kan action is available *)
let can_kan (cans : action_candidate) : bool =
  cans.can_daiminkan || cans.can_kakan || cans.can_ankan

(** Check if any agari action is available *)
let can_agari (cans : action_candidate) : bool =
  cans.can_tsumo_agari || cans.can_ron_agari

(** Check if pass action is available *)
let can_pass (cans : action_candidate) : bool =
  can_chi cans || cans.can_pon || cans.can_daiminkan || cans.can_ron_agari

(** Check if any action is available *)
let can_act (cans : action_candidate) : bool =
  cans.can_discard
  || can_chi cans
  || cans.can_pon
  || can_kan cans
  || cans.can_riichi
  || can_agari cans
  || cans.can_ryukyoku

(** Player game state *)
type player_state = {
  player_id : int;
  (* Tiles in hand (34-element array) *)
  mutable tehai : int array;
  (* Game state *)
  mutable bakaze : int;
  mutable jikaze : int;
  mutable kyoku : int;
  mutable honba : int;
  mutable kyotaku : int;
  mutable oya : int;
  mutable scores : int array;
  mutable tiles_left : int;
  (* Flags *)
  mutable riichi_declared : bool array;
  mutable riichi_accepted : bool array;
  mutable is_menzen : bool;
  mutable can_w_riichi : bool;
  mutable is_w_riichi : bool;
  mutable at_rinshan : bool;
  mutable at_ippatsu : bool;
  (* Turn info *)
  mutable at_turn : int;
  (* Last action info *)
  mutable last_self_tsumo : int option;
  mutable last_kawa_tile : int option;
  mutable last_cans : action_candidate;
  (* Kan tracking *)
  mutable kans_on_board : int;
  mutable chis : int list;
  mutable pons : int list;
  mutable minkans : int list;
  mutable ankans : int list;
  mutable ankan_candidates : int list;
  mutable kakan_candidates : int list;
  (* Advanced state tracking *)
  mutable shanten : int;
  mutable waits : bool array;
  mutable at_furiten : bool;
  mutable to_mark_same_cycle_furiten : bool;
  mutable chankan_chance : bool;
  mutable has_next_shanten_discard : bool;
  mutable keep_shanten_discards : bool array;
  mutable next_shanten_discards : bool array;
  mutable forbidden_tiles : bool array;
  mutable tehai_len_div3 : int;
  mutable tiles_seen : int array;
  mutable discarded_tiles : bool array;
  (* Dora tracking *)
  mutable dora_indicators : int list;  (* List of dora indicator tiles *)
  mutable dora_factor : int array;  (* Maps each tile to its dora count *)
  mutable doras_owned : int array;  (* Count of dora tiles in hand *)
  mutable doras_seen : int;  (* Total visible dora count *)
  (* Red tile (aka) tracking *)
  mutable akas_in_hand : bool array;  (* [5mr, 5pr, 5sr] presence in hand *)
  (* Meld overview (fuuro_overview) - tracks all melds with constituent tiles *)
  (* For each player (0-3), list of melds, each meld is a list of tiles *)
  mutable fuuro_overview : int list list array;  (* [player][meld][tile] *)
}

(** Create initial player state *)
let create_player_state (player_id : int) : player_state =
  assert (player_id >= 0 && player_id < 4);
  {
    player_id;
    tehai = Array.make 34 0;
    bakaze = 27;  (* E *)
    jikaze = 27 + player_id;
    kyoku = 0;
    honba = 0;
    kyotaku = 0;
    oya = 0;
    scores = Array.make 4 25000;
    tiles_left = 70;  (* Initial wall size *)
    riichi_declared = Array.make 4 false;
    riichi_accepted = Array.make 4 false;
    is_menzen = true;
    can_w_riichi = false;
    is_w_riichi = false;
    at_rinshan = false;
    at_ippatsu = false;
    at_turn = 0;
    last_self_tsumo = None;
    last_kawa_tile = None;
    last_cans = default_action_candidate;
    kans_on_board = 0;
    chis = [];
    pons = [];
    minkans = [];
    ankans = [];
    ankan_candidates = [];
    kakan_candidates = [];
    (* Advanced state tracking *)
    shanten = 8;  (* Max shanten *)
    waits = Array.make 34 false;
    at_furiten = false;
    to_mark_same_cycle_furiten = false;
    chankan_chance = false;
    has_next_shanten_discard = false;
    keep_shanten_discards = Array.make 34 false;
    next_shanten_discards = Array.make 34 false;
    forbidden_tiles = Array.make 34 false;
    tehai_len_div3 = 0;
    tiles_seen = Array.make 34 0;
    discarded_tiles = Array.make 34 false;
    (* Dora tracking *)
    dora_indicators = [];
    dora_factor = Array.make 34 0;
    doras_owned = Array.make 4 0;
    doras_seen = 0;
    (* Red tile tracking *)
    akas_in_hand = [|false; false; false|];
    (* Meld overview - 4 players, each with empty meld list *)
    fuuro_overview = [|[]; []; []; []|];
  }

(** Validate if a tile is in hand *)
let tile_in_hand (state : player_state) (tile : int) : bool =
  let tile_idx = Tiles.deaka tile in
  if tile_idx >= 0 && tile_idx < 34 then
    state.tehai.(tile_idx) > 0
  else
    false

(** Validate reaction to current state *)
let validate_reaction (state : player_state) (action : Mjai.event) : (unit, string) result =
  let cans = state.last_cans in

  match action with
  | Mjai.Ryukyoku _ ->
      if cans.can_ryukyoku then Ok ()
      else Error "cannot ryukyoku"

  | Mjai.None -> Ok ()

  | _ ->
      (* Check actor matches player_id *)
      let actor_ok =
        match Mjai.actor action with
        | Some actor when actor = state.player_id -> Ok ()
        | Some actor -> Error (Printf.sprintf "actor is %d, not self (%d)" actor state.player_id)
        | None -> Error "action does not have actor"
      in

      match actor_ok with
      | Error _ as e -> e
      | Ok () ->
      match action with
      | Mjai.Dahai { pai; tsumogiri; _ } ->
          if not cans.can_discard then
            Error "cannot discard"
          else if not (tile_in_hand state pai) then
            Error (Printf.sprintf "tile %d not in hand" pai)
          else if tsumogiri then
            (match state.last_self_tsumo with
             | Some tile when tile = pai -> Ok ()
             | Some _ -> Error "cannot tsumogiri different tile"
             | None -> Error "tsumogiri but no tsumo recorded")
          else
            Ok ()

      | Mjai.Reach _ ->
          if cans.can_riichi then Ok ()
          else Error "cannot riichi"

      | Mjai.Chi { actor; target; _ } ->
          if (target + 1) mod 4 <> actor then
            Error "chi from non-kamicha"
          else if not (can_chi cans) then
            Error "cannot chi"
          else
            Ok ()

      | Mjai.Pon { target; _ } ->
          if target = state.player_id then
            Error "pon from itself"
          else if not cans.can_pon then
            Error "cannot pon"
          else
            Ok ()

      | Mjai.Daiminkan { target; _ } ->
          if target = state.player_id then
            Error "daiminkan from itself"
          else if not cans.can_daiminkan then
            Error "cannot daiminkan"
          else
            Ok ()

      | Mjai.Kakan { pai; _ } ->
          if not cans.can_kakan then
            Error "cannot kakan"
          else if tile_in_hand state pai then
            Ok ()
          else
            Error (Printf.sprintf "kakan: tile %d not in hand" pai)

      | Mjai.Ankan _ ->
          if not cans.can_ankan then
            Error "cannot ankan"
          else
            Ok ()

      | Mjai.Hora { target; _ } ->
          if target = state.player_id then
            if cans.can_tsumo_agari then Ok ()
            else Error "cannot tsumo agari"
          else
            if cans.can_ron_agari then Ok ()
            else Error "cannot ron agari"

      | _ -> Error "unexpected action"

(** Start a new kyoku (round) *)
let start_kyoku (state : player_state)
  (bakaze : int) (kyoku : int) (honba : int) (kyotaku : int)
  (oya : int) (scores : int array) (tehais : int array array) : unit =
  let actor_tehai = tehais.(state.player_id) in
  state.bakaze <- bakaze;
  state.jikaze <- 27 + ((state.player_id + oya) mod 4);
  state.kyoku <- kyoku;
  state.honba <- honba;
  state.kyotaku <- kyotaku;
  state.oya <- oya;
  state.scores <- Array.copy scores;
  (* Reset and populate tehai *)
  state.tehai <- Array.make 34 0;
  Array.iter (fun tile ->
    let idx = Tiles.deaka tile in
    if idx >= 0 && idx < 34 then state.tehai.(idx) <- state.tehai.(idx) + 1
  ) actor_tehai;
  (* Reset flags *)
  state.riichi_declared <- Array.make 4 false;
  state.riichi_accepted <- Array.make 4 false;
  state.is_menzen <- true;
  state.is_w_riichi <- false;
  state.at_rinshan <- false;
  state.at_ippatsu <- false;
  state.last_self_tsumo <- None;
  state.kans_on_board <- 0;
  state.chis <- [];
  state.pons <- [];
  state.minkans <- [];
  state.ankans <- [];
  state.ankan_candidates <- [];
  state.kakan_candidates <- [];
  state.tiles_left <- 70;
  (* Reset advanced tracking *)
  state.tehai_len_div3 <- Array.fold_left (+) 0 state.tehai / 3;
  state.shanten <- 8;
  state.waits <- Array.make 34 false;
  state.at_furiten <- false;
  state.to_mark_same_cycle_furiten <- false;
  state.chankan_chance <- false;
  state.has_next_shanten_discard <- false;
  state.keep_shanten_discards <- Array.make 34 false;
  state.next_shanten_discards <- Array.make 34 false;
  state.forbidden_tiles <- Array.make 34 false;
  state.tiles_seen <- Array.make 34 0;
  state.discarded_tiles <- Array.make 34 false;
  (* Reset dora tracking *)
  state.dora_indicators <- [];
  Array.fill state.dora_factor 0 34 0;
  Array.fill state.doras_owned 0 4 0;
  state.doras_seen <- 0;
  (* Reset red tile tracking *)
  state.akas_in_hand <- [|false; false; false|];
  (* Reset meld overview *)
  state.fuuro_overview <- [|[]; []; []; []|]

(** Update shanten for current hand *)
let update_shanten (state : player_state) : unit =
  state.shanten <- Shanten.calc_all state.tehai state.tehai_len_div3

(** Update waits and furiten state *)
let update_waits_and_furiten (state : player_state) : unit =
  (* Reset furiten and waits *)
  state.at_furiten <- false;
  Array.fill state.waits 0 34 false;

  if state.shanten > 0 then
    ()
  else begin
    (* Check each tile as a potential wait *)
    for tile_idx = 0 to 33 do
      if state.tehai.(tile_idx) < 4 then begin
        (* Simulate adding this tile to hand *)
        let new_tehai = Array.copy state.tehai in
        new_tehai.(tile_idx) <- new_tehai.(tile_idx) + 1;

        (* Check if this completes the hand *)
        if Shanten.calc_all new_tehai state.tehai_len_div3 = -1 then begin
          (* Check for furiten: is this tile in discarded_tiles? *)
          if state.discarded_tiles.(tile_idx) then
            state.at_furiten <- true;
          (* Only a wait if we haven't seen all 4 of this tile *)
          state.waits.(tile_idx) <- state.tiles_seen.(tile_idx) < 4
        end
      end
    done
  end

(** Calculate real-time shanten considering 3n+2 hands *)
let real_time_shanten (state : player_state) : int =
  if not state.last_cans.can_discard then
    (* 3n+1, state.shanten is accurate *)
    state.shanten
  else if state.shanten > 0 then
    (* 3n+2, not tenpai *)
    state.shanten
  else
    (* 3n+2, tenpai - calculate actual shanten *)
    Shanten.calc_all state.tehai state.tehai_len_div3

(** Add a dora indicator and update dora tracking *)
let add_dora_indicator (state : player_state) (tile : int) : unit =
  let tile_idx = Tiles.deaka tile in
  if tile_idx >= 0 && tile_idx < 34 then begin
    (* Add to indicators list *)
    state.dora_indicators <- state.dora_indicators @ [tile];

    (* Get the dora tile (next tile after indicator) *)
    let dora_tile = Tiles.next tile in
    let dora_idx = Tiles.deaka dora_tile in

    if dora_idx >= 0 && dora_idx < 34 then begin
      (* Increment dora factor for this tile *)
      state.dora_factor.(dora_idx) <- state.dora_factor.(dora_idx) + 1;

      (* Count dora tiles in hand *)
      state.doras_owned.(0) <- state.doras_owned.(0) + state.tehai.(dora_idx);

      (* TODO: Count dora tiles in melds - requires fuuro_overview *)
    end
  end

(** Handle tsumo event *)
let tsumo (state : player_state) (actor : int) (pai : int) : unit =
  (* Clear kan candidates *)
  state.ankan_candidates <- [];
  state.kakan_candidates <- [];

  if actor = state.player_id then begin
    let idx = Tiles.deaka pai in
    if idx >= 0 && idx < 34 then begin
      (* Decrement tiles left *)
      if state.tiles_left > 0 then
        state.tiles_left <- state.tiles_left - 1;

      state.tehai.(idx) <- state.tehai.(idx) + 1;
      state.last_self_tsumo <- Some pai;
      (* Update akas_in_hand tracking *)
      (match pai with
       | t when t = Tiles.tile_id_5mr -> state.akas_in_hand.(0) <- true
       | t when t = Tiles.tile_id_5pr -> state.akas_in_hand.(1) <- true
       | t when t = Tiles.tile_id_5sr -> state.akas_in_hand.(2) <- true
       | _ -> ());
      (* Update advanced tracking after drawing *)
      state.tehai_len_div3 <- (Array.fold_left (+) 0 state.tehai) / 3;
      update_shanten state;
      update_waits_and_furiten state;

      (* Check for tsumo agari *)
      state.last_cans <- { state.last_cans with can_discard = true };
      if state.shanten = -1 then
        state.last_cans <- { state.last_cans with can_tsumo_agari = true };

      (* haitei tile (last tile) cannot be used for kan *)
      if state.tiles_left > 0 then begin
        (* Populate kan candidates if not in riichi or before riichi *)
        if not state.riichi_accepted.(0) && state.kans_on_board < 4 then begin
          (* Find ankan candidates (4 of a kind) *)
          for tile_idx = 0 to 33 do
            if state.tehai.(tile_idx) = 4 then begin
              state.last_cans <- { state.last_cans with can_ankan = true };
              state.ankan_candidates <- state.ankan_candidates @ [tile_idx]
            end
          done;

          (* Find kakan candidates (have pon and 4th tile in hand) *)
          List.iter (fun pon_tile ->
            let pon_idx = Tiles.deaka pon_tile in
            if pon_idx >= 0 && pon_idx < 34 && state.tehai.(pon_idx) > 0 then begin
              state.last_cans <- { state.last_cans with can_kakan = true };
              state.kakan_candidates <- state.kakan_candidates @ [pon_idx]
            end
          ) state.pons
        end;

        (* Check if can declare riichi *)
        state.last_cans <- { state.last_cans with
          can_riichi = state.is_menzen
            && state.tiles_left >= 4
            && state.scores.(0) >= 1000
            && state.shanten = 0
        }
      end
    end
  end

(** Check if chi is possible with a given tile *)
let set_can_chi_from_tile (state : player_state) (tile : int) : unit =
  let can_chi_low = ref false in
  let can_chi_mid = ref false in
  let can_chi_high = ref false in

  let tile_id = Tiles.deaka tile in
  if tile_id >= 0 && tile_id < 27 then begin  (* Chi only for numbered suits *)
    let literal_num = tile_id mod 9 + 1 in  (* 1-9 *)

    (* Check low chi: [tile, tile+1, tile+2] *)
    if literal_num <= 7 && tile_id + 2 < 34 then begin
      (* Check if we have the required tiles *)
      if state.tehai.(tile_id + 1) > 0 && state.tehai.(tile_id + 2) > 0 then begin
        (* Simulate removing the chi tiles from hand *)
        let tehai_after = Array.copy state.tehai in
        (* Set tile_id to 0 to prevent "cheating" - see Rust comment about 1111234 case *)
        tehai_after.(tile_id) <- 0;
        tehai_after.(tile_id + 1) <- tehai_after.(tile_id + 1) - 1;
        tehai_after.(tile_id + 2) <- tehai_after.(tile_id + 2) - 1;
        (* If literal_num < 7, also zero out tile_id + 3 to prevent using it for chi *)
        if literal_num < 7 && tile_id + 3 < 34 then
          tehai_after.(tile_id + 3) <- 0;
        (* Chi is possible if there are still tiles in hand *)
        can_chi_low := Array.fold_left (fun acc x -> if x > 0 then acc + 1 else acc) 0 tehai_after > 0
      end
    end;

    (* Check mid chi: [tile-1, tile, tile+1] *)
    if literal_num >= 2 && literal_num <= 8 && tile_id - 1 >= 0 && tile_id + 1 < 34 then begin
      if state.tehai.(tile_id - 1) > 0 && state.tehai.(tile_id + 1) > 0 then begin
        let tehai_after = Array.copy state.tehai in
        (* Set tile_id to 0 to prevent "cheating" *)
        tehai_after.(tile_id) <- 0;
        tehai_after.(tile_id - 1) <- tehai_after.(tile_id - 1) - 1;
        tehai_after.(tile_id + 1) <- tehai_after.(tile_id + 1) - 1;
        can_chi_mid := Array.fold_left (fun acc x -> if x > 0 then acc + 1 else acc) 0 tehai_after > 0
      end
    end;

    (* Check high chi: [tile-2, tile-1, tile] *)
    if literal_num >= 3 && tile_id - 2 >= 0 then begin
      if state.tehai.(tile_id - 2) > 0 && state.tehai.(tile_id - 1) > 0 then begin
        let tehai_after = Array.copy state.tehai in
        (* Set tile_id to 0 to prevent "cheating" *)
        tehai_after.(tile_id) <- 0;
        tehai_after.(tile_id - 2) <- tehai_after.(tile_id - 2) - 1;
        tehai_after.(tile_id - 1) <- tehai_after.(tile_id - 1) - 1;
        (* If literal_num > 3, also zero out tile_id - 3 *)
        if literal_num > 3 && tile_id - 3 >= 0 then
          tehai_after.(tile_id - 3) <- 0;
        can_chi_high := Array.fold_left (fun acc x -> if x > 0 then acc + 1 else acc) 0 tehai_after > 0
      end
    end
  end;

  (* Update last_cans with new chi flags *)
  state.last_cans <- { state.last_cans with
    can_chi_low = !can_chi_low;
    can_chi_mid = !can_chi_mid;
    can_chi_high = !can_chi_high;
  }

(** Handle dahai (discard) event *)
let dahai (state : player_state) (actor : int) (pai : int) (tsumogiri : bool) : unit =
  state.last_kawa_tile <- Some pai;

  if actor = state.player_id then begin
    let idx = Tiles.deaka pai in
    if idx >= 0 && idx < 34 && state.tehai.(idx) > 0 then begin
      state.tehai.(idx) <- state.tehai.(idx) - 1;
      (* Update akas_in_hand tracking if discarding an aka *)
      (match pai with
       | t when t = Tiles.tile_id_5mr -> state.akas_in_hand.(0) <- false
       | t when t = Tiles.tile_id_5pr -> state.akas_in_hand.(1) <- false
       | t when t = Tiles.tile_id_5sr -> state.akas_in_hand.(2) <- false
       | _ -> ());
      (* Track discarded tiles for furiten calculation *)
      state.discarded_tiles.(idx) <- true;
      if tsumogiri then
        state.last_self_tsumo <- None;
      (* Reset flags after discard *)
      state.at_rinshan <- false;
      state.at_ippatsu <- false;
      state.can_w_riichi <- false;
      (* Update advanced tracking after discarding *)
      state.tehai_len_div3 <- (Array.fold_left (+) 0 state.tehai) / 3;
      update_shanten state;
      update_waits_and_furiten state
    end
  end else begin
    (* Another player discarded - check if we can react *)
    if not state.riichi_accepted.(0) && state.tiles_left > 0 then begin
      let idx = Tiles.deaka pai in
      (* Check for chi (only from kamicha = actor + 1) *)
      let relative_pos = (actor - state.player_id + 4) mod 4 in
      if relative_pos = 3 && idx < 27 && state.tehai_len_div3 > 0 then
        set_can_chi_from_tile state pai;

      (* Check for pon *)
      if idx >= 0 && idx < 34 then begin
        state.last_cans <- { state.last_cans with
          can_pon = state.tehai.(idx) >= 2;
          can_daiminkan = (state.kans_on_board < 4 && state.tehai.(idx) = 3);
        }
      end;

      (* Check for ron agari *)
      if state.shanten = 0 && state.waits.(idx) && not state.at_furiten then
        state.last_cans <- { state.last_cans with can_ron_agari = true }
    end
  end

(** Handle chi (sequence meld) event *)
let chi (state : player_state) (actor : int) (pai : int) (consumed : int array) : unit =
  if actor = state.player_id then begin
    (* Remove consumed tiles from hand *)
    Array.iter (fun tile ->
      let idx = Tiles.deaka tile in
      if idx >= 0 && idx < 34 && state.tehai.(idx) > 0 then
        state.tehai.(idx) <- state.tehai.(idx) - 1
    ) consumed;
    (* Add chi to state *)
    state.chis <- state.chis @ [pai];
    state.is_menzen <- false;
    (* Update fuuro_overview with the complete meld (including called tile) *)
    (* consumed contains 2 tiles from hand, pai is the called tile *)
    let meld_tiles = Array.to_list consumed @ [pai] in
    state.fuuro_overview.(state.player_id) <-
      state.fuuro_overview.(state.player_id) @ [meld_tiles]
  end

(** Handle pon (triplet meld) event *)
let pon (state : player_state) (actor : int) (pai : int) (consumed : int array) : unit =
  if actor = state.player_id then begin
    (* Remove consumed tiles from hand *)
    Array.iter (fun tile ->
      let idx = Tiles.deaka tile in
      if idx >= 0 && idx < 34 && state.tehai.(idx) > 0 then
        state.tehai.(idx) <- state.tehai.(idx) - 1
    ) consumed;
    (* Add pon to state *)
    state.pons <- state.pons @ [pai];
    state.is_menzen <- false;
    (* Update fuuro_overview with the complete meld *)
    (* consumed contains 2 tiles from hand, pai is the called tile *)
    let meld_tiles = Array.to_list consumed @ [pai] in
    state.fuuro_overview.(state.player_id) <-
      state.fuuro_overview.(state.player_id) @ [meld_tiles]
  end

(** Handle reach (riichi) declaration *)
let reach (state : player_state) (actor : int) : unit =
  if actor = state.player_id then
    state.riichi_declared.(actor) <- true

(** Handle reach_accepted *)
let reach_accepted (state : player_state) (actor : int) : unit =
  state.riichi_accepted.(actor) <- true

(** Handle ankan (closed kan from hand) *)
let ankan (state : player_state) (actor : int) (consumed : int array) : unit =
  if actor = state.player_id then begin
    let tile = Tiles.deaka consumed.(0) in
    (* Remove 4 tiles from hand *)
    Array.iter (fun t ->
      let idx = Tiles.deaka t in
      if idx >= 0 && idx < 34 && state.tehai.(idx) > 0 then
        state.tehai.(idx) <- state.tehai.(idx) - 1
    ) consumed;
    (* Add to ankans list *)
    state.ankans <- state.ankans @ [tile];
    state.kans_on_board <- state.kans_on_board + 1;
    state.tehai_len_div3 <- state.tehai_len_div3 - 1;
    (* Update tracking *)
    update_shanten state;
    update_waits_and_furiten state
  end

(** Handle kakan (pon → kan) *)
let kakan (state : player_state) (actor : int) (pai : int) : unit =
  if actor = state.player_id then begin
    let tile_idx = Tiles.deaka pai in
    (* Remove tile from hand *)
    if tile_idx >= 0 && tile_idx < 34 && state.tehai.(tile_idx) > 0 then begin
      state.tehai.(tile_idx) <- state.tehai.(tile_idx) - 1;
      (* Remove from pons, add to minkans *)
      state.pons <- List.filter (fun t -> Tiles.deaka t <> tile_idx) state.pons;
      state.minkans <- state.minkans @ [tile_idx];
      state.kans_on_board <- state.kans_on_board + 1;
      (* Update fuuro_overview: add tile to existing pon meld *)
      let player_fuuro = state.fuuro_overview.(state.player_id) in
      let updated_fuuro =
        List.map (fun meld ->
          match meld with
          | hd :: _ when Tiles.deaka hd = tile_idx -> meld @ [pai]
          | _ -> meld
        ) player_fuuro
      in
      state.fuuro_overview.(state.player_id) <- updated_fuuro;
      (* Update tracking *)
      update_shanten state;
      update_waits_and_furiten state
    end
  end

(** Handle daiminkan (closed kan → open kan) *)
let daiminkan (state : player_state) (actor : int) (pai : int) (consumed : int array) : unit =
  if actor = state.player_id then begin
    (* Remove 3 tiles from hand *)
    Array.iter (fun tile ->
      let idx = Tiles.deaka tile in
      if idx >= 0 && idx < 34 && state.tehai.(idx) > 0 then
        state.tehai.(idx) <- state.tehai.(idx) - 1
    ) consumed;
    (* Add to minkans *)
    let tile_idx = Tiles.deaka pai in
    state.minkans <- state.minkans @ [tile_idx];
    state.kans_on_board <- state.kans_on_board + 1;
    state.is_menzen <- false;
    state.tehai_len_div3 <- state.tehai_len_div3 - 1;
    (* Update fuuro_overview *)
    let meld_tiles = Array.to_list consumed @ [pai] in
    state.fuuro_overview.(state.player_id) <-
      state.fuuro_overview.(state.player_id) @ [meld_tiles];
    (* Update tracking *)
    update_shanten state;
    update_waits_and_furiten state
  end


(** Main update function - process MJAI event and update state *)
let update (state : player_state) (event : Mjai.event) : unit =
  (* Determine new action candidates based on event *)
  let new_cans =
    match event with
    | Mjai.Start_game _ ->
        default_action_candidate

    | Mjai.Start_kyoku { bakaze; kyoku; honba; kyotaku; oya; dora_marker; scores; tehais } ->
        start_kyoku state bakaze kyoku honba kyotaku oya scores tehais;
        add_dora_indicator state dora_marker;
        { default_action_candidate with target_actor = state.player_id }

    | Mjai.Tsumo { actor; pai } ->
        tsumo state actor pai;
        { default_action_candidate with
          target_actor = actor;
          can_discard = true;
          can_riichi = state.is_menzen;
          can_tsumo_agari = true;
        }

    | Mjai.Dahai { actor; pai; tsumogiri } ->
        dahai state actor pai tsumogiri;
        (* After discard, check if next player can act *)
        let next_actor = (actor + 1) mod 4 in
        if next_actor = state.player_id then
          { default_action_candidate with target_actor = next_actor }
        else
          { default_action_candidate with
            target_actor = actor;
            can_pon = true;
            can_daiminkan = true;
            can_ron_agari = true;
          }

    | Mjai.Chi { actor; pai; consumed; _ } ->
        chi state actor pai consumed;
        { default_action_candidate with
          target_actor = actor;
          can_discard = true;
        }

    | Mjai.Pon { actor; pai; consumed; target=_ } ->
        pon state actor pai consumed;
        { default_action_candidate with
          target_actor = actor;
          can_discard = true;
        }

    | Mjai.Reach { actor } ->
        reach state actor;
        { default_action_candidate with target_actor = actor }

    | Mjai.Reach_accepted { actor } ->
        reach_accepted state actor;
        default_action_candidate

    | Mjai.Hora { target; _ } ->
        if target = state.player_id then
          { default_action_candidate with can_tsumo_agari = true }
        else
          { default_action_candidate with can_ron_agari = true }

    | Mjai.Ryukyoku _ ->
        { default_action_candidate with can_ryukyoku = true }

    | Mjai.Dora { dora_marker } ->
        add_dora_indicator state dora_marker;
        default_action_candidate

    | Mjai.End_kyoku | Mjai.End_game | Mjai.None ->
        default_action_candidate

    | Mjai.Ankan { actor; consumed } ->
        ankan state actor consumed;
        { default_action_candidate with target_actor = actor }

    | Mjai.Kakan { actor; pai; consumed=_ } ->
        kakan state actor pai;
        { default_action_candidate with target_actor = actor }

    | Mjai.Daiminkan { actor; target=_; pai; consumed } ->
        daiminkan state actor pai consumed;
        { default_action_candidate with target_actor = actor }
  in

  (* Update last_cans in state *)
  state.last_cans <- new_cans

(** {1 Getter Functions}

    These functions provide read-only access to player state fields.
    Corresponds to the getter module in Rust libriichi. *)

(** Get player ID *)
let player_id (state : player_state) : int = state.player_id

(** Get current round number (kyoku) *)
let kyoku (state : player_state) : int = state.kyoku

(** Get number of repeat counters (honba) *)
let honba (state : player_state) : int = state.honba

(** Get number of riichi bets (kyotaku) *)
let kyotaku (state : player_state) : int = state.kyotaku

(** Check if player is dealer (oya) *)
let is_oya (state : player_state) : bool = state.oya = 0

(** Get hand tiles (34-element array) *)
let tehai (state : player_state) : int array = state.tehai

(** Check if player can declare riichi *)
let can_w_riichi (state : player_state) : bool = state.can_w_riichi

(** Check if player has declared riichi *)
let self_riichi_declared (state : player_state) : bool =
  state.riichi_declared.(0)

(** Check if player's riichi was accepted *)
let self_riichi_accepted (state : player_state) : bool =
  state.riichi_accepted.(0)

(** Get last drawn tile (tsumo) *)
let last_self_tsumo (state : player_state) : int option =
  state.last_self_tsumo

(** Get last discarded tile *)
let last_kawa_tile (state : player_state) : int option =
  state.last_kawa_tile

(** Get available actions *)
let last_cans (state : player_state) : action_candidate =
  state.last_cans

(** Get chi melds *)
let chis (state : player_state) : int list = state.chis

(** Get pon melds *)
let pons (state : player_state) : int list = state.pons

(** Get open kans (minkans) *)
let minkans (state : player_state) : int list = state.minkans

(** Get closed kans (ankans) *)
let ankans (state : player_state) : int list = state.ankans

(** Get current turn count *)
let at_turn (state : player_state) : int = state.at_turn

(** Get shanten number *)
let shanten (state : player_state) : int = state.shanten

(** Get waits array (tiles that complete the hand) *)
let waits (state : player_state) : bool array = state.waits

(** Check if player is furiten *)
let at_furiten (state : player_state) : bool = state.at_furiten

(** Get candidates for ankan (closed kan) *)
let ankan_candidates (state : player_state) : int array =
  (* Find all tiles in hand with count >= 4 *)
  let candidates = ref [] in
  for tile_idx = 0 to 33 do
    if state.tehai.(tile_idx) >= 4 then
      candidates := tile_idx :: !candidates
  done;
  Array.of_list (List.rev !candidates)

(** Get candidates for kakan (open kan -> closed kan) *)
let kakan_candidates (state : player_state) : int list =
  (* Find pon melds where we have the 4th tile in hand *)
  List.filter_map (fun pon_tile ->
    let pon_idx = Tiles.deaka pon_tile in
    if pon_idx >= 0 && pon_idx < 34 && state.tehai.(pon_idx) > 0 then
      Some pon_idx
    else
      None
  ) state.pons

(** Get tiles seen count (for furiten calculation) *)
let tiles_seen (state : player_state) : int array = state.tiles_seen

(** Get discarded tiles flags *)
let discarded_tiles (state : player_state) : bool array = state.discarded_tiles

(** Get dora indicator tiles *)
let dora_indicators (state : player_state) : int list = state.dora_indicators

(** Get dora factor array (maps each tile to its dora count) *)
let dora_factor (state : player_state) : int array = state.dora_factor

(** Get count of dora tiles owned by player *)
let doras_owned (state : player_state) : int array = state.doras_owned

(** Get total visible dora count *)
let doras_seen (state : player_state) : int = state.doras_seen

(** Get red tiles (akas) in hand - [5mr, 5pr, 5sr] *)
let akas_in_hand (state : player_state) : bool array = state.akas_in_hand

(** Get fuuro overview - all melds for all players with constituent tiles *)
(* Returns: [player_id][meld_index][tile_index] *)
let fuuro_overview (state : player_state) : int list list array =
  state.fuuro_overview
