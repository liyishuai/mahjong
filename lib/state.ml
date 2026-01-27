(** Game state management and action validation.

    This module provides core state management for tracking game state
    and validating player actions based on MJAI events.

    Uses mutable state for performance.
*)

(** Action candidate representing possible actions from current state *)
type action_candidate =
  { can_discard : bool
  ; can_chi_low : bool
  ; can_chi_mid : bool
  ; can_chi_high : bool
  ; can_pon : bool
  ; can_daiminkan : bool
  ; can_kakan : bool
  ; can_ankan : bool
  ; can_riichi : bool
  ; can_tsumo_agari : bool
  ; can_ron_agari : bool
  ; can_ryukyoku : bool
  ; target_actor : int
  }

type sutehai =
  { tile : int
  ; is_dora : bool
  ; is_tedashi : bool
  ; is_riichi : bool
  }

type chi_pon =
  { consumed : int array
  ; target_tile : int
  }

type kawa_item =
  { sutehai : sutehai
  ; chi_pon : chi_pon option
  ; kan : int list
  }

(** Default action candidate (no actions available) *)
let default_action_candidate : action_candidate =
  { can_discard = false
  ; can_chi_low = false
  ; can_chi_mid = false
  ; can_chi_high = false
  ; can_pon = false
  ; can_daiminkan = false
  ; can_kakan = false
  ; can_ankan = false
  ; can_riichi = false
  ; can_tsumo_agari = false
  ; can_ron_agari = false
  ; can_ryukyoku = false
  ; target_actor = 0
  }
;;

(** Check if any chi action is available *)
let can_chi (cans : action_candidate) : bool =
  cans.can_chi_low || cans.can_chi_mid || cans.can_chi_high
;;

(** Check if any kan action is available *)
let can_kan (cans : action_candidate) : bool =
  cans.can_daiminkan || cans.can_kakan || cans.can_ankan
;;

(** Check if any agari action is available *)
let can_agari (cans : action_candidate) : bool =
  cans.can_tsumo_agari || cans.can_ron_agari
;;

(** Check if pass action is available *)
let can_pass (cans : action_candidate) : bool =
  can_chi cans || cans.can_pon || cans.can_daiminkan || cans.can_ron_agari
;;

(** Check if any action is available *)
let can_act (cans : action_candidate) : bool =
  cans.can_discard
  || can_chi cans
  || cans.can_pon
  || can_kan cans
  || cans.can_riichi
  || can_agari cans
  || cans.can_ryukyoku
;;

(** Player game state *)
type player_state =
  { player_id : int
  ; (* Tiles in hand (34-element array) *)
    mutable tehai : int array
  ; (* Game state *)
    mutable bakaze : int
  ; mutable jikaze : int
  ; mutable kyoku : int
  ; mutable honba : int
  ; mutable kyotaku : int
  ; mutable oya : int
  ; mutable scores : int array
  ; mutable rank : int
  ; mutable is_all_last : bool
  ; mutable tiles_left : int
  ; (* Flags *)
    mutable riichi_declared : bool array
  ; mutable riichi_accepted : bool array
  ; mutable is_menzen : bool
  ; mutable can_w_riichi : bool
  ; mutable is_w_riichi : bool
  ; mutable at_rinshan : bool
  ; mutable at_ippatsu : bool
  ; (* Turn info *)
    mutable at_turn : int
  ; (* Last action info *)
    mutable last_self_tsumo : int option
  ; mutable last_kawa_tile : int option
  ; mutable last_cans : action_candidate
  ; (* Kan tracking *)
    mutable kans_on_board : int
  ; mutable chis : int list
  ; mutable pons : int list
  ; mutable minkans : int list
  ; mutable ankans : int list
  ; mutable ankan_candidates : int list
  ; mutable kakan_candidates : int list
  ; (* Advanced state tracking *)
    mutable shanten : int
  ; mutable waits : bool array
  ; mutable at_furiten : bool
  ; mutable to_mark_same_cycle_furiten : bool
  ; mutable chankan_chance : bool
  ; mutable has_next_shanten_discard : bool
  ; mutable keep_shanten_discards : bool array
  ; mutable next_shanten_discards : bool array
  ; mutable forbidden_tiles : bool array
  ; mutable tehai_len_div3 : int
  ; mutable tiles_seen : int array
  ; mutable discarded_tiles : bool array
  ; (* Dora tracking *)
    mutable dora_indicators : int list (* List of dora indicator tiles *)
  ; mutable dora_factor : int array (* Maps each tile to its dora count *)
  ; mutable doras_owned : int array (* Count of dora tiles in hand *)
  ; mutable doras_seen : int (* Total visible dora count *)
  ; (* Red tile (aka) tracking *)
    mutable akas_in_hand : bool array (* [5mr, 5pr, 5sr] presence in hand *)
  ; mutable akas_seen : bool array (* [5mr, 5pr, 5sr] seen on board or in hand *)
  ; (* Meld overview (fuuro_overview) - tracks all melds with constituent tiles *)
    (* For each player (0-3), list of melds, each meld is a list of tiles *)
    mutable fuuro_overview : int list list array (* [player][meld][tile] *)
  ; mutable ankan_overview : int list array (* [player][tile_idx] *)
  ; (* River tracking *)
    (* kawa: list of turns, each turn is optional KawaItem. Newest first? Or Oldest first? *)
    (* Let's assume reversed (newest first) for efficient cons, but need to reverse for iteration *)
    (* We use a reversed list and reverse it when needed, or append if list is short. *)
    (* For 20 items, append is fine. `list = list @ [item]` *)
    (* Or keep it reversed and let obs_repr handle it. Let's keep it reversed (Stack-like). *)
    mutable kawa : kawa_item option list array (* [player][turn] *)
  ; mutable kawa_overview : int list array
    (* [player][tile] - flattened list of discards *)
  ; mutable last_tedashis : sutehai option array (* [player] *)
  ; mutable intermediate_kan : int list
  ; mutable intermediate_chi_pon : chi_pon option
  }

(** Create initial player state *)
let create_player_state (player_id : int) : player_state =
  assert (player_id >= 0 && player_id < 4);
  { player_id
  ; tehai = Array.make 34 0
  ; bakaze = Tiles.tile_id_E
  ; jikaze = Tiles.tile_id_E + player_id
  ; kyoku = 0
  ; honba = 0
  ; kyotaku = 0
  ; oya = 0
  ; scores = Array.make 4 25000
  ; rank = 0
  ; is_all_last = false
  ; tiles_left = 70
  ; (* Initial wall size *)
    riichi_declared = Array.make 4 false
  ; riichi_accepted = Array.make 4 false
  ; is_menzen = true
  ; can_w_riichi = false
  ; is_w_riichi = false
  ; at_rinshan = false
  ; at_ippatsu = false
  ; at_turn = 0
  ; last_self_tsumo = None
  ; last_kawa_tile = None
  ; last_cans = default_action_candidate
  ; kans_on_board = 0
  ; chis = []
  ; pons = []
  ; minkans = []
  ; ankans = []
  ; ankan_candidates = []
  ; kakan_candidates = []
  ; (* Advanced state tracking *)
    shanten = 8
  ; (* Max shanten *)
    waits = Array.make 34 false
  ; at_furiten = false
  ; to_mark_same_cycle_furiten = false
  ; chankan_chance = false
  ; has_next_shanten_discard = false
  ; keep_shanten_discards = Array.make 34 false
  ; next_shanten_discards = Array.make 34 false
  ; forbidden_tiles = Array.make 34 false
  ; tehai_len_div3 = 0
  ; tiles_seen = Array.make 34 0
  ; discarded_tiles = Array.make 34 false
  ; (* Dora tracking *)
    dora_indicators = []
  ; dora_factor = Array.make 34 0
  ; doras_owned = Array.make 4 0
  ; doras_seen = 0
  ; (* Red tile tracking *)
    akas_in_hand = [| false; false; false |]
  ; akas_seen = [| false; false; false |]
  ; (* Meld overview - 4 players, each with empty meld list *)
    fuuro_overview = [| []; []; []; [] |]
  ; ankan_overview = [| []; []; []; [] |]
  ; (* River tracking *)
    kawa = Array.make 4 []
  ; kawa_overview = Array.make 4 []
  ; last_tedashis = Array.make 4 None
  ; intermediate_kan = []
  ; intermediate_chi_pon = None
  }
;;

(** Validate if a tile is in hand *)
let tile_in_hand (state : player_state) (tile : int) : bool =
  let tile_idx = Tiles.deaka tile in
  if tile_idx >= 0 && tile_idx < 34 then state.tehai.(tile_idx) > 0 else false
;;

(** Ensure tiles are in hand, including aka validation *)
let ensure_tiles_in_hand (state : player_state) (tiles : int array)
  : (unit, string) result
  =
  try
    Array.iter
      (fun tile ->
         let tile_idx = Tiles.deaka tile in
         if tile_idx < 0 || tile_idx >= 34 || state.tehai.(tile_idx) = 0
         then failwith (Printf.sprintf "tile %d is not in hand" tile);
         (* Check aka tiles specifically *)
         if Tiles.is_aka tile
         then (
           let aka_idx =
             match tile with
             | t when t = Tiles.tile_id_5mr -> 0
             | t when t = Tiles.tile_id_5pr -> 1
             | t when t = Tiles.tile_id_5sr -> 2
             | _ -> -1
           in
           if aka_idx >= 0 && not state.akas_in_hand.(aka_idx)
           then failwith (Printf.sprintf "aka tile %d is not in hand" tile)))
      tiles;
    Ok ()
  with
  | Failure msg -> Error msg
;;

(** Validate reaction to current state *)
let validate_reaction (state : player_state) (action : Mjai.event) : (unit, string) result
  =
  let cans = state.last_cans in
  match action with
  | Mjai.Ryukyoku _ -> if cans.can_ryukyoku then Ok () else Error "cannot ryukyoku"
  | Mjai.None -> Ok ()
  | _ ->
    (* Check actor matches player_id *)
    let actor_ok =
      match Mjai.actor action with
      | Some actor when actor = state.player_id -> Ok ()
      | Some actor ->
        Error (Printf.sprintf "actor is %d, not self (%d)" actor state.player_id)
      | None -> Error "action does not have actor"
    in
    (match actor_ok with
     | Error _ as e -> e
     | Ok () ->
       (match action with
        | Mjai.Dahai { pai; tsumogiri; _ } ->
          if not cans.can_discard
          then Error "cannot discard"
          else if not (tile_in_hand state pai)
          then Error (Printf.sprintf "tile %d not in hand" pai)
          else if tsumogiri
          then (
            match state.last_self_tsumo with
            | Some tile when tile = pai -> Ok ()
            | Some _ -> Error "cannot tsumogiri different tile"
            | None -> Error "tsumogiri but no tsumo recorded")
          else Ok ()
        | Mjai.Reach _ -> if cans.can_riichi then Ok () else Error "cannot riichi"
        | Mjai.Chi { actor; target; pai; consumed } ->
          if (target + 1) mod 4 <> actor
          then Error "chi from non-kamicha"
          else if not (can_chi cans)
          then Error "cannot chi"
          else (
            match state.last_kawa_tile with
            | Some tile when tile = pai -> ensure_tiles_in_hand state consumed
            | Some _ -> Error "chi target is not the last kawa tile"
            | None -> Error "no kawa tile to chi")
        | Mjai.Pon { actor; target; pai; consumed } ->
          if target = actor
          then Error "pon from itself"
          else if not cans.can_pon
          then Error "cannot pon"
          else (
            match state.last_kawa_tile with
            | Some tile when tile = pai -> ensure_tiles_in_hand state consumed
            | Some _ -> Error "pon target is not the last kawa tile"
            | None -> Error "no kawa tile to pon")
        | Mjai.Daiminkan { actor; target; pai; consumed } ->
          if target = actor
          then Error "daiminkan from itself"
          else if not cans.can_daiminkan
          then Error "cannot daiminkan"
          else (
            match state.last_kawa_tile with
            | Some tile when tile = pai -> ensure_tiles_in_hand state consumed
            | Some _ -> Error "daiminkan target is not the last kawa tile"
            | None -> Error "no kawa tile for daiminkan")
        | Mjai.Kakan { pai; _ } ->
          if not cans.can_kakan
          then Error "cannot kakan"
          else (
            let tile_idx = Tiles.deaka pai in
            if List.mem tile_idx state.kakan_candidates
            then ensure_tiles_in_hand state [| pai |]
            else
              Error (Printf.sprintf "cannot kakan tile %d (not in kakan_candidates)" pai))
        | Mjai.Ankan { consumed; _ } ->
          if not cans.can_ankan
          then Error "cannot ankan"
          else (
            let tile_idx = Tiles.deaka consumed.(0) in
            if List.mem tile_idx state.ankan_candidates
            then ensure_tiles_in_hand state consumed
            else
              Error
                (Printf.sprintf
                   "cannot ankan tile %d (not in ankan_candidates)"
                   consumed.(0)))
        | Mjai.Hora { target; _ } ->
          if target = state.player_id
          then if cans.can_tsumo_agari then Ok () else Error "cannot tsumo agari"
          else if cans.can_ron_agari
          then Ok ()
          else Error "cannot ron agari"
        | _ -> Error "unexpected action"))
;;

(** Get relative player index (0=self, 1=shimocha, 2=toimen, 3=kamicha) *)
let rel (state : player_state) (actor : int) : int = (actor + 4 - state.player_id) mod 4

(** Calculate player's rank given current scores.
    @param state The player state
    @param scores_rel Relative scores from player's perspective [25000; 25000; 25000; 25000]
    @return Rank (0 = 1st, 1 = 2nd, 2 = 3rd, 3 = 4th) *)
let get_rank (state : player_state) (scores_rel : int array) : int =
  (* Rotate scores to absolute positions *)
  let scores_abs = Array.copy scores_rel in
  Array.blit scores_rel (4 - state.player_id) scores_abs 0 state.player_id;
  Array.blit scores_rel 0 scores_abs state.player_id (4 - state.player_id);
  (* Create player_by_rank: stable sort players by score (descending) *)
  let player_by_rank = [| 0; 1; 2; 3 |] in
  Array.stable_sort (fun a b -> compare scores_abs.(b) scores_abs.(a)) player_by_rank;
  (* Find rank of current player *)
  let rank = ref 0 in
  for i = 0 to 3 do
    if player_by_rank.(i) = state.player_id then rank := i
  done;
  !rank
;;

(** Update current player's rank *)
let update_rank (state : player_state) : unit = state.rank <- get_rank state state.scores

(** Updates tiles_seen, doras_seen and akas_seen. *)
let witness_tile state tile =
  let idx = Tiles.deaka tile in
  if idx >= 0 && idx < 34 then (
    state.tiles_seen.(idx) <- state.tiles_seen.(idx) + 1;
    state.doras_seen <- state.doras_seen + state.dora_factor.(idx);
    if Tiles.is_aka tile then (
      let aka_idx = match tile with
        | t when t = Tiles.tile_id_5mr -> 0
        | t when t = Tiles.tile_id_5pr -> 1
        | t when t = Tiles.tile_id_5sr -> 2
        | _ -> -1
      in
      if aka_idx >= 0 then (
        state.akas_seen.(aka_idx) <- true;
        state.doras_seen <- state.doras_seen + 1
      )
    )
  )

(** Update shanten for current hand *)
let update_shanten (state : player_state) : unit =
  let s = Shanten.calc_all state.tehai state.tehai_len_div3 in
  state.shanten <- (if s < 0 then 0 else s)
;;

(* Must be called at 3n+2 (after draw/chi/pon) *)
(** Update shanten discards (what to discard to improve/maintain shanten) *)
let update_shanten_discards (state : player_state) : unit =
  Array.fill state.next_shanten_discards 0 34 false;
  Array.fill state.keep_shanten_discards 0 34 false;
  state.has_next_shanten_discard <- false;
  for i = 0 to 33 do
    if state.tehai.(i) > 0
    then (
      state.tehai.(i) <- state.tehai.(i) - 1;
      let s_after = Shanten.calc_all state.tehai state.tehai_len_div3 in
      state.tehai.(i) <- state.tehai.(i) + 1;
      if s_after < state.shanten
      then (
        state.next_shanten_discards.(i) <- true;
        state.has_next_shanten_discard <- true)
      else if s_after = state.shanten
      then state.keep_shanten_discards.(i) <- true)
  done
;;

(** Update waits and furiten state *)
let update_waits_and_furiten (state : player_state) : unit =
  (* Reset furiten and waits *)
  state.at_furiten <- false;
  Array.fill state.waits 0 34 false;
  if state.shanten > 0
  then ()
  else
    (* Check each tile as a potential wait *)
    for tile_idx = Tiles.tile_id_1m to Tiles.tile_id_C do
      if state.tehai.(tile_idx) < 4
      then (
        (* Simulate adding this tile to hand *)
        let new_tehai = Array.copy state.tehai in
        new_tehai.(tile_idx) <- new_tehai.(tile_idx) + 1;
        (* Calculate new hand size and len_div3 *)
        let new_count = Array.fold_left ( + ) 0 new_tehai in
        (* For a 3n+1 or 3n+2 hand, len_div3 = (tiles - 1) / 3 *)
        (* This represents how many complete melds we're targeting *)
        let expected_len_div3 = (new_count - 1) / 3 in
        (* Check if this completes the hand *)
        let new_shanten = Shanten.calc_all new_tehai expected_len_div3 in
        if new_shanten = -1
        then (
          (* Check for furiten: is this tile in discarded_tiles? *)
          if tile_idx < 0 || tile_idx >= Array.length state.discarded_tiles
          then
            failwith
              (Printf.sprintf "tile_idx %d out of bounds for discarded_tiles" tile_idx);
          if state.discarded_tiles.(tile_idx) then state.at_furiten <- true;
          (* Only a wait if we haven't seen all 4 of this tile *)
          if tile_idx < 0 || tile_idx >= Array.length state.waits
          then failwith (Printf.sprintf "tile_idx %d out of bounds for waits" tile_idx);
          state.waits.(tile_idx) <- state.tiles_seen.(tile_idx) < 4))
    done
;;

(** Start a new kyoku (round) *)
let start_kyoku
      (state : player_state)
      (bakaze : int)
      (kyoku : int)
      (honba : int)
      (kyotaku : int)
      (oya : int)
      (scores : int array)
      (tehais : int array array)
  : unit
  =
  let actor_tehai = tehais.(state.player_id) in
  state.bakaze <- bakaze;
  state.jikaze <- Tiles.tile_id_E + ((state.player_id - oya + 4) mod 4);
  state.kyoku <- kyoku;
  state.honba <- honba;
  state.kyotaku <- kyotaku;
  state.oya <- oya;
  (* Store scores as relative to self *)
  let rel_scores = Array.make 4 0 in
  for i = 0 to 3 do
    rel_scores.((i + 4 - state.player_id) mod 4) <- scores.(i)
  done;
  state.scores <- rel_scores;
  update_rank state;
  state.is_all_last
  <- (match bakaze with
      | t when t = Tiles.tile_id_E -> false
      | t when t = Tiles.tile_id_S -> kyoku = 4
      | _ -> true);
  (* Reset and populate tehai *)
  state.tehai <- Array.make 34 0;
  (* Reset dora tracking *)
  state.dora_indicators <- [];
  Array.fill state.dora_factor 0 34 0;
  Array.fill state.doras_owned 0 4 0;
  state.doras_seen <- 0;
  (* Reset red tile tracking *)
  state.akas_in_hand <- [| false; false; false |];
  state.akas_seen <- [| false; false; false |];
  
  Array.iter (fun tile ->
    let idx = Tiles.deaka tile in
    if idx >= 0 && idx < 34 then (
      witness_tile state tile;
      state.tehai.(idx) <- state.tehai.(idx) + 1;
      if Tiles.is_aka tile then (
        let aka_idx = match tile with
          | t when t = Tiles.tile_id_5mr -> 0
          | t when t = Tiles.tile_id_5pr -> 1
          | t when t = Tiles.tile_id_5sr -> 2
          | _ -> -1
        in
        if aka_idx >= 0 then (
          state.akas_in_hand.(aka_idx) <- true;
          state.doras_owned.(0) <- state.doras_owned.(0) + 1
        )
      )
    )
  ) actor_tehai;
  (* Reset flags *)
  state.riichi_declared <- Array.make 4 false;
  state.riichi_accepted <- Array.make 4 false;
  state.is_menzen <- true;
  state.can_w_riichi <- true;
  state.is_w_riichi <- false;
  state.at_rinshan <- false;
  state.at_ippatsu <- false;
  state.last_self_tsumo <- None;
  state.last_kawa_tile <- None;
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
  (* Reset meld overview *)
  state.fuuro_overview <- [| []; []; []; [] |];
  state.ankan_overview <- [| []; []; []; [] |];
  (* Reset river tracking *)
  state.kawa <- Array.make 4 [];
  state.kawa_overview <- Array.make 4 [];
  state.last_tedashis <- Array.make 4 None;
  state.intermediate_kan <- [];
  state.intermediate_chi_pon <- None;
  update_shanten state;
  update_waits_and_furiten state;
  (* Pad kawa at start *)
  let rel_oya = (oya + 4 - state.player_id) mod 4 in
  for i = 0 to rel_oya - 1 do
    state.kawa.(i) <- [ None ]
  done
;;

(** Pad kawa for pon or daiminkan *)
let pad_kawa_for_pon_or_daiminkan
      (state : player_state)
      (abs_actor : int)
      (abs_target : int)
  : unit
  =
  let rec loop i =
    if i <> abs_actor
    then (
      let rel = (i + 4 - state.player_id) mod 4 in
      state.kawa.(rel) <- None :: state.kawa.(rel);
      loop ((i + 1) mod 4))
  in
  loop ((abs_target + 1) mod 4)
;;

(** Calculate real-time shanten considering 3n+2 hands *)
let real_time_shanten (state : player_state) : int =
  if not state.last_cans.can_discard
  then
    (* 3n+1, state.shanten is accurate *)
    state.shanten
  else if state.shanten > 0
  then
    (* 3n+2, not tenpai *)
    state.shanten
  else
    (* 3n+2, tenpai - calculate actual shanten *)
    Shanten.calc_all state.tehai state.tehai_len_div3
;;

(** Add a dora indicator and update dora tracking *)
let add_dora_indicator (state : player_state) (tile : int) : unit =
  let tile_idx = Tiles.deaka tile in
  if tile_idx >= 0 && tile_idx < 34
  then (
    (* Add to indicators list *)
    state.dora_indicators <- state.dora_indicators @ [ tile ];
    witness_tile state tile;
    (* Get the dora tile (next tile after indicator) *)
    let dora_tile = Tiles.next tile in
    let dora_idx = Tiles.deaka dora_tile in
    if dora_idx >= 0 && dora_idx < 34
    then (
      (* Increment dora factor for this tile *)
      state.dora_factor.(dora_idx) <- state.dora_factor.(dora_idx) + 1;
      (* Count dora tiles in hand *)
      state.doras_owned.(0) <- state.doras_owned.(0) + state.tehai.(dora_idx);
      (* Count NEW dora tiles in melds (fuuro_overview) *)
      (* Only count tiles that match the new dora *)
      List.iter
        (fun meld ->
           List.iter
             (fun tile ->
                let tile_idx = Tiles.deaka tile in
                if tile_idx = dora_idx
                then state.doras_owned.(0) <- state.doras_owned.(0) + 1)
             meld)
        state.fuuro_overview.(state.player_id);
      (* Count dora tiles in ankans *)
      (* Note: ankans list contains deaka'd tiles, each represents 4 tiles *)
      List.iter
        (fun ankan_tile ->
           if ankan_tile = dora_idx
           then state.doras_owned.(0) <- state.doras_owned.(0) + 4)
        state.ankans))
;;

(** Handle tsumo event *)
let tsumo (state : player_state) (actor : int) (pai : int) : unit =
  (* Clear kan candidates *)
  state.ankan_candidates <- [];
  state.kakan_candidates <- [];
  if actor = state.player_id
  then (
    let idx = Tiles.deaka pai in
    if idx >= 0 && idx < 34
    then (
      (* Decrement tiles left *)
      if state.tiles_left > 0 then state.tiles_left <- state.tiles_left - 1;
      witness_tile state pai;
      state.tehai.(idx) <- state.tehai.(idx) + 1;
      state.last_self_tsumo <- Some pai;
      (* Update akas_in_hand tracking *)
      (match pai with
       | t when t = Tiles.tile_id_5mr -> state.akas_in_hand.(0) <- true
       | t when t = Tiles.tile_id_5pr -> state.akas_in_hand.(1) <- true
       | t when t = Tiles.tile_id_5sr -> state.akas_in_hand.(2) <- true
       | _ -> ());
      (* Update doras_owned for the drawn tile *)
      state.doras_owned.(0) <- state.doras_owned.(0) + state.dora_factor.(idx);
      if Tiles.is_aka pai then state.doras_owned.(0) <- state.doras_owned.(0) + 1;
      (* Update advanced tracking after drawing *)
      state.tehai_len_div3 <- Array.fold_left ( + ) 0 state.tehai / 3;
      (* update_shanten state;  <-- REMOVED to match Rust parity *)
      (* update_waits_and_furiten state;  <-- REMOVED *)
      if not state.riichi_accepted.(0) then update_shanten_discards state;
      (* Check for tsumo agari *)
      state.last_cans <- { state.last_cans with can_discard = true };
      if state.waits.(idx)
      then (
        (* Check Yaku *)
        let additional_hans = ref 0 in
        if state.riichi_accepted.(0) then incr additional_hans;
        if state.is_w_riichi then incr additional_hans;
        if state.at_ippatsu then incr additional_hans;
        if state.tiles_left = 0 then incr additional_hans;
        (* Haitei *)
        if state.at_rinshan then incr additional_hans;
        if state.is_menzen then incr additional_hans;
        (* Menzen Tsumo *)
        let calc : Agari.agari_calculator =
          { tehai = state.tehai
          ; winning_tile = idx
          ; bakaze = state.bakaze
          ; jikaze = state.jikaze
          ; is_menzen = state.is_menzen
          ; is_ron = false
          ; chis = state.chis
          ; pons = state.pons
          ; minkans = state.minkans
          ; ankans = state.ankans
          }
        in
        match Agari.agari calc !additional_hans 0 with
        | Some _ -> state.last_cans <- { state.last_cans with can_tsumo_agari = true }
        | None -> ());
      (* haitei tile (last tile) cannot be used for kan *)
      if state.tiles_left > 0
      then (
        (* Populate kan candidates if not in riichi or before riichi *)
        if state.kans_on_board < 4
        then (
          (* Find ankan candidates (4 of a kind) *)
          for tile_idx = Tiles.tile_id_1m to Tiles.tile_id_C do
            if state.tehai.(tile_idx) = 4
            then (
              let can_ankan =
                if not state.riichi_accepted.(0)
                then true
                else
                  Agari.check_ankan_after_riichi
                    state.tehai
                    state.tehai_len_div3
                    tile_idx
                    true
              in
              if can_ankan
              then (
                state.last_cans <- { state.last_cans with can_ankan = true };
                state.ankan_candidates <- state.ankan_candidates @ [ tile_idx ]))
          done;
          (* Find kakan candidates (have pon and 4th tile in hand) *)
          if not state.riichi_accepted.(0)
          then
            List.iter
              (fun pon_tile ->
                 let pon_idx = Tiles.deaka pon_tile in
                 if pon_idx >= 0 && pon_idx < 34 && state.tehai.(pon_idx) > 0
                 then (
                   state.last_cans <- { state.last_cans with can_kakan = true };
                   state.kakan_candidates <- state.kakan_candidates @ [ pon_idx ]))
              state.pons);
        (* Check if can declare riichi *)
        state.last_cans
        <- { state.last_cans with
             can_riichi =
               state.is_menzen
               && state.tiles_left >= 4
               && state.scores.(0) >= 1000
               && state.shanten = 0
           })))
;;

(** Check if chi is possible with a given tile *)
let set_can_chi_from_tile (state : player_state) (tile : int) : unit =
  let can_chi_low = ref false in
  let can_chi_mid = ref false in
  let can_chi_high = ref false in
  let tile_id = Tiles.deaka tile in
  if tile_id >= 0 && tile_id < 27
  then (
    (* Chi only for numbered suits *)
    let literal_num = (tile_id mod 9) + 1 in
    (* 1-9 *)
    (* Check low chi: [tile, tile+1, tile+2] *)
    if literal_num <= 7 && tile_id + 2 < 34
    then
      (* Check if we have the required tiles *)
      if state.tehai.(tile_id + 1) > 0 && state.tehai.(tile_id + 2) > 0
      then (
        (* Simulate removing the chi tiles from hand *)
        let tehai_after = Array.copy state.tehai in
        (* Set tile_id to 0 to prevent "cheating" (1111234 case) *)
        tehai_after.(tile_id) <- 0;
        tehai_after.(tile_id + 1) <- tehai_after.(tile_id + 1) - 1;
        tehai_after.(tile_id + 2) <- tehai_after.(tile_id + 2) - 1;
        (* If literal_num < 7, also zero out tile_id + 3 to prevent using it for chi *)
        if literal_num < 7 && tile_id + 3 < 34 then tehai_after.(tile_id + 3) <- 0;
        (* Chi is possible if there are still tiles in hand *)
        can_chi_low
        := Array.fold_left (fun acc x -> if x > 0 then acc + 1 else acc) 0 tehai_after > 0);
    (* Check mid chi: [tile-1, tile, tile+1] *)
    if literal_num >= 2 && literal_num <= 8 && tile_id - 1 >= 0 && tile_id + 1 < 34
    then
      if state.tehai.(tile_id - 1) > 0 && state.tehai.(tile_id + 1) > 0
      then (
        let tehai_after = Array.copy state.tehai in
        (* Set tile_id to 0 to prevent "cheating" *)
        tehai_after.(tile_id) <- 0;
        tehai_after.(tile_id - 1) <- tehai_after.(tile_id - 1) - 1;
        tehai_after.(tile_id + 1) <- tehai_after.(tile_id + 1) - 1;
        can_chi_mid
        := Array.fold_left (fun acc x -> if x > 0 then acc + 1 else acc) 0 tehai_after > 0);
    (* Check high chi: [tile-2, tile-1, tile] *)
    if literal_num >= 3 && tile_id - 2 >= 0
    then
      if state.tehai.(tile_id - 2) > 0 && state.tehai.(tile_id - 1) > 0
      then (
        let tehai_after = Array.copy state.tehai in
        (* Set tile_id to 0 to prevent "cheating" *)
        tehai_after.(tile_id) <- 0;
        tehai_after.(tile_id - 2) <- tehai_after.(tile_id - 2) - 1;
        tehai_after.(tile_id - 1) <- tehai_after.(tile_id - 1) - 1;
        (* If literal_num > 3, also zero out tile_id - 3 *)
        if literal_num > 3 && tile_id - 3 >= 0 then tehai_after.(tile_id - 3) <- 0;
        can_chi_high
        := Array.fold_left (fun acc x -> if x > 0 then acc + 1 else acc) 0 tehai_after > 0));
  (* Update last_cans with new chi flags *)
  state.last_cans
  <- { state.last_cans with
       can_chi_low = !can_chi_low
     ; can_chi_mid = !can_chi_mid
     ; can_chi_high = !can_chi_high
     }
;;

(** Check if we can ron on a tile (considering yaku) *)
let check_can_ron (state : player_state) (pai : int) (is_chankan : bool) : bool =
  let idx = Tiles.deaka pai in
  if not (state.shanten = 0 && state.waits.(idx) && not state.at_furiten)
  then false
  else (
    (* Check Yaku *)
    let additional_hans = ref 0 in
    if state.riichi_accepted.(0) then incr additional_hans;
    if state.is_w_riichi then incr additional_hans;
    if state.at_ippatsu then incr additional_hans;
    if state.tiles_left = 0 then incr additional_hans;
    (* Houtei *)
    if is_chankan then incr additional_hans;
    (* Chankan *)
    let tehai = Array.copy state.tehai in
    tehai.(idx) <- tehai.(idx) + 1;
    let calc : Agari.agari_calculator =
      { tehai
      ; winning_tile = idx
      ; bakaze = state.bakaze
      ; jikaze = state.jikaze
      ; is_menzen = state.is_menzen
      ; is_ron = true
      ; chis = state.chis
      ; pons = state.pons
      ; minkans = state.minkans
      ; ankans = state.ankans
      }
    in
    match Agari.agari calc !additional_hans 0 with
    | Some _ -> true
    | None -> false)
;;

(** Handle dahai (discard) event *)
let dahai (state : player_state) (actor : int) (pai : int) (tsumogiri : bool) : unit =
  state.last_kawa_tile <- Some pai;
  let actor_rel = rel state actor in
  (* Construct KawaItem *)
  let is_riichi =
    state.riichi_declared.(actor_rel) && not state.riichi_accepted.(actor_rel)
  in
  let sutehai =
    { tile = pai
    ; is_dora = state.dora_factor.(Tiles.deaka pai) > 0
    ; is_tedashi = not tsumogiri
    ; is_riichi
    }
  in
  let kawa_item =
    { kan = state.intermediate_kan; chi_pon = state.intermediate_chi_pon; sutehai }
  in
  (* Clear intermediate buffers *)
  state.intermediate_kan <- [];
  state.intermediate_chi_pon <- None;
  (* Push to kawa (reversed list, so cons) *)
  state.kawa.(actor_rel) <- Some kawa_item :: state.kawa.(actor_rel);
  state.kawa_overview.(actor_rel) <- pai :: state.kawa_overview.(actor_rel);
  if not tsumogiri then state.last_tedashis.(actor_rel) <- Some sutehai;
  (* Note: riichi_sutehais tracking is not in player_state yet, but we have kawa_item which contains is_riichi *)
  if actor = state.player_id
  then (
    let idx = Tiles.deaka pai in
    if idx >= 0 && idx < 34 && state.tehai.(idx) > 0
    then (
      state.tehai.(idx) <- state.tehai.(idx) - 1;
      (* Update akas_in_hand tracking if discarding an aka *)
      (match pai with
       | t when t = Tiles.tile_id_5mr -> state.akas_in_hand.(0) <- false
       | t when t = Tiles.tile_id_5pr -> state.akas_in_hand.(1) <- false
       | t when t = Tiles.tile_id_5sr -> state.akas_in_hand.(2) <- false
       | _ -> ());
      (* Update doras_owned for the discarded tile *)
      state.doras_owned.(0) <- state.doras_owned.(0) - state.dora_factor.(idx);
      if Tiles.is_aka pai then state.doras_owned.(0) <- state.doras_owned.(0) - 1;
      (* Track discarded tiles for furiten calculation *)
      state.discarded_tiles.(idx) <- true;
      if tsumogiri then state.last_self_tsumo <- None;
      (* Reset flags after discard *)
      state.at_rinshan <- false;
      state.at_ippatsu <- false;
      state.can_w_riichi <- false;
      (* Update advanced tracking after discarding *)
      state.tehai_len_div3 <- Array.fold_left ( + ) 0 state.tehai / 3;
      update_shanten state;
      update_waits_and_furiten state))
  else (
    witness_tile state pai;
    if
      (* Another player discarded - check if we can react *)
      (not state.riichi_accepted.(0)) && state.tiles_left > 0
    then (
      let idx = Tiles.deaka pai in
      (* Check for chi (only from kamicha = actor + 1) *)
      let relative_pos = (actor - state.player_id + 4) mod 4 in
      if relative_pos = 3 && idx < 27 && state.tehai_len_div3 > 0
      then set_can_chi_from_tile state pai;
      (* Check for pon *)
      if idx >= 0 && idx < 34
      then
        state.last_cans
        <- { state.last_cans with
             can_pon = state.tehai.(idx) >= 2
           ; can_daiminkan = state.kans_on_board < 4 && state.tehai.(idx) = 3
           };
      (* Check for ron agari *)
      if check_can_ron state pai false
      then state.last_cans <- { state.last_cans with can_ron_agari = true }))
;;

(** Handle chi (sequence meld) event *)
let chi (state : player_state) (actor : int) (pai : int) (consumed : int array) : unit =
  let actor_rel = rel state actor in
  let full_set = Array.to_list consumed @ [pai] in
  state.fuuro_overview.(actor_rel) <- state.fuuro_overview.(actor_rel) @ [full_set];
  state.intermediate_chi_pon <- Some { consumed; target_tile = pai };

  if actor = state.player_id then begin
    (* Remove consumed tiles from hand *)
    Array.iter (fun tile ->
      let idx = Tiles.deaka tile in
      if idx >= 0 && idx < 34 && state.tehai.(idx) > 0 then
        state.tehai.(idx) <- state.tehai.(idx) - 1
    ) consumed;
    (* Add chi to state *)
    let c0 = Tiles.deaka consumed.(0) in
    let c1 = Tiles.deaka consumed.(1) in
    let p = Tiles.deaka pai in
    let min_tile = min c0 (min c1 p) in
    state.chis <- state.chis @ [min_tile];
    state.is_menzen <- false;
    state.tehai_len_div3 <- state.tehai_len_div3 - 1;
    state.last_self_tsumo <- None;
    
    (* Update doras_owned for the received tile *)
    let idx = Tiles.deaka pai in
    state.doras_owned.(0) <- state.doras_owned.(0) + state.dora_factor.(idx);
    if Tiles.is_aka pai then
      state.doras_owned.(0) <- state.doras_owned.(0) + 1;

    (* Update tracking *)
    (* Note: forbidden_tiles logic for kuikae is not fully implemented yet, but keeping it simple *)
    update_shanten state;
    update_shanten_discards state
  end else begin
    Array.iter (witness_tile state) consumed;
    state.can_w_riichi <- false;
    state.at_ippatsu <- false
  end
;;

(** Handle pon (triplet meld) event *)
let pon (state : player_state) (actor : int) (target : int) (pai : int) (consumed : int array) : unit =
  let actor_rel = rel state actor in
  let full_set = Array.to_list consumed @ [pai] in
  state.fuuro_overview.(actor_rel) <- state.fuuro_overview.(actor_rel) @ [full_set];
  state.intermediate_chi_pon <- Some { consumed; target_tile = pai };
  pad_kawa_for_pon_or_daiminkan state actor target;

  if actor = state.player_id then begin
    (* Remove consumed tiles from hand *)
    Array.iter (fun tile ->
      let idx = Tiles.deaka tile in
      if idx >= 0 && idx < 34 && state.tehai.(idx) > 0 then
        state.tehai.(idx) <- state.tehai.(idx) - 1
    ) consumed;
    (* Add pon to state *)
    state.pons <- state.pons @ [Tiles.deaka pai];
    state.is_menzen <- false;
    state.tehai_len_div3 <- state.tehai_len_div3 - 1;
    state.last_self_tsumo <- None;

    (* Update doras_owned for the received tile *)
    let idx = Tiles.deaka pai in
    state.doras_owned.(0) <- state.doras_owned.(0) + state.dora_factor.(idx);
    if Tiles.is_aka pai then
      state.doras_owned.(0) <- state.doras_owned.(0) + 1;

    (* Update tracking *)
    update_shanten state;
    update_shanten_discards state
  end else begin
    Array.iter (witness_tile state) consumed;
    state.can_w_riichi <- false;
    state.at_ippatsu <- false
  end
;;

(** Handle reach (riichi) declaration *)
let reach (state : player_state) (actor : int) : unit =
  let actor_rel = rel state actor in
  state.riichi_declared.(actor_rel) <- true;
  if actor_rel = 0
  then (
    state.is_w_riichi <- state.can_w_riichi;
    state.last_cans <- { state.last_cans with can_discard = true })
;;

(** Handle reach_accepted *)
let reach_accepted (state : player_state) (actor : int) : unit =
  let actor_rel = rel state actor in
  state.riichi_accepted.(actor_rel) <- true;
  state.scores.(actor_rel) <- state.scores.(actor_rel) - 1000;
  state.kyotaku <- state.kyotaku + 1;
  update_rank state;
  if actor_rel = 0 then state.at_ippatsu <- true
;;

(** Handle ankan (closed kan from hand) *)
let ankan (state : player_state) (actor : int) (consumed : int array) : unit =
  let actor_rel = rel state actor in
  state.ankan_overview.(actor_rel)
  <- state.ankan_overview.(actor_rel) @ [ Tiles.deaka consumed.(0) ];
  state.intermediate_kan <- state.intermediate_kan @ Array.to_list consumed;
  state.kans_on_board <- state.kans_on_board + 1;
  if actor = state.player_id
  then (
    let tile = Tiles.deaka consumed.(0) in
    (* Remove 4 tiles from hand *)
    Array.iter
      (fun t ->
         let idx = Tiles.deaka t in
         if idx >= 0 && idx < 34 && state.tehai.(idx) > 0
         then state.tehai.(idx) <- state.tehai.(idx) - 1)
      consumed;
    (* Add to ankans list *)
    state.ankans <- state.ankans @ [ tile ];
    state.tehai_len_div3 <- state.tehai_len_div3 - 1;
    (* Update tracking *)
    update_shanten state;
    update_waits_and_furiten state)
  else (
    Array.iter (witness_tile state) consumed;
    state.can_w_riichi <- false;
    state.at_ippatsu <- false)
;;

(** Handle kakan (pon → kan) *)
let kakan (state : player_state) (actor : int) (pai : int) : unit =
  let actor_rel = rel state actor in
  (* Update fuuro_overview: add tile to existing pon meld *)
  let player_fuuro = state.fuuro_overview.(actor_rel) in
  let updated_fuuro =
    List.map
      (fun meld ->
         match meld with
         | hd :: _ when Tiles.deaka hd = Tiles.deaka pai -> meld @ [ pai ]
         | _ -> meld)
      player_fuuro
  in
  state.fuuro_overview.(actor_rel) <- updated_fuuro;
  state.intermediate_kan <- state.intermediate_kan @ [ pai ];
  state.kans_on_board <- state.kans_on_board + 1;
  if actor = state.player_id
  then (
    let tile_idx = Tiles.deaka pai in
    (* Remove tile from hand *)
    if tile_idx >= 0 && tile_idx < 34 && state.tehai.(tile_idx) > 0
    then (
      state.tehai.(tile_idx) <- state.tehai.(tile_idx) - 1;
      (* Remove from pons, add to minkans *)
      state.pons <- List.filter (fun t -> Tiles.deaka t <> tile_idx) state.pons;
      state.minkans <- state.minkans @ [ tile_idx ];
      (* Update tracking *)
      update_shanten state;
      update_waits_and_furiten state))
  else (
    witness_tile state pai;
    state.can_w_riichi <- false;
    state.at_ippatsu <- false)
;;

(** Handle daiminkan (closed kan → open kan) *)
let daiminkan
      (state : player_state)
      (actor : int)
      (target : int)
      (pai : int)
      (consumed : int array)
  : unit
  =
  let actor_rel = rel state actor in
  let full_set = Array.to_list consumed @ [ pai ] in
  state.fuuro_overview.(actor_rel) <- state.fuuro_overview.(actor_rel) @ [ full_set ];
  state.intermediate_kan <- state.intermediate_kan @ [ pai ];
  pad_kawa_for_pon_or_daiminkan state actor target;
  state.kans_on_board <- state.kans_on_board + 1;
  if actor = state.player_id
  then (
    (* Remove 3 tiles from hand *)
    Array.iter
      (fun tile ->
         let idx = Tiles.deaka tile in
         if idx >= 0 && idx < 34 && state.tehai.(idx) > 0
         then state.tehai.(idx) <- state.tehai.(idx) - 1)
      consumed;
    (* Add to minkans *)
    let tile_idx = Tiles.deaka pai in
    state.minkans <- state.minkans @ [ tile_idx ];
    state.is_menzen <- false;
    state.tehai_len_div3 <- state.tehai_len_div3 - 1;
    (* Update doras_owned for the received tile *)
    state.doras_owned.(0) <- state.doras_owned.(0) + state.dora_factor.(tile_idx);
    if Tiles.is_aka pai then state.doras_owned.(0) <- state.doras_owned.(0) + 1;
    (* Update tracking *)
    update_shanten state;
    update_waits_and_furiten state)
  else (
    Array.iter (witness_tile state) consumed;
    state.can_w_riichi <- false;
    state.at_ippatsu <- false)
;;

(** Main update function - process MJAI event and update state *)
let update (state : player_state) (event : Mjai.event) : unit =
  (* Preserve last action candidates to check for passed actions (e.g. skipped ron -> furiten) *)
  let old_cans = state.last_cans in
  (* Reset action candidates for the new state *)
  state.last_cans <- default_action_candidate;
  match event with
  | Mjai.Start_game _ -> () (* No state change for this player *)
  | Mjai.Start_kyoku { bakaze; kyoku; honba; kyotaku; oya; dora_marker; scores; tehais }
    ->
    start_kyoku state bakaze kyoku honba kyotaku oya scores tehais;
    add_dora_indicator state dora_marker;
    (* After starting, we are waiting for the oya's tsumo/first action. *)
    state.last_cans <- { state.last_cans with target_actor = oya }
  | Mjai.Tsumo { actor; pai } ->
    (* If another player draws, and we could have claimed the last discard, we are furiten. *)
    if actor <> state.player_id && old_cans.can_ron_agari then state.at_furiten <- true;
    tsumo state actor pai
    (* `tsumo` helper handles setting all relevant `last_cans` fields if actor is self *)
  | Mjai.Dahai { actor; pai; tsumogiri } ->
    dahai state actor pai tsumogiri
    (* `dahai` helper handles setting `last_cans` for other players (chi, pon, etc.).
         If actor is self, no actions are possible, so default is correct. *)
  | Mjai.Chi { actor; pai; consumed; _ } ->
    if actor = state.player_id
    then (
      if old_cans.can_ron_agari then state.at_furiten <- true;
      chi state actor pai consumed;
      state.last_cans <- { state.last_cans with can_discard = true; target_actor = actor })
  | Mjai.Pon { actor; target; pai; consumed; _ } ->
    if actor = state.player_id
    then (
      if old_cans.can_ron_agari then state.at_furiten <- true;
      pon state actor target pai consumed;
      state.last_cans <- { state.last_cans with can_discard = true; target_actor = actor })
  | Mjai.Daiminkan { actor; target; pai; consumed; _ } ->
    if actor = state.player_id
    then (
      if old_cans.can_ron_agari then state.at_furiten <- true;
      daiminkan state actor target pai consumed;
      state.last_cans <- { state.last_cans with target_actor = actor })
  | Mjai.Ankan { actor; consumed } ->
    ankan state actor consumed;
    if actor = state.player_id
    then
      (* After ankan, a rinshan tsumo occurs. *)
      state.last_cans <- { state.last_cans with target_actor = actor }
    else (
      (* Check for Kokushi Chankan *)
      let tile = consumed.(0) in
      if check_can_ron state tile true
      then (
        (* Note: In strict rules, only Kokushi can rob Ankan. 
              Here we assume if we wait on it, we can ron it (simplification). 
              A more robust check would verify yaku. *)
        state.last_kawa_tile <- Some tile;
        state.last_cans
        <- { state.last_cans with can_ron_agari = true; target_actor = actor };
        state.chankan_chance <- true))
  | Mjai.Kakan { actor; pai; _ } ->
    kakan state actor pai;
    if actor = state.player_id
    then
      (* After kakan, a rinshan tsumo may occur. *)
      state.last_cans <- { state.last_cans with target_actor = actor }
    else if
      (* Check for Chankan *)
      check_can_ron state pai true
    then (
      state.last_kawa_tile <- Some pai;
      state.last_cans
      <- { state.last_cans with can_ron_agari = true; target_actor = actor };
      state.chankan_chance <- true)
  | Mjai.Reach { actor } ->
    reach state actor;
    (* The Reach event is followed by a Dahai event, which will set the cans. *)
    state.last_cans <- { state.last_cans with target_actor = actor }
  | Mjai.Reach_accepted { actor } ->
    reach_accepted state actor;
    () (* No immediate action *)
  | Mjai.Hora { actor; target; _ } ->
    (* This event ends the hand, but we can set the cans for clarity. *)
    if actor = state.player_id
    then
      state.last_cans
      <- { state.last_cans with
           can_tsumo_agari = target = actor
         ; can_ron_agari = target <> actor
         }
  | Mjai.Ryukyoku _ -> state.last_cans <- { state.last_cans with can_ryukyoku = true }
  | Mjai.Dora { dora_marker } ->
    add_dora_indicator state dora_marker;
    () (* No action change *)
  | Mjai.End_kyoku | Mjai.End_game | Mjai.None -> () (* No action *)
;;

(** Helper to check if a tile is yaokyuu (terminal or honor) *)
let is_yaokyuu (tile : int) : bool = Tiles.is_yaokyuu tile

(** Count number of unique yaokyuu tile types in hand *)
let yaokyuu_kind_count (state : player_state) : int =
  let count = ref 0 in
  let check_indices =
    [ Tiles.tile_id_1m
    ; Tiles.tile_id_9m
    ; Tiles.tile_id_1p
    ; Tiles.tile_id_9p
    ; Tiles.tile_id_1s
    ; Tiles.tile_id_9s
    ; Tiles.tile_id_E
    ; Tiles.tile_id_S
    ; Tiles.tile_id_W
    ; Tiles.tile_id_N
    ; Tiles.tile_id_P
    ; Tiles.tile_id_F
    ; Tiles.tile_id_C
    ]
  in
  List.iter (fun idx -> if state.tehai.(idx) > 0 then incr count) check_indices;
  !count
;;

(** Calculate agari points.
    @param state The player state
    @param is_ron True if winning by ron, false if tsumo
    @param ura_indicators List of ura dora indicator tiles
    @return Result containing points or error message *)
let agari_points (state : player_state) (is_ron : bool) (ura_indicators : int list)
  : (Point.point, string) result
  =
  let can_agari =
    if is_ron then state.last_cans.can_ron_agari else state.last_cans.can_tsumo_agari
  in
  if not can_agari
  then Error "cannot agari"
  else if
    (* Special case for Tenhou/Chihou (handled as yakuman) - strictly speaking not fully implemented here yet *)
    (not is_ron) && state.can_w_riichi
  then Ok (Point.yakuman (state.oya = 0) 1)
  else (
    let winning_tile_opt =
      if is_ron then state.last_kawa_tile else state.last_self_tsumo
    in
    match winning_tile_opt with
    | None -> Error "cannot find the winning tile"
    | Some winning_tile ->
      let additional_hans = ref 0 in
      if state.riichi_accepted.(0) then incr additional_hans;
      if state.is_w_riichi then incr additional_hans;
      if state.at_ippatsu then incr additional_hans;
      if state.tiles_left = 0 && if is_ron then true else not state.at_rinshan
      then incr additional_hans;
      (* Haitei/Houtei *)
      if state.at_rinshan then incr additional_hans;
      (* Rinshan *)
      if is_ron && state.chankan_chance then incr additional_hans;
      (* Chankan *)
      if (not is_ron) && state.is_menzen then incr additional_hans;
      (* Menzen Tsumo *)
      let tehai = Array.copy state.tehai in
      let final_doras_owned = ref state.doras_owned.(0) in
      if is_ron
      then (
        let tid = Tiles.deaka winning_tile in
        tehai.(tid) <- tehai.(tid) + 1;
        final_doras_owned := !final_doras_owned + state.dora_factor.(tid);
        if Tiles.is_aka winning_tile then incr final_doras_owned);
      if state.riichi_accepted.(0)
      then
        List.iter
          (fun ura ->
             let next = Tiles.next ura in
             let next_idx = Tiles.deaka next in
             let count = ref tehai.(next_idx) in
             (* Add count from ankans *)
             List.iter
               (fun t -> if Tiles.deaka t = next_idx then count := !count + 4)
               state.ankans;
             (* Add count from open melds *)
             List.iter
               (fun meld ->
                  List.iter (fun t -> if Tiles.deaka t = next_idx then incr count) meld)
               state.fuuro_overview.(state.player_id);
             final_doras_owned := !final_doras_owned + !count)
          ura_indicators;
      let calc : Agari.agari_calculator =
        { tehai
        ; winning_tile = Tiles.deaka winning_tile
        ; bakaze = state.bakaze
        ; jikaze = state.jikaze
        ; is_menzen = state.is_menzen
        ; is_ron
        ; chis = state.chis
        ; pons = state.pons
        ; minkans = state.minkans
        ; ankans = state.ankans
        }
      in
      (match Agari.agari calc !additional_hans !final_doras_owned with
       | Some result -> Ok (Agari.point result (state.oya = state.player_id))
       | None -> Error "not a hora hand"))
;;

(** Rule-based agari decision logic (minogashi / all last handling) *)
let rule_based_agari (state : player_state) : bool =
  let can_ron = state.last_cans.can_ron_agari in
  let can_tsumo = state.last_cans.can_tsumo_agari in
  if not (can_ron || can_tsumo)
  then false
  else if
    (* Helper to check if we should agari based on rules *)
    (* Rule-based agari decision logic *)

    (* Agari if it is not yet all-last, or we are oya ourselves, or we are not the last place *)
    (not state.is_all_last) || state.oya = state.player_id || state.rank < 3
  then true
  else (
    (* If we are in west round (sudden death) but not yet W4 *)
    let is_west_round = state.bakaze = Tiles.tile_id_W in
    if is_west_round && state.kyoku < 3
    then true
    else if Array.for_all (fun s -> s < 30000) state.scores
    then true (* West entry check *)
    else (
      (* Calculate theoretical max score *)
      (* Simplified implementation: assuming no ura dora if not riichi, or iterate ura doras if riichi *)
      let calculate_max_win_point () =
        if state.riichi_accepted.(0)
        then (
          (* Copy tehai to include ankans and open melds *)
          let tehai_full = Array.copy state.tehai in
          List.iter
            (fun t_idx ->
               if t_idx < 0 || t_idx >= 34
               then Printf.eprintf "DEBUG: invalid ankan tile %d\n" t_idx;
               tehai_full.(t_idx) <- tehai_full.(t_idx) + 4)
            state.ankans;
          (* ankans stores the tile ID *)
          List.iter
            (fun meld ->
               List.iter
                 (fun t ->
                    let idx = Tiles.deaka t in
                    if idx >= 0 && idx < 34 then tehai_full.(idx) <- tehai_full.(idx) + 1)
                 meld)
            state.fuuro_overview.(state.player_id);
          (* Create a list of (tile_idx, count) and sort by count descending *)
          let tehai_ordered = ref [] in
          Array.iteri
            (fun idx count ->
               if count > 0 then tehai_ordered := (idx, count) :: !tehai_ordered)
            tehai_full;
          let sorted_tehai =
            List.sort (fun (_, c1) (_, c2) -> compare c2 c1) !tehai_ordered
          in
          (* Try possible uradoras one by one *)
          let tiles_seen = Array.copy state.tiles_seen in
          let ura_indicators = ref [] in
          let num_dora_indicators = List.length state.dora_indicators in
          let rec find_indicators candidates =
            match candidates with
            | [] -> ()
            | (t_idx, _) :: rest ->
              let ura_ind = Tiles.prev t_idx in
              let rec add_indicators () =
                if List.length !ura_indicators >= num_dora_indicators
                then ()
                else if tiles_seen.(ura_ind) >= 4
                then
                  (* Try next most valuable *)
                  find_indicators rest
                else (
                  ura_indicators := ura_ind :: !ura_indicators;
                  tiles_seen.(ura_ind) <- tiles_seen.(ura_ind) + 1;
                  add_indicators ())
              in
              add_indicators ()
          in
          find_indicators sorted_tehai;
          match agari_points state can_ron !ura_indicators with
          | Ok p -> p
          | Error _ -> { ron = 0; tsumo_ko = 0; tsumo_oya = 0 })
        else (
          match agari_points state can_ron [] with
          | Ok p -> p
          | Error _ -> { ron = 0; tsumo_ko = 0; tsumo_oya = 0 })
      in
      let max_win_point = calculate_max_win_point () in
      let current_scores_rel = state.scores in
      (* relative scores *)
      let exp_scores = Array.copy current_scores_rel in
      if can_ron
      then (
        (* Ron *)
        let target_rel = rel state state.last_cans.target_actor in
        exp_scores.(0)
        <- exp_scores.(0)
           + max_win_point.ron
           + (state.kyotaku * 1000)
           + (state.honba * 300);
        exp_scores.(target_rel)
        <- exp_scores.(target_rel) - (max_win_point.ron + (state.honba * 300)))
      else (
        (* Tsumo *)
        let total_gain =
          (max_win_point.tsumo_ko * 2)
          + max_win_point.tsumo_oya
          + (state.kyotaku * 1000)
          + (state.honba * 300)
        in
        (* Note: Point structure has tsumo_ko and tsumo_oya *)
        exp_scores.(0) <- exp_scores.(0) + total_gain;
        let oya_rel = rel state state.oya in
        for i = 1 to 3 do
          if i = oya_rel
          then
            exp_scores.(i)
            <- exp_scores.(i) - (max_win_point.tsumo_oya + (state.honba * 100))
          else
            exp_scores.(i)
            <- exp_scores.(i) - (max_win_point.tsumo_ko + (state.honba * 100))
        done);
      (* Agari if West entry is possible (all < 30000) *)
      if Array.for_all (fun s -> s < 30000) exp_scores
      then true
      else (
        (* Agari if we avoid last place *)
        let new_rank = get_rank state exp_scores in
        new_rank < 3)))
;;

(** Get discard candidates (aka aware) *)
let discard_candidates_aka (state : player_state) : bool array =
  assert state.last_cans.can_discard;
  let ret = Array.make 37 false in
  if state.riichi_accepted.(0)
  then (
    match state.last_self_tsumo with
    | Some tile ->
      let idx =
        if Tiles.is_aka tile
        then (
          match tile with
          | t when t = Tiles.tile_id_5mr -> 34
          | t when t = Tiles.tile_id_5pr -> 35
          | t when t = Tiles.tile_id_5sr -> 36
          | _ -> Tiles.deaka tile)
        else tile
      in
      if idx < 37 then ret.(idx) <- true;
      ret
    | None -> ret (* Should not happen *))
  else (
    for i = Tiles.tile_id_1m to Tiles.tile_id_C do
      if state.tehai.(i) > 0
      then (
        let allowed =
          if state.riichi_declared.(0)
          then
            if state.shanten = 1
            then state.next_shanten_discards.(i)
            else state.keep_shanten_discards.(i)
          else not state.forbidden_tiles.(i)
        in
        if allowed then ret.(i) <- true)
    done;
    (* Handle aka tiles *)
    if ret.(4) && state.akas_in_hand.(0)
    then (
      (* 5m *)
      ret.(34) <- true;
      (* 5mr *)
      ret.(4) <- state.tehai.(4) > 1);
    if ret.(13) && state.akas_in_hand.(1)
    then (
      (* 5p *)
      ret.(35) <- true;
      (* 5pr *)
      ret.(13) <- state.tehai.(13) > 1);
    if ret.(22) && state.akas_in_hand.(2)
    then (
      (* 5s *)
      ret.(36) <- true;
      (* 5sr *)
      ret.(22) <- state.tehai.(22) > 1);
    ret)
;;

(** Get discard candidates that lead to unconditional tenpai *)
let discard_candidates_with_unconditional_tenpai_aka (state : player_state) : bool array =
  assert state.last_cans.can_discard;
  let ret = Array.make 37 false in
  (* Conditions where impossible or trivial *)
  if
    state.tiles_left = 0
    || state.shanten > 1
    || (state.shanten = 1 && not state.has_next_shanten_discard)
  then ret
  else (
    (* Check if already agari/furiten/riichi constraints *)
    let skip = ref false in
    (match state.last_self_tsumo with
     | Some last_tsumo ->
       if state.waits.(Tiles.deaka last_tsumo) then skip := true
       else if state.riichi_accepted.(0) && not state.at_furiten
       then (
         (* already riichi and not furiten - can only discard drawn tile *)
         let idx =
           if Tiles.is_aka last_tsumo
           then (
             match last_tsumo with
             | t when t = Tiles.tile_id_5mr -> 34
             | t when t = Tiles.tile_id_5pr -> 35
             | t when t = Tiles.tile_id_5sr -> 36
             | _ -> Tiles.deaka last_tsumo)
           else last_tsumo
         in
         if idx < 37 then ret.(idx) <- true;
         skip := true)
     | None -> if Shanten.calc_all state.tehai state.tehai_len_div3 = -1 then skip := true);
    if !skip
    then ret
    else (
      let tenpai_discards =
        if state.shanten = 1
        then state.next_shanten_discards
        else state.keep_shanten_discards
      in
      for discard = Tiles.tile_id_1m to Tiles.tile_id_C do
        if tenpai_discards.(discard) && not state.forbidden_tiles.(discard)
        then (
          let tehai_3n1 = Array.copy state.tehai in
          tehai_3n1.(discard) <- tehai_3n1.(discard) - 1;
          let has_yaku = ref false in
          let is_furiten = ref false in
          (* Check every seen tile as potential winning tile *)
          (* Iterate all 34 tiles *)
          for tsumo = Tiles.tile_id_1m to Tiles.tile_id_C do
            if not !is_furiten
            then
              if tsumo <> discard && tehai_3n1.(tsumo) < 4
              then (
                let tehai_3n2 = Array.copy tehai_3n1 in
                tehai_3n2.(tsumo) <- tehai_3n2.(tsumo) + 1;
                (* If this tile makes it agari *)
                if Shanten.calc_all tehai_3n2 state.tehai_len_div3 = -1
                then
                  if
                    (* Check furiten *)
                    state.discarded_tiles.(tsumo)
                  then is_furiten := true
                  else if (not !has_yaku) && state.tiles_seen.(tsumo) < 4
                  then (
                    (* Check yaku *)
                    let agari_calc : Agari.agari_calculator =
                      { tehai = tehai_3n2
                      ; winning_tile = tsumo
                      ; bakaze = state.bakaze
                      ; jikaze = state.jikaze
                      ; is_menzen = state.is_menzen
                      ; is_ron = true
                      ; chis = state.chis
                      ; pons = state.pons
                      ; minkans = state.minkans
                      ; ankans = state.ankans
                      }
                    in
                    if Option.is_some (Agari.search_yakus agari_calc)
                    then has_yaku := true))
          done;
          if !has_yaku && not !is_furiten then ret.(discard) <- true)
      done;
      (* Handle aka tiles *)
      if ret.(4) && state.akas_in_hand.(0)
      then (
        ret.(34) <- true;
        ret.(4) <- state.tehai.(4) > 1);
      if ret.(13) && state.akas_in_hand.(1)
      then (
        ret.(35) <- true;
        ret.(13) <- state.tehai.(13) > 1);
      if ret.(22) && state.akas_in_hand.(2)
      then (
        ret.(36) <- true;
        ret.(22) <- state.tehai.(22) > 1);
      ret))
;;

(** {1 Getter Functions}

    These functions provide read-only access to player state fields.
    Getter functions for accessing state fields. *)

(** Get player ID *)
let player_id (state : player_state) : int = state.player_id

(** Get current round number (kyoku) *)
let kyoku (state : player_state) : int = state.kyoku

(** Get number of repeat counters (honba) *)
let honba (state : player_state) : int = state.honba

(** Get number of riichi bets (kyotaku) *)
let kyotaku (state : player_state) : int = state.kyotaku

(** Check if player is dealer (oya) *)
let is_oya (state : player_state) : bool = state.oya = state.player_id

(** Get hand tiles (34-element array) *)
let tehai (state : player_state) : int array = state.tehai

(** Check if player can declare riichi *)
let can_w_riichi (state : player_state) : bool = state.can_w_riichi

(** Check if player has declared riichi *)
let self_riichi_declared (state : player_state) : bool = state.riichi_declared.(0)

(** Check if player's riichi was accepted *)
let self_riichi_accepted (state : player_state) : bool = state.riichi_accepted.(0)

(** Get last drawn tile (tsumo) *)
let last_self_tsumo (state : player_state) : int option = state.last_self_tsumo

(** Get last discarded tile *)
let last_kawa_tile (state : player_state) : int option = state.last_kawa_tile

(** Get available actions *)
let last_cans (state : player_state) : action_candidate = state.last_cans

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
  for tile_idx = Tiles.tile_id_1m to Tiles.tile_id_C do
    if state.tehai.(tile_idx) >= 4 then candidates := tile_idx :: !candidates
  done;
  Array.of_list (List.rev !candidates)
;;

(** Get candidates for kakan (open kan -> closed kan) *)
let kakan_candidates (state : player_state) : int list =
  (* Find pon melds where we have the 4th tile in hand *)
  List.filter_map
    (fun pon_tile ->
       let pon_idx = Tiles.deaka pon_tile in
       if pon_idx >= 0 && pon_idx < 34 && state.tehai.(pon_idx) > 0
       then Some pon_idx
       else None)
    state.pons
;;

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

(* Returns: [player_id][meld_index][tile_index] *)

(** Get fuuro overview - all melds for all players with constituent tiles *)
let fuuro_overview (state : player_state) : int list list array = state.fuuro_overview

(** ==================================================================== *)
(** Single Player Tables *)
(** ==================================================================== *)

(** Single player tables for RL agent *)
type single_player_tables = {
  max_ev_table : Sp.candidate list;
}


(** ==================================================================== *)
(** Observation Encoding *)
(** ==================================================================== *)

let action_space = 46

let obs_shape version =
  match version with
  | 1 -> 938, 34
  | 2 -> 942, 34
  | 3 -> 934, 34
  | 4 -> 1012, 34
  | _ -> failwith "invalid version"
;;

type obs =
  { features : float array
  ; mask : bool array
  }

type context =
  { _state : player_state
  ; arr : float array
  ; rows : int
  ; cols : int
  ; mask : bool array
  ; mutable idx : int
  ; _at_kan_select : bool
  ; version : int
  }

let create_context state version at_kan_select =
  let rows, cols = obs_shape version in
  { _state = state
  ; arr = Array.make (rows * cols) 0.0
  ; rows
  ; cols
  ; mask = Array.make action_space false
  ; idx = 0
  ; _at_kan_select = at_kan_select
  ; version
  }
;;

let assign_row ctx row_offset col value =
  if ctx.idx + row_offset < ctx.rows && col < ctx.cols
  then ctx.arr.(((ctx.idx + row_offset) * ctx.cols) + col) <- value
;;

let fill_row ctx row_offset value =
  if ctx.idx + row_offset < ctx.rows
  then
    for j = 0 to ctx.cols - 1 do
      ctx.arr.(((ctx.idx + row_offset) * ctx.cols) + j) <- value
    done
;;

let assign_rows ctx row_offset col n value =
  for i = 0 to n - 1 do
    assign_row ctx (row_offset + i) col value
  done
;;

let fill_rows ctx row_offset n value =
  for i = 0 to n - 1 do
    fill_row ctx (row_offset + i) value
  done
;;

type integer_encoder =
  { n : int
  ; cap : int
  ; one_hot : bool
  ; rescale : bool
  ; rbf_intervals : int option
  }

let encode_int ctx enc =
  let n = min enc.n enc.cap in
  match ctx.version with
  | 1 ->
    fill_rows ctx 0 n 1.0;
    ctx.idx <- ctx.idx + enc.cap
  | 2 | 3 ->
    if enc.one_hot
    then (
      fill_row ctx n 1.0;
      ctx.idx <- ctx.idx + enc.cap + 1);
    if enc.rescale
    then (
      let v = float_of_int n /. float_of_int enc.cap in
      fill_row ctx 0 v;
      ctx.idx <- ctx.idx + 1);
    (match enc.rbf_intervals with
     | Some intervals ->
       let interval_size = float_of_int enc.cap /. float_of_int intervals in
       for i = 1 to intervals - 1 do
         let x = float_of_int enc.n in
         let mu = float_of_int i *. interval_size in
         let sigma = interval_size in
         let v = exp ((-.((x -. mu) ** 2.0)) /. (2.0 *. (sigma ** 2.0))) in
         fill_row ctx 0 v;
         ctx.idx <- ctx.idx + 1
       done
     | None -> ())
  | 4 ->
    if enc.one_hot
    then (
      fill_row ctx n 1.0;
      ctx.idx <- ctx.idx + enc.cap + 1);
    if enc.rescale
    then (
      let v = float_of_int n /. float_of_int enc.cap in
      fill_row ctx 0 v;
      ctx.idx <- ctx.idx + 1)
  | _ -> assert false
;;

let encode_tile_set ctx tiles =
  let counts = Array.make 34 0 in
  List.iter
    (fun tile ->
       let tid = Tiles.deaka tile in
       if tid < ctx.cols
       then (
         let i = counts.(tid) in
         if i < 4 then (
           assign_row ctx i tid 1.0;
           counts.(tid) <- i + 1
         );
         if Tiles.is_aka tile then (
           let aka_idx = match tile with
             | t when t = Tiles.tile_id_5mr -> 0
             | t when t = Tiles.tile_id_5pr -> 1
             | t when t = Tiles.tile_id_5sr -> 2
             | _ -> -1
           in
           if aka_idx >= 0 then fill_row ctx (4 + aka_idx) 1.0
         )
       ))
    tiles;
  ctx.idx <- ctx.idx + 7
;;

let encode_self_kawa ctx item_opt =
  match item_opt with
  | Some item ->
    List.iter (fun t -> assign_row ctx 0 (Tiles.deaka t) 1.0) item.kan;
    let tid = Tiles.deaka item.sutehai.tile in
    assign_row ctx 1 tid 1.0;
    if Tiles.is_aka item.sutehai.tile then fill_row ctx 2 1.0;
    if item.sutehai.is_dora then fill_row ctx 3 1.0;
    ctx.idx <- ctx.idx + 4
  | None -> ctx.idx <- ctx.idx + 4
;;

let encode_kawa ctx item_opt =
  match item_opt with
  | Some item ->
    (match item.chi_pon with
     | Some cp ->
       assign_row ctx 0 (Tiles.deaka cp.consumed.(0)) 1.0;
       assign_row ctx 0 (Tiles.deaka cp.consumed.(1)) 1.0
     | None -> ());
    List.iter (fun t -> assign_row ctx 1 (Tiles.deaka t) 1.0) item.kan;
    let tid = Tiles.deaka item.sutehai.tile in
    assign_row ctx 2 tid 1.0;
    if Tiles.is_aka item.sutehai.tile then fill_row ctx 3 1.0;
    if item.sutehai.is_tedashi then fill_row ctx 4 1.0;
    if item.sutehai.is_riichi then fill_row ctx 5 1.0;
    if item.sutehai.is_dora then fill_row ctx 6 1.0;
    ctx.idx <- ctx.idx + 8
  | None -> ctx.idx <- ctx.idx + 8
;;

let encode_obs state version at_kan_select =
  let ctx = create_context state version at_kan_select in
  (* Tehai *)
  for tid = 0 to 33 do
    let count = state.tehai.(tid) in
    if count > 0 then assign_rows ctx 0 tid count 1.0
  done;
  ctx.idx <- ctx.idx + 4;
  (* Akas in hand *)
  for i = 0 to 2 do
    if state.akas_in_hand.(i) then fill_row ctx i 1.0
  done;
  ctx.idx <- ctx.idx + 3;
  (* Scores *)
  for i = 0 to 3 do
    let score = state.scores.(i) in
    let v = float_of_int (max 0 (min score 100000)) /. 100000.0 in
    fill_row ctx 0 v;
    ctx.idx <- ctx.idx + 1;
    match version with
    | 2 | 3 ->
      encode_int
        ctx
        { n = score / 100
        ; cap = 500
        ; one_hot = false
        ; rescale = false
        ; rbf_intervals = Some 10
        }
    | 4 ->
      let v2 = float_of_int (max 0 (min score 30000)) /. 30000.0 in
      fill_row ctx 0 v2;
      ctx.idx <- ctx.idx + 1
    | _ -> ()
  done;
  (* Rank *)
  fill_row ctx state.rank 1.0;
  ctx.idx <- ctx.idx + 4;
  (* Kyoku *)
  (match version with
   | 1 -> fill_rows ctx 0 state.kyoku 1.0
   | _ -> fill_row ctx state.kyoku 1.0);
  ctx.idx <- ctx.idx + 4;
  (* Honba and Kyotaku *)
  let cap = if version = 1 || version = 4 then 10 else 6 in
  encode_int
    ctx
    { n = state.honba
    ; cap
    ; one_hot = false
    ; rescale = version = 4
    ; rbf_intervals = Some 3
    };
  encode_int
    ctx
    { n = state.kyotaku
    ; cap
    ; one_hot = false
    ; rescale = version = 4
    ; rbf_intervals = Some 3
    };
  (* Winds *)
  assign_row ctx 0 (Tiles.deaka state.bakaze) 1.0;
  assign_row ctx 1 (Tiles.deaka state.jikaze) 1.0;
  ctx.idx <- ctx.idx + 2;
  if version >= 2
  then (
    let n = ((if Tiles.deaka state.bakaze = Tiles.tile_id_E then 0 else 1) * 4) + state.kyoku in
    encode_int ctx { n; cap = 7; one_hot = false; rescale = true; rbf_intervals = None });
  (* Dora indicators *)
  encode_tile_set ctx state.dora_indicators;
  (* Self Kawa *)
  let self_kawa = List.rev state.kawa.(0) in
  let self_kawa_len = List.length self_kawa in
  let rec take n l =
    if n <= 0
    then []
    else (
      match l with
      | [] -> []
      | h :: t -> h :: take (n - 1) t)
  in
  List.iter (encode_self_kawa ctx) (take 6 self_kawa);
  ctx.idx <- ctx.idx + (max 0 (6 - self_kawa_len) * 4);
  let rev_kawa = List.rev self_kawa in
  List.iter (encode_self_kawa ctx) (take 18 rev_kawa);
  ctx.idx <- ctx.idx + (max 0 (18 - self_kawa_len) * 4);
  let max_kawa_len = ref 0 in
  for i = 0 to 3 do
    max_kawa_len := max !max_kawa_len (List.length state.kawa.(i))
  done;
  let max_kawa_len = !max_kawa_len in
  if version >= 3
  then (
    List.iteri
      (fun turn item_opt ->
         match item_opt with
         | Some item ->
           let tid = Tiles.deaka item.sutehai.tile in
           let v = exp (-0.2 *. float_of_int (max_kawa_len - 1 - turn)) in
           assign_row ctx 0 tid v
         | None -> ())
      self_kawa;
    ctx.idx <- ctx.idx + 1);
  (* Others Kawa *)
  for i = 1 to 3 do
    let player_kawa = List.rev state.kawa.(i) in
    let player_kawa_len = List.length player_kawa in
    List.iter (encode_kawa ctx) (take 6 player_kawa);
    ctx.idx <- ctx.idx + (max 0 (6 - player_kawa_len) * 8);
    let rev_pkawa = List.rev player_kawa in
    List.iter (encode_kawa ctx) (take 18 rev_pkawa);
    ctx.idx <- ctx.idx + (max 0 (18 - player_kawa_len) * 8);
    match version with
    | 2 ->
      List.iteri
        (fun turn item_opt ->
           match item_opt with
           | Some item ->
             let row = min (turn / 6) 2 in
             let tid = Tiles.deaka item.sutehai.tile in
             assign_row ctx row tid 1.0;
             if item.sutehai.is_tedashi then assign_row ctx (3 + row) tid 1.0
           | None -> ())
        player_kawa;
      ctx.idx <- ctx.idx + 6
    | 3 | 4 ->
      List.iteri
        (fun turn item_opt ->
           match item_opt with
           | Some item ->
             let tid = Tiles.deaka item.sutehai.tile in
             let v = exp (-0.2 *. float_of_int (max_kawa_len - 1 - turn)) in
             assign_row ctx 0 tid v;
             if item.sutehai.is_tedashi then assign_row ctx 1 tid v;
             if item.sutehai.is_riichi then assign_row ctx 2 tid v
           | None -> ())
        player_kawa;
      ctx.idx <- ctx.idx + 3
    | _ -> ()
  done;
  (* Tiles left *)
  let v = float_of_int state.tiles_left /. 69.0 in
  fill_row ctx 0 v;
  ctx.idx <- ctx.idx + 1;
  (* Doras owned *)
  for i = 0 to 3 do
    encode_int
      ctx
      { n = state.doras_owned.(i)
      ; cap = 12
      ; one_hot = false
      ; rescale = true
      ; rbf_intervals = Some 3
      }
  done;
  (* Doras unseen *)
  let doras_unseen = (List.length state.dora_indicators * 4) + 3 - state.doras_seen in
  encode_int
    ctx
    { n = doras_unseen
    ; cap = (5 * 4) + 3
    ; one_hot = false
    ; rescale = true
    ; rbf_intervals = Some 4
    };
  (* Kawa Overview *)
  for i = 0 to 3 do
    encode_tile_set ctx state.kawa_overview.(i)
  done;
  (* Fuuro Overview *)
  for p = 0 to 3 do
    let player_fuuro = state.fuuro_overview.(p) in
    List.iter
      (fun meld ->
         List.iter
           (fun tile ->
              let tid = Tiles.deaka tile in
              let rec find_row r =
                if r >= 4
                then ()
                else if ctx.arr.(((ctx.idx + r) * ctx.cols) + tid) = 0.0
                then assign_row ctx r tid 1.0
                else find_row (r + 1)
              in
              find_row 0;
              if Tiles.is_aka tile then fill_row ctx 4 1.0)
           meld;
         ctx.idx <- ctx.idx + 5)
      player_fuuro;
    ctx.idx <- ctx.idx + ((4 - List.length player_fuuro) * 5)
  done;
  (* Ankan Overview *)
  for p = 0 to 3 do
    List.iter (fun tid -> assign_row ctx 0 tid 1.0) state.ankan_overview.(p);
    ctx.idx <- ctx.idx + 1
  done;
  if version >= 2
  then (
    (* Tiles Seen *)
    for tid = 0 to 33 do
      assign_row ctx 0 tid (float_of_int state.tiles_seen.(tid) /. 4.0)
    done;
    ctx.idx <- ctx.idx + 1;
    (* Last Tedashis *)
    for i = 1 to 3 do
      (match state.last_tedashis.(i) with
       | Some sute ->
         let tid = Tiles.deaka sute.tile in
         assign_row ctx 0 tid 1.0;
         if Tiles.is_aka sute.tile then fill_row ctx 1 1.0;
         if sute.is_dora then fill_row ctx 2 1.0
       | None -> ());
      ctx.idx <- ctx.idx + 3
    done;
    (* Riichi Sutehais *)
    for i = 1 to 3 do
      let found = ref false in
      List.iter
        (function
          | Some item when item.sutehai.is_riichi ->
            let tid = Tiles.deaka item.sutehai.tile in
            assign_row ctx 0 tid 1.0;
            if Tiles.is_aka item.sutehai.tile then fill_row ctx 1 1.0;
            if item.sutehai.is_dora then fill_row ctx 2 1.0;
            found := true
          | _ -> ())
        state.kawa.(i);
      ctx.idx <- ctx.idx + 3
    done);
  (* Riichi status *)
  for i = 1 to 3 do
    if state.riichi_declared.(i) then fill_row ctx 0 1.0;
    ctx.idx <- ctx.idx + 1
  done;
  for i = 1 to 3 do
    if state.riichi_accepted.(i) then fill_row ctx 0 1.0;
    ctx.idx <- ctx.idx + 1
  done;
  (* Waits *)
  for tid = 0 to 33 do
    if state.waits.(tid) then assign_row ctx 0 tid 1.0
  done;
  ctx.idx <- ctx.idx + 1;
  if state.at_furiten then fill_row ctx 0 1.0;
  ctx.idx <- ctx.idx + 1;
  (* Shanten *)
  encode_int
    ctx
    { n = state.shanten; cap = 6; one_hot = true; rescale = false; rbf_intervals = None };
  if state.riichi_accepted.(0) then fill_row ctx 0 1.0;
  ctx.idx <- ctx.idx + 1;
  if at_kan_select then fill_row ctx 0 1.0;
  ctx.idx <- ctx.idx + 1;
  (* Action Mask *)
  let cans = state.last_cans in
  if can_pass cans
  then (
    match state.last_kawa_tile with
    | Some tile ->
      let tid = Tiles.deaka tile in
      assign_row ctx 0 tid 1.0;
      if Tiles.is_aka tile then fill_row ctx 1 1.0;
      if state.dora_factor.(tid) > 0 then fill_row ctx 2 1.0;
      if not at_kan_select
      then ctx.mask.(action_space - 1) <- true
      else if cans.can_daiminkan
      then ctx.mask.(tid) <- true
    | None -> ());
  ctx.idx <- ctx.idx + 3;
  if cans.can_discard
  then (
    let discards = discard_candidates_aka state in
    Array.iteri
      (fun tid b ->
         if b
         then (
           let deaka_tid = Tiles.deaka tid in
           assign_row ctx 0 deaka_tid 1.0;
           if not at_kan_select then ctx.mask.(tid) <- true))
      discards;
    for tid = 0 to 33 do
      if state.keep_shanten_discards.(tid) then assign_row ctx 1 tid 1.0;
      if state.next_shanten_discards.(tid) then assign_row ctx 2 tid 1.0
    done;
    if state.shanten <= 1
    then (
      let unconditional = discard_candidates_with_unconditional_tenpai_aka state in
      Array.iteri (fun tid b -> if b then assign_row ctx 3 (Tiles.deaka tid) 1.0) unconditional);
    if state.riichi_declared.(0) then fill_row ctx 4 1.0);
  ctx.idx <- ctx.idx + 5;
  if cans.can_riichi
  then (
    fill_row ctx 0 1.0;
    if not at_kan_select then ctx.mask.(37) <- true);
  ctx.idx <- ctx.idx + 1;
  if cans.can_chi_low
  then (
    fill_row ctx 0 1.0;
    if not at_kan_select then ctx.mask.(38) <- true);
  if cans.can_chi_mid
  then (
    fill_row ctx 1 1.0;
    if not at_kan_select then ctx.mask.(39) <- true);
  if cans.can_chi_high
  then (
    fill_row ctx 2 1.0;
    if not at_kan_select then ctx.mask.(40) <- true);
  ctx.idx <- ctx.idx + 3;
  if cans.can_pon
  then (
    fill_row ctx 0 1.0;
    if not at_kan_select then ctx.mask.(41) <- true);
  ctx.idx <- ctx.idx + 1;
  if cans.can_daiminkan
  then (
    fill_row ctx 0 1.0;
    if not at_kan_select then ctx.mask.(42) <- true);
  ctx.idx <- ctx.idx + 1;
  if cans.can_ankan
  then (
    List.iter
      (fun tid ->
         assign_row ctx 0 tid 1.0;
         if at_kan_select then ctx.mask.(tid) <- true)
      state.ankan_candidates;
    if not at_kan_select then ctx.mask.(42) <- true);
  ctx.idx <- ctx.idx + 1;
  if cans.can_kakan
  then (
    List.iter
      (fun tid ->
         assign_row ctx 0 tid 1.0;
         if at_kan_select then ctx.mask.(tid) <- true)
      state.kakan_candidates;
    if not at_kan_select then ctx.mask.(42) <- true);
  ctx.idx <- ctx.idx + 1;
  if can_agari cans
  then (
    fill_row ctx 0 1.0;
    if not at_kan_select then ctx.mask.(43) <- true);
  ctx.idx <- ctx.idx + 1;
  if cans.can_ryukyoku
  then (
    fill_row ctx 0 1.0;
    if not at_kan_select then ctx.mask.(44) <- true);
  ctx.idx <- ctx.idx + 1;
  (* Version 4 SP features *)
  if version = 4
  then (
    let cur_shanten = real_time_shanten state in
    let can_calc = state.tiles_left >= 4 && cur_shanten >= 0 in
    let tables_opt =
      if not can_calc
      then None
      else (
        let can_discard = ref state.last_cans.can_discard in
        let tsumos_left, calc_haitei =
          if !can_discard
          then state.tiles_left / 4, state.tiles_left mod 4 = 0
          else (
            let target = rel state state.last_cans.target_actor in
            let left = max 0 (state.tiles_left - (4 - target)) in
            left / 4, left mod 4 = 0)
        in
        if tsumos_left < 1
        then None
        else (
          let num_doras_in_fuuro =
            if state.is_menzen && state.ankan_overview.(0) = []
            then 0
            else (
              let num_doras_in_tehai =
                List.fold_left
                  (fun acc ind -> acc + state.tehai.(Tiles.next ind))
                  0
                  state.dora_indicators
              in
              let num_akas =
                let c = ref 0 in
                for i = 0 to 2 do
                  if state.akas_in_hand.(i) then incr c
                done;
                !c
              in
              state.doras_owned.(0) - num_doras_in_tehai - num_akas)
          in
          let prefer_riichi = state.scores.(0) >= 1000 in
          let calc_double_riichi = !can_discard && state.can_w_riichi in
          let tehai = Array.copy state.tehai in
          let akas_in_hand = Array.copy state.akas_in_hand in
          let is_discard_after_riichi = !can_discard && state.riichi_accepted.(0) in
          if is_discard_after_riichi
          then (
            match state.last_self_tsumo with
            | Some last_tsumo ->
              let tid = Tiles.deaka last_tsumo in
              tehai.(tid) <- tehai.(tid) - 1;
              (match last_tsumo with
               | t when t = Tiles.tile_id_5mr -> akas_in_hand.(0) <- false
               | t when t = Tiles.tile_id_5pr -> akas_in_hand.(1) <- false
               | t when t = Tiles.tile_id_5sr -> akas_in_hand.(2) <- false
               | _ -> ());
              can_discard := false
            | None -> ());
          let sp_config =
            Sp.create_config
              ~tehai_len_div3:state.tehai_len_div3
              ~chis:state.chis
              ~pons:state.pons
              ~minkans:state.minkans
              ~ankans:state.ankans
              ~bakaze:state.bakaze
              ~jikaze:state.jikaze
              ~is_menzen:state.is_menzen
              ~num_doras_in_fuuro
              ~dora_indicators:state.dora_indicators
              ~calc_double_riichi
              ~calc_haitei
              ~prefer_riichi
              ~sort_result:true
              ~maximize_win_prob:false
              ~calc_tegawari:false
              ~calc_shanten_down:false
          in
          let sp_init_state =
            Sp.create_init_state
              ~tehai
              ~akas_in_hand
              ~tiles_seen:state.tiles_seen
              ~akas_seen:state.akas_seen
          in
          match Sp.calc sp_config sp_init_state !can_discard tsumos_left cur_shanten with
          | Ok candidates ->
            if is_discard_after_riichi
            then (
              match candidates with
              | first :: rest ->
                let updated =
                  { first with tile = Option.get state.last_self_tsumo }
                in
                Some (updated :: rest)
              | [] -> Some [])
            else Some candidates
          | Error _ -> None))
    in
    match tables_opt with
    | Some (best :: _ as candidates) ->
      let max_ev = best.exp_values.(0) in
      let v1 = max 0.0 (min 100000.0 max_ev) /. 100000.0 in
      fill_row ctx 0 v1;
      let v2 = max 0.0 (min 30000.0 max_ev) /. 30000.0 in
      fill_row ctx 1 v2;
      ctx.idx <- ctx.idx + 2;
      (* Required tiles *)
      if state.last_cans.can_discard
      then (
        List.iter
          (fun (c : Sp.candidate) ->
             let discard_tid = Tiles.deaka c.tile in
             List.iter
               (fun (r : Sp.required_tile) ->
                  let required_tid = Tiles.deaka r.tile in
                  let row_offset = if c.shanten_down then 34 + discard_tid else discard_tid in
                  assign_row ctx row_offset required_tid 1.0)
               c.required_tiles)
          candidates;
        ctx.idx <- ctx.idx + (2 * 34);
        (* Max required tiles tid *)
        let max_req_c =
          List.fold_left
            (fun acc c -> if Sp.cmp_by Sp.NotShantenDown c acc < 0 then c else acc)
            best
            candidates
        in
        assign_row ctx 0 (Tiles.deaka max_req_c.tile) 1.0;
        ctx.idx <- ctx.idx + 2)
      else (
        ctx.idx <- ctx.idx + (2 * 34) + 1;
        List.iter
          (fun (r : Sp.required_tile) ->
             assign_row ctx 0 (Tiles.deaka r.tile) 1.0)
          best.required_tiles;
        ctx.idx <- ctx.idx + 1);
      (* SP Table *)
      if best.tenpai_probs.(0) > 0.0
      then (
        let ev_scale = if max_ev < 1.0 then 0.0 else 1.0 /. max_ev in
        if state.last_cans.can_discard
        then
          List.iter
            (fun (c : Sp.candidate) ->
               let tid = Tiles.deaka c.tile in
               let n = Array.length c.tenpai_probs in
               for turn = 0 to n - 1 do
                 if c.tenpai_probs.(turn) > 0.0
                 then (
                   assign_row ctx turn tid c.tenpai_probs.(turn);
                   assign_row ctx (turn + 17) tid c.win_probs.(turn);
                   assign_row ctx (turn + 34) tid (min 1.0 (c.exp_values.(turn) *. ev_scale)))
               done)
            candidates
        else
          let n = Array.length best.tenpai_probs in
          for turn = 0 to n - 1 do
            if best.tenpai_probs.(turn) > 0.0
            then (
              fill_row ctx turn best.tenpai_probs.(turn);
              fill_row ctx (turn + 17) best.win_probs.(turn);
              fill_row ctx (turn + 34) (min 1.0 (best.exp_values.(turn) *. ev_scale)))
          done);
      ctx.idx <- ctx.idx + 51
    | _ ->
      (* Fallback: minimal tsumo agari *)
      let min_tsumo_agari =
        match agari_points state false [] with
        | Ok point -> float_of_int (Point.tsumo_total point (is_oya state))
        | Error _ -> 0.0
      in
      let v1 = max 0.0 (min 100000.0 min_tsumo_agari) /. 100000.0 in
      fill_row ctx 0 v1;
      let v2 = max 0.0 (min 30000.0 min_tsumo_agari) /. 30000.0 in
      fill_row ctx 1 v2;
      ctx.idx <- ctx.idx + 2 + (2 * 34) + 2 + 51);
  { features = ctx.arr; mask = ctx.mask }
;;
