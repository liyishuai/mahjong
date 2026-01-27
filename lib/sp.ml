(** Single Player (SP) probability calculations and observation encoding *)

(** ==================================================================== *)

(** Tile utilities *)

(** ==================================================================== *)

type required_tile =
  { tile : int
  ; count : int
  }

let create_required_tile (tile : int) (count : int) : required_tile =
  { tile; count }
;;

(** ==================================================================== *)

(** Candidate types and comparison *)

(** ==================================================================== *)

let max_tsumo = 17

type candidate =
  { tile : int
  ; tenpai_probs : float array
  ; win_probs : float array
  ; exp_values : float array
  ; required_tiles : required_tile list
  ; num_required_tiles : int
  ; shanten_down : bool
  }

type candidate_column =
  | EV
  | WinProb
  | TenpaiProb
  | NotShantenDown
  | NumRequiredTiles
  | DiscardPriority

let cmp_tile_discard_priority (t1 : int) (t2 : int) : int =
  Tiles.cmp_discard_priority t1 t2
;;

let rec cmp_by (by : candidate_column) (c1 : candidate) (c2 : candidate) : int =
  if c1.tile = c2.tile
  then 0
  else (
    match by with
    | EV ->
      if c1.exp_values.(0) <> c2.exp_values.(0)
      then compare c2.exp_values.(0) c1.exp_values.(0)
      else cmp_by WinProb c1 c2
    | WinProb ->
      if c1.win_probs.(0) <> c2.win_probs.(0)
      then compare c2.win_probs.(0) c1.win_probs.(0)
      else cmp_by TenpaiProb c1 c2
    | TenpaiProb ->
      if c1.tenpai_probs.(0) <> c2.tenpai_probs.(0)
      then compare c2.tenpai_probs.(0) c1.tenpai_probs.(0)
      else cmp_by NotShantenDown c1 c2
    | NotShantenDown ->
      (match c1.shanten_down, c2.shanten_down with
       | false, true -> -1 (* c1 > c2 *)
       | true, false -> 1 (* c1 < c2 *)
       | _ -> cmp_by NumRequiredTiles c1 c2)
    | NumRequiredTiles ->
      if c1.num_required_tiles <> c2.num_required_tiles
      then compare c2.num_required_tiles c1.num_required_tiles
      else cmp_by DiscardPriority c1 c2
    | DiscardPriority -> cmp_tile_discard_priority c1.tile c2.tile)
;;

(** ==================================================================== *)

(** State types *)

(** ==================================================================== *)

type init_state =
  { tehai : int array
  ; akas_in_hand : bool array
  ; tiles_seen : int array
  ; akas_seen : bool array
  }

let create_init_state
      ~(tehai : int array)
      ~(akas_in_hand : bool array)
      ~(tiles_seen : int array)
      ~(akas_seen : bool array)
  : init_state
  =
  { tehai; akas_in_hand; tiles_seen; akas_seen }
;;

type state =
  { tehai : int array
  ; akas_in_hand : bool array
  ; tiles_left : int array
  ; akas_left : bool array
  ; mutable n_extra_tsumo : int
  }

let of_init_state (init : init_state) : state =
  let tiles_left = Array.init 34 (fun i -> max 0 (4 - init.tiles_seen.(i))) in
  let akas_left = Array.init 3 (fun i -> not init.akas_seen.(i)) in
  { tehai = Array.copy init.tehai
  ; akas_in_hand = Array.copy init.akas_in_hand
  ; tiles_left
  ; akas_left
  ; n_extra_tsumo = 0
  }
;;

let get_tiles_in_hand (state : state) : int list =
  let rec loop i acc =
    if i >= 34 then acc
    else
      let count = state.tehai.(i) in
      let acc' = if count > 0 then acc @ List.init count (fun _ -> i) else acc in
      loop (i + 1) acc'
  in
  loop 0 []
;;

let is_tile_in_hand (state : state) (tile : int) : bool = state.tehai.(tile) > 0
let count_tiles_left (state : state) (tile : int) : int = state.tiles_left.(tile)

(** ==================================================================== *)

(** State Manipulation Operations *)

(** ==================================================================== *)

let discard (state : state) (tile : int) : unit =
  let deaka_tile = Tiles.deaka tile in
  state.tehai.(deaka_tile) <- state.tehai.(deaka_tile) - 1;
  match tile with
  | t when t = Tiles.tile_id_5mr -> state.akas_in_hand.(0) <- false
  | t when t = Tiles.tile_id_5pr -> state.akas_in_hand.(1) <- false
  | t when t = Tiles.tile_id_5sr -> state.akas_in_hand.(2) <- false
  | _ -> ()
;;

let undo_discard (state : state) (tile : int) : unit =
  let deaka_tile = Tiles.deaka tile in
  state.tehai.(deaka_tile) <- state.tehai.(deaka_tile) + 1;
  match tile with
  | t when t = Tiles.tile_id_5mr -> state.akas_in_hand.(0) <- true
  | t when t = Tiles.tile_id_5pr -> state.akas_in_hand.(1) <- true
  | t when t = Tiles.tile_id_5sr -> state.akas_in_hand.(2) <- true
  | _ -> ()
;;

let deal (state : state) (tile : int) : unit =
  let deaka_tile = Tiles.deaka tile in
  state.tiles_left.(deaka_tile) <- state.tiles_left.(deaka_tile) - 1;
  match tile with
  | t when t = Tiles.tile_id_5mr -> state.akas_left.(0) <- false
  | t when t = Tiles.tile_id_5pr -> state.akas_left.(1) <- false
  | t when t = Tiles.tile_id_5sr -> state.akas_left.(2) <- false
  | _ -> ();
  undo_discard state tile
;;

let undo_deal (state : state) (tile : int) : unit =
  discard state tile;
  let deaka_tile = Tiles.deaka tile in
  state.tiles_left.(deaka_tile) <- state.tiles_left.(deaka_tile) + 1;
  match tile with
  | t when t = Tiles.tile_id_5mr -> state.akas_left.(0) <- true
  | t when t = Tiles.tile_id_5pr -> state.akas_left.(1) <- true
  | t when t = Tiles.tile_id_5sr -> state.akas_left.(2) <- true
  | _ -> ()
;;

let with_discard state tile f =
  discard state tile;
  try let res = f () in undo_discard state tile; res
  with e -> undo_discard state tile; raise e
;;

let with_deal state tile f =
  deal state tile;
  try let res = f () in undo_deal state tile; res
  with e -> undo_deal state tile; raise e
;;

let sum_left_tiles (state : state) : int =
  Array.fold_left ( + ) 0 state.tiles_left
;;

type discard_tile =
  { d_tile : int
  ; d_shanten_diff : int
  }

type draw_tile =
  { dr_tile : int
  ; dr_count : int
  ; dr_shanten_diff : int
  }

let safe_calc_shanten tehai len =
  if Array.exists (fun c -> c < 0) tehai then 10 else Shanten.calc_all tehai len
;;

let get_discard_tiles (state : state) (shanten : int) (tehai_len_div3 : int)
  : discard_tile list
  =
  let result = ref [] in
  for tid = 0 to 33 do
    let count = state.tehai.(tid) in
    if count > 0
    then (
      let d_tile =
        match tid with
        | t when t = Tiles.tile_id_5m && state.akas_in_hand.(0) && count = 1
          -> Tiles.tile_id_5mr
        | t when t = Tiles.tile_id_5p && state.akas_in_hand.(1) && count = 1
          -> Tiles.tile_id_5pr
        | t when t = Tiles.tile_id_5s && state.akas_in_hand.(2) && count = 1
          -> Tiles.tile_id_5sr
        | t -> t
      in
      state.tehai.(tid) <- count - 1;
      let shanten_after = safe_calc_shanten state.tehai tehai_len_div3 in
      state.tehai.(tid) <- count;
      let d_shanten_diff = shanten_after - shanten in
      result := { d_tile; d_shanten_diff } :: !result)
  done;
  List.rev !result
;;

let get_draw_tiles (state : state) (shanten : int) (tehai_len_div3 : int)
  : draw_tile list
  =
  let result = ref [] in
  for tid = 0 to 33 do
    let dr_count = state.tiles_left.(tid) in
    if dr_count > 0
    then (
      state.tehai.(tid) <- state.tehai.(tid) + 1;
      let shanten_after = safe_calc_shanten state.tehai tehai_len_div3 in
      state.tehai.(tid) <- state.tehai.(tid) - 1;
      let dr_shanten_diff = shanten_after - shanten in
      match tid with
      | t when (t = Tiles.tile_id_5m && state.akas_left.(0)) ||
               (t = Tiles.tile_id_5p && state.akas_left.(1)) ||
               (t = Tiles.tile_id_5s && state.akas_left.(2)) ->
        if dr_count >= 2 then
          result := { dr_tile = tid; dr_count = dr_count - 1; dr_shanten_diff } :: !result;
        let aka = match tid with
          | t when t = Tiles.tile_id_5m -> Tiles.tile_id_5mr
          | t when t = Tiles.tile_id_5p -> Tiles.tile_id_5pr
          | _ -> Tiles.tile_id_5sr
        in
        result := { dr_tile = aka; dr_count = 1; dr_shanten_diff } :: !result
      | t -> result := { dr_tile = t; dr_count; dr_shanten_diff } :: !result)
  done;
  List.rev !result
;;

let get_required_tiles (state : state) (tehai_len_div3 : int) : required_tile list
  =
  let shanten = safe_calc_shanten state.tehai tehai_len_div3 in
  let result = ref [] in
  for tid = 0 to 33 do
    let count = state.tiles_left.(tid) in
    if count > 0
    then (
      state.tehai.(tid) <- state.tehai.(tid) + 1;
      let shanten_after = safe_calc_shanten state.tehai tehai_len_div3 in
      state.tehai.(tid) <- state.tehai.(tid) - 1;
      if shanten_after < shanten
      then result := { tile = tid; count } :: !result)
  done;
  List.rev !result
;;

(** ==================================================================== *)

(** Constants and Data *)

(** ==================================================================== *)

let uradora_prob_table = [|
  [|0.639485; 0.327801; 0.0327134; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.406736; 0.42281; 0.147966; 0.021674; 0.0008142; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.257516; 0.406819; 0.246851; 0.0757724; 0.0122266; 0.0008004; 1.43e-5; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.162199; 0.346513; 0.301539; 0.142396; 0.0401276; 0.0066491; 0.0005575; 1.85e-5; 0.; 0.; 0.; 0.; 0.|];
  [|0.101768; 0.275319; 0.313742; 0.20189; 0.081774; 0.0215394; 0.0035918; 0.0003607; 1.52e-5; 3e-7; 0.; 0.; 0.|]
|]

(** ==================================================================== *)

(** Internal Values and Caching *)

(** ==================================================================== *)

type values =
  { tenpai_probs : float array
  ; win_probs : float array
  ; exp_values : float array
  }

let make_values n =
  { tenpai_probs = Array.make n 0.0
  ; win_probs = Array.make n 0.0
  ; exp_values = Array.make n 0.0
  }
;;

module StateKey = struct
  type t = {
    tehai : int array;
    akas_in_hand : bool array;
    tiles_left : int array;
    akas_left : bool array;
    n_extra_tsumo : int;
    shanten : int;
  }
  let equal s1 s2 =
    s1.n_extra_tsumo = s2.n_extra_tsumo &&
    s1.shanten = s2.shanten &&
    s1.tehai = s2.tehai &&
    s1.akas_in_hand = s2.akas_in_hand &&
    s1.tiles_left = s2.tiles_left &&
    s1.akas_left = s2.akas_left
  let hash s = Hashtbl.hash s
end

module StateHash = Hashtbl.Make(StateKey)

type config =
  { tehai_len_div3 : int
  ; chis : int list
  ; pons : int list
  ; minkans : int list
  ; ankans : int list
  ; bakaze : int
  ; jikaze : int
  ; is_menzen : bool
  ; num_doras_in_fuuro : int
  ; dora_indicators : int list
  ; calc_double_riichi : bool
  ; calc_haitei : bool
  ; prefer_riichi : bool
  ; sort_result : bool
  ; maximize_win_prob : bool
  ; calc_tegawari : bool
  ; calc_shanten_down : bool
  }

let create_config
      ~(tehai_len_div3 : int)
      ~(chis : int list)
      ~(pons : int list)
      ~(minkans : int list)
      ~(ankans : int list)
      ~(bakaze : int)
      ~(jikaze : int)
      ~(is_menzen : bool)
      ~(num_doras_in_fuuro : int)
      ~(dora_indicators : int list)
      ~(calc_double_riichi : bool)
      ~(calc_haitei : bool)
      ~(prefer_riichi : bool)
      ~(sort_result : bool)
      ~(maximize_win_prob : bool)
      ~(calc_tegawari : bool)
      ~(calc_shanten_down : bool)
  : config
  =
  { tehai_len_div3
  ; chis
  ; pons
  ; minkans
  ; ankans
  ; bakaze
  ; jikaze
  ; is_menzen
  ; num_doras_in_fuuro
  ; dora_indicators
  ; calc_double_riichi
  ; calc_haitei
  ; prefer_riichi
  ; sort_result
  ; maximize_win_prob
  ; calc_tegawari
  ; calc_shanten_down
  }
;;

type calc_state = {
  config : config;
  state : state;
  tsumo_prob_table : float array array;
  not_tsumo_prob_table : float array array;
  cache : values StateHash.t;
  n_tsumo : int;
}

let build_tsumo_prob_table n_left_tiles n_tsumo =
  Array.init 4 (fun i ->
    Array.init n_tsumo (fun j ->
      float_of_int (i + 1) /. float_of_int (max 1 (n_left_tiles - j))))
;;

let build_not_tsumo_prob_table n_left_tiles n_tsumo =
  Array.init (n_left_tiles + 1) (fun i ->
    let row = Array.make n_tsumo 0.0 in
    row.(0) <- 1.0;
    for j = 0 to min (n_tsumo - 2) (n_left_tiles - i - 1) do
      row.(j+1) <- row.(j) *. float_of_int (n_left_tiles - i - j) /. float_of_int (max 1 (n_left_tiles - j))
    done;
    row)
;;

let get_score (cs : calc_state) (win_tile : int) : float array option =
  let agari_calc = {
    Agari.tehai = cs.state.tehai;
    winning_tile = Tiles.deaka win_tile;
    bakaze = cs.config.bakaze;
    jikaze = cs.config.jikaze;
    is_menzen = cs.config.is_menzen;
    is_ron = false;
    chis = cs.config.chis;
    pons = cs.config.pons;
    minkans = cs.config.minkans;
    ankans = cs.config.ankans;
  } in
  let is_oya = cs.config.jikaze = Tiles.tile_id_E in
  let additional_yakus =
    match cs.config.is_menzen, cs.config.prefer_riichi with
    | true, true -> 2
    | true, false -> 1
    | false, _ -> 0
  in
  let num_doras =
    List.fold_left (fun acc ind -> 
      if ind < 34 then acc + cs.state.tehai.(Tiles.next ind) else acc
    ) 0 cs.config.dora_indicators
    + (let c = ref 0 in for i = 0 to 2 do if cs.state.akas_in_hand.(i) then incr c done; !c)
    + cs.config.num_doras_in_fuuro
  in
  match try Agari.agari agari_calc additional_yakus num_doras with _ -> None with
  | None -> None
  | Some (Agari.Yakuman n) ->
    let p = float_of_int (Point.tsumo_total (Agari.point (Agari.Yakuman n) is_oya) is_oya) in
    Some [| p; p; p; p |]
  | Some (Agari.Normal { fu; han }) ->
    let scores = Array.make 4 0.0 in
    let assume_riichi = cs.config.is_menzen && cs.config.prefer_riichi in
    if assume_riichi && List.length cs.config.dora_indicators = 1 then
      let n_indicators = Array.make 5 0 in
      let sum_indicators = ref 0 in
      for tid = 0 to 33 do
        let count = cs.state.tehai.(tid) in
        if count > 0 then
          let ind_tile = try Tiles.prev tid with _ -> 37 in
          if ind_tile < 34 then (
            let ind_count = cs.state.tiles_left.(ind_tile) in
            let count_idx = min 4 count in
            n_indicators.(count_idx) <- n_indicators.(count_idx) + ind_count;
            sum_indicators := !sum_indicators + ind_count
          )
      done;
      let uradora_probs = Array.make 5 0.0 in
      let n_left = sum_left_tiles cs.state in
      uradora_probs.(0) <- float_of_int (n_left - !sum_indicators) /. float_of_int (max 1 n_left);
      for i = 1 to 4 do uradora_probs.(i) <- float_of_int n_indicators.(i) /. float_of_int (max 1 n_left) done;
      for i = 0 to 3 do
        for j = 0 to 4 do
          if uradora_probs.(j) > 0.0 then
            let p = Agari.point (Agari.Normal { fu; han = han + i + j }) is_oya in
            scores.(i) <- scores.(i) +. float_of_int (Point.tsumo_total p is_oya) *. uradora_probs.(j)
        done
      done;
      Some scores
    else if assume_riichi && List.length cs.config.dora_indicators > 1 then
      let n_inds = List.length cs.config.dora_indicators in
      let probs = uradora_prob_table.(min 4 (n_inds - 1)) in
      for i = 0 to 3 do
        for j = 0 to 12 do
          if probs.(j) > 0.0 then
            let p = Agari.point (Agari.Normal { fu; han = han + i + j }) is_oya in
            scores.(i) <- scores.(i) +. float_of_int (Point.tsumo_total p is_oya) *. probs.(j)
        done
      done;
      Some scores
    else (
      for i = 0 to 3 do
        let p = Agari.point (Agari.Normal { fu; han = han + i }) is_oya in
        scores.(i) <- float_of_int (Point.tsumo_total p is_oya)
      done;
      Some scores
    )

type scores_or_values =
  | Scores of float array
  | Values of values

let rec discard_recursive (cs : calc_state) (shanten : int) : values =
  let key = {
    StateKey.tehai = Array.copy cs.state.tehai;
    akas_in_hand = Array.copy cs.state.akas_in_hand;
    tiles_left = Array.copy cs.state.tiles_left;
    akas_left = Array.copy cs.state.akas_left;
    n_extra_tsumo = cs.state.n_extra_tsumo;
    shanten;
  } in
  match StateHash.find_opt cs.cache key with
  | Some v -> v
  | None ->
    let res = discard_slow cs shanten in
    StateHash.add cs.cache key res;
    res

and discard_slow cs shanten =
  let discards = get_discard_tiles cs.state shanten cs.config.tehai_len_div3 in
  let max_tp = Array.make cs.n_tsumo 0.0 in
  let max_wp = Array.make cs.n_tsumo 0.0 in
  let max_ev = Array.make cs.n_tsumo 0.0 in
  let max_tiles = Array.make cs.n_tsumo (-1) in
  let max_vals = Array.make cs.n_tsumo min_int in
  List.iter (fun dt ->
    let v_opt =
      if dt.d_shanten_diff = 0 then (
        Some (with_discard cs.state dt.d_tile (fun () -> draw_recursive cs shanten))
      ) else if cs.config.calc_shanten_down && cs.state.n_extra_tsumo = 0 && dt.d_shanten_diff = 1 && shanten < 3 then (
        cs.state.n_extra_tsumo <- cs.state.n_extra_tsumo + 1;
        let v = try Some (with_discard cs.state dt.d_tile (fun () -> draw_recursive cs (shanten + 1))) with _ -> None in
        cs.state.n_extra_tsumo <- cs.state.n_extra_tsumo - 1;
        v
      ) else None
    in
    match v_opt with
    | None -> ()
    | Some v ->
      for i = 0 to cs.n_tsumo - 1 do
        let cur_val = if cs.config.maximize_win_prob then int_of_float (v.win_probs.(i) *. 1e5) else int_of_float v.exp_values.(i) in
        if cur_val > max_vals.(i) || (cur_val = max_vals.(i) && cmp_tile_discard_priority dt.d_tile max_tiles.(i) > 0) then (
          max_tp.(i) <- v.tenpai_probs.(i);
          max_wp.(i) <- v.win_probs.(i);
          max_ev.(i) <- v.exp_values.(i);
          max_vals.(i) <- cur_val;
          max_tiles.(i) <- dt.d_tile
        )
      done
  ) discards;
  { tenpai_probs = max_tp; win_probs = max_wp; exp_values = max_ev }

and draw_recursive cs shanten =
  let key = {
    StateKey.tehai = Array.copy cs.state.tehai;
    akas_in_hand = Array.copy cs.state.akas_in_hand;
    tiles_left = Array.copy cs.state.tiles_left;
    akas_left = Array.copy cs.state.akas_left;
    n_extra_tsumo = cs.state.n_extra_tsumo;
    shanten;
  } in
  match StateHash.find_opt cs.cache key with
  | Some v -> v
  | None ->
    let res = if cs.config.calc_tegawari && cs.state.n_extra_tsumo = 0 then draw_with_tegawari cs shanten else draw_without_tegawari cs shanten in
    StateHash.add cs.cache key res;
    res

and draw_with_tegawari cs shanten =
  let v = make_values cs.n_tsumo in
  let draw_tiles = get_draw_tiles cs.state shanten cs.config.tehai_len_div3 in
  let sum_left = sum_left_tiles cs.state in
  List.iter (fun dt ->
    if dt.dr_shanten_diff = -1 then (
      let sv_opt = try (with_deal cs.state dt.dr_tile (fun () ->
        if shanten > 0 then Some (Values (discard_recursive cs (shanten - 1)))
        else match get_score cs dt.dr_tile with Some s -> Some (Scores s) | None -> None
      )) with _ -> None in
      (match sv_opt with
       | Some sv ->
         let prob = float_of_int dt.dr_count /. float_of_int (max 1 sum_left) in
         for i = 0 to cs.n_tsumo - 1 do
           match sv with
           | Scores s ->
             let assume_riichi = cs.config.is_menzen && cs.config.prefer_riichi in
             let han_plus = (if assume_riichi && cs.config.calc_double_riichi && i = 0 then 1 else 0)
                          + (if assume_riichi then 1 else 0)
                          + (if cs.config.calc_haitei && i = cs.n_tsumo - 1 then 1 else 0)
             in
             v.win_probs.(i) <- v.win_probs.(i) +. prob;
             v.exp_values.(i) <- v.exp_values.(i) +. prob *. s.(min 3 han_plus)
           | Values nv ->
             if shanten = 1 then v.tenpai_probs.(i) <- v.tenpai_probs.(i) +. prob;
             if i < cs.n_tsumo - 1 then (
               if shanten > 1 then v.tenpai_probs.(i) <- v.tenpai_probs.(i) +. prob *. nv.tenpai_probs.(i+1);
               v.win_probs.(i) <- v.win_probs.(i) +. prob *. nv.win_probs.(i+1);
               v.exp_values.(i) <- v.exp_values.(i) +. prob *. nv.exp_values.(i+1)
             )
         done
       | None -> ())
    )
  ) draw_tiles;
  List.iter (fun dt ->
    if dt.dr_shanten_diff = 0 then (
      cs.state.n_extra_tsumo <- cs.state.n_extra_tsumo + 1;
      let nv_opt = try Some (with_deal cs.state dt.dr_tile (fun () -> discard_recursive cs shanten)) with _ -> None in
      cs.state.n_extra_tsumo <- cs.state.n_extra_tsumo - 1;
      (match nv_opt with
       | Some nv ->
         let prob = float_of_int dt.dr_count /. float_of_int (max 1 sum_left) in
         for i = 0 to cs.n_tsumo - 2 do
           v.tenpai_probs.(i) <- v.tenpai_probs.(i) +. prob *. nv.tenpai_probs.(i+1);
           v.win_probs.(i) <- v.win_probs.(i) +. prob *. nv.win_probs.(i+1);
           v.exp_values.(i) <- v.exp_values.(i) +. prob *. nv.exp_values.(i+1)
         done
       | None -> ())
    )
  ) draw_tiles;
  v

and draw_without_tegawari cs shanten =
  let v = make_values cs.n_tsumo in
  let draw_tiles = get_draw_tiles cs.state shanten cs.config.tehai_len_div3 in
  let sum_req = List.fold_left (fun acc dt -> if dt.dr_shanten_diff = -1 then acc + dt.dr_count else acc) 0 draw_tiles in
  let not_tsumo_probs = cs.not_tsumo_prob_table.(min (Array.length cs.not_tsumo_prob_table - 1) sum_req) in
  List.iter (fun dt ->
    if dt.dr_shanten_diff = -1 then (
      let sv_opt = try (with_deal cs.state dt.dr_tile (fun () ->
        if shanten > 0 then Some (Values (discard_recursive cs (shanten - 1)))
        else match get_score cs dt.dr_tile with Some s -> Some (Scores s) | None -> None
      )) with _ -> None in
      (match sv_opt with
       | Some sv ->
         let tsumo_probs = cs.tsumo_prob_table.(min 3 (max 0 (dt.dr_count - 1))) in
         for i = 0 to cs.n_tsumo - 1 do
           let m = not_tsumo_probs.(i) in
           if m > 0.0 then
             for j = i to cs.n_tsumo - 1 do
               let n = not_tsumo_probs.(j) in
               if n > 0.0 then
                 let prob = tsumo_probs.(j) *. n /. m in
                 match sv with
                 | Scores s ->
                   let assume_riichi = cs.config.is_menzen && cs.config.prefer_riichi in
                   let han_plus = (if assume_riichi && cs.config.calc_double_riichi && i = 0 then 1 else 0)
                                + (if assume_riichi && j = i then 1 else 0)
                                + (if cs.config.calc_haitei && j = cs.n_tsumo - 1 then 1 else 0)
                   in
                   v.win_probs.(i) <- v.win_probs.(i) +. prob;
                   v.exp_values.(i) <- v.exp_values.(i) +. prob *. s.(min 3 han_plus)
                 | Values nv ->
                   if shanten = 1 then v.tenpai_probs.(i) <- v.tenpai_probs.(i) +. prob;
                   if j < cs.n_tsumo - 1 then (
                     if shanten > 1 then v.tenpai_probs.(i) <- v.tenpai_probs.(i) +. prob *. nv.tenpai_probs.(j+1);
                     v.win_probs.(i) <- v.win_probs.(i) +. prob *. nv.win_probs.(j+1);
                     v.exp_values.(i) <- v.exp_values.(i) +. prob *. nv.exp_values.(j+1)
                   )
             done
         done
       | None -> ())
    )
  ) draw_tiles;
  v

let calc config init_state can_discard tsumos_left cur_shanten =
  if cur_shanten < 0 then Error "Cannot calculate an agari hand"
  else if tsumos_left < 1 then Error "Need at least one more tsumo"
  else if tsumos_left > max_tsumo then Error "tsumos_left exceeds maximum"
  else
    let state = of_init_state init_state in
    let n_left = sum_left_tiles state in
    let cs = {
      config;
      state;
      tsumo_prob_table = build_tsumo_prob_table n_left tsumos_left;
      not_tsumo_prob_table = build_not_tsumo_prob_table n_left tsumos_left;
      cache = StateHash.create 4096;
      n_tsumo = tsumos_left;
    } in
    let candidates =
      if cur_shanten <= 3 then
        if can_discard then
          let discards = get_discard_tiles state cur_shanten config.tehai_len_div3 in
          List.filter_map (fun dt ->
            let res = try (with_discard state dt.d_tile (fun () ->
              if dt.d_shanten_diff = 0 then (
                let required_tiles = get_required_tiles state config.tehai_len_div3 in
                let num_req = List.fold_left (fun acc r -> acc + r.count) 0 required_tiles in
                let v = draw_recursive cs cur_shanten in
                let tp = if cur_shanten = 0 then Array.make tsumos_left 1.0 else v.tenpai_probs in
                Some { tile = dt.d_tile; tenpai_probs = tp; win_probs = v.win_probs; exp_values = v.exp_values; required_tiles; num_required_tiles = num_req; shanten_down = false }
              ) else if config.calc_shanten_down && dt.d_shanten_diff = 1 && cur_shanten < 3 then (
                let required_tiles = get_required_tiles state config.tehai_len_div3 in
                let num_req = List.fold_left (fun acc r -> acc + r.count) 0 required_tiles in
                state.n_extra_tsumo <- state.n_extra_tsumo + 1;
                let v = draw_recursive cs (cur_shanten + 1) in
                state.n_extra_tsumo <- state.n_extra_tsumo - 1;
                Some { tile = dt.d_tile; tenpai_probs = v.tenpai_probs; win_probs = v.win_probs; exp_values = v.exp_values; required_tiles; num_required_tiles = num_req; shanten_down = true }
              ) else None
            )) with _ -> None in
            res
          ) discards
        else
          let required_tiles = try get_required_tiles state config.tehai_len_div3 with _ -> [] in
          let num_req = List.fold_left (fun acc r -> acc + r.count) 0 required_tiles in
          let v = draw_recursive cs cur_shanten in
          let tp = if cur_shanten = 0 then Array.make tsumos_left 1.0 else v.tenpai_probs in
          [ { tile = -1; tenpai_probs = tp; win_probs = v.win_probs; exp_values = v.exp_values; required_tiles; num_required_tiles = num_req; shanten_down = false } ]
      else
        if can_discard then
          let discards = get_discard_tiles state cur_shanten config.tehai_len_div3 in
          List.map (fun dt ->
            with_discard state dt.d_tile (fun () ->
              let required_tiles = try get_required_tiles state config.tehai_len_div3 with _ -> [] in
              let num_req = List.fold_left (fun acc r -> acc + r.count) 0 required_tiles in
              { tile = dt.d_tile; tenpai_probs = Array.make tsumos_left 0.0; win_probs = Array.make tsumos_left 0.0; exp_values = Array.make tsumos_left 0.0; required_tiles; num_required_tiles = num_req; shanten_down = dt.d_shanten_diff = 1 }
            )
          ) discards
        else
          let required_tiles = try get_required_tiles state config.tehai_len_div3 with _ -> [] in
          let num_req = List.fold_left (fun acc r -> acc + r.count) 0 required_tiles in
          [ { tile = -1; tenpai_probs = Array.make tsumos_left 0.0; win_probs = Array.make tsumos_left 0.0; exp_values = Array.make tsumos_left 0.0; required_tiles; num_required_tiles = num_req; shanten_down = false } ]
    in
    let sorted =
      if config.sort_result then
        let by = if cur_shanten <= 3 then (if config.maximize_win_prob then WinProb else EV) else NotShantenDown in
        List.sort (fun c1 c2 -> cmp_by by c1 c2) candidates
      else candidates
    in
    Ok sorted
;;
