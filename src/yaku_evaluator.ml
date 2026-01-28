open Base
open Yaku_types

module Win_score = struct
  type t = {
    yaku : (Yaku.t, int) Hashtbl.t;
    yakuman : Yaku.t Hash_set.t;
    mutable fu : int option;
  }

  let create () = {
    yaku = Hashtbl.create (module Yaku);
    yakuman = Hash_set.create (module Yaku);
    fu = None;
  }

  let total_fan t =
    Hashtbl.fold t.yaku ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data)

  let add_yaku t yaku fan =
    Hashtbl.update t.yaku yaku ~f:(function
      | None -> fan
      | Some old_fan -> max old_fan fan)

  let add_yakuman t yakuman = Hash_set.add t.yakuman yakuman
  let set_fu t fu = t.fu <- Some fu
end

let is_yaocyu tt =
  let i = Tile.TileType.to_int tt in
  i = 0 || i = 8 || i = 9 || i = 17 || i = 18 || i = 26 || i >= 27

let is_terminal tt =
  let i = Tile.TileType.to_int tt in
  i = 0 || i = 8 || i = 9 || i = 17 || i = 18 || i = 26

let is_dragon tt =
  let i = Tile.TileType.to_int tt in
  i >= 31

let same_wind tt wind =
  let i = Tile.TileType.to_int tt in
  match wind with
  | Wind.East -> i = 27
  | Wind.South -> i = 28
  | Wind.West -> i = 29
  | Wind.North -> i = 30

let judge_yakuman (win_info : WinInfo.t) (score : Win_score.t) =
  let h = win_info.hand in
  let all_tile_types = h.all_tile_types in
  
  let has_big_three_dragons () =
    List.for_all [Tile.TileType.WD; Tile.TileType.GD; Tile.TileType.RD] ~f:(fun tt ->
      Hashtbl.find all_tile_types tt |> Option.value ~default:0 >= 3)
  in
  if has_big_three_dragons () then Win_score.add_yakuman score Yaku.BigThreeDragons;
  
  let has_all_honours = ref true in
  Hashtbl.iteri all_tile_types ~f:(fun ~key:tt ~data:count ->
    if count > 0 && Tile.TileType.to_int tt < 27 then has_all_honours := false);
  if !has_all_honours then Win_score.add_yakuman score Yaku.AllHonours;
  
  let has_all_terminals = ref true in
  Hashtbl.iteri all_tile_types ~f:(fun ~key:tt ~data:count ->
    if count > 0 && not (is_terminal tt) then has_all_terminals := false);
  if !has_all_terminals && not !has_all_honours then Win_score.add_yakuman score Yaku.AllTerminals;
  
  let has_thirteen_orphans () =
    let orphans = [0; 8; 9; 17; 18; 26; 27; 28; 29; 30; 31; 32; 33] in
    let counts = List.map orphans ~f:(fun i -> 
      Hashtbl.find all_tile_types (Option.value_exn (Tile.TileType.of_int i)) |> Option.value ~default:0) 
    in
    List.for_all counts ~f:(fun c -> c >= 1) && List.exists counts ~f:(fun c -> c >= 2)
  in
  if has_thirteen_orphans () then Win_score.add_yakuman score Yaku.ThirteenOrphans

let judge_simple_yaku (win_info : WinInfo.t) (score : Win_score.t) =
  let h = win_info.hand in
  let s = win_info.state in
  if h.is_menzen && (match h.stage with AfterTsumo | AfterTsumoAfterKan -> true | _ -> false) then
    Win_score.add_yaku score Yaku.FullyConcealedHand 1;
  if h.double_riichi then Win_score.add_yaku score Yaku.DoubleRiichi 2
  else if h.under_riichi then Win_score.add_yaku score Yaku.Riichi 1;
  if s.is_ippatsu then Win_score.add_yaku score Yaku.Ippatsu 1;
  
  let is_tanyao = ref true in
  Hashtbl.iter_keys h.all_tile_types ~f:(fun tt -> if is_yaocyu tt then is_tanyao := false);
  if !is_tanyao then Win_score.add_yaku score Yaku.AllSimples 1;
  
  let check_yakuhai tt yaku =
    if Hashtbl.find h.all_tile_types tt |> Option.value ~default:0 >= 3 then
      Win_score.add_yaku score yaku 1
  in
  check_yakuhai Tile.TileType.WD Yaku.WhiteDragon;
  check_yakuhai Tile.TileType.GD Yaku.GreenDragon;
  check_yakuhai Tile.TileType.RD Yaku.RedDragon;
  let check_wind_yakuhai tt wind yaku =
    if same_wind tt wind && (Hashtbl.find h.all_tile_types tt |> Option.value ~default:0 >= 3) then
      Win_score.add_yaku score yaku 1
  in
  check_wind_yakuhai Tile.TileType.EW s.seat_wind Yaku.SeatWindEast;
  check_wind_yakuhai Tile.TileType.SW s.seat_wind Yaku.SeatWindSouth;
  check_wind_yakuhai Tile.TileType.WW s.seat_wind Yaku.SeatWindWest;
  check_wind_yakuhai Tile.TileType.NW s.seat_wind Yaku.SeatWindNorth;
  check_wind_yakuhai Tile.TileType.EW s.prevalent_wind Yaku.PrevalentWindEast;
  check_wind_yakuhai Tile.TileType.SW s.prevalent_wind Yaku.PrevalentWindSouth;
  check_wind_yakuhai Tile.TileType.WW s.prevalent_wind Yaku.PrevalentWindWest;
  check_wind_yakuhai Tile.TileType.NW s.prevalent_wind Yaku.PrevalentWindNorth;
  
  let check_flush () =
    let colors = Hash_set.create (module Int) in
    let has_honour = ref false in
    Hashtbl.iteri h.all_tile_types ~f:(fun ~key:tt ~data:count ->
      if count > 0 then (
        let i = Tile.TileType.to_int tt in
        if i >= 27 then has_honour := true
        else Hash_set.add colors (i / 9)
      ));
    match Hash_set.to_list colors with
    | [c] -> 
      let _ = c in
      if !has_honour then Win_score.add_yaku score Yaku.HalfFlush (if h.is_menzen then 3 else 2)
      else Win_score.add_yaku score Yaku.FullFlush (if h.is_menzen then 6 else 5)
    | [] when !has_honour -> () (* All Honours handled in judge_yakuman *)
    | _ -> ()
  in
  check_flush ()

let judge_dora (win_info : WinInfo.t) (score : Win_score.t) =
  let h = win_info.hand in
  let s = win_info.state in
  let count_dora (table : (Tile.TileType.t, int) Hashtbl.t) =
    Hashtbl.fold h.all_tile_types ~init:0 ~f:(fun ~key:tt ~data:count acc ->
      acc + (count * (Hashtbl.find table tt |> Option.value ~default:0)))
  in
  let dora_count = count_dora s.dora in
  if dora_count > 0 then Win_score.add_yaku score Yaku.Dora dora_count;
  let rev_dora_count = if h.under_riichi then count_dora s.reversed_dora else 0 in
  if rev_dora_count > 0 then Win_score.add_yaku score Yaku.ReversedDora rev_dora_count;
  let red_count = List.count h.closed_tiles ~f:Tile.is_red_five in
  let red_count = List.fold h.opens ~init:red_count ~f:(fun acc o ->
    acc + List.count (Open.tiles o) ~f:Tile.is_red_five)
  in
  if red_count > 0 then Win_score.add_yaku score Yaku.RedDora red_count

(* Helper: check if a set is a triplet (koutsu) *)
let is_triplet (set : Win_cache.tile_count) : bool =
  Hashtbl.length set = 1 &&
  Hashtbl.exists set ~f:(fun count -> count = 3)

(* Helper: check if a set is a sequence (shuntsu) and get start tile *)
let is_sequence (set : Win_cache.tile_count) : Tile.TileType.t option =
  if Hashtbl.length set <> 3 then None
  else
    let tiles = Hashtbl.keys set |> List.sort ~compare:(fun a b ->
      Int.compare (Tile.TileType.to_int a) (Tile.TileType.to_int b))
    in
    match tiles with
    | [t1; t2; t3] ->
      let i1 = Tile.TileType.to_int t1 in
      let i2 = Tile.TileType.to_int t2 in
      let i3 = Tile.TileType.to_int t3 in
      if i2 = i1 + 1 && i3 = i2 + 1 &&
         Hashtbl.find_exn set t1 = 1 &&
         Hashtbl.find_exn set t2 = 1 &&
         Hashtbl.find_exn set t3 = 1
      then Some t1
      else None
    | _ -> None

(* Get the tile type from a head (pair) *)
let head_tile (head : Win_cache.tile_count) : Tile.TileType.t option =
  if Hashtbl.length head = 1 then
    Hashtbl.keys head |> List.hd
  else None

let check_pinfu (win_info : WinInfo.t) sets heads =
  let h = win_info.hand in
  let s = win_info.state in
  if not h.is_menzen then None
  else
    let has_triplet = List.exists sets ~f:is_triplet in
    if has_triplet then None
    else
      let head_tile_opt = match heads with
        | [head] -> head_tile head
        | _ -> None
      in
      match head_tile_opt with
      | None -> None
      | Some head_tt ->
        let head_is_yaku = is_dragon head_tt || same_wind head_tt s.seat_wind || same_wind head_tt s.prevalent_wind in
        if head_is_yaku then None
        else
          match h.win_tile with
          | None -> None
          | Some wt ->
            let wt_type = Tile.type_ wt in
            let wt_num = Tile.num wt in
            let has_ryanmen = List.exists sets ~f:(fun set ->
              match is_sequence set with
              | None -> false
              | Some st ->
                let st_num = (Tile.TileType.to_int st) % 9 + 1 in
                (Tile.TileType.equal wt_type st && wt_num = st_num && wt_num <> 7) ||
                (Tile.TileType.equal wt_type st && wt_num = st_num + 2 && wt_num <> 3)
            ) in
            if has_ryanmen then Some 1 else None

let check_all_pons sets =
  if List.for_all sets ~f:is_triplet then Some 2 else None

let maximize_total_fan (win_info : WinInfo.t) =
  let h = win_info.hand in
  let counts = Array.create ~len:34 0 in
  Hashtbl.iteri h.closed_tile_types ~f:(fun ~key:tt ~data:count ->
    counts.(Tile.TileType.to_int tt) <- count);
  Win_cache.ensure_loaded ();
  let patterns = Win_cache.sets_and_heads counts in
  
  let check_seven_pairs () =
    let num_pairs = Hashtbl.fold h.closed_tile_types ~init:0 ~f:(fun ~key:_ ~data:c acc -> if c >= 2 then acc + 1 else acc) in
    if num_pairs = 7 then Some 2 else None
  in
  
  let best_yaku = ref (Hashtbl.create (module Yaku)) in
  let best_fan = ref (-1) in
  let best_fu = ref 30 in
  
  (match check_seven_pairs () with
   | Some f -> 
     best_fan := f;
     Hashtbl.set !best_yaku ~key:Yaku.SevenPairs ~data:f;
     best_fu := 25
   | None -> ());

  List.iter patterns ~f:(fun (sets, heads) ->
    let current_yaku = Hashtbl.create (module Yaku) in
    (match check_pinfu win_info sets heads with
     | Some f -> Hashtbl.set current_yaku ~key:Yaku.Pinfu ~data:f
     | None -> ());
    (match check_all_pons sets with
     | Some f -> Hashtbl.set current_yaku ~key:Yaku.AllPons ~data:f
     | None -> ());

    let current_fan = Hashtbl.fold current_yaku ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data) in
    if current_fan > !best_fan then (
      best_fan := current_fan;
      best_yaku := current_yaku;
    )
  );
  (!best_yaku, !best_fu)

let evaluate (win_info : WinInfo.t) : Win_score.t =
  let score = Win_score.create () in
  judge_yakuman win_info score;
  if Hash_set.is_empty score.yakuman then (
    judge_simple_yaku win_info score;
    let pattern_yaku, fu = maximize_total_fan win_info in
    Hashtbl.iteri pattern_yaku ~f:(fun ~key:y ~data:f -> Win_score.add_yaku score y f);
    Win_score.set_fu score fu;
    judge_dora win_info score
  );
  score
