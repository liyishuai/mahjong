open Base
open Mahjong
open Yaku_types

let test_tanyao () =
  let tiles = List.init 14 ~f:(fun i -> 
    let type_ = Option.value_exn (Tile.TileType.of_int (1 + (i % 7))) in (* m2..m8 *)
    Tile.create type_ ~offset:(i / 7)) 
  in
  let all_tile_types = Hashtbl.create (module Tile.TileType) in
  List.iter tiles ~f:(fun t -> 
    Hashtbl.update all_tile_types (Tile.type_ t) ~f:(function None -> 1 | Some c -> c + 1));
  
  let win_info : WinInfo.t = {
    state = {
      seat_wind = Wind.East;
      prevalent_wind = Wind.East;
      is_bottom = false;
      is_ippatsu = false;
      is_first_tsumo = false;
      is_dealer = true;
      is_robbing_kan = false;
      dora = Hashtbl.create (module Tile.TileType);
      reversed_dora = Hashtbl.create (module Tile.TileType);
    };
    hand = {
      closed_tiles = tiles;
      opens = [];
      closed_tile_types = all_tile_types;
      all_tile_types = all_tile_types;
      win_tile = Some (List.last_exn tiles);
      stage = AfterTsumo;
      under_riichi = false;
      double_riichi = false;
      is_menzen = true;
    };
  } in
  let score = Yaku_evaluator.evaluate win_info in
  Alcotest.(check bool) "has tanyao" true (Hashtbl.mem score.yaku Yaku.AllSimples);
  Alcotest.(check bool) "has concealed tsumo" true (Hashtbl.mem score.yaku Yaku.FullyConcealedHand)

let test_yakuman () =
  let all_tile_types = Hashtbl.create (module Tile.TileType) in
  Hashtbl.set all_tile_types ~key:Tile.TileType.WD ~data:3;
  Hashtbl.set all_tile_types ~key:Tile.TileType.GD ~data:3;
  Hashtbl.set all_tile_types ~key:Tile.TileType.RD ~data:3;
  Hashtbl.set all_tile_types ~key:Tile.TileType.M1 ~data:3;
  Hashtbl.set all_tile_types ~key:Tile.TileType.M2 ~data:2;
  
  let win_info : WinInfo.t = {
    state = {
      seat_wind = Wind.East;
      prevalent_wind = Wind.East;
      is_bottom = false;
      is_ippatsu = false;
      is_first_tsumo = false;
      is_dealer = true;
      is_robbing_kan = false;
      dora = Hashtbl.create (module Tile.TileType);
      reversed_dora = Hashtbl.create (module Tile.TileType);
    };
    hand = {
      closed_tiles = [];
      opens = [];
      closed_tile_types = all_tile_types;
      all_tile_types = all_tile_types;
      win_tile = Some (Tile.create Tile.TileType.M2 ~offset:0);
      stage = AfterRon;
      under_riichi = false;
      double_riichi = false;
      is_menzen = false;
    };
  } in
  let score = Yaku_evaluator.evaluate win_info in
  Alcotest.(check bool) "has daisangen" true (Hash_set.mem score.yakuman Yaku.BigThreeDragons)

let () =
  let open Alcotest in
  run "Yaku" [
    "tanyao", [test_case "tanyao" `Quick test_tanyao];
    "yakuman", [test_case "yakuman" `Quick test_yakuman];
  ]
