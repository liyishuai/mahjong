open Base
open Mahjong

let test_encode_discard () =
  let tile = Tile.create Tile.TileType.M5 ~offset:1 in
  let action = Action.create_discard ~who:Action.AbsolutePos.InitEast ~tile in
  Alcotest.(check int) "encode discard" 4 (Action.encode action)

let test_encode_discard_red () =
  let tile = Tile.create Tile.TileType.M5 ~offset:0 in (* red five *)
  let action = Action.create_discard ~who:Action.AbsolutePos.InitEast ~tile in
  Alcotest.(check int) "encode discard red" 34 (Action.encode action)

let test_encode_tsumogiri () =
  let tile = Tile.create Tile.TileType.M5 ~offset:1 in
  let action = Action.create_tsumogiri ~who:Action.AbsolutePos.InitEast ~tile in
  Alcotest.(check int) "encode tsumogiri" 41 (Action.encode action)

let test_encode_tsumogiri_red () =
  let tile = Tile.create Tile.TileType.M5 ~offset:0 in (* red five *)
  let action = Action.create_tsumogiri ~who:Action.AbsolutePos.InitEast ~tile in
  Alcotest.(check int) "encode tsumogiri red" 71 (Action.encode action)

let test_encode_chi () =
  let tiles = [
    Tile.create Tile.TileType.M4 ~offset:0; (* 12 *)
    Tile.create Tile.TileType.M5 ~offset:1; (* 17 *)
    Tile.create Tile.TileType.M6 ~offset:0; (* 20 *)
  ] in
  let chi = Open.create_chi ~tiles ~stolen:(List.nth_exn tiles 0) in
  let action = Action.create_open ~who:Action.AbsolutePos.InitEast ~open_:chi in
  Alcotest.(check int) "encode chi" 77 (Action.encode action)

let test_encode_chi_2 () =
  let tiles = [
    Tile.create Tile.TileType.P4 ~offset:0; (* 48 *)
    Tile.create Tile.TileType.P5 ~offset:1; (* 53 *)
    Tile.create Tile.TileType.P6 ~offset:0; (* 56 *)
  ] in
  let chi = Open.create_chi ~tiles ~stolen:(List.nth_exn tiles 0) in
  let action = Action.create_open ~who:Action.AbsolutePos.InitEast ~open_:chi in
  Alcotest.(check int) "encode chi 2" 84 (Action.encode action)

let test_encode_chi_with_red () =
  let tiles = [
    Tile.create Tile.TileType.M4 ~offset:0; (* 12 *)
    Tile.create Tile.TileType.M5 ~offset:0; (* 16 - red *)
    Tile.create Tile.TileType.M6 ~offset:0; (* 20 *)
  ] in
  let chi = Open.create_chi ~tiles ~stolen:(List.nth_exn tiles 0) in
  let action = Action.create_open ~who:Action.AbsolutePos.InitEast ~open_:chi in
  Alcotest.(check int) "encode chi with red" 96 (Action.encode action)

let test_encode_pon () =
  let stolen = Tile.create Tile.TileType.M5 ~offset:1 in (* 17 *)
  let unused = Tile.create Tile.TileType.M5 ~offset:0 in (* 16 *)
  let pon = Open.create_pon ~stolen ~unused ~from:Open.RelativePos.Left in
  let action = Action.create_open ~who:Action.AbsolutePos.InitEast ~open_:pon in
  Alcotest.(check int) "encode pon" 108 (Action.encode action)

let test_encode_pon_with_red () =
  let stolen = Tile.create Tile.TileType.M5 ~offset:0 in (* 16 - red *)
  let unused = Tile.create Tile.TileType.M5 ~offset:1 in (* 17 *)
  let pon = Open.create_pon ~stolen ~unused ~from:Open.RelativePos.Left in
  let action = Action.create_open ~who:Action.AbsolutePos.InitEast ~open_:pon in
  Alcotest.(check int) "encode pon with red" 138 (Action.encode action)

let test_encode_kan_closed () =
  let tile = Tile.create Tile.TileType.M5 ~offset:0 in (* 16 - red *)
  let kan = Open.create_kan_closed ~tile in
  let action = Action.create_open ~who:Action.AbsolutePos.InitEast ~open_:kan in
  Alcotest.(check int) "encode kan closed" 145 (Action.encode action)

let test_encode_kan_opened () =
  let stolen = Tile.create Tile.TileType.M5 ~offset:0 in (* 16 - red *)
  let kan = Open.create_kan_opened ~stolen ~from:Open.RelativePos.Left in
  let action = Action.create_open ~who:Action.AbsolutePos.InitEast ~open_:kan in
  Alcotest.(check int) "encode kan opened" 145 (Action.encode action)

let test_encode_tsumo () =
  let tile = Tile.create Tile.TileType.M1 ~offset:0 in (* 3 *)
  let action = Action.create_tsumo ~who:Action.AbsolutePos.InitEast ~tile in
  Alcotest.(check int) "encode tsumo" 175 (Action.encode action)

let test_encode_ron () =
  let tile = Tile.create Tile.TileType.M2 ~offset:0 in (* 5 *)
  let action = Action.create_ron ~who:Action.AbsolutePos.InitEast ~tile in
  Alcotest.(check int) "encode ron" 176 (Action.encode action)

let test_encode_riichi () =
  let action = Action.create_riichi ~who:Action.AbsolutePos.InitEast in
  Alcotest.(check int) "encode riichi" 177 (Action.encode action)

let test_encode_nine_tiles () =
  let action = Action.create_nine_tiles ~who:Action.AbsolutePos.InitEast in
  Alcotest.(check int) "encode nine_tiles" 178 (Action.encode action)

let test_encode_no () =
  let action = Action.create_no ~who:Action.AbsolutePos.InitEast in
  Alcotest.(check int) "encode no" 179 (Action.encode action)

let () =
  let open Alcotest in
  run "Action tests" [
    "encode_discard", [test_case "encode_discard" `Quick test_encode_discard];
    "encode_discard_red", [test_case "encode_discard_red" `Quick test_encode_discard_red];
    "encode_tsumogiri", [test_case "encode_tsumogiri" `Quick test_encode_tsumogiri];
    "encode_tsumogiri_red", [test_case "encode_tsumogiri_red" `Quick test_encode_tsumogiri_red];
    "encode_chi", [test_case "encode_chi" `Quick test_encode_chi];
    "encode_chi_2", [test_case "encode_chi_2" `Quick test_encode_chi_2];
    "encode_chi_with_red", [test_case "encode_chi_with_red" `Quick test_encode_chi_with_red];
    "encode_pon", [test_case "encode_pon" `Quick test_encode_pon];
    "encode_pon_with_red", [test_case "encode_pon_with_red" `Quick test_encode_pon_with_red];
    "encode_kan_closed", [test_case "encode_kan_closed" `Quick test_encode_kan_closed];
    "encode_kan_opened", [test_case "encode_kan_opened" `Quick test_encode_kan_opened];
    "encode_tsumo", [test_case "encode_tsumo" `Quick test_encode_tsumo];
    "encode_ron", [test_case "encode_ron" `Quick test_encode_ron];
    "encode_riichi", [test_case "encode_riichi" `Quick test_encode_riichi];
    "encode_nine_tiles", [test_case "encode_nine_tiles" `Quick test_encode_nine_tiles];
    "encode_no", [test_case "encode_no" `Quick test_encode_no];
  ]
