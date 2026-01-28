
open Mahjong

let test_create () =
  let t = Tile.create Tile.TileType.M1 ~offset:0 in
  Alcotest.(check int) "id" 0 (Tile.to_int t);
  Alcotest.(check int) "num" 1 (Tile.num t);
  let t = Tile.create Tile.TileType.RD ~offset:3 in
  Alcotest.(check int) "id" 135 (Tile.to_int t);
  Alcotest.(check int) "num" 7 (Tile.num t)

let test_red_five () =
  let m5_0 = Tile.create Tile.TileType.M5 ~offset:0 in
  Alcotest.(check bool) "m5_0 is red" true (Tile.is_red_five m5_0);
  let m5_1 = Tile.create Tile.TileType.M5 ~offset:1 in
  Alcotest.(check bool) "m5_1 is not red" false (Tile.is_red_five m5_1)

let () =
  let open Alcotest in
  run "Tile" [
    "create", [test_case "create" `Quick test_create];
    "red_five", [test_case "red_five" `Quick test_red_five];
  ]
