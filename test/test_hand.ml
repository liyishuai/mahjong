open Base
open Mahjong

let test_create () =
  let tiles = List.init 13 ~f:(fun i -> 
    let type_ = Option.value_exn (Tile.TileType.of_int (i / 4)) in
    Tile.create type_ ~offset:(i % 4)) 
  in
  let hand = Hand.create tiles in
  Alcotest.(check int) "size" 13 (Hand.size hand);
  Alcotest.(check int) "size_closed" 13 (Hand.size_closed hand);
  Alcotest.(check int) "size_opened" 0 (Hand.size_opened hand)

let () =
  let open Alcotest in
  run "Hand" [
    "create", [test_case "create" `Quick test_create];
  ]
