
open Mahjong

let test_chi () =
  let t1 = Tile.create Tile.TileType.M1 ~offset:0 in
  let t2 = Tile.create Tile.TileType.M2 ~offset:0 in
  let t3 = Tile.create Tile.TileType.M3 ~offset:0 in
  let chi = Open.create_chi ~tiles:[t1; t2; t3] ~stolen:t1 in
  Alcotest.(check bool) "is chi" true (Open.OpenType.equal (Open.type_ chi) Open.OpenType.Chi);
  Alcotest.(check int) "size" 3 (Open.size chi)

let () =
  let open Alcotest in
  run "Open" [
    "chi", [test_case "chi" `Quick test_chi];
  ]
