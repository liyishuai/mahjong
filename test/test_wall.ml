open Base
open Mahjong

let test_initial_hand () =
  (* Test from https://tenhou.net/0/?log=2011020417gm-00a9-0000-b67fcaa3&tw=1 *)
  let tiles = [
    48; 16; 19; 34; 17; 62; 79; 52; 55; 30; 12; 26; 120; 130;
    42; 67; 2; 76; 13; 7; 56; 57; 82; 98; 31; 90; 3; 4;
    114; 93; 5; 61; 128; 1; 39; 121; 32; 103; 24; 70; 80; 125;
    66; 102; 20; 108; 41; 100; 87; 54; 78; 84; 107; 47; 14; 131;
    96; 51; 68; 85; 28; 10; 6; 18; 122; 49; 134; 109; 116; 127;
    105; 65; 92; 101; 29; 23; 83; 115; 77; 38; 15; 43; 94; 21;
    50; 91; 89; 45; 97; 37; 25; 35; 60; 132; 119; 135; 59; 0;
    9; 27; 53; 58; 118; 110; 22; 124; 69; 44; 33; 8; 74; 129;
    64; 88; 72; 75; 104; 73; 71; 81; 111; 86; 36; 99; 133; 11;
    40; 113; 123; 95; 112; 117; 46; 126; 63; 106
  ] in
  let wall = Wall.from_tiles ~round:0 tiles in
  let east_hand = Wall.initial_hand_tiles wall ~pos:Action.AbsolutePos.InitEast in
  let south_hand = Wall.initial_hand_tiles wall ~pos:Action.AbsolutePos.InitSouth in
  let west_hand = Wall.initial_hand_tiles wall ~pos:Action.AbsolutePos.InitWest in
  let north_hand = Wall.initial_hand_tiles wall ~pos:Action.AbsolutePos.InitNorth in

  Alcotest.(check (list int)) "East initial hand" [48; 16; 19; 34; 2; 76; 13; 7; 128; 1; 39; 121; 87] east_hand;
  Alcotest.(check (list int)) "South initial hand" [17; 62; 79; 52; 56; 57; 82; 98; 32; 103; 24; 70; 54] south_hand;
  Alcotest.(check (list int)) "West initial hand" [55; 30; 12; 26; 31; 90; 3; 4; 80; 125; 66; 102; 78] west_hand;
  Alcotest.(check (list int)) "North initial hand" [120; 130; 42; 67; 114; 93; 5; 61; 20; 108; 41; 100; 84] north_hand

let test_initial_hand_round5 () =
  let tiles = [
    117; 42; 114; 28; 70; 124; 97; 56; 5; 32; 81; 46; 52; 41;
    105; 21; 80; 87; 73; 2; 33; 71; 13; 118; 7; 119; 129; 116;
    83; 40; 17; 89; 31; 27; 68; 25; 24; 86; 90; 101; 104; 103;
    30; 130; 50; 4; 11; 60; 47; 34; 3; 120; 62; 59; 113; 82;
    22; 108; 100; 43; 132; 79; 88; 94; 12; 63; 84; 38; 107; 131;
    111; 77; 95; 109; 8; 106; 61; 16; 75; 96; 6; 58; 133; 125;
    102; 98; 23; 19; 36; 14; 91; 69; 37; 44; 78; 127; 54; 122;
    51; 76; 0; 10; 135; 39; 121; 134; 93; 64; 85; 35; 9; 45;
    67; 18; 74; 128; 115; 48; 110; 26; 65; 112; 29; 20; 66; 49;
    1; 15; 55; 53; 72; 99; 92; 126; 123; 57
  ] in
  let wall = Wall.from_tiles ~round:5 tiles in
  let east_hand = Wall.initial_hand_tiles wall ~pos:Action.AbsolutePos.InitEast in
  let south_hand = Wall.initial_hand_tiles wall ~pos:Action.AbsolutePos.InitSouth in
  let west_hand = Wall.initial_hand_tiles wall ~pos:Action.AbsolutePos.InitWest in

  Alcotest.(check (list int)) "East hand round 5" [52; 41; 105; 21; 83; 40; 17; 89; 50; 4; 11; 60; 120] east_hand;
  Alcotest.(check (list int)) "South hand round 5" [117; 42; 114; 28; 80; 87; 73; 2; 31; 27; 68; 25; 47] south_hand;
  Alcotest.(check (list int)) "West hand round 5" [70; 124; 97; 56; 33; 71; 13; 118; 24; 86; 90; 101; 34] west_hand

let test_draw () =
  (* 70 draws available without kan *)
  let wall = Wall.create ~round:0 ~honba:0 ~game_seed:[] in
  for i = 0 to 69 do
    Alcotest.(check bool) (Printf.sprintf "Has draw left %d" i) true (Wall.has_draw_left wall);
    ignore (Wall.draw wall)
  done;
  Alcotest.(check bool) "No draw left after 70" false (Wall.has_draw_left wall)

let test_kan_draw () =
  let wall = Wall.create ~round:0 ~honba:0 ~game_seed:[] in

  (* 35 normal draws *)
  for _i = 0 to 34 do
    ignore (Wall.draw wall)
  done;

  (* 4 kan draws with dora *)
  for _i = 0 to 3 do
    ignore (Wall.kan_draw wall);
    ignore (Wall.add_kan_dora wall)
  done;

  (* 31 more normal draws *)
  for _i = 0 to 30 do
    Alcotest.(check bool) "Has draw left" true (Wall.has_draw_left wall);
    ignore (Wall.draw wall)
  done;

  Alcotest.(check bool) "No draw left after kans" false (Wall.has_draw_left wall)

let test_add_kan_dora () =
  let wall = Wall.create ~round:0 ~honba:0 ~game_seed:[] in
  ignore (Wall.kan_draw wall);
  let (kan_dora1, ura_dora1) = Wall.add_kan_dora wall in
  let doras = Wall.dora_indicators wall in
  let ura_doras = Wall.ura_dora_indicators wall in

  Alcotest.(check int) "Kan dora indicator matches" (List.last_exn doras) kan_dora1;
  Alcotest.(check int) "Ura dora indicator matches" (List.last_exn ura_doras) ura_dora1;

  let (kan_dora2, ura_dora2) = Wall.add_kan_dora wall in
  let doras = Wall.dora_indicators wall in
  let ura_doras = Wall.ura_dora_indicators wall in

  Alcotest.(check int) "Second kan dora matches" (List.last_exn doras) kan_dora2;
  Alcotest.(check int) "Second ura dora matches" (List.last_exn ura_doras) ura_dora2

let test_doras () =
  let wall = Wall.create ~round:0 ~honba:0 ~game_seed:[] in

  Alcotest.(check int) "One dora initially" 1 (List.length (Wall.dora_indicators wall));
  Alcotest.(check int) "One ura dora initially" 1 (List.length (Wall.ura_dora_indicators wall));

  (* Add 4 kans *)
  for _i = 0 to 3 do
    ignore (Wall.kan_draw wall);
    ignore (Wall.add_kan_dora wall)
  done;

  Alcotest.(check int) "Five doras after 4 kans" 5 (List.length (Wall.dora_indicators wall));
  Alcotest.(check int) "Five ura doras after 4 kans" 5 (List.length (Wall.ura_dora_indicators wall))

let test_dora_specific () =
  (* Test from https://tenhou.net/0/?log=2010112714gm-00a9-0000-d497e395 *)
  let tiles = [
    24; 48; 11; 122; 135; 75; 128; 65; 111; 13; 46; 131; 120; 125;
    71; 107; 118; 54; 52; 121; 109; 26; 42; 93; 119; 123; 21; 8;
    80; 0; 116; 58; 132; 50; 64; 133; 63; 32; 35; 117; 100; 34;
    82; 83; 74; 87; 89; 72; 29; 1; 67; 56; 78; 103; 124; 126;
    59; 69; 38; 112; 61; 22; 25; 60; 86; 31; 99; 127; 23; 68;
    3; 5; 33; 51; 15; 73; 43; 18; 85; 55; 53; 92; 114; 129;
    97; 113; 40; 115; 90; 81; 4; 2; 19; 45; 105; 6; 36; 10;
    94; 91; 12; 28; 17; 30; 106; 101; 79; 47; 49; 102; 104; 95;
    41; 20; 16; 110; 96; 14; 134; 37; 108; 70; 130; 27; 39; 7;
    57; 84; 44; 77; 76; 66; 98; 9; 88; 62
  ] in
  let wall = Wall.from_tiles ~round:2 tiles in
  ignore (Wall.kan_draw wall);
  ignore (Wall.add_kan_dora wall);

  let doras = Wall.dora_indicators wall in
  Alcotest.(check (list int)) "Specific dora indicators" [76; 44] doras

let test_ura_dora_specific () =
  (* Test from https://tenhou.net/0/?log=2010112714gm-00a9-0000-d497e395 *)
  let tiles = [
    24; 48; 11; 122; 135; 75; 128; 65; 111; 13; 46; 131; 120; 125;
    71; 107; 118; 54; 52; 121; 109; 26; 42; 93; 119; 123; 21; 8;
    80; 0; 116; 58; 132; 50; 64; 133; 63; 32; 35; 117; 100; 34;
    82; 83; 74; 87; 89; 72; 29; 1; 67; 56; 78; 103; 124; 126;
    59; 69; 38; 112; 61; 22; 25; 60; 86; 31; 99; 127; 23; 68;
    3; 5; 33; 51; 15; 73; 43; 18; 85; 55; 53; 92; 114; 129;
    97; 113; 40; 115; 90; 81; 4; 2; 19; 45; 105; 6; 36; 10;
    94; 91; 12; 28; 17; 30; 106; 101; 79; 47; 49; 102; 104; 95;
    41; 20; 16; 110; 96; 14; 134; 37; 108; 70; 130; 27; 39; 7;
    57; 84; 44; 77; 76; 66; 98; 9; 88; 62
  ] in
  let wall = Wall.from_tiles ~round:2 tiles in
  ignore (Wall.kan_draw wall);
  ignore (Wall.add_kan_dora wall);

  let ura_doras = Wall.ura_dora_indicators wall in
  Alcotest.(check (list int)) "Specific ura dora indicators" [66; 77] ura_doras

let () =
  let open Alcotest in
  run "Wall tests" [
    "initial_hand", [test_case "initial_hand" `Quick test_initial_hand];
    "initial_hand_round5", [test_case "initial_hand_round5" `Quick test_initial_hand_round5];
    "draw", [test_case "draw" `Quick test_draw];
    "kan_draw", [test_case "kan_draw" `Quick test_kan_draw];
    "add_kan_dora", [test_case "add_kan_dora" `Quick test_add_kan_dora];
    "doras", [test_case "doras" `Quick test_doras];
    "dora_specific", [test_case "dora_specific" `Quick test_dora_specific];
    "ura_dora_specific", [test_case "ura_dora_specific" `Quick test_ura_dora_specific];
  ]
