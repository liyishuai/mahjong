open Mahjong

let () =
  print_endline "=== Mahjong Simulator with Tenhou Bot ===\n";
  
  (* Initialize random state *)
  Random.self_init ();
  let seed = Array.init 17 (fun _ -> Random.bits ()) in
  
  (* Display simulation options *)
  print_endline "Game modes supported:";
  print_endline "  - 4-player half-game (半荘)";
  print_endline "  - 4-player East only (東風)";
  print_endline "  - 3-player half-game (三人麻雀 半荘)";
  print_endline "  - 3-player East only (三人麻雀 東風)\n";
  
  (* Run a sample simulation with 4-player rules *)
  let rules = Rules.default_four_player in
  let state = State.init_game_state rules seed in
  
  Printf.printf "Local simulation initialized:\n";
  Printf.printf "  Players: %d\n" (Rules.num_players rules);
  Printf.printf "  Wind rounds: %s\n" 
    (match rules.wind_rounds with Rules.EastOnly -> "East only" | Rules.HalfGame -> "Half game");
  Printf.printf "  Starting points: %d\n" rules.points.start_points;
  Printf.printf "  Red dora: %s\n" 
    (match rules.dora.aka_dora with
     | Some aka -> Printf.sprintf "Man5:%d So5:%d Pin5:%d" aka.man5 aka.so5 aka.pin5
     | None -> "None");
  Printf.printf "  Total rounds: %d\n\n" (Rules.total_rounds rules);
  
  (* Show wall *)
  let wall_str = Tiles.string_of_tiles state.round.wall in
  Printf.printf "Wall (first 14): %s\n" (String.sub wall_str 0 (min 42 (String.length wall_str)));
  Printf.printf "Wall total tiles: %d\n\n" (Array.length state.round.wall);
  
  (* Show Tenhou bot configuration *)
  print_endline "=== Tenhou Bot Interface ===\n";
  let bot_config = Tenhou_bot.default_config in
  Printf.printf "Tenhou server: %s:%d\n" bot_config.server bot_config.port;
  Printf.printf "Default lobby: %d\n" bot_config.lobby;
  Printf.printf "Game type: %d\n\n" bot_config.game_type;
  
  (* Demonstrate Tenhou protocol *)
  print_endline "Tenhou tile encoding examples:";
  let test_tiles = [|
    Tiles.Man Tiles.One;
    Tiles.Man Tiles.Aka;
    Tiles.Pin Tiles.Five;
    Tiles.So Tiles.Nine;
    Tiles.Honor Tiles.East;
    Tiles.Honor Tiles.Red
  |] in
  Array.iter (fun tile ->
    let code = Tenhou_protocol.tenhou_tile_of_tile tile 0 in
    Printf.printf "  %s -> code %d\n" (Tiles.string_of_tile tile) code
  ) test_tiles;
  
  print_endline "\nTenhou protocol messages:";
  Printf.printf "  Discard: %s\n" (Tenhou_protocol.encode_discard 45);
  Printf.printf "  Riichi:  %s\n" (Tenhou_protocol.encode_reach 45);
  Printf.printf "  Pass:    %s\n" (Tenhou_protocol.encode_noop ());
  
  (* Show neural network interface *)
  print_endline "\n=== Neural Network Interface ===\n";
  print_endline "Training architecture:";
  print_endline "  - OCaml: Game logic, Tenhou protocol, state management";
  print_endline "  - External NN (Python/Swift): Training, inference on Mac";
  print_endline "  - Communication: JSON via file or socket\n";
  
  print_endline "Feature vector size: ~800+ dimensions";
  print_endline "  - Hand encoding: 136 (one-hot)";
  print_endline "  - Discards: 136 x 4 players = 544";
  print_endline "  - Game state: ~50 features";
  print_endline "  - Strategic hints: ~100 features\n";
  
  print_endline "To train on Mac:";
  print_endline "  1. Run OCaml bot to collect training data";
  print_endline "  2. Load data into Python/TensorFlow or Swift/CoreML";
  print_endline "  3. Train neural network";
  print_endline "  4. Export model for inference";
  print_endline "  5. Connect trained model to Tenhou bot\n";
  
  print_endline "Ready for Tenhou connection. Set username and auth_token in config."
