open Mahjong

let () =
  print_endline "=== Mahjong Simulator ===\n";
  
  (* Initialize random state *)
  Random.self_init ();
  let seed = Array.init 17 (fun _ -> Random.bits ()) in
  
  (* Display simulation options *)
  print_endline "Available game modes:";
  print_endline "  - 4-player half-game (半荘)";
  print_endline "  - 4-player East only (東風)";
  print_endline "  - 3-player half-game (三人麻雀 半荘)";
  print_endline "  - 3-player East only (三人麻雀 東風)\n";
  
  (* Run a sample simulation with 4-player rules *)
  let rules = Rules.default_four_player in
  let state = State.init_game_state rules seed in
  
  Printf.printf "Game initialized:\n";
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
  
  (* Demonstrate training config *)
  print_endline "Training configuration:";
  let config = Training.default_config in
  Printf.printf "  Games to simulate: %d\n" config.num_games;
  Printf.printf "  Batch size: %d\n" config.batch_size;
  Printf.printf "  Learning rate: %f\n" config.learning_rate;
  Printf.printf "  Threads (for MacBook): %d\n" config.num_threads;
  
  print_endline "\nSimulator ready. Use the Training module to run simulations."
