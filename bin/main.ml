open Mahjong

(** Parse command line arguments *)
let parse_args () =
  let mode = ref "info" in
  let port = ref 8080 in
  let games = ref 1000 in
  let specs = [
    ("--web", Arg.Unit (fun () -> mode := "web"), "Start web server");
    ("--train", Arg.Unit (fun () -> mode := "train"), "Run training simulation");
    ("--tenhou", Arg.Unit (fun () -> mode := "tenhou"), "Start Tenhou bot");
    ("--port", Arg.Set_int port, "Web server port (default: 8080)");
    ("--games", Arg.Set_int games, "Number of games to simulate (default: 1000)");
  ] in
  Arg.parse specs (fun _ -> ()) "Mahjong AI - Usage: mahjong [options]";
  (!mode, !port, !games)

(** Show information and demo *)
let show_info () =
  print_endline "=== Mahjong AI System ===\n";
  
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
  print_endline "  - MLX (Python): Neural network training on Apple Silicon";
  print_endline "  - Communication: JSON via file or WebSocket\n";
  
  print_endline "Feature vector size: ~800+ dimensions";
  print_endline "  - Hand encoding: 136 (one-hot)";
  print_endline "  - Discards: 136 x 4 players = 544";
  print_endline "  - Game state: ~50 features";
  print_endline "  - Strategic hints: ~100 features\n";
  
  print_endline "=== Usage ===\n";
  print_endline "Start web interface:";
  print_endline "  python -m web.server --port 8080\n";
  print_endline "Train with MLX:";
  print_endline "  python -m mlx_training.train --games 10000 --mode 4p-half\n";
  print_endline "Run simulation:";
  print_endline "  ./mahjong --train --games 1000\n";
  print_endline "Connect to Tenhou:";
  print_endline "  ./mahjong --tenhou"

(** Run training simulation *)
let run_training num_games =
  Printf.printf "=== Training Simulation ===\n\n";
  Printf.printf "Running %d games...\n" num_games;
  
  let rules = Rules.default_four_player in
  let config = { Training.default_config with num_games } in
  
  Training.train config;
  
  print_endline "\nTraining complete!"

(** Start web server info *)
let start_web port =
  Printf.printf "=== Web Server ===\n\n";
  Printf.printf "To start the web server, run:\n";
  Printf.printf "  python -m web.server --port %d\n\n" port;
  Printf.printf "Then open http://localhost:%d in your browser.\n\n" port;
  print_endline "Features:";
  print_endline "  - Training progress visualization";
  print_endline "  - Human vs AI gameplay";
  print_endline "  - Tenhou bot control panel"

(** Start Tenhou bot *)
let start_tenhou () =
  print_endline "=== Tenhou Bot ===\n";
  print_endline "Tenhou bot ready.";
  print_endline "Configure username and auth_token in the code or via web interface.\n";
  
  let config = Tenhou_bot.default_config in
  Printf.printf "Server: %s:%d\n" config.server config.port;
  Printf.printf "Lobby: %d\n" config.lobby;
  Printf.printf "Game type: %d\n\n" config.game_type;
  
  print_endline "Note: Actual connection requires network socket implementation."

let () =
  let (mode, port, games) = parse_args () in
  match mode with
  | "web" -> start_web port
  | "train" -> run_training games
  | "tenhou" -> start_tenhou ()
  | _ -> show_info ()
