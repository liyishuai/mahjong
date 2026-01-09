open Mahjong

(** Parse command line arguments *)
let parse_args () =
  let mode = ref "info" in
  let port = ref 8080 in
  let games = ref 1000 in
  let specs = [
    ("--train", Arg.Unit (fun () -> mode := "train"), "Run training simulation");
    ("--demo", Arg.Unit (fun () -> mode := "demo"), "Run demo game with pretty-print");
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

  (* Show neural network interface *)
  print_endline "=== Neural Network Policy ===\n";
  print_endline "Training architecture:";
  print_endline "  - OCaml: Game logic, simulation, and simple NN policy";
  print_endline "  - Policy: Random-initialized neural network";
  print_endline "  - Future: MLX (Python) for advanced training on Apple Silicon\n";

  print_endline "Feature vector size: ~600+ dimensions";
  print_endline "  - Hand encoding: 136 (one-hot)";
  print_endline "  - Rivers: 136 x 2 (own + others) = 272";
  print_endline "  - Game state: wind, rounds, points, riichi";
  print_endline "  - Dora features\n";

  print_endline "=== Usage ===\n";
  print_endline "Run demo game:";
  print_endline "  ./mahjong --demo\n";
  print_endline "Run simulation with NN policy:";
  print_endline "  ./mahjong --train --games 1000"

(** Run training simulation *)
let run_training num_games =
  Printf.printf "=== Training Simulation ===\n\n";
  Printf.printf "Running %d games with neural network policy...\n" num_games;
  Printf.printf "Hidden layer size: 128 neurons\n\n";

  Random.self_init ();
  let policy = Policy.create_nn_policy ~hidden_size:128 in
  let policy_fn = Policy.policy_function policy in
  let config = { Training.default_config with num_games; policy = policy_fn } in

  Training.train config;

  print_endline "\nTraining complete!"

(** Run demo game with pretty-print *)
let run_demo () =
  Printf.printf "=== Demo Game with Neural Network Policy ===\n\n";
  Random.self_init ();
  let seed = Array.init 17 (fun _ -> Random.bits ()) in
  let rules = Rules.default_four_player in

  Printf.printf "Starting 4-player half-game\n";
  Printf.printf "Players: East, South, West, North\n";
  Printf.printf "Starting points: %d each\n" rules.Rules.points.start_points;
  Printf.printf "Policy: Random-initialized neural network (128 hidden neurons)\n\n";

  (* Create NN policy *)
  let policy = Policy.create_nn_policy ~hidden_size:128 in
  let policy_fn = Policy.policy_function policy in

  let final_state = Simulation.simulate_game rules seed policy_fn in

  Printf.printf "Game complete!\n";
  Printf.printf "Final scores:\n";
  Array.iteri (fun _i player ->
    let wind_name = match player.State.seat with
      | State.East -> "East"
      | State.South -> "South"
      | State.West -> "West"
      | State.North -> "North"
    in
    Printf.printf "  %s: %d points\n" wind_name player.State.points
  ) final_state.State.players;

  Printf.printf "\nTotal rounds played: %d\n" final_state.State.round.State.round_num;
  Printf.printf "Game log entries: %d\n" (List.length final_state.State.game_log)

let () =
  let (mode, _port, games) = parse_args () in
  match mode with
  | "train" -> run_training games
  | "demo" -> run_demo ()
  | _ -> show_info ()
