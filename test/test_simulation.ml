(** Tests for Mahjong simulation *)

open Mahjong

(** Test rules configuration *)
let test_rules () =
  (* Test 4-player rules *)
  let rules4 = Rules.default_four_player in
  assert (Rules.num_players rules4 = 4);
  assert (Rules.total_rounds rules4 = 8);
  
  (* Test 3-player rules *)
  let rules3 = Rules.default_three_player in
  assert (Rules.num_players rules3 = 3);
  assert (Rules.total_rounds rules3 = 6);
  
  (* Test East only variants *)
  let rules4e = Rules.default_four_player_east in
  assert (Rules.num_players rules4e = 4);
  assert (Rules.total_rounds rules4e = 4);
  
  let rules3e = Rules.default_three_player_east in
  assert (Rules.num_players rules3e = 3);
  assert (Rules.total_rounds rules3e = 3);
  
  print_endline "✓ Rules tests passed"

(** Test game state initialization *)
let test_state_init () =
  let rules = Rules.default_four_player in
  let seed = Array.init 17 (fun i -> i * 12345) in
  let state = State.init_game_state rules seed in
  
  (* Check player count *)
  assert (Array.length state.State.players = 4);
  
  (* Check initial points *)
  Array.iter (fun (p : State.player_state) ->
    assert (p.State.points = rules.Rules.points.start_points)
  ) state.State.players;
  
  (* Check initial round *)
  assert (state.State.round.State.round_num = 1);
  assert (state.State.round.State.honba = 0);
  assert (state.State.round.State.turn = 0);
  
  (* Check wall size for 4 players *)
  assert (Array.length state.State.round.State.wall = 136);
  
  print_endline "✓ State initialization tests passed"

(** Test 3-player wall generation with removed tiles *)
let test_sanma_wall () =
  let rules = Rules.default_three_player in
  let seed = Array.init 17 (fun i -> i * 54321) in
  let state = State.init_game_state rules seed in
  
  (* 3-player removes Man 2-8 (including Aka), so wall should be smaller *)
  (* Original: 136 tiles, removed: 7 man tiles × 4 sets = 28 tiles *)
  let wall_len = Array.length state.State.round.State.wall in
  let expected = 136 - 28 in
  if wall_len <> expected then begin
    Printf.printf "Wall length: %d, expected: %d\n" wall_len expected;
    assert false
  end;
  
  (* Verify no Man 2-8 in wall *)
  Array.iter (fun tile ->
    match tile with
    | Tiles.Man n -> 
        let num = Tiles.int_of_number n in
        if num <> 1 && num <> 9 then begin
          Printf.printf "Found unexpected Man tile: %d\n" num;
          assert false
        end
    | _ -> ()
  ) state.State.round.State.wall;
  
  print_endline "✓ Sanma wall tests passed"

(** Test basic simulation mechanics *)
let test_simulation_draw () =
  let rules = Rules.default_four_player in
  let seed = Array.init 17 (fun i -> i * 11111) in
  let state = State.init_game_state rules seed in
  
  (* Test draw action *)
  (match Simulation.apply_action state Simulation.DrawAction with
   | Simulation.Continue new_state ->
       let player = State.current_player new_state in
       assert (Array.length player.State.hand.Hand.tiles = 1);
       assert (new_state.State.round.State.wall_index = 1)
   | _ -> assert false);
  
  print_endline "✓ Simulation draw tests passed"

(** Test feature extraction *)
let test_feature_extraction () =
  let rules = Rules.default_four_player in
  let seed = Array.init 17 (fun i -> i * 99999) in
  let state = State.init_game_state rules seed in
  
  let features = Training.extract_features state 0 in
  
  (* Initial features should be empty/default *)
  assert (features.Training.is_riichi = 0.0);
  assert (features.Training.num_furos = 0.0);
  assert (features.Training.round_num >= 0.0);
  
  print_endline "✓ Feature extraction tests passed"

(** Test training config *)
let test_training_config () =
  let config = Training.default_config in
  
  assert (config.Training.num_games > 0);
  assert (config.Training.batch_size > 0);
  assert (config.Training.learning_rate > 0.0);
  assert (config.Training.num_threads > 0);
  
  print_endline "✓ Training config tests passed"

(** Test Tenhou tile encoding *)
let test_tenhou_encoding () =
  (* Test basic tile encoding *)
  let man1 = Tiles.Man Tiles.One in
  let code = Tenhou_protocol.tenhou_tile_of_tile man1 0 in
  assert (code >= 0 && code < 4);
  
  (* Test roundtrip *)
  let decoded = Tenhou_protocol.tile_of_tenhou_tile code in
  (match decoded with
   | Tiles.Man Tiles.One -> ()
   | _ -> assert false);
  
  (* Test aka tiles *)
  let man_aka = Tiles.Man Tiles.Aka in
  let aka_code = Tenhou_protocol.tenhou_tile_of_tile man_aka 0 in
  assert (aka_code = 16);  (* Man 5 aka is tile 16 *)
  
  (* Test honor tiles *)
  let east = Tiles.Honor Tiles.East in
  let east_code = Tenhou_protocol.tenhou_tile_of_tile east 0 in
  assert (east_code >= 108 && east_code < 112);
  
  print_endline "✓ Tenhou encoding tests passed"

(** Test Tenhou protocol parsing *)
let test_tenhou_protocol () =
  (* Test parsing INIT message *)
  let init_xml = "<INIT seed=\"0,0,0,1,2,34\" ten=\"250,250,250,250\" oya=\"0\" hai=\"1,2,3,4,5,6,7,8,9,10,11,12,13\"/>" in
  (match Tenhou_protocol.parse_message init_xml with
   | Tenhou_protocol.Init info ->
       assert (Array.length info.seed = 6);
       assert (Array.length info.hai = 13);
       assert (info.oya = 0)
   | _ -> assert false);
  
  (* Test parsing draw message *)
  let draw_xml = "<T45/>" in
  (match Tenhou_protocol.parse_message draw_xml with
   | Tenhou_protocol.Draw tile -> assert (tile = 45)
   | _ -> assert false);
  
  (* Test encoding discard *)
  let discard = Tenhou_protocol.encode_discard 45 in
  assert (String.length discard > 0);
  
  print_endline "✓ Tenhou protocol tests passed"

(** Test Tenhou bot state *)
let test_tenhou_bot () =
  let config = Tenhou_bot.default_config in
  let state = Tenhou_bot.init_bot_state config in
  
  assert (Array.length state.Tenhou_bot.points = 4);
  assert (state.Tenhou_bot.seat = 0);
  assert (Array.length state.Tenhou_bot.hand = 0);
  
  print_endline "✓ Tenhou bot tests passed"

(** Run all tests *)
let () =
  print_endline "\n=== Running Simulation Tests ===\n";
  test_rules ();
  test_state_init ();
  test_sanma_wall ();
  test_simulation_draw ();
  test_feature_extraction ();
  test_training_config ();
  test_tenhou_encoding ();
  test_tenhou_protocol ();
  test_tenhou_bot ();
  print_endline "\n=== All Tests Passed ===\n"
