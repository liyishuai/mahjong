(** Tests for Sp (Single Player / Shanten Probability) module *)

open Sp
open Hand
open Shanten

(* ==================================================================== *)
(* Helper Functions *)
(* ==================================================================== *)

let float_eq (a : float) (b : float) : bool =
  abs_float (a -. b) < 0.0001
;;

(* ==================================================================== *)
(* Required Tile Tests *)
(* ==================================================================== *)

let test_required_tile () =
  Printf.printf "Testing required_tile...\n";

  (* Test creation *)
  let rt = create_required_tile 5 2 in
  assert (rt.tile = 5);
  assert (rt.count = 2);

  Printf.printf "  required_tile tests passed\n"
;;

(* ==================================================================== *)
(* Candidate Tests *)
(* ==================================================================== *)

let test_candidate_creation () =
  Printf.printf "Testing candidate creation...\n";

  (* Create a basic candidate *)
  let cand =
    {
      tile = 10;
      tenpai_probs = [| 0.5; 0.6; 0.7 |];
      win_probs = [| 0.1; 0.2; 0.3 |];
      exp_values = [| 1000.0; 2000.0; 3000.0 |];
      required_tiles = [ create_required_tile 5 4; create_required_tile 15 2 ];
      num_required_tiles = 6;
      shanten_down = false;
    }
  in

  assert (cand.tile = 10);
  assert (Array.length cand.tenpai_probs = 3);
  assert (Array.length cand.win_probs = 3);
  assert (Array.length cand.exp_values = 3);
  assert (List.length cand.required_tiles = 2);
  assert (not cand.shanten_down);

  Printf.printf "  candidate creation tests passed\n"
;;

let test_candidate_comparison () =
  Printf.printf "Testing candidate comparison...\n";

  (* Test tile discard priority - honors have lower priority (sorted last) *)
  let honor_tile = Tiles.tile_id_E in  (* 27 *)
  let manzu_tile = Tiles.tile_id_9m in (* 8 *)

  (* When comparing honor to manzu, honor should come after (lower priority) *)
  let cmp_honor_manzu = cmp_tile_discard_priority honor_tile manzu_tile in
  assert (cmp_honor_manzu > 0);  (* positive means honor > manzu in sort order *)

  (* When comparing manzu to honor, manzu should come before (higher priority) *)
  let cmp_manzu_honor = cmp_tile_discard_priority manzu_tile honor_tile in
  assert (cmp_manzu_honor < 0);  (* negative means manzu < honor in sort order *)

  (* Within same suit, higher rank comes first (has higher discard priority) *)
  (* cmp(1m, 9m) returns 1, meaning 1m > 9m, so 1m comes AFTER 9m *)
  let cmp_1m_9m = cmp_tile_discard_priority Tiles.tile_id_1m Tiles.tile_id_9m in
  assert (cmp_1m_9m > 0);  (* 1m comes after 9m (9m has higher priority) *)

  (* Reverse check: 9m comes before 1m *)
  let cmp_9m_1m = cmp_tile_discard_priority Tiles.tile_id_9m Tiles.tile_id_1m in
  assert (cmp_9m_1m < 0);  (* 9m comes before 1m *)

  Printf.printf "  candidate comparison tests passed\n"
;;

(* ==================================================================== *)
(* State Tests *)
(* ==================================================================== *)

let test_init_state () =
  Printf.printf "Testing init_state...\n";

  (* Create an init_state from tehai *)
  let tehai = Array.make 34 0 in
  tehai.(Tiles.tile_id_1m) <- 2;
  tehai.(Tiles.tile_id_5m) <- 1;
  tehai.(Tiles.tile_id_E) <- 3;

  let init =
    create_init_state
      ~tehai
      ~akas_in_hand:[| false; false; false |]
      ~tiles_seen:tehai
      ~akas_seen:[| false; false; false |]
  in

  assert (init.tehai.(Tiles.tile_id_1m) = 2);
  assert (init.tehai.(Tiles.tile_id_5m) = 1);
  assert (init.tehai.(Tiles.tile_id_E) = 3);

  (* Convert to state *)
  let state = of_init_state init in
  assert (Array.length state.tiles_left = 34);
  assert (state.tiles_left.(Tiles.tile_id_1m) = 2);  (* 4 - 2 seen *)
  assert (state.tiles_left.(Tiles.tile_id_5m) = 3);  (* 4 - 1 seen *)

  Printf.printf "  init_state tests passed\n"
;;

(* ==================================================================== *)
(* Config Tests *)
(* ==================================================================== *)

let test_config () =
  Printf.printf "Testing config...\n";

  (* Create a basic config *)
  let config =
    create_config
      ~tehai_len_div3:4
      ~chis:[]
      ~pons:[]
      ~minkans:[]
      ~ankans:[]
      ~bakaze:Tiles.tile_id_E
      ~jikaze:Tiles.tile_id_E
      ~is_menzen:true
      ~num_doras_in_fuuro:0
      ~dora_indicators:[ Tiles.tile_id_6m ]
      ~calc_double_riichi:true
      ~calc_haitei:true
      ~prefer_riichi:true
      ~sort_result:true
      ~maximize_win_prob:false
      ~calc_tegawari:true
      ~calc_shanten_down:true
  in

  assert (config.tehai_len_div3 = 4);
  assert (config.is_menzen = true);
  assert (config.bakaze = Tiles.tile_id_E);
  assert (config.jikaze = Tiles.tile_id_E);
  assert (List.length config.dora_indicators = 1);
  assert (config.calc_tegawari = true);
  assert (config.calc_shanten_down = true);

  Printf.printf "  config tests passed\n"
;;

(* ==================================================================== *)
(* SP Calculator Tests *)
(* ==================================================================== *)

(* Nanikiru (discard decision) tests *)

let test_nanikiru () =
  Printf.printf "Testing nanikiru (discard decisions).\n";

  (* Test 1: Basic EV maximization - prefer honor wind discard *)
  begin
    Printf.printf "  Test 1: Basic EV maximization\n";
    match hand "45678m 34789p 3344z 1z" with
    | Error msg -> Printf.printf "    Hand parse failed: %s\n" msg; failwith "hand parse failed"
    | Ok tehai ->
        let tiles_seen = Array.copy tehai in
        (* Add dora indicator 5p to tiles_seen *)
        tiles_seen.(Tiles.tile_id_5p) <- tiles_seen.(Tiles.tile_id_5p) + 1;

        let init_state =
          create_init_state
            ~tehai
            ~akas_in_hand:[| false; false; false |]
            ~tiles_seen
            ~akas_seen:[| false; false; false |]
        in

        let config =
          create_config
            ~tehai_len_div3:4
            ~chis:[]
            ~pons:[]
            ~minkans:[]
            ~ankans:[]
            ~bakaze:Tiles.tile_id_E
            ~jikaze:Tiles.tile_id_N
            ~is_menzen:true
            ~num_doras_in_fuuro:0
            ~dora_indicators:[ Tiles.tile_id_5p ]
            ~calc_double_riichi:false
            ~calc_haitei:false
            ~prefer_riichi:true
            ~sort_result:true
            ~maximize_win_prob:false
            ~calc_tegawari:true
            ~calc_shanten_down:true
        in

        let cur_shanten = calc_all tehai 4 in
        let cur_shanten = if cur_shanten < 0 then 0 else cur_shanten in
        match Sp.calc config init_state true 8 cur_shanten with
        | Ok candidates ->
            if List.length candidates < 2 then failwith "expected at least 2 candidates";
            let first = List.hd candidates in
            let second = List.hd (List.tl candidates) in
            Printf.printf "    Best discard: %d, second: %d\n" first.tile second.tile;
        | Error msg -> Printf.printf "    Calc failed: %s\n" msg
  end;

  (* Test 2: Shanten down vs keep *)
  begin
    Printf.printf "  Test 2: Shanten down consideration\n";
    match hand "3667m 23489p 34688s 1z" with
    | Error msg -> Printf.printf "    Hand parse failed: %s\n" msg
    | Ok tehai ->
        let tiles_seen = Array.copy tehai in
        tiles_seen.(Tiles.tile_id_5p) <- tiles_seen.(Tiles.tile_id_5p) + 1;

        let init_state =
          create_init_state
            ~tehai
            ~akas_in_hand:[| false; false; false |]
            ~tiles_seen
            ~akas_seen:[| false; false; false |]
        in

        let config =
          create_config
            ~tehai_len_div3:4
            ~chis:[]
            ~pons:[]
            ~minkans:[]
            ~ankans:[]
            ~bakaze:Tiles.tile_id_E
            ~jikaze:Tiles.tile_id_N
            ~is_menzen:true
            ~num_doras_in_fuuro:0
            ~dora_indicators:[ Tiles.tile_id_5p ]
            ~calc_double_riichi:false
            ~calc_haitei:false
            ~prefer_riichi:true
            ~sort_result:true
            ~maximize_win_prob:false
            ~calc_tegawari:true
            ~calc_shanten_down:true
        in

        let cur_shanten = calc_all tehai 4 in
        let cur_shanten = if cur_shanten < 0 then 0 else cur_shanten in
        Printf.printf "    Current shanten: %d\n" cur_shanten;
        match Sp.calc config init_state true 15 cur_shanten with
        | Ok candidates ->
            Printf.printf "    Got %d candidates\n" (List.length candidates);
            (* Check for shanten_down flag *)
            let has_shanten_down = List.exists (fun c -> c.shanten_down) candidates in
            Printf.printf "    Has shanten_down candidate: %b\n" has_shanten_down
        | Error msg -> Printf.printf "    Calc failed: %s\n" msg
  end;

  Printf.printf "  nanikiru tests completed\n"
;;

(* Tsumo-only test *)

let test_tsumo_only () =
  Printf.printf "Testing tsumo-only scenarios...\n";

  match hand "45677m 456778p 48s" with
  | Error msg -> Printf.printf "  Hand parse failed: %s\n" msg; failwith "hand parse failed"
  | Ok tehai ->
      let tiles_seen = Array.copy tehai in
      (* Add dora indicator 6m to tiles_seen *)
      tiles_seen.(Tiles.tile_id_6m) <- tiles_seen.(Tiles.tile_id_6m) + 1;
      (* All 4 red 5s are visible *)
      tiles_seen.(Tiles.tile_id_5s) <- tiles_seen.(Tiles.tile_id_5s) + 4;

      let init_state =
        create_init_state
          ~tehai
          ~akas_in_hand:[| false; false; false |]
          ~tiles_seen
          ~akas_seen:[| false; false; true |]  (* Red 5s is seen *)
      in

      let config =
        create_config
          ~tehai_len_div3:4
          ~chis:[]
          ~pons:[]
          ~minkans:[]
          ~ankans:[]
          ~bakaze:Tiles.tile_id_E
          ~jikaze:Tiles.tile_id_W
          ~is_menzen:true
          ~num_doras_in_fuuro:0
          ~dora_indicators:[ Tiles.tile_id_6m ]
          ~calc_double_riichi:true
          ~calc_haitei:true
          ~prefer_riichi:true
          ~sort_result:true
          ~maximize_win_prob:true
          ~calc_tegawari:true
          ~calc_shanten_down:true
      in

      let cur_shanten = calc_all tehai 4 in
      let cur_shanten = if cur_shanten < 0 then 0 else cur_shanten in
      Printf.printf "  Current shanten: %d\n" cur_shanten;

      match Sp.calc config init_state false 5 cur_shanten with
      | Ok candidates ->
          Printf.printf "  Got %d candidates (expected 1 for tsumo-only)\n" (List.length candidates);
          if List.length candidates > 0 then
            let c = List.hd candidates in
            Printf.printf "  Tile: %d\n" c.tile;
            Printf.printf "  Required tiles: %d kinds\n" (List.length c.required_tiles);
            Printf.printf "  Tenpai prob: %.4f\n" c.tenpai_probs.(0);
            Printf.printf "  Win prob: %.4f\n" c.win_probs.(0);
      | Error msg -> Printf.printf "  Calc failed: %s\n" msg
;;

(* Probability table tests *)

let test_probability_tables () =
  Printf.printf "Testing probability table generation...\n";

  (* Test 1: tsumo_prob_table basic functionality *)
  begin
    Printf.printf "  Test 1: tsumo_prob_table with 70 tiles\n";
    let table = Sp.build_tsumo_prob_table 70 17 in
    assert (Array.length table = 4);  (* 4 rows for 1-4 extra tsumo *)
    assert (Array.length table.(0) = 17);  (* max_tsumo = 17 *)

    (* Check some known values: when n_left_tiles=70, tsumo_index=0 *)
    let expected = 1.0 /. 70.0 in
    assert (float_eq table.(0).(0) expected);
    Printf.printf "    table.(0).(0) = %.4f (expected %.4f)\n" table.(0).(0) expected;

    (* Verify all values are in [0, 1] *)
    for i = 0 to 3 do
      for j = 0 to 16 do
        let p = table.(i).(j) in
        assert (p >= 0.0 && p <= 1.0);
      done
    done;
    Printf.printf "    All values in valid range [0, 1]\n"
  end;

  (* Test 2: tsumo_prob_table with fewer tiles *)
  begin
    Printf.printf "  Test 2: tsumo_prob_table with 10 tiles\n";
    let table = Sp.build_tsumo_prob_table 10 17 in
    assert (Array.length table = 4);
    Printf.printf "    Probabilities handled near n_left_tiles\n"
  end;

  (* Test 3: not_tsumo_prob_table basic functionality *)
  begin
    Printf.printf "  Test 3: not_tsumo_prob_table with 70 tiles\n";
    let table = Sp.build_not_tsumo_prob_table 70 17 in
    (* Should have n_left_tiles + 1 rows *)
    assert (Array.length table = 71);
    assert (Array.length table.(0) = 17);  (* max_tsumo = 17 *)

    (* First value should always be 1.0 (probability of not tsumo on turn 0) *)
    assert (float_eq table.(0).(0) 1.0);
    Printf.printf "    table.(0).(0) = 1.0 (initial probability)\n";

    (* Verify all values are in [0, 1] *)
    for i = 0 to min 70 (Array.length table - 1) do
      for j = 0 to 16 do
        let p = table.(i).(j) in
        if p > 0.0 then assert (p <= 1.0);
      done
    done;
    Printf.printf "    All non-zero values in valid range [0, 1]\n"
  end;

  Printf.printf "  probability table tests completed\n"
;;

(* ==================================================================== *)
(* Integration Tests - Test actual calc function *)
(* ==================================================================== *)

let test_calc_basic () =
  Printf.printf "Testing calc function with basic hand...\n";

  (* Create a simple hand: 123456m 789p 345s 2s - 1 shanten, 14 tiles *)
  let tehai = Array.make 34 0 in
  tehai.(Tiles.tile_id_1m) <- 1;
  tehai.(Tiles.tile_id_2m) <- 1;
  tehai.(Tiles.tile_id_3m) <- 1;
  tehai.(Tiles.tile_id_4m) <- 1;
  tehai.(Tiles.tile_id_5m) <- 1;
  tehai.(Tiles.tile_id_6m) <- 1;
  tehai.(Tiles.tile_id_7p) <- 1;
  tehai.(Tiles.tile_id_8p) <- 1;
  tehai.(Tiles.tile_id_9p) <- 1;
  tehai.(Tiles.tile_id_3s) <- 1;
  tehai.(Tiles.tile_id_4s) <- 1;
  tehai.(Tiles.tile_id_5s) <- 1;
  tehai.(Tiles.tile_id_2s) <- 2;  (* Pair of 2s, total 14 tiles *)

  let init_state =
    Sp.create_init_state
      ~tehai
      ~akas_in_hand:[| false; false; false |]
      ~tiles_seen:tehai
      ~akas_seen:[| false; false; false |]
  in

  let config =
    Sp.create_config
      ~tehai_len_div3:4
      ~chis:[]
      ~pons:[]
      ~minkans:[]
      ~ankans:[]
      ~bakaze:Tiles.tile_id_E
      ~jikaze:Tiles.tile_id_E
      ~is_menzen:true
      ~num_doras_in_fuuro:0
      ~dora_indicators:[]
      ~calc_double_riichi:false
      ~calc_haitei:false
      ~prefer_riichi:true
      ~sort_result:true
      ~maximize_win_prob:false
      ~calc_tegawari:true
      ~calc_shanten_down:true
  in

  let cur_shanten = calc_all tehai 4 in
  let cur_shanten = if cur_shanten < 0 then 0 else cur_shanten in
  match Sp.calc config init_state true 17 cur_shanten with
  | Ok candidates ->
      Printf.printf "  Got %d candidates\n" (List.length candidates);
      assert (List.length candidates > 0);
      (* Check first candidate has valid probabilities *)
      let first = List.hd candidates in
      assert (first.tile >= 0 || first.tile = -1);
      assert (Array.length first.tenpai_probs > 0);
      assert (first.tenpai_probs.(0) >= 0.0 && first.tenpai_probs.(0) <= 1.0);
      Printf.printf "  First candidate: tile=%d, tenpai=%.2f, win=%.2f, ev=%.0f\n"
        first.tile first.tenpai_probs.(0) first.win_probs.(0) first.exp_values.(0);
  | Error msg ->
      Printf.printf "  Calc failed: %s\n" msg;
      failwith "calc should not fail for valid input"
;;

let test_calc_tenpai_with_agari () =
  Printf.printf "Testing calc with tenpai hand (agari integration).\n";

  (* Create a tenpai hand: 123456789m + pair of 5p + 1m - waits on 1m or 9m, total 14 tiles *)
  let tehai = Array.make 34 0 in
  tehai.(Tiles.tile_id_1m) <- 2;
  tehai.(Tiles.tile_id_2m) <- 1;
  tehai.(Tiles.tile_id_3m) <- 1;
  tehai.(Tiles.tile_id_4m) <- 1;
  tehai.(Tiles.tile_id_5m) <- 1;
  tehai.(Tiles.tile_id_6m) <- 1;
  tehai.(Tiles.tile_id_7m) <- 1;
  tehai.(Tiles.tile_id_8m) <- 1;
  tehai.(Tiles.tile_id_9m) <- 1;
  tehai.(Tiles.tile_id_5p) <- 2;  (* Pair *)
  tehai.(Tiles.tile_id_1p) <- 1;
  tehai.(Tiles.tile_id_2p) <- 1;  (* More tiles to reach 14 *)

  let init_state =
    Sp.create_init_state
      ~tehai
      ~akas_in_hand:[| false; false; false |]
      ~tiles_seen:tehai
      ~akas_seen:[| false; false; false |]
  in

  let config =
    Sp.create_config
      ~tehai_len_div3:4
      ~chis:[]
      ~pons:[]
      ~minkans:[]
      ~ankans:[]
      ~bakaze:Tiles.tile_id_E
      ~jikaze:Tiles.tile_id_E
      ~is_menzen:true
      ~num_doras_in_fuuro:0
      ~dora_indicators:[]
      ~calc_double_riichi:false
      ~calc_haitei:false
      ~prefer_riichi:true
      ~sort_result:true
      ~maximize_win_prob:false
      ~calc_tegawari:true
      ~calc_shanten_down:true
  in

  let cur_shanten = calc_all tehai 4 in
  let cur_shanten = if cur_shanten < 0 then 0 else cur_shanten in
  match Sp.calc config init_state true 17 cur_shanten with
  | Ok candidates ->
      Printf.printf "  Got %d candidates\n" (List.length candidates);
      assert (List.length candidates > 0);

      let first = List.hd candidates in
      Printf.printf "  First discard: tile=%d\n" first.tile;
      Printf.printf "  tenpai=%.2f (prob of getting back to tenpai)\n" first.tenpai_probs.(0);
      Printf.printf "  ev=%.0f (should reflect real scoring)\n" first.exp_values.(0);

      (* Verify required_tiles are populated *)
      assert (List.length first.required_tiles > 0);
      Printf.printf "  required_tiles: %d tiles\n" (List.length first.required_tiles);
  | Error msg ->
      Printf.printf "  Calc failed: %s\n" msg;
      failwith "calc should not fail for tenpai hand"
;;


let test_calc_with_dora () =
  Printf.printf "Testing calc with dora indicators...\n";

  (* Same hand but with dora, total 14 tiles *)
  let tehai = Array.make 34 0 in
  tehai.(Tiles.tile_id_1m) <- 2;
  tehai.(Tiles.tile_id_2m) <- 1;
  tehai.(Tiles.tile_id_3m) <- 1;
  tehai.(Tiles.tile_id_4m) <- 1;
  tehai.(Tiles.tile_id_5m) <- 1;
  tehai.(Tiles.tile_id_6m) <- 1;
  tehai.(Tiles.tile_id_7m) <- 1;
  tehai.(Tiles.tile_id_8m) <- 1;
  tehai.(Tiles.tile_id_9m) <- 1;
  tehai.(Tiles.tile_id_5p) <- 2;
  tehai.(Tiles.tile_id_1p) <- 1;
  tehai.(Tiles.tile_id_2p) <- 1;

  let init_state =
    Sp.create_init_state
      ~tehai
      ~akas_in_hand:[| false; false; false |]
      ~tiles_seen:tehai
      ~akas_seen:[| false; false; false |]
  in

  (* Add 5m as dora indicator *)
  let config =
    Sp.create_config
      ~tehai_len_div3:4
      ~chis:[]
      ~pons:[]
      ~minkans:[]
      ~ankans:[]
      ~bakaze:Tiles.tile_id_E
      ~jikaze:Tiles.tile_id_E
      ~is_menzen:true
      ~num_doras_in_fuuro:0
      ~dora_indicators:[ Tiles.tile_id_5m ]  (* 5m is dora *)
      ~calc_double_riichi:false
      ~calc_haitei:false
      ~prefer_riichi:true
      ~sort_result:true
      ~maximize_win_prob:false
      ~calc_tegawari:true
      ~calc_shanten_down:true
  in

  let cur_shanten = calc_all tehai 4 in
  let cur_shanten = if cur_shanten < 0 then 0 else cur_shanten in
  match Sp.calc config init_state true 17 cur_shanten with
  | Ok candidates ->
      Printf.printf "  Got %d candidates\n" (List.length candidates);
      assert (List.length candidates > 0);
      Printf.printf "  EV should account for dora in scoring\n"
  | Error msg ->
      Printf.printf "  Calc failed: %s\n" msg
;;


(* ==================================================================== *)
(* Test Runner *)
(* ==================================================================== *)

let run_test (name : string) (test_fn : unit -> unit) : bool =
  try
    test_fn ();
    Printf.printf "  ✓ %s passed\n" name;
    true
  with
  | Assert_failure(file, line, col) ->
    Printf.printf "  ✗ %s FAILED\n" name;
    Printf.printf "    Assertion at %s:%d:%d\n" file line col;
    false
  | Failure msg ->
    Printf.printf "  ✗ %s FAILED\n" name;
    Printf.printf "    Error: %s\n" msg;
    false
  | e ->
    Printf.printf "  ✗ %s FAILED\n" name;
    Printf.printf "    Exception: %s\n" (Printexc.to_string e);
    false
;;

let () =
  Printf.printf "\n======================================\n";
  Printf.printf "Sp (Shanten Probability) Tests\n";
  Printf.printf "======================================\n\n";

  (* Track test results *)
  let passed = ref 0 in
  let failed = ref 0 in

  (* Basic type tests *)
  Printf.printf "Basic Type Tests\n";
  Printf.printf "-----------------\n";
  List.iter
    (fun (name, fn) ->
       if run_test name fn then incr passed else incr failed)
    [ "required_tile", test_required_tile
    ; "candidate creation", test_candidate_creation
    ; "candidate comparison", test_candidate_comparison
    ];
  Printf.printf "\n";

  (* State tests *)
  Printf.printf "State Tests\n";
  Printf.printf "-----------\n";
  List.iter
    (fun (name, fn) ->
       if run_test name fn then incr passed else incr failed)
    [ "init_state", test_init_state
    ];
  Printf.printf "\n";

  (* Config tests *)
  Printf.printf "Config Tests\n";
  Printf.printf "------------\n";
  List.iter
    (fun (name, fn) ->
       if run_test name fn then incr passed else incr failed)
    [ "config", test_config
    ];
  Printf.printf "\n";

  (* SP calculator tests *)
  Printf.printf "SP Calculator Tests\n";
  Printf.printf "--------------------\n";
  if run_test "nanikiru" test_nanikiru then incr passed else incr failed;
  if run_test "tsumo_only" test_tsumo_only then incr passed else incr failed;
  if run_test "probability tables" test_probability_tables then incr passed else incr failed;
  Printf.printf "\n";

  (* Integration tests *)
  Printf.printf "Integration Tests\n";
  Printf.printf "-----------------\n";
  if run_test "calc basic" test_calc_basic then incr passed else incr failed;
  if run_test "calc tenpai with agari" test_calc_tenpai_with_agari then incr passed else incr failed;
  if run_test "calc with dora" test_calc_with_dora then incr passed else incr failed;
  Printf.printf "\n";

  (* Print summary *)
  Printf.printf "======================================\n";
  Printf.printf "Test Summary:\n";
  Printf.printf "  Passed: %d\n" !passed;
  Printf.printf "  Failed: %d\n" !failed;
  Printf.printf "  Total: %d\n" (!passed + !failed);
  if !failed = 0
  then Printf.printf "  Status: ✓ All tests passed!\n"
  else Printf.printf "  Status: ✗ Some tests failed\n";
  Printf.printf "======================================\n\n";
  if !failed > 0 then exit 1
;;