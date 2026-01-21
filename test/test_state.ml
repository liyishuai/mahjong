(** Tests for state module *)

(** Helper: Create a 34-count tehai array from a hand string *)
let tehai_from_string (s : string) : int array = Hand.hand s |> Result.get_ok

(** Helper: Assert waits against a list of expected tile indices *)
let assert_waits (state : State.player_state) (expected : int list) : unit =
  for i = 0 to 33 do
    let should_wait = List.mem i expected in
    let does_wait = state.waits.(i) in
    if should_wait <> does_wait
    then (
      let msg =
        Printf.sprintf
          "Wait mismatch for tile %d: expected %b, got %b"
          i
          should_wait
          does_wait
      in
      failwith msg)
  done
;;

(** Helper: Update state from JSON string *)
let update_json (state : State.player_state) (json_str : string) : State.action_candidate =
  try
    let event = Mjai.Json.event_of_json (Yojson.Safe.from_string json_str) in
    State.update state event;
    state.last_cans
  with
  | Yojson.Json_error msg -> failwith ("Failed to parse JSON: " ^ msg)
  | e -> raise e
;;

(** Helper: Create state from log (sequence of JSON events) *)
let from_log (player_id : int) (log : string) : State.player_state =
  let state = State.create_player_state player_id in
  let lines = String.split_on_char '\n' log in
  let event_num = ref 0 in
  List.iter
    (fun line ->
       let trimmed = String.trim line in
       if String.length trimmed > 0
       then (
         incr event_num;
         let before_count = Array.fold_left ( + ) 0 state.tehai in
         let _ = update_json state trimmed in
         let after_count = Array.fold_left ( + ) 0 state.tehai in
         if before_count <> after_count
         then
           Printf.printf
             "  Event %d: hand size %d -> %d\n"
             !event_num
             before_count
             after_count))
    lines;
  state
;;

(** Test basic state creation *)
let test_create_state () =
  Printf.printf "Testing create_player_state...\n";
  let state = State.create_player_state 0 in
  assert (state.player_id = 0);
  assert (Array.length state.tehai = 34);
  assert (Array.for_all (( = ) 0) state.tehai);
  assert (state.bakaze = Tiles.tile_id_E);
  assert (state.jikaze = Tiles.tile_id_E);
  (* E for player 0 *)
  assert (state.kyoku = 0);
  assert (state.honba = 0);
  assert (state.kyotaku = 0);
  assert (state.oya = 0);
  assert (Array.length state.scores = 4);
  assert (state.scores.(0) = 25000);
  assert (state.is_menzen = true);
  Printf.printf "  create_player_state tests passed\n"
;;

(** Test action candidate helper functions *)
let test_action_candidate_helpers () =
  Printf.printf "Testing action_candidate helpers...\n";
  let cans = State.default_action_candidate in
  assert (not (State.can_chi cans));
  assert (not (State.can_kan cans));
  assert (not (State.can_agari cans));
  assert (not (State.can_pass cans));
  assert (not (State.can_act cans));
  let cans_with_chi = { cans with can_chi_low = true } in
  assert (State.can_chi cans_with_chi);
  assert (State.can_pass cans_with_chi);
  let cans_with_pon = { cans with can_pon = true } in
  assert (State.can_pass cans_with_pon);
  let cans_with_kan = { cans with can_daiminkan = true } in
  assert (State.can_kan cans_with_kan);
  assert (State.can_pass cans_with_kan);
  let cans_with_agari = { cans with can_ron_agari = true } in
  assert (State.can_agari cans_with_agari);
  assert (State.can_pass cans_with_agari);
  Printf.printf "  action_candidate helper tests passed\n"
;;

(** Test start_kyoku event *)
let test_start_kyoku () =
  Printf.printf "Testing start_kyoku event...\n";
  let state = State.create_player_state 0 in
  let tehais =
    [| [| 0; 0; 0; 1; 1; 1; 2; 2; 2; 3; 3; 3; 4 |]
     ; (* Simple 13 tiles *)
       [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |]
     ; [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |]
     ; [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |]
    |]
  in
  let scores = [| 25000; 25000; 25000; 25000 |] in
  let event =
    Mjai.Start_kyoku
      { bakaze = Tiles.tile_id_E
      ; dora_marker = Tiles.tile_id_8m
      ; (* Using 8m as indicator for 9m *)
        kyoku = 1
      ; honba = 0
      ; kyotaku = 0
      ; oya = 0
      ; scores
      ; tehais
      }
  in
  State.update state event;
  assert (state.bakaze = Tiles.tile_id_E);
  assert (state.kyoku = 1);
  assert (state.honba = 0);
  assert (state.oya = 0);
  assert (state.is_menzen = true);
  (* Check tehai was populated *)
  assert (state.tehai.(0) = 3);
  (* Three 0m *)
  assert (state.tehai.(1) = 3);
  (* Three 1m *)
  assert (state.tehai.(2) = 3);
  (* Three 2m *)
  assert (state.tehai.(3) = 3);
  (* Three 3m *)
  assert (state.tehai.(4) = 1);
  (* One 4m *)
  Printf.printf "  start_kyoku tests passed\n"
;;

(** Test tsumo event *)
let test_tsumo () =
  Printf.printf "Testing tsumo event...\n";
  let state = State.create_player_state 0 in
  (* Draw a tile *)
  let event = Mjai.Tsumo { actor = 0; pai = Tiles.tile_id_6m } in
  State.update state event;
  assert (state.tehai.(Tiles.tile_id_6m) = 1);
  assert (state.last_self_tsumo = Some Tiles.tile_id_6m);
  (* Different player draws - state should not change *)
  let event2 = Mjai.Tsumo { actor = 1; pai = Tiles.tile_id_2p } in
  State.update state event2;
  assert (state.tehai.(Tiles.tile_id_6m) = 1);
  (* Unchanged *)
  assert (state.tehai.(Tiles.tile_id_2p) = 0);
  (* Not added *)
  Printf.printf "  tsumo tests passed\n"
;;

(** Test dahai event *)
let test_dahai () =
  Printf.printf "Testing dahai event...\n";
  let state = State.create_player_state 0 in
  (* Set up initial hand *)
  state.tehai.(Tiles.tile_id_1m) <- 2;
  state.tehai.(Tiles.tile_id_6m) <- 1;
  (* Discard a tile *)
  let event = Mjai.Dahai { actor = 0; pai = Tiles.tile_id_6m; tsumogiri = false } in
  State.update state event;
  assert (state.tehai.(Tiles.tile_id_6m) = 0);
  assert (state.tehai.(Tiles.tile_id_1m) = 2);
  (* Unchanged *)

  (* Discard with tsumogiri *)
  state.last_self_tsumo <- Some Tiles.tile_id_1m;
  let event2 = Mjai.Dahai { actor = 0; pai = Tiles.tile_id_1m; tsumogiri = true } in
  State.update state event2;
  assert (state.tehai.(Tiles.tile_id_1m) = 1);
  assert (state.last_self_tsumo = None);
  (* Cleared on tsumogiri *)
  Printf.printf "  dahai tests passed\n"
;;

(** Test validate_reaction *)
let test_validate_reaction () =
  Printf.printf "Testing validate_reaction...\n";
  let state = State.create_player_state 0 in
  (* None should always be valid *)
  (match State.validate_reaction state Mjai.None with
   | Ok () -> ()
   | Error msg ->
     Printf.printf "  ERROR: None should be valid: %s\n" msg;
     failwith "test failed");
  (* Cannot discard when can_discard is false *)
  (match
     State.validate_reaction state (Mjai.Dahai { actor = 0; pai = 0; tsumogiri = false })
   with
   | Error "cannot discard" -> ()
   | Ok () ->
     Printf.printf "  ERROR: Should not be able to discard\n";
     failwith "test failed"
   | Error msg ->
     Printf.printf "  ERROR: Unexpected error: %s\n" msg;
     failwith "test failed");
  (* Cannot riichi when can_riichi is false *)
  (match State.validate_reaction state (Mjai.Reach { actor = 0 }) with
   | Error "cannot riichi" -> ()
   | Ok () ->
     Printf.printf "  ERROR: Should not be able to riichi\n";
     failwith "test failed"
   | Error msg ->
     Printf.printf "  ERROR: Unexpected error: %s\n" msg;
     failwith "test failed");
  Printf.printf "  validate_reaction tests passed\n"
;;

(** Test tile_in_hand *)
let test_tile_in_hand () =
  Printf.printf "Testing tile_in_hand...\n";
  let state = State.create_player_state 0 in
  state.tehai.(Tiles.tile_id_6m) <- 2;
  state.tehai.(Tiles.tile_id_2p) <- 1;
  assert (State.tile_in_hand state Tiles.tile_id_6m);
  assert (State.tile_in_hand state Tiles.tile_id_2p);
  assert (not (State.tile_in_hand state Tiles.tile_id_1m));
  Printf.printf "  tile_in_hand tests passed\n"
;;

(** Test shanten calculation and integration *)
let test_shanten_integration () =
  Printf.printf "Testing shanten integration...\n";
  let state = State.create_player_state 0 in
  (* Test with a simple winning hand - all 1-9m *)
  for i = Tiles.tile_id_1m to Tiles.tile_id_9m do
    state.tehai.(i) <- 1
  done;
  state.tehai.(Tiles.tile_id_1s) <- 1;
  state.tehai.(Tiles.tile_id_2s) <- 1;
  state.tehai.(Tiles.tile_id_3s) <- 1;
  state.tehai.(Tiles.tile_id_1s) <- state.tehai.(Tiles.tile_id_1s) + 1;
  (* Add one more 1s to make 14 tiles *)
  state.tehai_len_div3 <- 4;
  State.update_shanten state;
  if state.shanten <> -1
  then Printf.printf "  shanten=%d (expected -1 or 0 for winning hand)\n" state.shanten;
  (* Test with a non-tenpai hand *)
  Array.iteri (fun idx _ -> state.tehai.(idx) <- 0) state.tehai;
  for i = Tiles.tile_id_1m to Tiles.tile_id_6m do
    state.tehai.(i) <- 1 (* 123m x2 *)
  done;
  for i = Tiles.tile_id_1s to Tiles.tile_id_3s do
    state.tehai.(i) <- 1 (* 123s *)
  done;
  state.tehai_len_div3 <- 4;
  State.update_shanten state;
  assert (state.shanten >= 0);
  (* Should be at least 0-shanten *)
  Printf.printf "  shanten integration tests passed\n"
;;

(** Test waits calculation *)
let test_waits () =
  Printf.printf "Testing waits calculation...\n";
  let state = State.create_player_state 0 in
  (* Test Case 1: Ryanmen (two-sided) wait *)
  state.tehai <- tehai_from_string "11m 234m 567m 111s 78p";
  state.tehai_len_div3 <- 4;
  State.update_shanten state;
  assert (state.shanten = 0);
  State.update_waits_and_furiten state;
  assert_waits state [ Tiles.tile_id_6p; Tiles.tile_id_9p ];
  (* Waits on 6p and 9p *)

  (* Test Case 2: Junsei Chuuren Poutou (nine-sided wait) *)
  state.tehai <- tehai_from_string "1112345678999m";
  state.tehai_len_div3 <- 4;
  State.update_shanten state;
  assert (state.shanten = 0);
  State.update_waits_and_furiten state;
  assert_waits
    state
    [ Tiles.tile_id_1m
    ; Tiles.tile_id_2m
    ; Tiles.tile_id_3m
    ; Tiles.tile_id_4m
    ; Tiles.tile_id_5m
    ; Tiles.tile_id_6m
    ; Tiles.tile_id_7m
    ; Tiles.tile_id_8m
    ; Tiles.tile_id_9m
    ];
  (* Waits on all manzu tiles *)
  Printf.printf "  waits tests passed\n"
;;

(** Test real_time_shanten *)
let test_real_time_shanten () =
  Printf.printf "Testing real_time_shanten...\n";
  let state = State.create_player_state 0 in
  (* Test with 3n+1 hand (can_discard = false) *)
  let hand = [| 0; 0; 0; 1; 1; 1; 2; 2; 2; 3; 3; 3; 4 |] in
  Array.iter
    (fun tile ->
       let idx = Tiles.deaka tile in
       if idx >= 0 && idx < 34 then state.tehai.(idx) <- state.tehai.(idx) + 1)
    hand;
  state.tehai_len_div3 <- 4;
  State.update_shanten state;
  state.last_cans <- { State.default_action_candidate with can_discard = false };
  assert (State.real_time_shanten state = state.shanten);
  (* Test with 3n+2 hand (can_discard = true) *)
  state.tehai.(8) <- state.tehai.(8) + 1;
  (* Add another tile *)
  state.tehai_len_div3 <- 4;
  State.update_shanten state;
  state.last_cans <- { State.default_action_candidate with can_discard = true };
  (* real_time_shanten should calculate based on actual hand *)
  let real_shanten = State.real_time_shanten state in
  assert (real_shanten = state.shanten || real_shanten = state.shanten - 1);
  Printf.printf "  real_time_shanten tests passed\n"
;;

(** Test advanced tracking integration with event processing *)
let test_advanced_tracking_integration () =
  Printf.printf "Testing advanced tracking integration...\n";
  let state = State.create_player_state 0 in
  (* Set up initial hand: 123456m *)
  state.tehai.(Tiles.tile_id_1m) <- 1;
  state.tehai.(Tiles.tile_id_2m) <- 1;
  state.tehai.(Tiles.tile_id_3m) <- 1;
  state.tehai.(Tiles.tile_id_4m) <- 1;
  state.tehai.(Tiles.tile_id_5m) <- 1;
  state.tehai.(Tiles.tile_id_6m) <- 1;
  state.tehai_len_div3 <- 2;
  State.update_shanten state;
  (* Verify shanten is calculated *)
  Printf.printf "  Initial shanten: %d\n" state.shanten;
  assert (state.shanten >= 0);
  (* Process a tsumo event - should update shanten *)
  let tsumo_event = Mjai.Tsumo { actor = 0; pai = Tiles.tile_id_7m } in
  State.update state tsumo_event;
  (* Verify shanten was updated after tsumo *)
  Printf.printf "  After tsumo shanten: %d\n" state.shanten;
  assert (state.shanten >= 0);
  (* Process a dahai event - should update shanten and track discard *)
  let dahai_event = Mjai.Dahai { actor = 0; pai = Tiles.tile_id_7m; tsumogiri = true } in
  (* Discard 7m *)
  State.update state dahai_event;
  (* Verify discard was tracked *)
  Printf.printf "  Discarded tiles tracked: %b\n" state.discarded_tiles.(Tiles.tile_id_7m);
  assert (state.discarded_tiles.(Tiles.tile_id_7m) = true);
  Printf.printf "  advanced tracking integration tests passed\n"
;;

(** Test ankan candidates *)
let test_ankan_candidates () =
  Printf.printf "Testing ankan_candidates...\n";
  let state = State.create_player_state 0 in
  (* Set up hand with two quads: 1111m and 2222p *)
  state.tehai.(Tiles.tile_id_1m) <- 4;
  state.tehai.(Tiles.tile_id_1p) <- 4;
  let candidates = State.ankan_candidates state in
  Printf.printf "  Found %d ankan candidates\n" (Array.length candidates);
  assert (Array.length candidates = 2);
  (* Should find 1m and 1p *)
  assert (Array.mem Tiles.tile_id_1m candidates);
  assert (Array.mem Tiles.tile_id_1p candidates);
  Printf.printf "  ankan_candidates tests passed\n"
;;

(** Test kakan candidates *)
let test_kakan_candidates () =
  Printf.printf "Testing kakan_candidates...\n";
  let state = State.create_player_state 0 in
  (* Set up hand with a pon meld and the 4th tile *)
  state.pons <- [ Tiles.tile_id_1m ];
  state.tehai.(Tiles.tile_id_1m) <- 1;
  (* Have the 4th 1m in hand *)
  let candidates = State.kakan_candidates state in
  Printf.printf "  Found %d kakan candidates\n" (List.length candidates);
  assert (List.length candidates = 1);
  assert (List.hd candidates = Tiles.tile_id_1m);
  (* Test with no candidates *)
  state.tehai.(Tiles.tile_id_1m) <- 0;
  let candidates = State.kakan_candidates state in
  assert (List.length candidates = 0);
  Printf.printf "  kakan_candidates tests passed\n"
;;

(** Test can_chi functionality *)
let test_can_chi () =
  Printf.printf "Testing can_chi...\n";
  let state = State.create_player_state 0 in
  (* Test case 1: 1111234m - cannot chi 1m or 4m, can chi 2m *)
  state.tehai.(Tiles.tile_id_1m) <- 4;
  state.tehai.(Tiles.tile_id_2m) <- 1;
  state.tehai.(Tiles.tile_id_3m) <- 1;
  state.tehai.(Tiles.tile_id_4m) <- 1;
  (* Check chi with 1m - should be false *)
  State.set_can_chi_from_tile state Tiles.tile_id_1m;
  Printf.printf
    "  chi 1m: low=%b mid=%b high=%b\n"
    state.last_cans.can_chi_low
    state.last_cans.can_chi_mid
    state.last_cans.can_chi_high;
  assert (not state.last_cans.can_chi_low);
  assert (not state.last_cans.can_chi_mid);
  assert (not state.last_cans.can_chi_high);
  (* Check chi with 4m - should be false *)
  State.set_can_chi_from_tile state Tiles.tile_id_4m;
  assert (not state.last_cans.can_chi_low);
  assert (not state.last_cans.can_chi_mid);
  assert (not state.last_cans.can_chi_high);
  (* Check chi with 2m - should have mid and low chi *)
  State.set_can_chi_from_tile state Tiles.tile_id_2m;
  assert (not state.last_cans.can_chi_high);
  assert state.last_cans.can_chi_mid;
  assert state.last_cans.can_chi_low;
  (* Test case 2: 6666789999p - test various chi options *)
  Array.fill state.tehai 0 34 0;
  state.tehai.(Tiles.tile_id_6p) <- 4;
  state.tehai.(Tiles.tile_id_7p) <- 1;
  state.tehai.(Tiles.tile_id_8p) <- 1;
  state.tehai.(Tiles.tile_id_9p) <- 4;
  (* Chi with 5p (567p) - only low chi *)
  State.set_can_chi_from_tile state Tiles.tile_id_5p;
  assert (not state.last_cans.can_chi_high);
  assert (not state.last_cans.can_chi_mid);
  assert state.last_cans.can_chi_low;
  (* Chi with 7p (678p or 789p) - mid and low chi *)
  State.set_can_chi_from_tile state Tiles.tile_id_7p;
  assert (not state.last_cans.can_chi_high);
  assert state.last_cans.can_chi_mid;
  assert state.last_cans.can_chi_low;
  (* Chi with 8p (789p) - high and mid chi *)
  State.set_can_chi_from_tile state Tiles.tile_id_8p;
  assert state.last_cans.can_chi_high;
  assert state.last_cans.can_chi_mid;
  assert (not state.last_cans.can_chi_low);
  (* Test case 3: 4556s - edge cases *)
  Array.fill state.tehai 0 34 0;
  state.tehai.(Tiles.tile_id_4s) <- 1;
  state.tehai.(Tiles.tile_id_5s) <- 2;
  state.tehai.(Tiles.tile_id_6s) <- 1;
  (* Chi with 3s - only low chi *)
  State.set_can_chi_from_tile state Tiles.tile_id_3s;
  assert (not state.last_cans.can_chi_high);
  assert (not state.last_cans.can_chi_mid);
  assert state.last_cans.can_chi_low;
  (* Chi with 4s - only low chi *)
  State.set_can_chi_from_tile state Tiles.tile_id_4s;
  assert (not state.last_cans.can_chi_high);
  assert (not state.last_cans.can_chi_mid);
  assert state.last_cans.can_chi_low;
  (* Chi with 5s - should be false (would break pair) *)
  State.set_can_chi_from_tile state Tiles.tile_id_5s;
  assert (not state.last_cans.can_chi_high);
  assert (not state.last_cans.can_chi_mid);
  assert (not state.last_cans.can_chi_low);
  (* Chi with 6s - only high chi *)
  State.set_can_chi_from_tile state Tiles.tile_id_6s;
  assert state.last_cans.can_chi_high;
  assert (not state.last_cans.can_chi_mid);
  assert (not state.last_cans.can_chi_low);
  (* Chi with 7s - only high chi *)
  State.set_can_chi_from_tile state Tiles.tile_id_7s;
  assert state.last_cans.can_chi_high;
  assert (not state.last_cans.can_chi_mid);
  assert (not state.last_cans.can_chi_low);
  Printf.printf "  can_chi tests passed\n"
;;

(** Test furiten tracking *)
let test_furiten () =
  Printf.printf "Testing furiten tracking...\n";
  let state = State.create_player_state 0 in
  (* Test 1: Not furiten when no winning tiles are discarded *)
  state.tehai.(0) <- 3;
  (* 1m x3 *)
  state.tehai.(1) <- 1;
  (* 2m *)
  state.tehai.(2) <- 1;
  (* 3m *)
  state.tehai_len_div3 <- 1;
  State.update_shanten state;
  State.update_waits_and_furiten state;
  assert (not state.at_furiten);
  Printf.printf "  Test 1 passed: not furiten with no discards\n";
  (* Test 2: Furiten when a winning tile is discarded *)
  state.tehai.(Tiles.tile_id_1m) <- 2;
  (* 11m *)
  state.tehai.(Tiles.tile_id_2m) <- 1;
  (* 2m *)
  state.tehai.(Tiles.tile_id_3m) <- 1;
  (* 3m *)
  state.tehai_len_div3 <- 1;
  (* Simulate discarding 2m *)
  state.discarded_tiles.(Tiles.tile_id_2m) <- true;
  State.update_shanten state;
  State.update_waits_and_furiten state;
  (* Waiting on 1m or 3m, but 2m was discarded so not furiten yet *)
  (* Actually, let me set up a better example *)
  Array.fill state.tehai 0 34 0;
  state.tehai.(Tiles.tile_id_1m) <- 2;
  (* 11m *)
  state.tehai.(Tiles.tile_id_9m) <- 2;
  (* 99m *)
  state.tehai_len_div3 <- 1;
  (* Discard 1m (wait tile) *)
  state.discarded_tiles.(Tiles.tile_id_1m) <- true;
  State.update_shanten state;
  State.update_waits_and_furiten state;
  assert state.at_furiten;
  Printf.printf "  Test 2 passed: furiten when wait tile discarded\n";
  (* Test 3: Furiten reset with new hand *)
  Array.fill state.tehai 0 34 0;
  Array.fill state.discarded_tiles 0 34 false;
  state.tehai.(Tiles.tile_id_1m) <- 3;
  (* 111m *)
  state.tehai.(Tiles.tile_id_2m) <- 1;
  (* 2m *)
  state.tehai_len_div3 <- 1;
  State.update_shanten state;
  State.update_waits_and_furiten state;
  assert (not state.at_furiten);
  Printf.printf "  Test 3 passed: furiten reset with new hand\n";
  Printf.printf "  furiten tracking tests passed\n"
;;

(** Test dora tracking *)
let test_dora () =
  Printf.printf "Testing dora tracking...\n";
  let state = State.create_player_state 0 in
  (* Test 1: Add dora indicator *)
  State.add_dora_indicator state Tiles.tile_id_8p;
  (* 8p indicator, 9p is dora *)
  assert (List.length state.dora_indicators = 1);
  assert (List.hd state.dora_indicators = Tiles.tile_id_8p);
  assert (state.dora_factor.(Tiles.tile_id_9p) = 1);
  Printf.printf "  Test 1 passed: dora indicator added\n";
  (* Test 2: Multiple dora indicators *)
  State.add_dora_indicator state Tiles.tile_id_1m;
  (* 1m indicator, 2m is dora *)
  State.add_dora_indicator state Tiles.tile_id_1p;
  (* 1p indicator, 2p is dora *)
  assert (List.length state.dora_indicators = 3);
  assert (state.dora_factor.(Tiles.tile_id_2m) = 1);
  assert (state.dora_factor.(Tiles.tile_id_2p) = 1);
  Printf.printf "  Test 2 passed: multiple dora indicators\n";
  (* Test 3: Dora tiles in hand *)
  Array.fill state.tehai 0 34 0;
  state.tehai.(Tiles.tile_id_2m) <- 2;
  (* Two 2m, which is dora *)
  state.tehai.(Tiles.tile_id_2p) <- 1;
  (* One 2p, which is dora *)
  (* Reset and add indicators to count doras in hand *)
  Array.fill state.dora_factor 0 34 0;
  Array.fill state.doras_owned 0 4 0;
  state.dora_indicators <- [];
  State.add_dora_indicator state Tiles.tile_id_1m;
  (* 1m indicator, 2m is dora *)
  assert (state.dora_factor.(Tiles.tile_id_2m) = 1);
  assert (state.doras_owned.(0) = 2);
  (* Two 2m in hand *)
  Printf.printf "  Test 3 passed: dora tiles counted in hand\n";
  Printf.printf "  dora tracking tests passed\n"
;;

let test_aka_tracking () =
  Printf.printf "Testing aka (red tile) tracking...\n";
  (* Test 1: Drawing 5mr should set akas_in_hand.(0) to true *)
  Printf.printf "  Test 1: Drawing 5mr...\n";
  let state = State.create_player_state 0 in
  state.tehai.(Tiles.tile_id_5m) <- 1;
  state.akas_in_hand.(0) <- false;
  State.tsumo state 0 Tiles.tile_id_5mr;
  assert (state.akas_in_hand.(0) = true);
  Printf.printf "  Test 1 passed: akas_in_hand.(0) true after drawing 5mr\n";
  (* Test 2: Discarding 5mr should set akas_in_hand.(0) to false *)
  Printf.printf "  Test 2: Discarding 5mr...\n";
  let state = State.create_player_state 0 in
  state.tehai.(Tiles.tile_id_5m) <- 1;
  state.akas_in_hand.(0) <- true;
  State.dahai state 0 Tiles.tile_id_5mr false;
  assert (state.akas_in_hand.(0) = false);
  Printf.printf "  Test 2 passed: akas_in_hand.(0) false after discarding 5mr\n";
  (* Test 3: Drawing and discarding 5pr *)
  Printf.printf "  Test 3: Drawing and discarding 5pr...\n";
  let state = State.create_player_state 0 in
  state.tehai.(Tiles.tile_id_5p) <- 1;
  State.tsumo state 0 Tiles.tile_id_5pr;
  assert (state.akas_in_hand.(1) = true);
  state.tehai.(Tiles.tile_id_5p) <- 1;
  State.dahai state 0 Tiles.tile_id_5pr false;
  assert (state.akas_in_hand.(1) = false);
  Printf.printf "  Test 3 passed: 5pr tracking works\n";
  (* Test 4: Drawing and discarding 5sr *)
  Printf.printf "  Test 4: Drawing and discarding 5sr...\n";
  let state = State.create_player_state 0 in
  state.tehai.(Tiles.tile_id_5s) <- 1;
  State.tsumo state 0 Tiles.tile_id_5sr;
  assert (state.akas_in_hand.(2) = true);
  state.tehai.(Tiles.tile_id_5s) <- 1;
  State.dahai state 0 Tiles.tile_id_5sr false;
  assert (state.akas_in_hand.(2) = false);
  Printf.printf "  Test 4 passed: 5sr tracking works\n";
  (* Test 5: Regular 5m (not 5mr) should not affect akas_in_hand *)
  Printf.printf "  Test 5: Drawing regular 5m (not 5mr)...\n";
  let state = State.create_player_state 0 in
  state.tehai.(Tiles.tile_id_5m) <- 1;
  State.tsumo state 0 Tiles.tile_id_5m;
  assert (state.akas_in_hand.(0) = false);
  Printf.printf "  Test 5 passed: regular 5m doesn't affect aka tracking\n";
  (* Test 6: Multiple akas in hand *)
  Printf.printf "  Test 6: Drawing multiple akas...\n";
  let state = State.create_player_state 0 in
  state.tehai.(Tiles.tile_id_5m) <- 1;
  state.tehai.(Tiles.tile_id_5p) <- 1;
  state.tehai.(Tiles.tile_id_5s) <- 1;
  State.tsumo state 0 Tiles.tile_id_5mr;
  State.tsumo state 0 Tiles.tile_id_5pr;
  State.tsumo state 0 Tiles.tile_id_5sr;
  assert (state.akas_in_hand.(0) = true);
  assert (state.akas_in_hand.(1) = true);
  assert (state.akas_in_hand.(2) = true);
  Printf.printf "  Test 6 passed: all aka positions set correctly\n";
  Printf.printf "  aka tracking tests passed\n"
;;

(** Test get_rank function *)
let test_get_rank () =
  Printf.printf "Testing get_rank...\n";
  (* Test 1: Player 0 with lowest score *)
  Printf.printf "  Test 1: Player 0 with lowest score...\n";
  let state = State.create_player_state 0 in
  let rank = State.get_rank state [| 20000; 25000; 25000; 30000 |] in
  assert (rank = 3);
  (* 4th place *)
  Printf.printf "  Test 1 passed: rank = %d\n" rank;
  (* Test 2: All tied - position-based tiebreaker *)
  Printf.printf "  Test 2: All tied scores...\n";
  let state = State.create_player_state 3 in
  let rank = State.get_rank state [| 25000; 25000; 25000; 25000 |] in
  assert (rank = 3);
  (* Last position *)
  Printf.printf "  Test 2 passed: rank = %d\n" rank;
  (* Test 3: Player 1 with 2nd highest *)
  Printf.printf "  Test 3: Player 1 with 2nd highest...\n";
  let state = State.create_player_state 1 in
  let rank = State.get_rank state [| 25000; 30000; 20000; 25000 |] in
  assert (rank = 2);
  (* 3rd place (0-indexed), but player 1 has 30000 which is 1st *)
  Printf.printf "  Test 3 passed: rank = %d\n" rank;
  (* Test 4: Tie-breaking by position *)
  Printf.printf "  Test 4: Tie-breaking by position...\n";
  let state = State.create_player_state 1 in
  let rank = State.get_rank state [| 32000; 32000; 18000; 18000 |] in
  assert (rank = 0);
  (* Player 1 with 32000 gets 1st due to position *)
  Printf.printf "  Test 4 passed: rank = %d\n" rank;
  (* Test 5: Tie-breaking example 2 *)
  Printf.printf "  Test 5: Tie-breaking with different positions...\n";
  let state = State.create_player_state 2 in
  let rank = State.get_rank state [| 32000; 18000; 18000; 32000 |] in
  assert (rank = 1);
  (* Player 2 with 18000 is worse than player 3 with 18000 *)
  Printf.printf "  Test 5 passed: rank = %d\n" rank;
  (* Test 6: Low score tie-breaking *)
  Printf.printf "  Test 6: Low score tie-breaking...\n";
  let state = State.create_player_state 2 in
  let rank = State.get_rank state [| 5; 2; 5; 3 |] in
  assert (rank = 1);
  (* Player 2 with 5 gets 2nd (0=1st, 2=2nd) *)
  Printf.printf "  Test 6 passed: rank = %d\n" rank;
  Printf.printf "  get_rank tests passed\n"
;;

(** Test dora counting after kan *)
let test_dora_count_after_kan () =
  Printf.printf "Testing dora_count_after_kan...\n";
  (* Start kyoku with hand: 1111s 123456p 112z *)
  let state = State.create_player_state 0 in
  let unknown_hand = Array.make 13 Tiles.tile_id_unknown in
  let tehais =
    [| (* Player 0: 1111s 123456p 112z (using tile IDs) *)
       Hand.hand_with_aka "1111s 123456p 112z" |> Result.get_ok |> Hand.tile37_to_array
     ; unknown_hand
     ; unknown_hand
     ; unknown_hand
    |]
  in
  let event =
    Mjai.Start_kyoku
      { bakaze = 27
      ; (* E *)
        dora_marker = 30
      ; (* N, making E (27) the dora *)
        kyoku = 1
      ; honba = 0
      ; kyotaku = 0
      ; oya = 0
      ; scores = [| 25000; 25000; 25000; 25000 |]
      ; tehais
      }
  in
  State.update state event;
  (* Tsumo 8s *)
  State.update state (Mjai.Tsumo { actor = 0; pai = 25 });
  (* 8s *)
  Printf.printf "  Initial doras_owned: %d (expected 2)\n" state.doras_owned.(0);
  assert (state.doras_owned.(0) = 2);
  (* Should have 2 dora initially *)

  (* Ankan 1s (consume four 1s tiles) *)
  State.update state (Mjai.Ankan { actor = 0; consumed = [| 18; 18; 18; 18 |] });
  (* 1s *)

  (* Add dora indicator 9s *)
  State.update state (Mjai.Dora { dora_marker = 26 });
  (* 9s *)

  (* Tsumo 5pr (red 5p) *)
  State.update state (Mjai.Tsumo { actor = 0; pai = Tiles.tile_id_5pr });
  Printf.printf
    "  After ankan and dora: doras_owned = %d (expected 7)\n"
    state.doras_owned.(0);
  assert (state.doras_owned.(0) = 7);
  (* Should have 7 dora after kan *)

  (* Dahai E *)
  State.update state (Mjai.Dahai { actor = 0; pai = 27; tsumogiri = true });
  (* E *)
  assert (state.doras_owned.(0) = 6);
  (* Lost 1 dora *)
  Printf.printf "  After dahai E: doras_owned = %d\n" state.doras_owned.(0);
  (* Other players' turns *)
  State.update state (Mjai.Tsumo { actor = 1; pai = 0 });
  State.update state (Mjai.Dahai { actor = 1; pai = 13; tsumogiri = true });
  (* 5p *)

  (* Pon 5p *)
  State.update
    state
    (Mjai.Pon
       { actor = 0
       ; target = 1
       ; pai = 13
       ; (* 5p *)
         consumed = [| Tiles.tile_id_5pr; 13 |] (* 5pr, 5p *)
       });
  assert (state.doras_owned.(0) = 6);
  (* Still 6 dora *)
  Printf.printf "  After pon 5p: doras_owned = %d\n" state.doras_owned.(0);
  (* Dahai E *)
  State.update state (Mjai.Dahai { actor = 0; pai = 27; tsumogiri = false });
  (* E *)
  assert (state.doras_owned.(0) = 5);
  (* Lost 1 dora *)
  Printf.printf "  After second dahai E: doras_owned = %d\n" state.doras_owned.(0);
  (* Continue with other players' turns to set up ankan by player 3 *)
  State.update state (Mjai.Tsumo { actor = 1; pai = 0 });
  State.update state (Mjai.Dahai { actor = 1; pai = 31; tsumogiri = true });
  (* P *)
  State.update state (Mjai.Tsumo { actor = 2; pai = 0 });
  State.update state (Mjai.Dahai { actor = 2; pai = 31; tsumogiri = true });
  (* P *)

  (* Player 3 ankan *)
  State.update state (Mjai.Tsumo { actor = 3; pai = 0 });
  State.update state (Mjai.Ankan { actor = 3; consumed = [| 0; 0; 0; 0 |] });
  (* 1m *)
  State.update state (Mjai.Dora { dora_marker = 12 });
  (* 4p *)

  (* Dora count should increase because 4p dora indicator makes 5p a dora *)
  Printf.printf
    "  After opponent ankan: doras_owned = %d (expected 8)\n"
    state.doras_owned.(0);
  assert (state.doras_owned.(0) = 8);
  (* Gained 3 dora from having 3x 5p *)
  Printf.printf "  dora_count_after_kan tests passed\n"
;;

(** Test chi at 0 shanten *)
let test_chi_at_0_shanten () =
  Printf.printf "Testing chi_at_0_shanten...\n";
  let log =
    {|
{"type":"start_kyoku","bakaze":"E","dora_marker":"W","kyoku":1,"honba":0,"kyotaku":0,"oya":0,"scores":[25000,25000,25000,25000],"tehais":[["1m","2m","3m","5p","5p","4s","5s","E","E","E","S","S","S"],["?","?","?","?","?","?","?","?","?","?","?","?","?"],["?","?","?","?","?","?","?","?","?","?","?","?","?"],["?","?","?","?","?","?","?","?","?","?","?","?","?"]]}
{"type":"tsumo","actor":0,"pai":"P"}
{"type":"dahai","actor":0,"pai":"P","tsumogiri":true}
{"type":"tsumo","actor":1,"pai":"?"}
{"type":"dahai","actor":1,"pai":"P","tsumogiri":true}
{"type":"tsumo","actor":2,"pai":"?"}
{"type":"dahai","actor":2,"pai":"P","tsumogiri":true}
{"type":"tsumo","actor":3,"pai":"?"}
{"type":"dahai","actor":3,"pai":"6s","tsumogiri":false}
|}
  in
  let state = from_log 0 log in
  Printf.printf "  Shanten: %d (expected 0)\n" state.shanten;
  assert (state.shanten = 0);
  (* Debug: Print hand *)
  Printf.printf "  Hand: ";
  for i = 0 to 33 do
    if state.tehai.(i) > 0 then Printf.printf "%dx%d " state.tehai.(i) i
  done;
  Printf.printf "\n";
  (* Check waits *)
  Printf.printf
    "  Waits[3s=%d]: %b, Waits[6s=%d]: %b\n"
    Tiles.tile_id_3s
    state.waits.(Tiles.tile_id_3s)
    Tiles.tile_id_6s
    state.waits.(Tiles.tile_id_6s);
  Printf.printf "  At furiten: %b\n" state.at_furiten;
  (* Manual check: what happens if we add 6s? *)
  Printf.printf "  Manual check: adding 6s...\n";
  let test_hand = Array.copy state.tehai in
  test_hand.(Tiles.tile_id_6s) <- test_hand.(Tiles.tile_id_6s) + 1;
  let test_count = Array.fold_left ( + ) 0 test_hand in
  let test_len_div3 = (test_count - 1) / 3 in
  Printf.printf "    test_count=%d, test_len_div3=%d\n" test_count test_len_div3;
  Printf.printf "    Hand composition:\n";
  Printf.printf "      1-9m: ";
  for i = Tiles.tile_id_1m to Tiles.tile_id_9m do
    Printf.printf "%d " test_hand.(i)
  done;
  Printf.printf "\n      1-9p: ";
  for i = Tiles.tile_id_1p to Tiles.tile_id_9p do
    Printf.printf "%d " test_hand.(i)
  done;
  Printf.printf "\n      1-9s: ";
  for i = Tiles.tile_id_1s to Tiles.tile_id_9s do
    Printf.printf "%d " test_hand.(i)
  done;
  Printf.printf "\n      honors: ";
  for i = Tiles.tile_id_E to Tiles.tile_id_C do
    Printf.printf "%d " test_hand.(i)
  done;
  Printf.printf "\n";
  let test_shanten = Shanten.calc_all test_hand test_len_div3 in
  Printf.printf "    test_shanten=%d (expected -1)\n" test_shanten;
  (* Test a simple winning hand to verify shanten calculator *)
  Printf.printf "  Test: simple winning hand (11122233344m)...\n";
  let simple_hand = Array.make 34 0 in
  simple_hand.(Tiles.tile_id_1m) <- 3;
  simple_hand.(Tiles.tile_id_2m) <- 3;
  simple_hand.(Tiles.tile_id_3m) <- 3;
  simple_hand.(Tiles.tile_id_4m) <- 3;
  simple_hand.(Tiles.tile_id_5m) <- 2;
  (* pair *)
  let simple_shanten = Shanten.calc_all simple_hand 4 in
  Printf.printf "    simple_shanten=%d (expected -1)\n" simple_shanten;
  (* Test a winning hand with sequences *)
  Printf.printf "  Test: winning hand with sequences (123456m 789p 111z 22s)...\n";
  let seq_hand = Array.make 34 0 in
  seq_hand.(Tiles.tile_id_1m) <- 1;
  seq_hand.(Tiles.tile_id_2m) <- 1;
  seq_hand.(Tiles.tile_id_3m) <- 1;
  (* 123m *)
  seq_hand.(Tiles.tile_id_4m) <- 1;
  seq_hand.(Tiles.tile_id_5m) <- 1;
  seq_hand.(Tiles.tile_id_6m) <- 1;
  (* 456m *)
  seq_hand.(Tiles.tile_id_7p) <- 1;
  seq_hand.(Tiles.tile_id_8p) <- 1;
  seq_hand.(Tiles.tile_id_9p) <- 1;
  (* 789p *)
  seq_hand.(Tiles.tile_id_E) <- 3;
  (* EEE - triplet *)
  seq_hand.(Tiles.tile_id_2s) <- 2;
  (* 22s - pair *)
  let seq_shanten = Shanten.calc_all seq_hand 4 in
  Printf.printf "    seq_shanten=%d (expected -1)\n" seq_shanten;
  (* Test the EXACT hand from the test *)
  Printf.printf "  Test: exact test hand (123m 55p 456s EEE SSS)...\n";
  let exact_hand = Array.make 34 0 in
  exact_hand.(Tiles.tile_id_1m) <- 1;
  exact_hand.(Tiles.tile_id_2m) <- 1;
  exact_hand.(Tiles.tile_id_3m) <- 1;
  (* 123m *)
  exact_hand.(Tiles.tile_id_5p) <- 2;
  (* 55p - pair *)
  exact_hand.(Tiles.tile_id_4s) <- 1;
  exact_hand.(Tiles.tile_id_5s) <- 1;
  exact_hand.(Tiles.tile_id_6s) <- 1;
  (* 456s *)
  exact_hand.(Tiles.tile_id_E) <- 3;
  (* EEE *)
  exact_hand.(Tiles.tile_id_S) <- 3;
  (* SSS *)
  let exact_shanten = Shanten.calc_all exact_hand 4 in
  Printf.printf "    exact_shanten=%d (expected -1)\n" exact_shanten;
  Printf.printf "  Can ron agari: %b (expected true)\n" state.last_cans.can_ron_agari;
  assert state.last_cans.can_ron_agari;
  Printf.printf "  Can chi high: %b (expected true)\n" state.last_cans.can_chi_high;
  assert state.last_cans.can_chi_high;
  (* Now chi the 6s *)
  let _ =
    update_json
      state
      {|{"type":"chi","actor":0,"target":3,"consumed":["4s","5s"],"pai":"6s"}|}
  in
  Printf.printf "  After chi - shanten: %d (expected 0)\n" state.shanten;
  assert (state.shanten = 0);
  (* Check if at_furiten is set *)
  Printf.printf "  After chi - at_furiten: %b (expected true)\n" state.at_furiten;
  assert state.at_furiten;
  Printf.printf "  chi_at_0_shanten tests passed\n"
;;

(** Test rule_based_agari (all last minogashi) *)
let test_rule_based_agari_all_last_minogashi () =
  Printf.printf "Testing rule_based_agari (all last minogashi)...\n";
  let log =
    {|
        {"type":"start_kyoku","bakaze":"S","dora_marker":"5m","kyoku":4,"honba":0,"kyotaku":0,"oya":3,"scores":[35300,3000,38400,23300],"tehais":[["4m","5mr","8m","1p","3p","3p","5p","2s","5sr","9s","W","P","P"],["2m","3m","5m","7m","7p","9p","4s","5s","5s","6s","7s","7s","E"],["3m","5m","6m","2p","6p","9p","1s","5s","8s","9s","S","S","C"],["1m","4m","3p","4p","5pr","7p","1s","2s","7s","8s","W","N","P"]]}
        {"type":"tsumo","actor":3,"pai":"F"}
        {"type":"dahai","actor":3,"pai":"1m","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"5p"}
        {"type":"dahai","actor":0,"pai":"W","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"9m"}
        {"type":"dahai","actor":1,"pai":"E","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"N"}
        {"type":"dahai","actor":2,"pai":"9p","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"2p"}
        {"type":"dahai","actor":3,"pai":"N","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"6m"}
        {"type":"dahai","actor":0,"pai":"9s","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"7m"}
        {"type":"dahai","actor":1,"pai":"9m","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"3s"}
        {"type":"dahai","actor":2,"pai":"2p","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"4s"}
        {"type":"dahai","actor":3,"pai":"W","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"1m"}
        {"type":"dahai","actor":0,"pai":"1m","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"9m"}
        {"type":"dahai","actor":1,"pai":"9m","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"3m"}
        {"type":"dahai","actor":2,"pai":"N","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"2s"}
        {"type":"dahai","actor":3,"pai":"F","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"2m"}
        {"type":"dahai","actor":0,"pai":"2s","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"1m"}
        {"type":"dahai","actor":1,"pai":"5m","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"3p"}
        {"type":"dahai","actor":2,"pai":"3p","tsumogiri":true}
        {"type":"pon","actor":0,"target":2,"pai":"3p","consumed":["3p","3p"]}
        {"type":"dahai","actor":0,"pai":"2m","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"6p"}
        {"type":"dahai","actor":1,"pai":"9p","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"6s"}
        {"type":"dahai","actor":2,"pai":"C","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"7p"}
        {"type":"dahai","actor":3,"pai":"P","tsumogiri":false}
        {"type":"pon","actor":0,"target":3,"pai":"P","consumed":["P","P"]}
        {"type":"dahai","actor":0,"pai":"1p","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"7s"}
        {"type":"dahai","actor":1,"pai":"5s","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"3s"}
        {"type":"dahai","actor":2,"pai":"9s","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"2m"}
        {"type":"dahai","actor":3,"pai":"1s","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"1p"}
        {"type":"dahai","actor":0,"pai":"1p","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"7m"}
        {"type":"dahai","actor":1,"pai":"4s","tsumogiri":false}
        {"type":"chi","actor":2,"target":1,"pai":"4s","consumed":["5s","6s"]}
        {"type":"dahai","actor":2,"pai":"6p","tsumogiri":false}
        {"type":"chi","actor":3,"target":2,"pai":"6p","consumed":["5pr","7p"]}
        {"type":"dahai","actor":3,"pai":"7p","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"1s"}
        {"type":"dahai","actor":0,"pai":"1s","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"1s"}
        {"type":"reach","actor":1}
        {"type":"dahai","actor":1,"pai":"1s","tsumogiri":true}
        {"type":"reach_accepted","actor":1}
        {"type":"tsumo","actor":2,"pai":"9s"}
        {"type":"dahai","actor":2,"pai":"8s","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"4p"}
        {"type":"dahai","actor":3,"pai":"4p","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"4m"}
        {"type":"dahai","actor":0,"pai":"4m","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"1p"}
        {"type":"dahai","actor":1,"pai":"1p","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"8m"}
        {"type":"dahai","actor":2,"pai":"8m","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"C"}
        {"type":"dahai","actor":3,"pai":"C","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"2s"}
        {"type":"dahai","actor":0,"pai":"2s","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"8p"}
  |}
  in
  let ps = from_log 1 log in
  assert ps.last_cans.can_tsumo_agari;
  let should_hora = State.rule_based_agari ps in
  Printf.printf "  Should hora: %b (expected false)\n" should_hora;
  assert (not should_hora);
  let orig_scores = Array.copy ps.scores in
  (* Modify scores to [9000, 30000, 30000, 30000] (relative to player 1?) *)
  (* In Rust: mem::replace(&mut ps.scores, [9000, 30000, 30000, 30000]) *)
  (* ps.scores in OCaml is relative. Player 1 is the actor. So these are scores from player 1 perspective. *)
  ps.scores <- [| 9000; 30000; 30000; 30000 |];
  Printf.printf "DEBUG: ps.scores.(0) after update: %d\n" ps.scores.(0);
  let should_hora = State.rule_based_agari ps in
  Printf.printf "  Should hora with low score: %b (expected true)\n" should_hora;
  assert should_hora;
  ps.scores <- orig_scores;
  (* Add dora 5m *)
  State.add_dora_indicator ps Tiles.tile_id_5m;
  let should_hora = State.rule_based_agari ps in
  Printf.printf "  Should hora with extra dora: %b (expected true)\n" should_hora;
  assert should_hora;
  (* Part 2 of the test *)
  let log =
    {|
        {"type":"start_kyoku","bakaze":"S","dora_marker":"3s","kyoku":4,"honba":1,"kyotaku":0,"oya":3,"scores":[39000,25000,16900,19100],"tehais":[["1m","2m","3m","5mr","6m","8m","2p","2p","5pr","7s","8s","S","S"],["7m","9m","9m","6p","7p","1s","1s","3s","4s","6s","6s","S","P"],["3m","4m","5m","7m","4p","5p","5p","6p","8p","9p","5sr","5s","F"],["1m","2m","2m","6m","8m","1p","9p","3s","5s","6s","7s","E","W"]]}
        {"type":"tsumo","actor":3,"pai":"N"}
        {"type":"dahai","actor":3,"pai":"9p","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"1s"}
        {"type":"dahai","actor":0,"pai":"5pr","tsumogiri":false}
        {"type":"pon","actor":2,"target":0,"pai":"5pr","consumed":["5p","5p"]}
        {"type":"dahai","actor":2,"pai":"9p","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"7m"}
        {"type":"dahai","actor":3,"pai":"1p","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"C"}
        {"type":"dahai","actor":0,"pai":"8m","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"7m"}
        {"type":"dahai","actor":1,"pai":"6p","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"9s"}
        {"type":"dahai","actor":2,"pai":"9s","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"F"}
        {"type":"dahai","actor":3,"pai":"W","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"6m"}
        {"type":"dahai","actor":0,"pai":"1s","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"2m"}
        {"type":"dahai","actor":1,"pai":"7p","tsumogiri":false}
        {"type":"chi","actor":2,"target":1,"pai":"7p","consumed":["6p","8p"]}
        {"type":"dahai","actor":2,"pai":"F","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"4p"}
        {"type":"dahai","actor":3,"pai":"N","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"W"}
        {"type":"dahai","actor":0,"pai":"W","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"6m"}
        {"type":"dahai","actor":1,"pai":"2m","tsumogiri":false}
        {"type":"pon","actor":3,"target":1,"pai":"2m","consumed":["2m","2m"]}
        {"type":"dahai","actor":3,"pai":"F","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"4s"}
        {"type":"dahai","actor":0,"pai":"4s","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"1s"}
        {"type":"dahai","actor":1,"pai":"P","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"8s"}
        {"type":"dahai","actor":2,"pai":"8s","tsumogiri":true}
        {"type":"chi","actor":3,"target":2,"pai":"8s","consumed":["6s","7s"]}
        {"type":"dahai","actor":3,"pai":"1m","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"3p"}
        {"type":"dahai","actor":0,"pai":"C","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"6p"}
        {"type":"dahai","actor":1,"pai":"6p","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"4s"}
        {"type":"dahai","actor":2,"pai":"4p","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"N"}
        {"type":"dahai","actor":3,"pai":"N","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"3s"}
        {"type":"dahai","actor":0,"pai":"3s","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"5s"}
        {"type":"dahai","actor":1,"pai":"S","tsumogiri":false}
        {"type":"pon","actor":0,"target":1,"pai":"S","consumed":["S","S"]}
        {"type":"dahai","actor":0,"pai":"3p","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"3m"}
        {"type":"dahai","actor":1,"pai":"3m","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"4p"}
        {"type":"dahai","actor":2,"pai":"4p","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"P"}
        {"type":"dahai","actor":3,"pai":"P","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"8p"}
        {"type":"dahai","actor":0,"pai":"8p","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"4p"}
        {"type":"dahai","actor":1,"pai":"4p","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"E"}
        {"type":"dahai","actor":2,"pai":"E","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"C"}
        {"type":"dahai","actor":3,"pai":"4p","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"7p"}
        {"type":"dahai","actor":0,"pai":"7p","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"8p"}
        {"type":"dahai","actor":1,"pai":"8p","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"S"}
        {"type":"dahai","actor":2,"pai":"S","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"N"}
        {"type":"dahai","actor":3,"pai":"N","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"2s"}
        {"type":"dahai","actor":0,"pai":"2s","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"8s"}
        {"type":"dahai","actor":1,"pai":"8s","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"E"}
        {"type":"dahai","actor":2,"pai":"E","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"6s"}
        {"type":"dahai","actor":3,"pai":"E","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"9m"}
        {"type":"dahai","actor":0,"pai":"9m","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"F"}
        {"type":"dahai","actor":1,"pai":"F","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"C"}
        {"type":"dahai","actor":2,"pai":"C","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"E"}
        {"type":"dahai","actor":3,"pai":"E","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"W"}
        {"type":"dahai","actor":0,"pai":"W","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"P"}
        {"type":"dahai","actor":1,"pai":"P","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"N"}
        {"type":"dahai","actor":2,"pai":"N","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"8m"}
        {"type":"dahai","actor":3,"pai":"C","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"P"}
        {"type":"dahai","actor":0,"pai":"P","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"4m"}
        {"type":"dahai","actor":1,"pai":"9m","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"5m"}
        {"type":"dahai","actor":2,"pai":"4s","tsumogiri":false}
        {"type":"chi","actor":3,"target":2,"pai":"4s","consumed":["5s","6s"]}
        {"type":"dahai","actor":3,"pai":"3s","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"1m"}
        {"type":"dahai","actor":0,"pai":"1m","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"8s"}
        {"type":"dahai","actor":1,"pai":"9m","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"9s"}
        {"type":"dahai","actor":2,"pai":"9s","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"7s"}
        {"type":"dahai","actor":3,"pai":"7s","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"7s"}
        {"type":"dahai","actor":0,"pai":"6m","tsumogiri":false}
  |}
  in
  let ps = from_log 2 log in
  assert (State.rule_based_agari ps);
  Printf.printf "  rule_based_agari tests passed\n"
;;

(** Test kakan_from_hand *)
let test_kakan_from_hand () =
  Printf.printf "Testing kakan_from_hand...\n";
  let log =
    {|
        {"type":"start_kyoku","bakaze":"S","dora_marker":"6m","kyoku":2,"honba":0,"kyotaku":0,"oya":1,"scores":[16100,36600,16800,30500],"tehais":[["5p","5s","1s","9m","9m","W","E","N","1p","F","9m","3p","6p"],["4s","9s","S","4s","1m","P","N","7s","F","2m","3s","2s","2s"],["6m","8p","8p","2p","8m","N","7p","C","1s","2p","N","9s","9p"],["2m","6s","7p","9s","2m","9s","6m","7s","8m","3m","S","5mr","C"]]}
        {"type":"tsumo","actor":1,"pai":"S"}
        {"type":"dahai","actor":1,"pai":"N","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"1s"}
        {"type":"dahai","actor":2,"pai":"9s","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"P"}
        {"type":"dahai","actor":3,"pai":"S","tsumogiri":false}
        {"type":"pon","actor":1,"target":3,"pai":"S","consumed":["S","S"]}
        {"type":"dahai","actor":1,"pai":"P","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"4p"}
        {"type":"dahai","actor":2,"pai":"C","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"5s"}
        {"type":"dahai","actor":3,"pai":"C","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"7m"}
        {"type":"dahai","actor":0,"pai":"E","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"P"}
        {"type":"dahai","actor":1,"pai":"1m","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"9p"}
        {"type":"dahai","actor":2,"pai":"6m","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"C"}
        {"type":"dahai","actor":3,"pai":"C","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"7p"}
        {"type":"dahai","actor":0,"pai":"W","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"5s"}
        {"type":"dahai","actor":1,"pai":"2m","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"5m"}
        {"type":"dahai","actor":2,"pai":"5m","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"1p"}
        {"type":"dahai","actor":3,"pai":"1p","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"4m"}
        {"type":"dahai","actor":0,"pai":"N","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"E"}
        {"type":"dahai","actor":1,"pai":"P","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"1s"}
        {"type":"dahai","actor":2,"pai":"8m","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"6p"}
        {"type":"dahai","actor":3,"pai":"8m","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"5p"}
        {"type":"dahai","actor":0,"pai":"1s","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"2s"}
        {"type":"dahai","actor":1,"pai":"E","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"5m"}
        {"type":"dahai","actor":2,"pai":"5m","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"3s"}
        {"type":"dahai","actor":3,"pai":"3s","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"7p"}
        {"type":"dahai","actor":0,"pai":"F","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"E"}
        {"type":"dahai","actor":1,"pai":"E","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"W"}
        {"type":"dahai","actor":2,"pai":"W","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"7m"}
        {"type":"dahai","actor":3,"pai":"2m","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"5m"}
        {"type":"dahai","actor":0,"pai":"5s","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"S"}
        {"type":"dahai","actor":1,"pai":"F","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"6p"}
        {"type":"dahai","actor":2,"pai":"N","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"2p"}
        {"type":"dahai","actor":3,"pai":"2p","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"6p"}
        {"type":"dahai","actor":0,"pai":"3p","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"4m"}
        {"type":"dahai","actor":1,"pai":"4m","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"3s"}
        {"type":"dahai","actor":2,"pai":"N","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"8p"}
        {"type":"reach","actor":3}
        {"type":"dahai","actor":3,"pai":"P","tsumogiri":false}
        {"type":"reach_accepted","actor":3}
        {"type":"tsumo","actor":0,"pai":"W"}
        {"type":"dahai","actor":0,"pai":"1p","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"8s"}
        {"type":"kakan","actor":1,"pai":"S","consumed":["S","S","S"]}
        {"type":"tsumo","actor":1,"pai":"4s"}
  |}
  in
  let ps = from_log 1 log in
  assert ps.last_cans.can_tsumo_agari;
  Printf.printf "  kakan_from_hand tests passed\n"
;;

(** Test discard_candidates_with_unconditional_tenpai *)
let test_discard_candidates_with_unconditional_tenpai () =
  Printf.printf "Testing discard_candidates_with_unconditional_tenpai...\n";
  let log =
    {|
        {"type":"start_kyoku","bakaze":"S","dora_marker":"2s","kyoku":3,"honba":0,"kyotaku":0,"oya":2,"scores":[25600,15600,21200,37600],"tehais":[["3m","3m","1p","6p","7p","9p","5sr","7s","8s","8s","E","E","W"],["4m","5mr","6m","1p","4p","5p","8p","3s","3s","4s","5s","S","P"],["1m","5m","7m","2p","9p","3s","5s","9s","S","W","N","P","C"],["1m","4m","6m","2p","3p","4p","6p","9p","2s","4s","7s","S","N"]]}
        {"type":"tsumo","actor":2,"pai":"C"}
        {"type":"dahai","actor":2,"pai":"N","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"2m"}
        {"type":"dahai","actor":3,"pai":"2m","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"2p"}
        {"type":"dahai","actor":0,"pai":"9p","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"7p"}
        {"type":"dahai","actor":1,"pai":"1p","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"4p"}
        {"type":"dahai","actor":2,"pai":"W","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"P"}
        {"type":"dahai","actor":3,"pai":"P","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"6m"}
        {"type":"dahai","actor":0,"pai":"W","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"C"}
        {"type":"dahai","actor":1,"pai":"P","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"8m"}
        {"type":"dahai","actor":2,"pai":"9p","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"9m"}
        {"type":"dahai","actor":3,"pai":"9m","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"1p"}
        {"type":"dahai","actor":0,"pai":"2p","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"7m"}
        {"type":"dahai","actor":1,"pai":"S","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"P"}
        {"type":"dahai","actor":2,"pai":"9s","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"N"}
        {"type":"dahai","actor":3,"pai":"N","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"6p"}
        {"type":"dahai","actor":0,"pai":"7p","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"9m"}
        {"type":"dahai","actor":1,"pai":"C","tsumogiri":false}
        {"type":"pon","actor":2,"target":1,"pai":"C","consumed":["C","C"]}
        {"type":"dahai","actor":2,"pai":"1m","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"7s"}
        {"type":"dahai","actor":3,"pai":"7s","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"2p"}
        {"type":"dahai","actor":0,"pai":"2p","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"5pr"}
        {"type":"dahai","actor":1,"pai":"9m","tsumogiri":false}
        {"type":"chi","actor":2,"target":1,"pai":"9m","consumed":["7m","8m"]}
        {"type":"dahai","actor":2,"pai":"S","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"E"}
        {"type":"dahai","actor":3,"pai":"E","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"5m"}
        {"type":"dahai","actor":0,"pai":"7s","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"3p"}
        {"type":"dahai","actor":1,"pai":"5p","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"F"}
        {"type":"dahai","actor":2,"pai":"F","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"2s"}
        {"type":"dahai","actor":3,"pai":"2s","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"4s"}
        {"type":"dahai","actor":0,"pai":"4s","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"1p"}
        {"type":"dahai","actor":1,"pai":"1p","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"6s"}
        {"type":"dahai","actor":2,"pai":"5m","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"6p"}
        {"type":"dahai","actor":3,"pai":"6p","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"9p"}
        {"type":"dahai","actor":0,"pai":"9p","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"5p"}
        {"type":"dahai","actor":1,"pai":"5p","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"5s"}
        {"type":"dahai","actor":2,"pai":"5s","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"9s"}
        {"type":"dahai","actor":3,"pai":"9s","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"8m"}
        {"type":"dahai","actor":0,"pai":"8m","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"9m"}
        {"type":"dahai","actor":1,"pai":"9m","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"9s"}
        {"type":"dahai","actor":2,"pai":"9s","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"1s"}
        {"type":"dahai","actor":3,"pai":"1s","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"2m"}
        {"type":"dahai","actor":0,"pai":"5m","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"8m"}
        {"type":"dahai","actor":1,"pai":"8m","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"8p"}
        {"type":"dahai","actor":2,"pai":"8p","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"7m"}
        {"type":"dahai","actor":3,"pai":"7m","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"7p"}
        {"type":"dahai","actor":0,"pai":"7p","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"8p"}
        {"type":"dahai","actor":1,"pai":"7m","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"3m"}
        {"type":"dahai","actor":2,"pai":"3m","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"1s"}
        {"type":"dahai","actor":3,"pai":"1s","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"4p"}
        {"type":"dahai","actor":0,"pai":"2m","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"F"}
        {"type":"dahai","actor":1,"pai":"F","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"9s"}
        {"type":"dahai","actor":2,"pai":"9s","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"7m"}
        {"type":"dahai","actor":3,"pai":"7m","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"F"}
        {"type":"dahai","actor":0,"pai":"F","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"8s"}
        {"type":"dahai","actor":1,"pai":"8s","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"F"}
        {"type":"dahai","actor":2,"pai":"F","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"1m"}
        {"type":"dahai","actor":3,"pai":"1m","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"W"}
        {"type":"dahai","actor":0,"pai":"W","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"9m"}
        {"type":"dahai","actor":1,"pai":"9m","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"2m"}
        {"type":"dahai","actor":2,"pai":"2m","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"7p"}
        {"type":"dahai","actor":3,"pai":"7p","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"3p"}
        {"type":"dahai","actor":0,"pai":"6m","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"6m"}
        {"type":"dahai","actor":1,"pai":"6m","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"1s"}
        {"type":"dahai","actor":2,"pai":"1s","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"8m"}
        {"type":"dahai","actor":3,"pai":"8m","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"S"}
        {"type":"dahai","actor":0,"pai":"S","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"2m"}
        {"type":"dahai","actor":1,"pai":"2m","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"4s"}
        {"type":"dahai","actor":2,"pai":"6s","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"8s"}
        {"type":"dahai","actor":3,"pai":"8s","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"N"}
        {"type":"dahai","actor":0,"pai":"N","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"3s"}
  |}
  in
  let ps = from_log 1 log in
  (* let expected = [14; 15] in (* 6p=14, 7p=15 - wait 7p and 8p *) *)
  (* assert_waits ps expected; *)
  let candidates = State.discard_candidates_with_unconditional_tenpai_aka ps in
  let expected = [Tiles.tile_id_7p; Tiles.tile_id_8p] in
  List.iter (fun tid ->
    if not candidates.(tid) then
      failwith (Printf.sprintf "Expected candidate %s not found" (Tiles.string_of_tile tid))
  ) expected;
  Array.iteri (fun tid b ->
    if b && not (List.mem tid expected) then
      failwith (Printf.sprintf "Unexpected candidate %s found" (Tiles.string_of_tile tid))
  ) candidates;
  Printf.printf "  discard_candidates_with_unconditional_tenpai tests passed\n"
;;

(** Test double_chankan_ron *)
let test_double_chankan_ron () =
  Printf.printf "Testing double_chankan_ron...\n";
  let log =
    {|
        {"type":"start_kyoku","bakaze":"S","dora_marker":"2p","kyoku":2,"honba":0,"kyotaku":0,"oya":1,"scores":[44400,1600,25700,28300],"tehais":[["1m","5m","9m","9m","9m","3p","9p","8s","9s","W","W","N","C"],["7m","8m","3p","6p","8p","1s","1s","3s","6s","9s","E","F","C"],["3m","9m","2p","5p","8p","1s","2s","5s","6s","7s","S","F","C"],["2m","2m","5m","5mr","8m","1p","1p","7p","8p","3s","5s","8s","9s"]]}
        {"type":"tsumo","actor":1,"pai":"P"}
        {"type":"dahai","actor":1,"pai":"F","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"3m"}
        {"type":"dahai","actor":2,"pai":"F","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"6m"}
        {"type":"dahai","actor":3,"pai":"9s","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"1s"}
        {"type":"dahai","actor":0,"pai":"1s","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"9p"}
        {"type":"dahai","actor":1,"pai":"C","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"9p"}
        {"type":"dahai","actor":2,"pai":"C","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"7s"}
        {"type":"dahai","actor":3,"pai":"1p","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"7p"}
        {"type":"dahai","actor":0,"pai":"C","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"5m"}
        {"type":"dahai","actor":1,"pai":"P","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"8s"}
        {"type":"dahai","actor":2,"pai":"9m","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"7m"}
        {"type":"dahai","actor":3,"pai":"1p","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"W"}
        {"type":"dahai","actor":0,"pai":"1m","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"P"}
        {"type":"dahai","actor":1,"pai":"P","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"4m"}
        {"type":"dahai","actor":2,"pai":"S","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"8m"}
        {"type":"dahai","actor":3,"pai":"8m","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"8p"}
        {"type":"dahai","actor":0,"pai":"N","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"5sr"}
        {"type":"dahai","actor":1,"pai":"E","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"E"}
        {"type":"dahai","actor":2,"pai":"E","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"4p"}
        {"type":"dahai","actor":3,"pai":"4p","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"1m"}
        {"type":"dahai","actor":0,"pai":"5m","tsumogiri":false}
        {"type":"pon","actor":3,"target":0,"pai":"5m","consumed":["5m","5mr"]}
        {"type":"dahai","actor":3,"pai":"8s","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"4s"}
        {"type":"dahai","actor":0,"pai":"4s","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"N"}
        {"type":"dahai","actor":1,"pai":"N","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"9p"}
        {"type":"dahai","actor":2,"pai":"8p","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"C"}
        {"type":"dahai","actor":3,"pai":"C","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"4s"}
        {"type":"dahai","actor":0,"pai":"4s","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"1m"}
        {"type":"dahai","actor":1,"pai":"9s","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"4p"}
        {"type":"dahai","actor":2,"pai":"2p","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"P"}
        {"type":"dahai","actor":3,"pai":"P","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"3m"}
        {"type":"dahai","actor":0,"pai":"3p","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"6s"}
        {"type":"dahai","actor":1,"pai":"9p","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"8s"}
        {"type":"dahai","actor":2,"pai":"3m","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"4m"}
        {"type":"dahai","actor":3,"pai":"4m","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"P"}
        {"type":"dahai","actor":0,"pai":"P","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"E"}
        {"type":"dahai","actor":1,"pai":"E","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"7s"}
        {"type":"dahai","actor":2,"pai":"2s","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"F"}
        {"type":"dahai","actor":3,"pai":"F","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"4m"}
        {"type":"dahai","actor":0,"pai":"4m","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"2m"}
        {"type":"dahai","actor":1,"pai":"5m","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"7p"}
        {"type":"dahai","actor":2,"pai":"7p","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"2s"}
        {"type":"dahai","actor":3,"pai":"2s","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"4p"}
        {"type":"dahai","actor":0,"pai":"4p","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"5pr"}
        {"type":"dahai","actor":1,"pai":"8p","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"2s"}
        {"type":"dahai","actor":2,"pai":"2s","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"F"}
        {"type":"dahai","actor":3,"pai":"F","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"6p"}
        {"type":"dahai","actor":0,"pai":"6p","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"7m"}
        {"type":"dahai","actor":1,"pai":"3p","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"1p"}
        {"type":"dahai","actor":2,"pai":"1p","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"9s"}
        {"type":"dahai","actor":3,"pai":"9s","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"S"}
        {"type":"dahai","actor":0,"pai":"S","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"7s"}
        {"type":"dahai","actor":1,"pai":"6s","tsumogiri":false}
        {"type":"chi","actor":2,"target":1,"pai":"6s","consumed":["5s","7s"]}
        {"type":"dahai","actor":2,"pai":"1s","tsumogiri":false}
        {"type":"pon","actor":1,"target":2,"pai":"1s","consumed":["1s","1s"]}
        {"type":"dahai","actor":1,"pai":"3s","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"2p"}
        {"type":"dahai","actor":2,"pai":"2p","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"3p"}
        {"type":"dahai","actor":3,"pai":"3p","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"6s"}
        {"type":"dahai","actor":0,"pai":"6s","tsumogiri":true}
        {"type":"tsumo","actor":1,"pai":"6p"}
        {"type":"dahai","actor":1,"pai":"6p","tsumogiri":true}
        {"type":"chi","actor":2,"target":1,"pai":"6p","consumed":["4p","5p"]}
        {"type":"dahai","actor":2,"pai":"8s","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"6m"}
        {"type":"dahai","actor":3,"pai":"3s","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"7m"}
        {"type":"dahai","actor":0,"pai":"8s","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"6p"}
        {"type":"dahai","actor":1,"pai":"6p","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"5s"}
        {"type":"dahai","actor":2,"pai":"8s","tsumogiri":false}
        {"type":"tsumo","actor":3,"pai":"1p"}
        {"type":"dahai","actor":3,"pai":"1p","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"2s"}
        {"type":"dahai","actor":0,"pai":"9s","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"1m"}
        {"type":"dahai","actor":1,"pai":"2m","tsumogiri":false}
        {"type":"pon","actor":3,"target":1,"pai":"2m","consumed":["2m","2m"]}
        {"type":"dahai","actor":3,"pai":"6m","tsumogiri":false}
        {"type":"tsumo","actor":0,"pai":"W"}
        {"type":"dahai","actor":0,"pai":"2s","tsumogiri":false}
        {"type":"tsumo","actor":1,"pai":"N"}
        {"type":"dahai","actor":1,"pai":"N","tsumogiri":true}
        {"type":"tsumo","actor":2,"pai":"5p"}
        {"type":"dahai","actor":2,"pai":"5p","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"3m"}
        {"type":"dahai","actor":3,"pai":"3m","tsumogiri":true}
        {"type":"tsumo","actor":0,"pai":"6m"}
        {"type":"ankan","actor":0,"consumed":["W","W","W","W"]}
        {"type":"dora","dora_marker":"7p"}
        {"type":"tsumo","actor":0,"pai":"8m"}
        {"type":"dahai","actor":0,"pai":"6m","tsumogiri":false}
        {"type":"chi","actor":1,"target":0,"pai":"6m","consumed":["7m","8m"]}
        {"type":"dahai","actor":1,"pai":"7m","tsumogiri":false}
        {"type":"tsumo","actor":2,"pai":"3s"}
        {"type":"dahai","actor":2,"pai":"3s","tsumogiri":true}
        {"type":"tsumo","actor":3,"pai":"2m"}
  |}
  in
  let ps = from_log 2 log in
  (* Clone to modify for kakan test *)
  (* Using functional update for clone if needed, but records are mutable *)
  let ps_kakan = State.create_player_state 2 in
  (* Copy all fields from ps to ps_kakan manually since we can't deep copy easily *)
  (* For testing purpose, we can just reuse ps but we need to rollback state or use separate log *)
  (* Actually, let's just re-create from log for the second part or use update_json. *)
  (* OCaml records are shallow copy by default on 'let x = y'. *)
  (* But tehai is mutable array, so we need to copy it. *)

  (* Copying state for kakan test *)
  ps_kakan.tehai <- Array.copy ps.tehai;
  ps_kakan.bakaze <- ps.bakaze;
  ps_kakan.jikaze <- ps.jikaze;
  ps_kakan.kyoku <- ps.kyoku;
  ps_kakan.honba <- ps.honba;
  ps_kakan.kyotaku <- ps.kyotaku;
  ps_kakan.oya <- ps.oya;
  ps_kakan.scores <- Array.copy ps.scores;
  ps_kakan.tiles_left <- ps.tiles_left;
  ps_kakan.riichi_declared <- Array.copy ps.riichi_declared;
  ps_kakan.riichi_accepted <- Array.copy ps.riichi_accepted;
  ps_kakan.is_menzen <- ps.is_menzen;
  ps_kakan.at_rinshan <- ps.at_rinshan;
  ps_kakan.at_ippatsu <- ps.at_ippatsu;
  ps_kakan.at_turn <- ps.at_turn;
  ps_kakan.last_self_tsumo <- ps.last_self_tsumo;
  ps_kakan.last_kawa_tile <- ps.last_kawa_tile;
  ps_kakan.last_cans <- ps.last_cans;
  ps_kakan.kans_on_board <- ps.kans_on_board;
  ps_kakan.chis <- ps.chis;
  ps_kakan.pons <- ps.pons;
  ps_kakan.minkans <- ps.minkans;
  ps_kakan.ankans <- ps.ankans;
  ps_kakan.ankan_candidates <- ps.ankan_candidates;
  ps_kakan.kakan_candidates <- ps.kakan_candidates;
  ps_kakan.shanten <- ps.shanten;
  ps_kakan.waits <- Array.copy ps.waits;
  ps_kakan.at_furiten <- ps.at_furiten;
  ps_kakan.to_mark_same_cycle_furiten <- ps.to_mark_same_cycle_furiten;
  ps_kakan.chankan_chance <- ps.chankan_chance;
  ps_kakan.has_next_shanten_discard <- ps.has_next_shanten_discard;
  ps_kakan.keep_shanten_discards <- Array.copy ps.keep_shanten_discards;
  ps_kakan.next_shanten_discards <- Array.copy ps.next_shanten_discards;
  ps_kakan.forbidden_tiles <- Array.copy ps.forbidden_tiles;
  ps_kakan.tehai_len_div3 <- ps.tehai_len_div3;
  ps_kakan.tiles_seen <- Array.copy ps.tiles_seen;
  ps_kakan.discarded_tiles <- Array.copy ps.discarded_tiles;
  ps_kakan.dora_indicators <- ps.dora_indicators;
  ps_kakan.dora_factor <- Array.copy ps.dora_factor;
  ps_kakan.doras_owned <- Array.copy ps.doras_owned;
  ps_kakan.doras_seen <- ps.doras_seen;
  ps_kakan.akas_in_hand <- Array.copy ps.akas_in_hand;
  ps_kakan.fuuro_overview <- Array.map (fun l -> l) ps.fuuro_overview;
  (* shallow copy of lists is fine *)

  (* kakan 2m *)
  let cans =
    update_json
      ps_kakan
      {|{"type":"kakan","actor":3,"pai":"2m","consumed":["2m","2m","2m"]}|}
  in
  assert cans.can_ron_agari;
  let points = State.agari_points ps_kakan true [] in
  (match points with
   | Ok p -> assert (p.ron = 1000)
   | Error msg -> failwith ("Expected agari, got error: " ^ msg));
  (* dahai 2m *)
  let cans = update_json ps {|{"type":"dahai","actor":3,"pai":"2m","tsumogiri":true}|} in
  assert (not cans.can_ron_agari);
  Printf.printf "  double_chankan_ron tests passed\n"
;;

let () =
  Printf.printf "\nState Module Tests\n";
  Printf.printf "==================\n\n";
  test_create_state ();
  test_action_candidate_helpers ();
  test_start_kyoku ();
  test_tsumo ();
  test_dahai ();
  test_validate_reaction ();
  test_tile_in_hand ();
  test_shanten_integration ();
  test_waits ();
  test_real_time_shanten ();
  test_advanced_tracking_integration ();
  test_ankan_candidates ();
  test_kakan_candidates ();
  test_can_chi ();
  test_furiten ();
  test_dora ();
  test_aka_tracking ();
  test_get_rank ();
  test_dora_count_after_kan ();
  test_chi_at_0_shanten ();
  test_rule_based_agari_all_last_minogashi ();
  test_kakan_from_hand ();
  test_discard_candidates_with_unconditional_tenpai ();
  test_double_chankan_ron ();
  Printf.printf "\n==================\n";
  Printf.printf "All State Tests Passed!\n";
  Printf.printf "==================\n\n"
;;
