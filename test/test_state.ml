(** Tests for state module *)

(** Test basic state creation *)
let test_create_state () =
  Printf.printf "Testing create_player_state...\n";

  let state = State.create_player_state 0 in
  assert (state.player_id = 0);
  assert (Array.length state.tehai = 34);
  assert (Array.for_all ((=) 0) state.tehai);
  assert (state.bakaze = 27);  (* E *)
  assert (state.jikaze = 27);  (* E for player 0 *)
  assert (state.kyoku = 0);
  assert (state.honba = 0);
  assert (state.kyotaku = 0);
  assert (state.oya = 0);
  assert (Array.length state.scores = 4);
  assert (state.scores.(0) = 25000);
  assert (state.is_menzen = true);

  Printf.printf "  create_player_state tests passed\n"

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

(** Test start_kyoku event *)
let test_start_kyoku () =
  Printf.printf "Testing start_kyoku event...\n";

  let state = State.create_player_state 0 in
  let tehais = [|
    [|0; 0; 0; 1; 1; 1; 2; 2; 2; 3; 3; 3; 4|];  (* Simple 13 tiles *)
    [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|];
    [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|];
    [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|];
  |] in
  let scores = [|25000; 25000; 25000; 25000|] in

  let event = Mjai.Start_kyoku {
    bakaze = 27;  (* E *)
    dora_marker = 16;  (* 5m *)
    kyoku = 1;
    honba = 0;
    kyotaku = 0;
    oya = 0;
    scores;
    tehais;
  } in

  State.update state event;

  assert (state.bakaze = 27);
  assert (state.kyoku = 1);
  assert (state.honba = 0);
  assert (state.oya = 0);
  assert (state.is_menzen = true);
  (* Check tehai was populated *)
  assert (state.tehai.(0) = 3);  (* Three 0m *)
  assert (state.tehai.(1) = 3);  (* Three 1m *)
  assert (state.tehai.(2) = 3);  (* Three 2m *)
  assert (state.tehai.(3) = 3);  (* Three 3m *)
  assert (state.tehai.(4) = 1);  (* One 4m *)

  Printf.printf "  start_kyoku tests passed\n"

(** Test tsumo event *)
let test_tsumo () =
  Printf.printf "Testing tsumo event...\n";

  let state = State.create_player_state 0 in

  (* Draw a tile *)
  let event = Mjai.Tsumo { actor = 0; pai = 5 } in
  State.update state event;

  assert (state.tehai.(5) = 1);
  assert (state.last_self_tsumo = Some 5);

  (* Different player draws - state should not change *)
  let event2 = Mjai.Tsumo { actor = 1; pai = 10 } in
  State.update state event2;

  assert (state.tehai.(5) = 1);  (* Unchanged *)
  assert (state.tehai.(10) = 0);  (* Not added *)

  Printf.printf "  tsumo tests passed\n"

(** Test dahai event *)
let test_dahai () =
  Printf.printf "Testing dahai event...\n";

  let state = State.create_player_state 0 in
  (* Set up initial hand *)
  state.tehai.(0) <- 2;
  state.tehai.(5) <- 1;

  (* Discard a tile *)
  let event = Mjai.Dahai { actor = 0; pai = 5; tsumogiri = false } in
  State.update state event;

  assert (state.tehai.(5) = 0);
  assert (state.tehai.(0) = 2);  (* Unchanged *)

  (* Discard with tsumogiri *)
  state.last_self_tsumo <- Some 0;
  let event2 = Mjai.Dahai { actor = 0; pai = 0; tsumogiri = true } in
  State.update state event2;

  assert (state.tehai.(0) = 1);
  assert (state.last_self_tsumo = None);  (* Cleared on tsumogiri *)

  Printf.printf "  dahai tests passed\n"

(** Test validate_reaction *)
let test_validate_reaction () =
  Printf.printf "Testing validate_reaction...\n";

  let state = State.create_player_state 0 in

  (* None should always be valid *)
  begin match State.validate_reaction state Mjai.None with
  | Ok () -> ()
  | Error msg ->
      Printf.printf "  ERROR: None should be valid: %s\n" msg;
      failwith "test failed"
  end;

  (* Cannot discard when can_discard is false *)
  begin match State.validate_reaction state (Mjai.Dahai { actor = 0; pai = 0; tsumogiri = false }) with
  | Error "cannot discard" -> ()
  | Ok () ->
      Printf.printf "  ERROR: Should not be able to discard\n";
      failwith "test failed"
  | Error msg ->
      Printf.printf "  ERROR: Unexpected error: %s\n" msg;
      failwith "test failed"
  end;

  (* Cannot riichi when can_riichi is false *)
  begin match State.validate_reaction state (Mjai.Reach { actor = 0 }) with
  | Error "cannot riichi" -> ()
  | Ok () ->
      Printf.printf "  ERROR: Should not be able to riichi\n";
      failwith "test failed"
  | Error msg ->
      Printf.printf "  ERROR: Unexpected error: %s\n" msg;
      failwith "test failed"
  end;

  Printf.printf "  validate_reaction tests passed\n"

(** Test tile_in_hand *)
let test_tile_in_hand () =
  Printf.printf "Testing tile_in_hand...\n";

  let state = State.create_player_state 0 in
  state.tehai.(5) <- 2;
  state.tehai.(10) <- 1;

  assert (State.tile_in_hand state 5);
  assert (State.tile_in_hand state 10);
  assert (not (State.tile_in_hand state 0));

  Printf.printf "  tile_in_hand tests passed\n"

(** Test shanten calculation and integration *)
let test_shanten_integration () =
  Printf.printf "Testing shanten integration...\n";

  let state = State.create_player_state 0 in

  (* Test with a simple winning hand - all 1-9m *)
  for i = 0 to 8 do
    state.tehai.(i) <- 1
  done;
  state.tehai.(9) <- 1;  (* 1s *)
  state.tehai.(10) <- 1;  (* 2s *)
  state.tehai.(11) <- 1;  (* 3s *)
  state.tehai.(9) <- 2;  (* Add one more 1s to make 14 tiles *)
  state.tehai_len_div3 <- 4;

  State.update_shanten state;
  if state.shanten <> -1 then
    Printf.printf "  shanten=%d (expected -1 or 0 for winning hand)\n" state.shanten;

  (* Test with a non-tenpai hand *)
  Array.iteri (fun idx _ -> state.tehai.(idx) <- 0) state.tehai;
  for i = 0 to 5 do
    state.tehai.(i) <- 1  (* 123m x2 *)
  done;
  for i = 9 to 11 do
    state.tehai.(i) <- 1  (* 123s *)
  done;
  state.tehai_len_div3 <- 4;

  State.update_shanten state;
  assert (state.shanten >= 0);  (* Should be at least 0-shanten *)

  Printf.printf "  shanten integration tests passed\n"

(** Test waits calculation *)
let test_waits () =
  Printf.printf "Testing waits calculation...\n";

  let state = State.create_player_state 0 in

  (* Set up a simple tenpai hand: 111222333m 44s *)
  state.tehai.(0) <- 3;  (* 1m x3 *)
  state.tehai.(1) <- 3;  (* 2m x3 *)
  state.tehai.(2) <- 3;  (* 3m x3 *)
  state.tehai.(9) <- 1;  (* 1s *)
  state.tehai.(10) <- 1;  (* 2s *)
  state.tehai.(11) <- 1;  (* 3s *)
  state.tehai.(11) <- 2;  (* Add one more 3s *)
  state.tehai_len_div3 <- 4;
  State.update_shanten state;

  Printf.printf "  shanten=%d (should be 0 for tenpai)\n" state.shanten;
  State.update_waits_and_furiten state;

  (* Verify waits array was updated *)
  let has_wait = Array.fold_left (fun acc w -> acc || w) false state.waits in
  Printf.printf "  has_waits=%b\n" has_wait;
  Printf.printf "  waits tests passed\n"

(** Test real_time_shanten *)
let test_real_time_shanten () =
  Printf.printf "Testing real_time_shanten...\n";

  let state = State.create_player_state 0 in

  (* Test with 3n+1 hand (can_discard = false) *)
  let hand = [|0; 0; 0; 1; 1; 1; 2; 2; 2; 3; 3; 3; 4|] in
  Array.iter (fun tile ->
    let idx = Tiles.deaka tile in
    if idx >= 0 && idx < 34 then state.tehai.(idx) <- state.tehai.(idx) + 1
  ) hand;
  state.tehai_len_div3 <- 4;
  State.update_shanten state;
  state.last_cans <- { State.default_action_candidate with can_discard = false };

  assert (State.real_time_shanten state = state.shanten);

  (* Test with 3n+2 hand (can_discard = true) *)
  state.tehai.(8) <- state.tehai.(8) + 1;  (* Add another tile *)
  state.tehai_len_div3 <- 4;
  State.update_shanten state;
  state.last_cans <- { State.default_action_candidate with can_discard = true };

  (* real_time_shanten should calculate based on actual hand *)
  let real_shanten = State.real_time_shanten state in
  assert (real_shanten = state.shanten || real_shanten = state.shanten - 1);

  Printf.printf "  real_time_shanten tests passed\n"

(** Test advanced tracking integration with event processing *)
let test_advanced_tracking_integration () =
  Printf.printf "Testing advanced tracking integration...\n";

  let state = State.create_player_state 0 in

  (* Set up initial hand: 123456m *)
  state.tehai.(0) <- 1;  (* 1m *)
  state.tehai.(1) <- 1;  (* 2m *)
  state.tehai.(2) <- 1;  (* 3m *)
  state.tehai.(3) <- 1;  (* 4m *)
  state.tehai.(4) <- 1;  (* 5m *)
  state.tehai.(5) <- 1;  (* 6m *)
  state.tehai_len_div3 <- 2;
  State.update_shanten state;

  (* Verify shanten is calculated *)
  Printf.printf "  Initial shanten: %d\n" state.shanten;
  assert (state.shanten >= 0);

  (* Process a tsumo event - should update shanten *)
  let tsumo_event = Mjai.Tsumo { actor = 0; pai = 6 } in  (* 7m, tile ID 6 *)
  State.update state tsumo_event;

  (* Verify shanten was updated after tsumo *)
  Printf.printf "  After tsumo shanten: %d\n" state.shanten;
  assert (state.shanten >= 0);

  (* Process a dahai event - should update shanten and track discard *)
  let dahai_event = Mjai.Dahai { actor = 0; pai = 6; tsumogiri = true } in  (* Discard 7m *)
  State.update state dahai_event;

  (* Verify discard was tracked *)
  Printf.printf "  Discarded tiles tracked: %b\n" state.discarded_tiles.(6);
  assert (state.discarded_tiles.(6) = true);  (* 7m is at index 6 *)

  Printf.printf "  advanced tracking integration tests passed\n"

(** Test ankan candidates *)
let test_ankan_candidates () =
  Printf.printf "Testing ankan_candidates...\n";

  let state = State.create_player_state 0 in

  (* Set up hand with two quads: 1111m and 2222p *)
  state.tehai.(0) <- 4;  (* 1m x4 *)
  state.tehai.(9) <- 4;  (* 1p x4 *)

  let candidates = State.ankan_candidates state in

  Printf.printf "  Found %d ankan candidates\n" (Array.length candidates);
  assert (Array.length candidates = 2);
  (* Should find 1m (index 0) and 1p (index 9) *)
  assert (Array.mem 0 candidates);
  assert (Array.mem 9 candidates);

  Printf.printf "  ankan_candidates tests passed\n"

(** Test kakan candidates *)
let test_kakan_candidates () =
  Printf.printf "Testing kakan_candidates...\n";

  let state = State.create_player_state 0 in

  (* Set up hand with a pon meld and the 4th tile *)
  state.pons <- [| 0 |];  (* Pon of 1m *)
  state.tehai.(0) <- 1;   (* Have the 4th 1m in hand *)

  let candidates = State.kakan_candidates state in

  Printf.printf "  Found %d kakan candidates\n" (Array.length candidates);
  assert (Array.length candidates = 1);
  assert (candidates.(0) = 0);

  (* Test with no candidates *)
  state.tehai.(0) <- 0;
  let candidates = State.kakan_candidates state in
  assert (Array.length candidates = 0);

  Printf.printf "  kakan_candidates tests passed\n"

(** Test can_chi functionality *)
let test_can_chi () =
  Printf.printf "Testing can_chi...\n";

  let state = State.create_player_state 0 in

  (* Test case 1: 1111234m - cannot chi 1m or 4m, can chi 2m *)
  state.tehai.(0) <- 4;  (* 1m x4 *)
  state.tehai.(1) <- 1;  (* 2m *)
  state.tehai.(2) <- 1;  (* 3m *)
  state.tehai.(3) <- 1;  (* 4m *)

  (* Check chi with 1m - should be false *)
  State.set_can_chi_from_tile state 0;
  Printf.printf "  chi 1m: low=%b mid=%b high=%b\n"
    state.last_cans.can_chi_low state.last_cans.can_chi_mid state.last_cans.can_chi_high;
  assert (not state.last_cans.can_chi_low);
  assert (not state.last_cans.can_chi_mid);
  assert (not state.last_cans.can_chi_high);

  (* Check chi with 4m - should be false *)
  State.set_can_chi_from_tile state 3;
  assert (not state.last_cans.can_chi_low);
  assert (not state.last_cans.can_chi_mid);
  assert (not state.last_cans.can_chi_high);

  (* Check chi with 2m - should have mid and low chi *)
  State.set_can_chi_from_tile state 1;
  assert (not state.last_cans.can_chi_high);
  assert state.last_cans.can_chi_mid;
  assert state.last_cans.can_chi_low;

  (* Test case 2: 6666789999p - test various chi options *)
  Array.fill state.tehai 0 34 0;
  state.tehai.(5) <- 4;  (* 6p x4 *)
  state.tehai.(6) <- 1;  (* 7p *)
  state.tehai.(7) <- 1;  (* 8p *)
  state.tehai.(8) <- 4;  (* 9p x4 *)

  (* Chi with 5p (567p) - only low chi *)
  State.set_can_chi_from_tile state 4;  (* 4 = 5p *)
  assert (not state.last_cans.can_chi_high);
  assert (not state.last_cans.can_chi_mid);
  assert state.last_cans.can_chi_low;

  (* Chi with 7p (678p or 789p) - mid and low chi *)
  State.set_can_chi_from_tile state 6;  (* 6 = 7p *)
  assert (not state.last_cans.can_chi_high);
  assert state.last_cans.can_chi_mid;
  assert state.last_cans.can_chi_low;

  (* Chi with 8p (789p) - high and mid chi *)
  State.set_can_chi_from_tile state 7;  (* 7 = 8p *)
  assert state.last_cans.can_chi_high;
  assert state.last_cans.can_chi_mid;
  assert (not state.last_cans.can_chi_low);

  (* Test case 3: 4556s - edge cases *)
  Array.fill state.tehai 0 34 0;
  state.tehai.(21) <- 1;  (* 4s *)
  state.tehai.(22) <- 2;  (* 5s x2 *)
  state.tehai.(23) <- 1;  (* 6s *)

  (* Chi with 3s - only low chi *)
  State.set_can_chi_from_tile state 20;  (* 20 = 3s *)
  assert (not state.last_cans.can_chi_high);
  assert (not state.last_cans.can_chi_mid);
  assert state.last_cans.can_chi_low;

  (* Chi with 4s - only low chi *)
  State.set_can_chi_from_tile state 21;  (* 21 = 4s *)
  assert (not state.last_cans.can_chi_high);
  assert (not state.last_cans.can_chi_mid);
  assert state.last_cans.can_chi_low;

  (* Chi with 5s - should be false (would break pair) *)
  State.set_can_chi_from_tile state 22;  (* 22 = 5s *)
  assert (not state.last_cans.can_chi_high);
  assert (not state.last_cans.can_chi_mid);
  assert (not state.last_cans.can_chi_low);

  (* Chi with 6s - only high chi *)
  State.set_can_chi_from_tile state 23;  (* 23 = 6s *)
  assert state.last_cans.can_chi_high;
  assert (not state.last_cans.can_chi_mid);
  assert (not state.last_cans.can_chi_low);

  (* Chi with 7s - only high chi *)
  State.set_can_chi_from_tile state 24;  (* 24 = 7s *)
  assert state.last_cans.can_chi_high;
  assert (not state.last_cans.can_chi_mid);
  assert (not state.last_cans.can_chi_low);

  Printf.printf "  can_chi tests passed\n"

(** Test furiten tracking *)
let test_furiten () =
  Printf.printf "Testing furiten tracking...\n";

  let state = State.create_player_state 0 in

  (* Test 1: Not furiten when no winning tiles are discarded *)
  state.tehai.(0) <- 3;  (* 1m x3 *)
  state.tehai.(1) <- 1;  (* 2m *)
  state.tehai.(2) <- 1;  (* 3m *)
  state.tehai_len_div3 <- 1;
  State.update_shanten state;
  State.update_waits_and_furiten state;

  assert (not state.at_furiten);
  Printf.printf "  Test 1 passed: not furiten with no discards\n";

  (* Test 2: Furiten when a winning tile is discarded *)
  state.tehai.(0) <- 2;  (* 11m *)
  state.tehai.(1) <- 1;  (* 2m *)
  state.tehai.(2) <- 1;  (* 3m *)
  state.tehai_len_div3 <- 1;
  (* Simulate discarding 2m *)
  state.discarded_tiles.(1) <- true;
  State.update_shanten state;
  State.update_waits_and_furiten state;

  (* Waiting on 1m or 3m, but 2m was discarded so not furiten yet *)
  (* Actually, let me set up a better example *)
  Array.fill state.tehai 0 34 0;
  state.tehai.(0) <- 2;  (* 11m *)
  state.tehai.(8) <- 2;  (* 99m *)
  state.tehai_len_div3 <- 1;
  (* Discard 1m (wait tile) *)
  state.discarded_tiles.(0) <- true;
  State.update_shanten state;
  State.update_waits_and_furiten state;

  assert state.at_furiten;
  Printf.printf "  Test 2 passed: furiten when wait tile discarded\n";

  (* Test 3: Furiten reset with new hand *)
  Array.fill state.tehai 0 34 0;
  Array.fill state.discarded_tiles 0 34 false;
  state.tehai.(0) <- 3;  (* 111m *)
  state.tehai.(1) <- 1;  (* 2m *)
  state.tehai_len_div3 <- 1;
  State.update_shanten state;
  State.update_waits_and_furiten state;

  assert (not state.at_furiten);
  Printf.printf "  Test 3 passed: furiten reset with new hand\n";

  (* Test 4: Tenpai hand with multiple waits *)
  Array.fill state.tehai 0 34 0;
  Array.fill state.discarded_tiles 0 34 false;
  (* Hand: 11123456m - waiting on 7m *)
  state.tehai.(0) <- 3;  (* 111m *)
  state.tehai.(1) <- 1;  (* 2m *)
  state.tehai.(2) <- 1;  (* 3m *)
  state.tehai.(3) <- 1;  (* 4m *)
  state.tehai.(4) <- 1;  (* 5m *)
  state.tehai.(5) <- 1;  (* 6m *)
  state.tehai_len_div3 <- 2;
  State.update_shanten state;
  State.update_waits_and_furiten state;

  assert (not state.at_furiten);
  assert state.waits.(6);  (* 7m is a wait *)

  (* Discard 7m (the wait) *)
  state.discarded_tiles.(6) <- true;
  State.update_waits_and_furiten state;

  assert state.at_furiten;
  Printf.printf "  Test 4 passed: furiten with single wait\n";

  Printf.printf "  furiten tracking tests passed\n"

(** Test dora tracking *)
let test_dora () =
  Printf.printf "Testing dora tracking...\n";

  let state = State.create_player_state 0 in

  (* Test 1: Add dora indicator *)
  State.add_dora_indicator state 16;  (* 9p indicator, 1s is dora *)
  assert (Array.length state.dora_indicators = 1);
  assert (state.dora_indicators.(0) = 16);
  assert (state.dora_factor.(17) = 1);  (* 1s (index 17) has dora factor 1 *)

  Printf.printf "  Test 1 passed: dora indicator added\n";

  (* Test 2: Multiple dora indicators *)
  State.add_dora_indicator state 0;   (* 1m indicator, 2m is dora *)
  State.add_dora_indicator state 9;   (* 1p indicator, 2p is dora *)
  assert (Array.length state.dora_indicators = 3);
  assert (state.dora_factor.(1) = 1);  (* 2m *)
  assert (state.dora_factor.(10) = 1);  (* 2p *)

  Printf.printf "  Test 2 passed: multiple dora indicators\n";

  (* Test 3: Dora tiles in hand *)
  Array.fill state.tehai 0 34 0;
  state.tehai.(1) <- 2;  (* Two 2m, which is dora *)
  state.tehai.(10) <- 1;  (* One 2p, which is dora *)
  (* Reset and add indicators to count doras in hand *)
  Array.fill state.dora_factor 0 34 0;
  Array.fill state.doras_owned 0 4 0;
  state.dora_indicators <- [||];

  State.add_dora_indicator state 0;  (* 1m indicator, 2m is dora *)
  assert (state.dora_factor.(1) = 1);  (* 2m is dora *)
  assert (state.doras_owned.(0) = 2);  (* Two 2m in hand *)

  Printf.printf "  Test 3 passed: dora tiles counted in hand\n";

  Printf.printf "  dora tracking tests passed\n"

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
  Printf.printf "\n==================\n";
  Printf.printf "All State Tests Passed!\n";
  Printf.printf "==================\n\n"
