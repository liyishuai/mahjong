open Obs_repr
open State

let test_encode_obs_v4 () =
  Printf.printf "Testing encode_obs version 4...\n";
  let state = create_player_state 0 in
  (* Populate tehai with some tiles *)
  for i = 0 to 12 do
    state.tehai.(i) <- 1
  done;
  state.last_cans <- { default_action_candidate with can_discard = true };
  
  let obs = encode_obs state 4 false in
  let rows, cols = obs_shape 4 in
  assert (Array.length obs.features = rows * cols);
  assert (Array.length obs.mask = action_space);
  Printf.printf "  encode_obs v4 shape: (%d, %d) OK\n" rows cols;
  Printf.printf "  encode_obs v4 passed\n"

let () =
  Printf.printf "\nObs Repr Tests\n";
  Printf.printf "==============\n";
  test_encode_obs_v4 ();
  Printf.printf "\n==================\n";
  Printf.printf "All Obs Repr Tests Passed!\n";
  Printf.printf "==================\n\n"
