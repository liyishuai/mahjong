(** Tests for agari module *)

(* ========== Part 1: Detection Tests ========== *)

(** Helper to parse hand string and create tiles34 array *)
let parse_hand (s : string) : int array =
  match Hand.hand s with
  | Ok tiles -> tiles
  | Error msg -> failwith (Printf.sprintf "Failed to parse hand '%s': %s" s msg)
;;

(** Test is_agari with known winning hands *)
let test_is_agari_positive () =
  Printf.printf "Testing is_agari with winning hands...\n";
  let winning_hands =
    [ "123456789m 12344s"
    ; "11122345678999s"
    ; "111234678m 11122z"
    ; "22334455m 234s 234p"
    ; "111222333m 234s 11z"
    ; "112233m 112233p 11z"
    ; "11223344556677z"
    ; (* Seven pairs *)
      "1133556699m 1122s"
    ; (* Seven pairs *)
      "11m 345p"
    ; "11m 112233p"
    ; "11m 123456789p"
    ; "11m 111p 111s"
    ; "111m 11p 111s"
    ; "111m 111p 11s"
    ]
  in
  List.iter
    (fun hand_str ->
       let tiles = parse_hand hand_str in
       if not (Agari.is_agari tiles)
       then (
         Printf.printf "  FAILED: %s should be agari\n" hand_str;
         assert false))
    winning_hands;
  Printf.printf "  Winning hands tests passed (%d hands)\n" (List.length winning_hands)
;;

(** Test is_agari with non-winning hands *)
let test_is_agari_negative () =
  Printf.printf "Testing is_agari with non-winning hands...\n";
  let non_winning_hands =
    [ "1133555599m 1122s"; "1122m"; "8888p"; "7777z"; "66778p 1122345s 77z" ]
  in
  List.iter
    (fun hand_str ->
       let tiles = parse_hand hand_str in
       if Agari.is_agari tiles
       then (
         Printf.printf "  FAILED: %s should not be agari\n" hand_str;
         assert false))
    non_winning_hands;
  Printf.printf
    "  Non-winning hands tests passed (%d hands)\n"
    (List.length non_winning_hands)
;;

(** Test randomized agreement between is_agari and is_agari_ref *)
let test_detection_randomized () =
  Printf.printf "Testing randomized hands (is_agari vs is_agari_ref)...\n%!";
  Random.self_init ();
  let module StringSet = Set.Make (String) in
  let seen_hands = ref StringSet.empty in
  let max_tests = 10000 in
  let hand_key hand = String.concat "" (Array.to_list (Array.map string_of_int hand)) in
  for _ = 1 to max_tests do
    let rec generate_hand () =
      let hand = Array.make 34 0 in
      let tiles_left = ref 14 in
      while !tiles_left > 0 do
        let idx = Random.int 34 in
        if hand.(idx) < 4
        then (
          hand.(idx) <- hand.(idx) + 1;
          decr tiles_left)
      done;
      let key = hand_key hand in
      if StringSet.mem key !seen_hands then generate_hand () else hand
    in
    let hand = generate_hand () in
    seen_hands := StringSet.add (hand_key hand) !seen_hands;
    let result = Agari.is_agari hand in
    let result_ref = Agari.is_agari_ref hand in
    if result <> result_ref
    then (
      let aka = [| false; false; false |] in
      let hand_str = Hand.tiles_to_string hand aka in
      Printf.printf
        "  MISMATCH: is_agari=%b vs is_agari_ref=%b, Hand: %s\n"
        result
        result_ref
        hand_str;
      failwith "is_agari and is_agari_ref disagree")
  done;
  Printf.printf "  Randomized tests passed (%d hands)\n" max_tests
;;

let () =
  Printf.printf "Agari Module Tests\n";
  Printf.printf "==================\n\n";
  Printf.printf "--- Detection ---\n";
  test_is_agari_positive ();
  test_is_agari_negative ();
  test_detection_randomized ();
  Printf.printf "\n==================\n";
  Printf.printf "All Agari Tests Completed!\n";
  Printf.printf "==================\n\n"
;;
