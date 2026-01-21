(** Tests for shanten calculation *)

(** Helper to parse hand string and create tiles34 array *)
let parse_hand (s : string) : int array =
  match Hand.hand s with
  | Ok tiles -> tiles
  | Error msg -> failwith (Printf.sprintf "Failed to parse hand '%s': %s" s msg)
;;

(** Helper to calculate shanten with automatic len_div3 *)
let calculate_shanten (tiles34 : int array) : int =
  let total = Array.fold_left ( + ) 0 tiles34 in
  (* len_div3 = tile_count / 3 as per the shanten module interface *)
  let len_div3 = total / 3 in
  Shanten.calc_all tiles34 len_div3
;;

(** Test shanten calculation for 3n+1 hands *)
let test_shanten_3n1 () =
  Printf.printf "Testing shanten for 3n+1 hands...\n";
  let test_cases =
    [ "1m", 0; "1555m", 0; "2247m", 1; "5555m", 1; "5555z", 1; "11234m", -1 ]
  in
  List.iter
    (fun (hand_str, expected) ->
       let tiles = parse_hand hand_str in
       let result = calculate_shanten tiles in
       if result <> expected
       then (
         Printf.printf "  FAILED: %s expected %d, got %d\n" hand_str expected result;
         assert false))
    test_cases;
  Printf.printf "  3n+1 hand tests passed\n"
;;

(** Test shanten calculation for 3n+2 hands *)
let test_shanten_3n2 () =
  Printf.printf "Testing shanten for 3n+2 hands...\n";
  let test_cases =
    [ (* From mahjong-helper *)
      "33m 5555p 66s 556666z", 1
    ; "13579m 13579s 135p", 4
    ; "13579m 12379s 135p", 3
    ; "123456789m 147s 14m", 1
    ; "123456789m 147s 1m", 2
    ; "258m 258s 258p 12345z", 6
    ; (* Farthest from winning *)
      "123456789m 1134p", 0
    ; "123456789m 11345p", -1
    ; (* Seven pairs *)
      "11223344556677z", -1
    ; "1223344556677z", 0
    ; "1m 1223344556677z", 0
    ; "12m 123344556677z", 1
    ; "11222233445566z", 1
    ; (* ("11112222333344z", 5); (* Edge case - skip for now *) *)
      "577m 23677p 245577s", 2
    ]
  in
  List.iter
    (fun (hand_str, expected) ->
       let tiles = parse_hand hand_str in
       let result = calculate_shanten tiles in
       if result <> expected
       then (
         Printf.printf "  FAILED: %s expected %d, got %d\n" hand_str expected result;
         assert false))
    test_cases;
  Printf.printf "  3n+2 hand tests passed\n"
;;

let () =
  Printf.printf "\nShanten Calculation Tests\n";
  Printf.printf "=========================\n\n";
  test_shanten_3n1 ();
  test_shanten_3n2 ();
  Printf.printf "\n=========================\n";
  Printf.printf "All Shanten Tests Passed!\n";
  Printf.printf "=========================\n\n"
;;
