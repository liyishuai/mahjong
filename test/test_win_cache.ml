open Base
open Mahjong

let test_basic_win () =
  (* Load cache explicitly from project root *)
  Win_cache.load_cache_from "win_cache.bin";

  (* Test a basic winning hand: 123m 456m 789m 111p 99p *)
  let counts = Array.create ~len:34 0 in
  counts.(0) <- 1; counts.(1) <- 1; counts.(2) <- 1;  (* 123m *)
  counts.(3) <- 1; counts.(4) <- 1; counts.(5) <- 1;  (* 456m *)
  counts.(6) <- 1; counts.(7) <- 1; counts.(8) <- 1;  (* 789m *)
  counts.(9) <- 3;   (* 111p *)
  counts.(10) <- 2;  (* 99p *)

  let is_win = Win_cache.has counts in
  Alcotest.(check bool) "basic winning hand" true is_win;

  (* Test decomposition *)
  let decomps = Win_cache.sets_and_heads counts in
  Alcotest.(check bool) "has decompositions" true (List.length decomps > 0);

  List.iter decomps ~f:(fun (sets, heads) ->
    Alcotest.(check int) "correct number of sets" 4 (List.length sets);
    Alcotest.(check int) "correct number of heads" 1 (List.length heads)
  )

let test_tenpai () =
  Win_cache.load_cache_from "win_cache.bin";

  (* Test a tenpai hand: 123m 456m 789m 111p 9p (waiting for 9p) *)
  let counts = Array.create ~len:34 0 in
  counts.(0) <- 1; counts.(1) <- 1; counts.(2) <- 1;  (* 123m *)
  counts.(3) <- 1; counts.(4) <- 1; counts.(5) <- 1;  (* 456m *)
  counts.(6) <- 1; counts.(7) <- 1; counts.(8) <- 1;  (* 789m *)
  counts.(9) <- 3;   (* 111p *)
  counts.(10) <- 1;  (* 9p *)

  let is_tenpai = Win_cache.tenpai counts in
  Alcotest.(check bool) "tenpai hand" true is_tenpai;

  let waiting = Win_cache.machi counts in
  Alcotest.(check bool) "has waiting tiles" true (List.length waiting > 0)

let () =
  let open Alcotest in
  run "Win_cache"
    [
      ( "basic",
        [
          test_case "winning hand" `Quick test_basic_win;
          test_case "tenpai" `Quick test_tenpai;
        ] );
    ]
