open Base
open Mahjong

let tile_string_to_type = function
  | "m1" -> 0
  | "m2" -> 1
  | "m3" -> 2
  | "m4" -> 3
  | "m5" -> 4
  | "m6" -> 5
  | "m7" -> 6
  | "m8" -> 7
  | "m9" -> 8
  | "p1" -> 9
  | "p2" -> 10
  | "p3" -> 11
  | "p4" -> 12
  | "p5" -> 13
  | "p6" -> 14
  | "p7" -> 15
  | "p8" -> 16
  | "p9" -> 17
  | "s1" -> 18
  | "s2" -> 19
  | "s3" -> 20
  | "s4" -> 21
  | "s5" -> 22
  | "s6" -> 23
  | "s7" -> 24
  | "s8" -> 25
  | "s9" -> 26
  | "ew" -> 27
  | "sw" -> 28
  | "ww" -> 29
  | "nw" -> 30
  | "wd" -> 31
  | "gd" -> 32
  | "rd" -> 33
  | s -> failwith ("Unknown tile string: " ^ s)
;;

let tiles_from_strings strs =
  let counts = Array.create ~len:34 0 in
  List.iter strs ~f:(fun s ->
    let type_ = tile_string_to_type s in
    counts.(type_) <- counts.(type_) + 1);
  counts
;;

let test_normal () =
  let tiles =
    tiles_from_strings
      [ "m1"; "m1"; "m2"; "m3"; "m4"; "m6"; "m6"; "m6"; "p2"; "p2"; "p2"; "ww"; "ww" ]
  in
  let s = Shanten.calculate tiles 0 in
  Alcotest.(check int) "normal tenpai" 0 s
;;

let test_thirteen_orphans () =
  let tiles =
    tiles_from_strings
      [ "m1"; "m9"; "p1"; "p9"; "s1"; "s9"; "ew"; "sw"; "ww"; "nw"; "wd"; "gd"; "p4" ]
  in
  let s = Shanten.calculate tiles 0 in
  Alcotest.(check int) "thirteen orphans 1-shanten" 1 s
;;

let test_seven_pairs () =
  let tiles =
    tiles_from_strings
      [ "m1"; "m1"; "m2"; "m2"; "p3"; "p3"; "p7"; "p7"; "ew"; "ew"; "sw"; "rd"; "wd" ]
  in
  let s = Shanten.calculate tiles 0 in
  Alcotest.(check int) "seven pairs 1-shanten" 1 s
;;

let test_proceeding () =
  let tiles =
    tiles_from_strings
      [ "m3"; "m4"; "m7"; "m8"; "s2"; "s3"; "p1"; "p2"; "p3"; "p4"; "p4"; "ew"; "sw" ]
  in
  let proceeding = Shanten.proceeding_tiles tiles 0 in
  let expected = Array.create ~len:34 false in
  List.iter [ "m2"; "m5"; "m6"; "m9"; "s1"; "s4" ] ~f:(fun s ->
    expected.(tile_string_to_type s) <- true);
  Alcotest.(check (array bool)) "proceeding tiles" expected proceeding
;;

let test_proceeding_pairs () =
  let tiles =
    tiles_from_strings
      [ "m1"; "m1"; "m1"; "m1"; "m2"; "m3"; "m4"; "m5"; "m6"; "m7"; "m8"; "m9"; "m9" ]
  in
  let proceeding = Shanten.proceeding_tiles tiles 0 in
  let expected = Array.create ~len:34 false in
  List.iter [ "m3"; "m6"; "m9" ] ~f:(fun s -> expected.(tile_string_to_type s) <- true);
  Alcotest.(check (array bool)) "proceeding pairs" expected proceeding
;;

let process_test_file filename =
  let path = "resources/shanten_testcases/" ^ filename in
  let ic = Stdlib.open_in path in
  let expected_acc = ref [] in
  let actual_acc = ref [] in
  try
    let rec loop line_num =
      try
        let line = Stdlib.input_line ic in
        let line = String.strip line in
        let parts = String.split_on_chars ~on:[ ' ' ] line in
        let parts = List.filter ~f:(fun s -> not (String.is_empty s)) parts in
        let nums = List.map parts ~f:Int.of_string in
        let hand_tiles = List.take nums 14 in
        let tiles = Array.create ~len:34 0 in
        List.iter hand_tiles ~f:(fun t -> tiles.(t) <- tiles.(t) + 1);
        let normal = List.nth_exn nums 14 in
        let thirteen_orphans = List.nth_exn nums 15 in
        let seven_pairs = List.nth_exn nums 16 in
        expected_acc := seven_pairs :: thirteen_orphans :: normal :: !expected_acc;
        actual_acc
        := Shanten.shanten_seven_pairs tiles
           :: Shanten.shanten_thirteen_orphans tiles
           :: Shanten.shanten_normal tiles 0
           :: !actual_acc;
        loop (line_num + 1)
      with
      | End_of_file -> ()
    in
    loop 1;
    Stdlib.close_in ic;
    List.rev !expected_acc, List.rev !actual_acc
  with
  | e ->
    Stdlib.close_in_noerr ic;
    raise e
;;

let test_many_cases () =
  let test_files =
    [ "p_hon_10000.txt"; "p_koku_10000.txt"; "p_normal_10000.txt"; "p_tin_10000.txt" ]
  in
  (* Process files in parallel using Domains *)
  let domains =
    List.map test_files ~f:(fun filename ->
      Domain.spawn (fun () -> process_test_file filename))
  in
  let results = List.map domains ~f:Domain.join in
  List.iter2_exn test_files results ~f:(fun filename (expected, actual) ->
    Alcotest.(check (list int)) filename expected actual)
;;

let () =
  let open Alcotest in
  run
    "Shanten"
    [ "normal", [ test_case "normal" `Quick test_normal ]
    ; "thirteen_orphans", [ test_case "thirteen_orphans" `Quick test_thirteen_orphans ]
    ; "seven_pairs", [ test_case "seven_pairs" `Quick test_seven_pairs ]
    ; "proceeding", [ test_case "proceeding" `Quick test_proceeding ]
    ; "proceeding_pairs", [ test_case "proceeding_pairs" `Quick test_proceeding_pairs ]
    ; "many_cases", [ test_case "many_cases" `Slow test_many_cases ]
    ]
;;
