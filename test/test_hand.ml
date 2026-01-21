(** Tests for hand parsing and conversion *)

(** Test hand string parsing *)
let test_parsing () =
  Printf.printf "Testing hand parsing...\n";
  (* Test basic parsing *)
  (match Hand.hand "1111m 333p 222s 444z" with
   | Ok tiles ->
     assert (tiles.(0) = 4);
     (* four 1m *)
     assert (tiles.(11) = 3);
     (* three 3p *)
     assert (tiles.(19) = 3);
     (* three 2s *)
     assert (tiles.(30) = 3)
     (* three 4z (North) *)
   | Error _ -> assert false);
  (* Test with aka (red fives) *)
  (match Hand.hand_with_aka "22334450m234p2s3s4s" with
   | Ok tiles ->
     assert (tiles.(1) = 2);
     (* two 2m *)
     assert (tiles.(2) = 2);
     (* two 3m *)
     assert (tiles.(3) = 2);
     (* two 4m *)
     assert (tiles.(4) = 1);
     (* one 5m *)
     assert (tiles.(34) = 1)
     (* one 5mr *)
   | Error _ -> assert false);
  Printf.printf "  Hand parsing passed\n"
;;

(** Test tiles_to_string conversion *)
let test_to_string () =
  Printf.printf "Testing tiles_to_string...\n";
  let tiles34 =
    [| 0
     ; 0
     ; 2
     ; 0
     ; 1
     ; 1
     ; 1
     ; 0
     ; 0 (* m *)
     ; 0
     ; 0
     ; 1
     ; 1
     ; 1
     ; 1
     ; 1
     ; 1
     ; 0 (* p *)
     ; 0
     ; 0
     ; 0
     ; 0
     ; 0
     ; 1
     ; 1
     ; 1
     ; 0 (* s *)
     ; 0
     ; 0
     ; 0
     ; 0
     ; 0
     ; 0
     ; 0 (* z *)
    |]
  in
  let aka = [| true; false; false |] in
  let result = Hand.tiles_to_string tiles34 aka in
  assert (result = "33067m 345678p 678s");
  Printf.printf "  tiles_to_string passed\n"
;;

(** Main test runner *)
let () =
  Printf.printf "\n";
  Printf.printf "Hand Parsing Tests\n";
  Printf.printf "==================\n\n";
  test_parsing ();
  test_to_string ();
  Printf.printf "\n";
  Printf.printf "==================\n";
  Printf.printf "All Hand Tests Completed!\n";
  Printf.printf "==================\n\n"
;;
