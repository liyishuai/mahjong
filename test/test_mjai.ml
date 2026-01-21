(** Tests for MJAI protocol event handling *)

(** Test JSON parsing from MJAI protocol logs *)
let test_json_parsing () =
  (* Test data from MJAI protocol *)
  let test_lines =
    [ {|{"type":"start_game","names":["NoName","NoName","NoName","NoName"]}|}
    ; {|{"type":"start_kyoku","bakaze":"E","oya":3,"dora_marker":"8m","kyoku":1,"honba":0,"kyotaku":0,"scores":[25000,25000,25000,25000],"tehais":[["1m","2m","3m","E","E","E","S","S","S","W","W","W","N"],["N","S","W","E","P","F","C","1m","2m","3m","4m","5m","6m"],["1p","2p","3p","4p","5p","6p","7p","8p","9p","1s","2s","3s","4s"],["5s","6s","7s","8s","9s","E","S","W","N","P","F","C","1m"]]}|}
    ; {|{"type":"tsumo","actor":3,"pai":"2m"}|}
    ; {|{"type":"dahai","actor":3,"pai":"4m","tsumogiri":false}|}
    ; {|{"type":"chi","actor":0,"target":3,"pai":"4m","consumed":["2m","3m"]}|}
    ; {|{"type":"dahai","actor":0,"pai":"1m","tsumogiri":false}|}
    ; {|{"type":"tsumo","actor":0,"pai":"N"}|}
    ; {|{"type":"dahai","actor":0,"pai":"N","tsumogiri":true}|}
    ; {|{"type":"pon","actor":1,"target":0,"pai":"N","consumed":["N","N"]}|}
    ; {|{"type":"reach","actor":2}|}
    ; {|{"type":"reach_accepted","actor":2}|}
    ; {|{"type":"hora","actor":2,"target":3}|}
    ; {|{"type":"end_kyoku"}|}
    ; {|{"type":"end_game"}|}
    ]
  in
  List.iter
    (fun line ->
       try
         let _event = Mjai.Json.event_from_string line in
         ()
       with
       | Failure msg ->
         Printf.printf "Failed to parse: %s\nError: %s\n" line msg;
         assert false
       | Mjai.Out_of_bound n ->
         Printf.printf "Out of bound error for: %s (value: %d)\n" line n;
         assert false)
    test_lines;
  Printf.printf "JSON parsing: OK (%d test cases)\n" (List.length test_lines)
;;

(** Test JSON round-trip *)
let test_json_roundtrip () =
  let test_cases =
    [ {|{"type":"none"}|}
    ; {|{"type":"tsumo","actor":0,"pai":"1m"}|}
    ; {|{"type":"dahai","actor":1,"pai":"E","tsumogiri":true}|}
    ; {|{"type":"chi","actor":2,"target":1,"pai":"5m","consumed":["3m","4m"]}|}
    ; {|{"type":"pon","actor":3,"target":2,"pai":"N","consumed":["N","N"]}|}
    ; {|{"type":"reach","actor":0}|}
    ; {|{"type":"reach_accepted","actor":0}|}
    ; {|{"type":"dora","dora_marker":"2s"}|}
    ]
  in
  List.iter
    (fun json_str ->
       let event = Mjai.Json.event_from_string json_str in
       let json_out = Mjai.Json.event_to_string event in
       let event2 = Mjai.Json.event_from_string json_out in
       if event <> event2
       then (
         Printf.printf "Round-trip failed for: %s\n" json_str;
         Printf.printf "Got back: %s\n" json_out;
         assert false))
    test_cases;
  Printf.printf "JSON round-trip: OK (%d test cases)\n" (List.length test_cases)
;;

(** Test actor extraction *)
let test_actor () =
  let ev1 = Mjai.Tsumo { actor = 2; pai = 0 } in
  assert (Mjai.actor ev1 = Some 2);
  let ev2 = Mjai.Dahai { actor = 1; pai = 5; tsumogiri = true } in
  assert (Mjai.actor ev2 = Some 1);
  let ev3 = Mjai.Dora { dora_marker = 10 } in
  assert (Mjai.actor ev3 = None);
  let ev4 = Mjai.End_kyoku in
  assert (Mjai.actor ev4 = None);
  Printf.printf "Actor extraction: OK\n"
;;

(** Test bound checking *)
let test_bound_checking () =
  (* Actor out of bounds *)
  (try
     let _ = Mjai.Json.event_from_string {|{"type":"reach","actor":4}|} in
     assert false
   with
   | Mjai.Out_of_bound _ -> ());
  (* Kyoku out of bounds *)
  (try
     let _ =
       Mjai.Json.event_from_string
         {|{"type":"start_kyoku","bakaze":"E","dora_marker":"5s","kyoku":0,"honba":0,"kyotaku":0,"oya":0,"scores":[25000,25000,25000,25000],"tehais":[["N","3p","W","W","7m","N","S","C","7m","P","8p","2m","5m"],["7p","1p","2m","3m","4m","C","7s","7s","9s","9p","1m","C","1s"],["3s","E","5m","P","5m","F","7p","6m","5s","9p","1s","S","N"],["2p","4s","4p","E","5p","F","3p","1s","8p","6s","8s","7s","5p"]]}|}
     in
     assert false
   with
   | Mjai.Out_of_bound _ -> ());
  Printf.printf "Bound checking: OK\n"
;;

let () =
  Printf.printf "=== MJAI Tests ===\n";
  test_json_parsing ();
  test_json_roundtrip ();
  test_actor ();
  test_bound_checking ();
  Printf.printf "All MJAI tests passed!\n"
;;
