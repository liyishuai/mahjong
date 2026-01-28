open Base
open Mahjong

let test_create_draw () =
  let event = Event.create_draw ~who:Action.AbsolutePos.InitEast in
  Alcotest.(check bool) "Draw event valid" true (Event.is_valid event);
  Alcotest.(check (option int)) "Draw event who" (Some 0) (Event.who event);
  Alcotest.(check bool) "Draw event has no tile" true (Option.is_none (Event.tile event))

let test_create_discard () =
  let event = Event.create_discard ~who:Action.AbsolutePos.InitSouth ~discard:5 ~tsumogiri:false in
  Alcotest.(check bool) "Discard event valid" true (Event.is_valid event);
  Alcotest.(check (option int)) "Discard event who" (Some 1) (Event.who event);
  Alcotest.(check (option int)) "Discard event tile" (Some 5) (Event.tile event)

let test_create_tsumogiri () =
  let event = Event.create_discard ~who:Action.AbsolutePos.InitWest ~discard:10 ~tsumogiri:true in
  Alcotest.(check bool) "Tsumogiri event valid" true (Event.is_valid event);
  let event_type = Event.type_ event in
  Alcotest.(check bool) "Tsumogiri event type" true (Event.EventType.equal event_type Event.EventType.Tsumogiri)

let test_create_riichi () =
  let event = Event.create_riichi ~who:Action.AbsolutePos.InitNorth in
  Alcotest.(check bool) "Riichi event valid" true (Event.is_valid event);
  Alcotest.(check (option int)) "Riichi event who" (Some 3) (Event.who event)

let test_create_new_dora () =
  let event = Event.create_new_dora ~dora_indicator:7 in
  Alcotest.(check bool) "New dora event valid" true (Event.is_valid event);
  Alcotest.(check (option int)) "New dora tile" (Some 7) (Event.tile event)

let () =
  let open Alcotest in
  run "Event tests" [
    "create_draw", [test_case "create_draw" `Quick test_create_draw];
    "create_discard", [test_case "create_discard" `Quick test_create_discard];
    "create_tsumogiri", [test_case "create_tsumogiri" `Quick test_create_tsumogiri];
    "create_riichi", [test_case "create_riichi" `Quick test_create_riichi];
    "create_new_dora", [test_case "create_new_dora" `Quick test_create_new_dora];
  ]
