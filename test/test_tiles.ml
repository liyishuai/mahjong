(** Tests for tile conversions and properties *)

(** Test tile string/int conversions *)
let test_conversions () =
  Printf.printf "Testing tile conversions...\n";
  (* Test valid string conversions *)
  (match Tiles.tile_of_string "E" with
   | Ok tile -> assert (Tiles.int_of_tile tile = 27)
   | Error _ -> failwith "Failed to parse E");
  (match Tiles.tile_of_string "5mr" with
   | Ok tile -> assert (Tiles.int_of_tile tile = 34)
   | Error _ -> failwith "Failed to parse 5mr");
  (match Tiles.tile_of_string "?" with
   | Ok tile -> assert (Tiles.int_of_tile tile = 37)
   | Error _ -> failwith "Failed to parse ?");
  (* Test valid int conversions *)
  (match Tiles.tile_of_int 0 with
   | Ok tile -> assert (Tiles.int_of_tile tile = 0)
   | Error _ -> failwith "Failed to convert 0");
  (match Tiles.tile_of_int 36 with
   | Ok tile -> assert (Tiles.int_of_tile tile = 36)
   | Error _ -> failwith "Failed to convert 36");
  (match Tiles.tile_of_int 37 with
   | Ok tile -> assert (Tiles.int_of_tile tile = 37)
   | Error _ -> failwith "Failed to convert 37");
  (* Test invalid conversions *)
  (match Tiles.tile_of_string "" with
   | Ok _ -> failwith "Should reject empty string"
   | Error _ -> ());
  (match Tiles.tile_of_string "0s" with
   | Ok _ -> failwith "Should reject 0s"
   | Error _ -> ());
  (match Tiles.tile_of_string "!" with
   | Ok _ -> failwith "Should reject !"
   | Error _ -> ());
  (match Tiles.tile_of_int 38 with
   | Ok _ -> failwith "Should reject 38"
   | Error _ -> ());
  (match Tiles.tile_of_int 255 with
   | Ok _ -> failwith "Should reject 255"
   | Error _ -> ());
  (* Test tile to string round-trip *)
  let tile1 = Tiles.tile_of_int_exn 0 in
  assert (Tiles.string_of_tile tile1 = "1m");
  let tile2 = Tiles.tile_of_int_exn 27 in
  assert (Tiles.string_of_tile tile2 = "E");
  let tile3 = Tiles.tile_of_int_exn 34 in
  assert (Tiles.string_of_tile tile3 = "5mr");
  Printf.printf "  Tile conversions passed\n"
;;

(** Test tile properties (aka, jihai, yaokyuu, next/prev) *)
let test_properties () =
  Printf.printf "Testing tile properties...\n";
  (* Test aka (red fives) *)
  let tile_5mr = Tiles.tile_of_int_exn 34 in
  assert (Tiles.is_aka tile_5mr);
  assert (Tiles.int_of_tile (Tiles.deaka tile_5mr) = 4);
  let tile_5m = Tiles.tile_of_int_exn 4 in
  assert (not (Tiles.is_aka tile_5m));
  assert (Tiles.int_of_tile (Tiles.akaize tile_5m) = 34);
  (* Test jihai (honor tiles) *)
  assert (Tiles.is_jihai (Tiles.tile_of_int_exn 27));
  (* E *)
  assert (not (Tiles.is_jihai (Tiles.tile_of_int_exn 0)));
  (* 1m *)

  (* Test yaokyuu (terminals and honors) *)
  assert (Tiles.is_yaokyuu (Tiles.tile_of_int_exn 0));
  (* 1m *)
  assert (Tiles.is_yaokyuu (Tiles.tile_of_int_exn 8));
  (* 9m *)
  assert (Tiles.is_yaokyuu (Tiles.tile_of_int_exn 27));
  (* E *)
  assert (not (Tiles.is_yaokyuu (Tiles.tile_of_int_exn 4)));
  (* 5m *)

  (* Test next/prev round-trip for all tiles *)
  for i = 0 to 36 do
    let tile = Tiles.tile_of_int_exn i in
    let tile_deaka = Tiles.deaka tile in
    assert (Tiles.prev (Tiles.next tile) = tile_deaka);
    assert (Tiles.next (Tiles.prev tile) = tile_deaka)
  done;
  Printf.printf "  Tile properties passed\n"
;;

(** Main test runner *)
let () =
  Printf.printf "\n";
  Printf.printf "Tile Tests\n";
  Printf.printf "==========\n\n";
  test_conversions ();
  test_properties ();
  Printf.printf "\n";
  Printf.printf "==========\n";
  Printf.printf "All Tile Tests Completed!\n";
  Printf.printf "==========\n\n"
;;
