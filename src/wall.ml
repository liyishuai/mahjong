open Base

type t =
  { round : int
  ; game_seed : int list
  ; tiles : Tile.t array
  ; mutable draw_ix : int
  ; mutable num_kan_draw : int
  ; mutable num_kan_dora : int
  }

(* Tile count for deck creation *)
let all_tiles_count = 136

(* Create all 136 tiles in sorted order *)
let create_all_tiles () = Array.init all_tiles_count ~f:(fun i -> i)

(* Shuffle tiles using Array.shuffle *)
let shuffle_tiles ~seed tiles =
  Stdlib.Random.full_init seed;
  let result = Array.copy tiles in
  Stdlib.Array.shuffle ~rand:Stdlib.Random.int result;
  result
;;

(* Create from round, honba, and game_seed *)
let create ~round ~honba ~game_seed =
  let tiles = create_all_tiles () in
  let seed = Array.of_list (round :: honba :: game_seed) in
  let tiles = shuffle_tiles ~seed tiles in
  { round; game_seed; tiles; draw_ix = 52; num_kan_draw = 0; num_kan_dora = 0 }
;;

(* Create from round and tiles (for replay from human data) *)
let from_tiles ~round tiles =
  assert (List.length tiles = all_tiles_count);
  { round
  ; game_seed = []
  ; (* Special seed for replay *)
    tiles = Array.of_list tiles
  ; draw_ix = 52
  ; num_kan_draw = 0
  ; num_kan_dora = 0
  }
;;

(* Get initial hand tiles (13 tiles) for a player *)
let initial_hand_tiles t ~pos =
  let pos_ix = Action.AbsolutePos.to_int pos in
  let tiles = ref [] in
  let base = ((pos_ix % 4) - (t.round % 4) + 4) % 4 * 4 in
  let ix = ref base in
  (* Three rounds of 4 tiles each *)
  for _i = 0 to 2 do
    for _j = 0 to 3 do
      tiles := t.tiles.(!ix) :: !tiles;
      ix := !ix + 1
    done;
    ix := !ix + 12
  done;
  (* Final 13th tile *)
  let ix_final = (((pos_ix % 4) - (t.round % 4) + 4) % 4) + 48 in
  tiles := t.tiles.(ix_final) :: !tiles;
  List.rev !tiles
;;

(* Check if normal draws are available *)
let has_draw_left t =
  abs (t.num_kan_draw - t.num_kan_dora) <= 1 && t.draw_ix + t.num_kan_draw < 122
;;

(* Check if next player draw is available *)
let has_next_draw_left t = t.draw_ix + t.num_kan_draw <= 118

(* Normal draw *)
let draw t =
  assert (has_draw_left t);
  let tile = t.tiles.(t.draw_ix) in
  t.draw_ix <- t.draw_ix + 1;
  tile
;;

(* Kan draw - uses special positions at the end *)
let kan_draw t =
  assert (abs (t.num_kan_draw - t.num_kan_dora) <= 1);
  assert (t.num_kan_draw <= 3);
  (* Kan draw indices: [134, 135, 132, 133] *)
  let kan_indices = [| 134; 135; 132; 133 |] in
  let tile = t.tiles.(kan_indices.(t.num_kan_draw)) in
  t.num_kan_draw <- t.num_kan_draw + 1;
  tile
;;

(* Add kan dora indicator - flip new dora *)
let add_kan_dora t =
  assert (abs (t.num_kan_draw - t.num_kan_dora) <= 1);
  assert (t.num_kan_dora <= 3);
  t.num_kan_dora <- t.num_kan_dora + 1;
  let kan_dora_indicator = t.tiles.(130 - (2 * t.num_kan_dora)) in
  let ura_kan_dora_indicator = t.tiles.(131 - (2 * t.num_kan_dora)) in
  kan_dora_indicator, ura_kan_dora_indicator
;;

(* Get dora indicators *)
let dora_indicators t =
  assert (abs (t.num_kan_draw - t.num_kan_dora) <= 1);
  let indicators = ref [ t.tiles.(130) ] in
  for i = 0 to t.num_kan_dora - 1 do
    indicators := t.tiles.(128 - (2 * i)) :: !indicators
  done;
  List.rev !indicators
;;

(* Get ura dora indicators *)
let ura_dora_indicators t =
  assert (abs (t.num_kan_draw - t.num_kan_dora) <= 1);
  let indicators = ref [ t.tiles.(131) ] in
  for i = 0 to t.num_kan_dora - 1 do
    indicators := t.tiles.(129 - (2 * i)) :: !indicators
  done;
  List.rev !indicators
;;

(* Convert dora indicator to dora tile type *)
let indicator_to_dora tile_type =
  match tile_type with
  | Tile.TileType.M9 -> Tile.TileType.M1
  | Tile.TileType.P9 -> Tile.TileType.P1
  | Tile.TileType.S9 -> Tile.TileType.S1
  | Tile.TileType.NW -> Tile.TileType.EW
  | Tile.TileType.RD -> Tile.TileType.WD
  | _ ->
    let n = Tile.TileType.to_int tile_type in
    (match Tile.TileType.of_int (n + 1) with
     | Some tt -> tt
     | None -> failwith "Invalid tile type for dora indicator")
;;

(* Get dora count as an association list *)
let dora_count t =
  let indicators = dora_indicators t in
  (* Count occurrences of each dora type *)
  let rec add_count acc tile_type =
    match acc with
    | [] -> [ tile_type, 1 ]
    | (tt, count) :: rest when Tile.TileType.equal tt tile_type -> (tt, count + 1) :: rest
    | (tt, count) :: rest -> (tt, count) :: add_count rest tile_type
  in
  List.fold indicators ~init:[] ~f:(fun acc indicator ->
    let dora_type = indicator_to_dora (Tile.type_ indicator) in
    add_count acc dora_type)
;;

(* Get ura dora count as an association list *)
let ura_dora_count t =
  let indicators = ura_dora_indicators t in
  let rec add_count acc tile_type =
    match acc with
    | [] -> [ tile_type, 1 ]
    | (tt, count) :: rest when Tile.TileType.equal tt tile_type -> (tt, count + 1) :: rest
    | (tt, count) :: rest -> (tt, count) :: add_count rest tile_type
  in
  List.fold indicators ~init:[] ~f:(fun acc indicator ->
    let dora_type = indicator_to_dora (Tile.type_ indicator) in
    add_count acc dora_type)
;;

(* Get all tiles (for testing) *)
let tiles t = Array.to_list t.tiles

(* Get game seed *)
let game_seed t = t.game_seed

(* Get number of kan draws *)
let num_kan_draw t = t.num_kan_draw

(* Get number of kan dora *)
let num_kan_dora t = t.num_kan_dora

(* Get round *)
let round t = t.round
