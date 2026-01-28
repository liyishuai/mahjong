open Base

(* Win cache for checking winning hands and decompositions *)

(* Build abstract hand key matching C++ CreateAbstructHand *)
let create_abstruct_hand (counts : int array) : string =
  let buf = Buffer.create 32 in
  let need_sep = ref true in

  let process_suit start_tile end_tile =
    for i = start_tile to end_tile do
      let c = counts.(i) in
      if c > 0 then begin
        if !need_sep && Buffer.length buf > 0 then Buffer.add_char buf ',';
        Buffer.add_char buf (Char.of_int_exn (Char.to_int '0' + c));
        need_sep := false
      end else
        need_sep := true
    done;
    need_sep := true
  in

  (* Process suits: manzu, pinzu, souzu *)
  process_suit 0 8;
  process_suit 9 17;
  process_suit 18 26;

  (* Process honors (each separated) *)
  for i = 27 to 33 do
    let c = counts.(i) in
    if c > 0 then begin
      if Buffer.length buf > 0 then Buffer.add_char buf ',';
      Buffer.add_char buf (Char.of_int_exn (Char.to_int '0' + c))
    end
  done;

  Buffer.contents buf

(* Build abstract hand with tile types for pattern reconstruction *)
let create_abstruct_hand_with_tile_types (counts : int array)
    : string * Tile.TileType.t array =
  let buf = Buffer.create 32 in
  let need_sep = ref true in
  let tile_types = ref [] in

  let process_suit start_tile end_tile =
    for i = start_tile to end_tile do
      let c = counts.(i) in
      if c > 0 then begin
        if !need_sep && Buffer.length buf > 0 then Buffer.add_char buf ',';
        Buffer.add_char buf (Char.of_int_exn (Char.to_int '0' + c));
        tile_types := Option.value_exn (Tile.TileType.of_int i) :: !tile_types;
        need_sep := false
      end else
        need_sep := true
    done;
    need_sep := true
  in

  (* Process suits: manzu, pinzu, souzu *)
  process_suit 0 8;
  process_suit 9 17;
  process_suit 18 26;

  (* Process honors (each separated) *)
  for i = 27 to 33 do
    let c = counts.(i) in
    if c > 0 then begin
      if Buffer.length buf > 0 then Buffer.add_char buf ',';
      Buffer.add_char buf (Char.of_int_exn (Char.to_int '0' + c));
      tile_types := Option.value_exn (Tile.TileType.of_int i) :: !tile_types
    end
  done;

  (Buffer.contents buf, Array.of_list (List.rev !tile_types))

(* Cache type: abstruct_hand -> list of patterns (int list list list) *)
type cache_t = (string, int list list list) Stdlib.Hashtbl.t

(* Mutable global cache - initialized from compiled data *)
let win_cache : cache_t option ref = ref None
let tenpai_cache : (string, unit) Stdlib.Hashtbl.t option ref = ref None

(* Initialize from embedded data *)
let initialize_cache () =
  if Option.is_none !win_cache then begin
    win_cache := Some (Win_cache_data.create_win_cache ());
    tenpai_cache := Some (Win_cache_data.create_tenpai_cache ())
  end

(* For backwards compatibility *)
let load_cache _path = initialize_cache (); true
let load_cache_from _path = initialize_cache ()

(* Check if a tile is yaocyu (terminal or honor) *)
let is_yaocyu (tile : Tile.TileType.t) : bool =
  let idx = Tile.TileType.to_int tile in
  match idx with
  | 0 | 8 | 9 | 17 | 18 | 26 -> true  (* 1, 9 of each suit *)
  | i when i >= 27 && i <= 33 -> true  (* All honors *)
  | _ -> false

(* Has: Check if hand is a winning hand *)
let has (counts : int array) : bool =
  match !win_cache with
  | None -> false
  | Some cache ->
    let key = create_abstruct_hand counts in
    if Stdlib.Hashtbl.mem cache key then true
    else begin
      (* Check for kokushi musou (国士無双) - 13 orphans *)
      let all_yaocyu = ref true in
      let has_all_yaocyu = ref true in
      for i = 0 to 33 do
        if counts.(i) > 0 then begin
          let tile = Option.value_exn (Tile.TileType.of_int i) in
          if not (is_yaocyu tile) then all_yaocyu := false
        end;
        let tile = Option.value_exn (Tile.TileType.of_int i) in
        if is_yaocyu tile && counts.(i) = 0 then has_all_yaocyu := false
      done;
      !all_yaocyu && !has_all_yaocyu
    end

(* Tenpai: Check if hand is ready (1 tile away from winning) *)
let tenpai (counts : int array) : bool =
  match !tenpai_cache with
  | None -> false
  | Some cache ->
    let key = create_abstruct_hand counts in
    if Stdlib.Hashtbl.mem cache key then true
    else begin
      (* Check for kokushi tenpai - needs 12+ different yaocyu tiles *)
      let yaocyu_types = ref 0 in
      let has_non_yaocyu = ref false in
      for i = 0 to 33 do
        if counts.(i) > 0 then begin
          let tile = Option.value_exn (Tile.TileType.of_int i) in
          if is_yaocyu tile then Int.incr yaocyu_types
          else has_non_yaocyu := true
        end
      done;
      (not !has_non_yaocyu) && !yaocyu_types >= 12
    end

(* Machi: Get waiting tiles *)
let machi (counts : int array) : Tile.TileType.t list =
  if not (tenpai counts) then []
  else begin
    let waiting = ref [] in
    for i = 0 to 33 do
      counts.(i) <- counts.(i) + 1;
      if has counts then
        waiting := Option.value_exn (Tile.TileType.of_int i) :: !waiting;
      counts.(i) <- counts.(i) - 1
    done;
    List.rev !waiting
  end

(* SetAndHeads: Decompose hand into sets (mentsu) and heads (jantou) *)
type tile_count = (Tile.TileType.t, int) Hashtbl.t

let sets_and_heads (counts : int array)
    : (tile_count list * tile_count list) list =
  match !win_cache with
  | None -> []
  | Some cache ->
    let (abstruct_hand, tile_types) =
      create_abstruct_hand_with_tile_types counts in

    match Stdlib.Hashtbl.find_opt cache abstruct_hand with
    | None -> []
    | Some pattern_list ->
      List.map pattern_list ~f:(fun pattern ->
        let sets = ref [] in
        let heads = ref [] in

        List.iter pattern ~f:(fun block ->
          let count = Hashtbl.create (module Tile.TileType) in
          List.iter block ~f:(fun tile_idx ->
            let tile = tile_types.(tile_idx) in
            Hashtbl.update count tile ~f:(function
              | None -> 1
              | Some n -> n + 1
            )
          );
          if List.length block = 3 then
            sets := count :: !sets
          else
            heads := count :: !heads
        );

        (List.rev !sets, List.rev !heads)
      )

(* Ensure cache is loaded *)
let ensure_loaded () = initialize_cache ()
