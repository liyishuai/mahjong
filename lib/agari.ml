(** Agari (winning hand) detection using native backtracking algorithm.

    Based on the algorithm by 山岡忠夫 (Yamaoka Tadao)
    Reference: http://hp.vector.co.jp/authors/VA046927/mjscore/mjalgorism.html

    Algorithm:
    1. Extract pair (雀頭) - try each tile with count >= 2
    2. Extract melds in two possible orders:
       - Order 1 (刻子→順子): triplets first, then sequences
       - Order 2 (順子→刻子): sequences first, then triplets
    3. Both orders must be tried because they affect yaku scoring
    4. Special case: seven pairs (chiitoi)
    5. Special case: kokushi (handled separately via shanten)
*)

type agari_result =
  | Normal of
      { fu : int
      ; han : int
      }
  | Yakuman of int

let compare_agari (a : agari_result) (b : agari_result) : int =
  match a, b with
  | Normal { fu = fu1; han = han1 }, Normal { fu = fu2; han = han2 } ->
    let cmp_han = Int.compare han1 han2 in
    if cmp_han = 0 then Int.compare fu1 fu2 else cmp_han
  | Yakuman y1, Yakuman y2 -> Int.compare y1 y2
  | Normal _, Yakuman _ -> -1
  | Yakuman _, Normal _ -> 1
;;

(** Pretty printer for agari_result *)
let pp_agari_result (ppf : Format.formatter) (result : agari_result) : unit =
  match result with
  | Normal { fu; han } -> Format.fprintf ppf "Normal {fu=%d; han=%d}" fu han
  | Yakuman n -> Format.fprintf ppf "Yakuman %d" n
;;

let string_of_agari_result (result : agari_result) : string =
  match result with
  | Normal { fu; han } -> Printf.sprintf "Normal {fu=%d; han=%d}" fu han
  | Yakuman n -> Printf.sprintf "Yakuman %d" n
;;

type yaku =
  | Riichi
  | Double_Riichi
  | Ippatsu
  | Tsumo
  | Tsumo_Ron
  | Iipeikou
  | Ryanpeikou
  | Sanankou
  | Sankantsu
  | Suukantsu
  | Toitoi
  | Sanshoku
  | Shousangen
  | Daisangen
  | Honroto
  | Chinroto
  | Honitsu
  | Chinitsu
  | Haku
  | Hatsu
  | Chun
  | Jikaze
  | Bakaze
  | Akahai
  | Dora
  | Akadora
  | Chuuren
  | Kokushi
  | Suuankou
  | Tsuuiisou
  | Ryuuiisou

type agari_calculator =
  { tehai : int array
    (* 34-element array of tile COUNTS (0-4 each), complete hand including winning tile *)
  ; winning_tile : int (* INDEX (0-33) of which tile is the winning tile *)
  ; bakaze : int
  ; jikaze : int
  ; is_menzen : bool
  ; is_ron : bool
  ; chis : int list (* List of STARTING tiles for chi melds *)
  ; pons : int list (* List of tiles for pon melds *)
  ; minkans : int list (* List of tiles for minkan melds *)
  ; ankans : int list (* List of STARTING tiles for ankan melds *)
  }

type div =
  { pair_idx : int
  ; kotsu_idxs : int array
  ; shuntsu_idxs : int array
  ; has_chitoi : bool
  ; has_chuuren : bool
  ; has_ittsuu : bool
  ; has_ryanpeikou : bool
  ; has_ipeikou : bool
  }

let div_from_int v =
  let pair_idx = (v lsr 6) land 0xf in
  let kotsu_count = v land 0x7 in
  let kotsu_idxs = Array.init kotsu_count (fun i -> (v lsr (10 + (i * 4))) land 0xf) in
  let shuntsu_count = (v lsr 3) land 0x7 in
  let shuntsu_idxs =
    Array.init shuntsu_count (fun i -> (v lsr (10 + ((kotsu_count + i) * 4))) land 0xf)
  in
  let has_chitoi = (v lsr 26) land 1 = 1 in
  let has_chuuren = (v lsr 27) land 1 = 1 in
  let has_ittsuu = (v lsr 28) land 1 = 1 in
  let has_ryanpeikou = (v lsr 29) land 1 = 1 in
  let has_ipeikou = (v lsr 30) land 1 = 1 in
  { pair_idx
  ; kotsu_idxs
  ; shuntsu_idxs
  ; has_chitoi
  ; has_chuuren
  ; has_ittsuu
  ; has_ryanpeikou
  ; has_ipeikou
  }
;;

let get_tile14_and_key tiles34 =
  let tile14 = ref [] in
  let key = ref 0 in
  let bit_idx = ref (-1) in
  let prev_in_hand = ref None in
  for kind = 0 to 2 do
    prev_in_hand := None;
    for num = 0 to 8 do
      let idx = (kind * 9) + num in
      let c = tiles34.(idx) in
      if c > 0
      then (
        tile14 := idx :: !tile14;
        incr bit_idx;
        (match c with
         | 2 ->
           key := !key lor (0b11 lsl !bit_idx);
           bit_idx := !bit_idx + 2
         | 3 ->
           key := !key lor (0b1111 lsl !bit_idx);
           bit_idx := !bit_idx + 4
         | 4 ->
           key := !key lor (0b111111 lsl !bit_idx);
           bit_idx := !bit_idx + 6
         | _ -> ());
        prev_in_hand := Some ())
      else if !prev_in_hand <> None
      then (
        key := !key lor (0b1 lsl !bit_idx);
        incr bit_idx;
        prev_in_hand := None)
    done;
    if !prev_in_hand <> None
    then (
      key := !key lor (0b1 lsl !bit_idx);
      incr bit_idx)
  done;
  for i = Tiles.tile_id_E to Tiles.tile_id_C do
    let c = tiles34.(i) in
    if c > 0
    then (
      tile14 := i :: !tile14;
      incr bit_idx;
      (match c with
       | 2 ->
         key := !key lor (0b11 lsl !bit_idx);
         bit_idx := !bit_idx + 2
       | 3 ->
         key := !key lor (0b1111 lsl !bit_idx);
         bit_idx := !bit_idx + 4
       | 4 ->
         key := !key lor (0b111111 lsl !bit_idx);
         bit_idx := !bit_idx + 6
       | _ -> ());
      key := !key lor (0b1 lsl !bit_idx);
      incr bit_idx)
  done;
  Array.of_list (List.rev !tile14), !key
;;

(** Read a 32-bit little-endian unsigned integer *)
let read_u32_le (ic : in_channel) : int =
  let b0 = input_byte ic in
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)
;;

let agari_table =
  lazy
    ((* Try to find agari.bin in multiple possible locations *)
     let open_file filename =
       try Some (open_in_bin filename) with
       | Sys_error _ -> None
     in
     let ic =
       match
         List.find_map
           open_file
           [ "agari.bin"; "../lib/agari.bin"; "_build/default/lib/agari.bin" ]
       with
       | Some ic -> ic
       | None -> raise (Sys_error "agari.bin: No such file or directory")
     in
     let table = Hashtbl.create 10000 in
     try
       while true do
         let key = read_u32_le ic in
         let v_size = input_byte ic in
         let values = Array.init v_size (fun _ -> read_u32_le ic) in
         let divs = Array.map div_from_int values in
         Hashtbl.add table key divs
       done;
       table
     with
     | End_of_file ->
       close_in ic;
       table)
;;

(** Table-based agari check (default) *)
let is_agari tiles34 =
  let total = Array.fold_left ( + ) 0 tiles34 in
  if total mod 3 <> 2
  then false
  else (
    let _, key = get_tile14_and_key tiles34 in
    Hashtbl.mem (Lazy.force agari_table) key)
;;

(** Table-based divide *)
let divide_tiles_table tiles34 =
  let _, key = get_tile14_and_key tiles34 in
  try Hashtbl.find (Lazy.force agari_table) key with
  | Not_found -> [||]
;;

(** Recursive check if tiles can form valid melds.

    Tries triplets first, then sequences.
    All melds_left melds must be formed and all tiles must be used.
*)
let rec can_form_melds_kotsu_first (tiles : int array) (melds_left : int) : bool =
  if melds_left = 0
  then Array.for_all (fun count -> count = 0) tiles
  else (
    let found = ref false in
    for idx = 0 to 33 do
      if (not !found) && tiles.(idx) > 0
      then (
        (* Try kan/ankan (quad) - takes 4 tiles as one meld *)
        if tiles.(idx) >= 4
        then (
          tiles.(idx) <- tiles.(idx) - 4;
          if can_form_melds_kotsu_first tiles (melds_left - 1) then found := true;
          tiles.(idx) <- tiles.(idx) + 4);
        (* Try triplet (刻子) *)
        if (not !found) && tiles.(idx) >= 3
        then (
          tiles.(idx) <- tiles.(idx) - 3;
          if can_form_melds_kotsu_first tiles (melds_left - 1) then found := true;
          tiles.(idx) <- tiles.(idx) + 3);
        (* Try sequence (順子) - only for numbered suits *)
        if (not !found) && idx <= 24
        then (
          let rank = idx mod 9 in
          if rank <= 6
          then (
            let idx2 = idx + 1 in
            let idx3 = idx + 2 in
            if tiles.(idx2) > 0 && tiles.(idx3) > 0
            then (
              tiles.(idx) <- tiles.(idx) - 1;
              tiles.(idx2) <- tiles.(idx2) - 1;
              tiles.(idx3) <- tiles.(idx3) - 1;
              if can_form_melds_kotsu_first tiles (melds_left - 1) then found := true;
              tiles.(idx) <- tiles.(idx) + 1;
              tiles.(idx2) <- tiles.(idx2) + 1;
              tiles.(idx3) <- tiles.(idx3) + 1))))
    done;
    !found)
;;

(** Recursive check if tiles can form valid melds.

    Tries sequences first, then triplets.
    All melds_left melds must be formed and all tiles must be used.
*)
let rec can_form_melds_shuntsu_first (tiles : int array) (melds_left : int) : bool =
  if melds_left = 0
  then Array.for_all (fun count -> count = 0) tiles
  else (
    let found = ref false in
    for idx = 0 to 33 do
      if (not !found) && tiles.(idx) > 0
      then (
        (* Try sequence (順子) - only for numbered suits *)
        if idx <= 24
        then (
          let rank = idx mod 9 in
          if rank <= 6
          then (
            let idx2 = idx + 1 in
            let idx3 = idx + 2 in
            if tiles.(idx2) > 0 && tiles.(idx3) > 0
            then (
              tiles.(idx) <- tiles.(idx) - 1;
              tiles.(idx2) <- tiles.(idx2) - 1;
              tiles.(idx3) <- tiles.(idx3) - 1;
              if can_form_melds_shuntsu_first tiles (melds_left - 1) then found := true;
              tiles.(idx) <- tiles.(idx) + 1;
              tiles.(idx2) <- tiles.(idx2) + 1;
              tiles.(idx3) <- tiles.(idx3) + 1)));
        (* Try kan/ankan (quad) - takes 4 tiles as one meld *)
        if (not !found) && tiles.(idx) >= 4
        then (
          tiles.(idx) <- tiles.(idx) - 4;
          if can_form_melds_shuntsu_first tiles (melds_left - 1) then found := true;
          tiles.(idx) <- tiles.(idx) + 4);
        (* Try triplet (刻子) *)
        if (not !found) && tiles.(idx) >= 3
        then (
          tiles.(idx) <- tiles.(idx) - 3;
          if can_form_melds_shuntsu_first tiles (melds_left - 1) then found := true;
          tiles.(idx) <- tiles.(idx) + 3))
    done;
    !found)
;;

(** Check for seven pairs (chiitoi) *)
let is_seven_pairs (tiles34 : int array) : bool =
  if Array.fold_left ( + ) 0 tiles34 <> 14
  then false
  else (
    let pair_count = ref 0 in
    Array.iter (fun count -> if count >= 2 then incr pair_count) tiles34;
    !pair_count = 7)
;;

(** Check if a 3n+2 hand can be divided into melds (reference implementation).

    Implements the backtracking algorithm from mjalgorism.html:
    1. Try seven pairs special form first
    2. Try extracting each tile type as the pair
    3. For remaining tiles after pair, try both meld extraction orders:
       - Order 1: triplets first, then sequences
       - Order 2: sequences first, then triplets
    4. Return true if any valid division found
*)
let is_agari_ref (tiles34 : int array) : bool =
  let total = Array.fold_left ( + ) 0 tiles34 in
  (* Must be 3n+2 tiles *)
  if total mod 3 <> 2
  then false (* Seven pairs special case *)
  else if is_seven_pairs tiles34
  then true
  (* Normal form: extract pair, then melds *)
  else (
    let tiles = Array.copy tiles34 in
    let found = ref false in
    (* Try each tile type as the pair *)
    for pair_idx = 0 to 33 do
      if (not !found) && tiles.(pair_idx) >= 2
      then (
        tiles.(pair_idx) <- tiles.(pair_idx) - 2;
        let melds_needed = (total - 2) / 3 in
        (* Try both meld extraction orders *)
        let valid =
          can_form_melds_kotsu_first tiles melds_needed
          || can_form_melds_shuntsu_first tiles melds_needed
        in
        if valid then found := true;
        tiles.(pair_idx) <- tiles.(pair_idx) + 2)
    done;
    !found)
;;

(** Helper to check if tile is a wind (E, S, W, N) *)
let is_wind (tile : int) : bool = tile >= Tiles.tile_id_E && tile <= Tiles.tile_id_N

(** Helper to check if tile is a dragon (P, F, C) *)
let is_dragon (tile : int) : bool = tile >= Tiles.tile_id_P && tile <= Tiles.tile_id_C

(** Check if the winning tile makes a minkou instead of completing a shuntsu.
    This happens in ambiguous patterns like 45556 + 5 where the 5 could either:
    - Complete the shuntsu 456 + 5 (preferred)
    - Turn ankou 555 into minkou 55 + 5

    We prefer the shuntsu interpretation unless there's no shuntsu that can
    contain the winning tile.
*)
let winning_tile_makes_minkou (calc : agari_calculator) (div : div) (tile14 : int array)
  : bool
  =
  if not calc.is_ron
  then false
  else (
    (* Check if winning tile is in any ankou *)
    let has_ankou_with_winning_tile =
      Array.exists
        (fun k -> k = calc.winning_tile)
        (Array.map
           (fun idx ->
              if idx >= Array.length tile14
              then (
                Printf.eprintf
                  "winning_tile_makes_minkou: kotsu_idx %d out of bounds (len %d)\n"
                  idx
                  (Array.length tile14);
                flush stderr;
                failwith "kotsu_idx out of bounds");
              tile14.(idx))
           div.kotsu_idxs)
    in
    if not has_ankou_with_winning_tile
    then false
    else (
      (* Check if there's a shuntsu that can cover the winning tile *)
      let winning_tile = calc.winning_tile in
      if winning_tile >= Tiles.tile_id_E
      then true (* honors can't form shuntsu *)
      else (
        let kind = winning_tile / 9 in
        let num = winning_tile mod 9 in
        let low = (kind * 9) + max 0 (num - 2) in
        let high = (kind * 9) + min 6 num in
        (* Check if any shuntsu contains the winning tile *)
        not
          (Array.exists
             (fun s -> s >= low && s <= high)
             (Array.map
                (fun idx ->
                   if idx >= Array.length tile14
                   then (
                     Printf.eprintf
                       "winning_tile_makes_minkou: shuntsu_idx %d out of bounds (len %d)\n"
                       idx
                       (Array.length tile14);
                     flush stderr;
                     failwith "shuntsu_idx out of bounds");
                   tile14.(idx))
                div.shuntsu_idxs)))))
;;

(** Check for pinfu (all sequences, simple pair, proper wait) *)
let check_pinfu (calc : agari_calculator) (div : div) (tile14 : int array) : bool =
  (* Must have 4 shuntsu and no shuntsu from open calls *)
  Array.length div.shuntsu_idxs = 4
  && calc.chis = []
  &&
  (* Pair must not be honors or winds *)
  let pair_tile =
    if div.pair_idx >= Array.length tile14
    then (
      Printf.eprintf
        "check_pinfu: pair_idx %d out of bounds (len %d)\n"
        div.pair_idx
        (Array.length tile14);
      flush stderr;
      failwith "pair_idx out of bounds");
    tile14.(div.pair_idx)
  in
  (not (Tiles.is_jihai pair_tile))
  && pair_tile <> calc.bakaze
  && pair_tile <> calc.jikaze
  &&
  (* Winning tile must complete a shuntsu with ryanmen wait *)
  Array.exists
    (fun s ->
       let num = (s mod 9) + 1 in
       (* 1-indexed like Rust *)
       (* For ryanmen: can wait for first tile or last tile *)
       (num <= 6 && s = calc.winning_tile) || (num >= 2 && s + 2 = calc.winning_tile))
    (Array.map
       (fun idx ->
          if idx >= Array.length tile14
          then (
            Printf.eprintf
              "check_pinfu: shuntsu_idx %d out of bounds (len %d)\n"
              idx
              (Array.length tile14);
            flush stderr;
            failwith "shuntsu_idx out of bounds");
          tile14.(idx))
       div.shuntsu_idxs)
;;

(** Calculate fu for a specific division *)
let calc_fu_for_div (calc : agari_calculator) (div : div) (tile14 : int array) : int =
  try
    (* Chitoi is always 25 fu *)
    if div.has_chitoi
    then 25
    else (
      let fu = ref 20 in
      (* Fu for menzen kotsu *)
      let makes_minkou = winning_tile_makes_minkou calc div tile14 in
      Array.iter
        (fun k ->
           let is_minkou = makes_minkou && k = calc.winning_tile in
           match is_minkou, Tiles.is_yaokyuu k with
           | false, true -> fu := !fu + 8 (* concealed honor triplet *)
           | false, false | true, true ->
             fu := !fu + 4 (* concealed simple or open honor *)
           | true, false -> fu := !fu + 2 (* open simple triplet *))
        (Array.map (fun idx -> tile14.(idx)) div.kotsu_idxs);
      (* Fu for open pons *)
      List.iter
        (fun k -> if Tiles.is_yaokyuu k then fu := !fu + 4 else fu := !fu + 2)
        calc.pons;
      (* Fu for ankans *)
      List.iter
        (fun k -> if Tiles.is_yaokyuu k then fu := !fu + 32 else fu := !fu + 16)
        calc.ankans;
      (* Fu for minkans *)
      List.iter
        (fun k -> if Tiles.is_yaokyuu k then fu := !fu + 16 else fu := !fu + 8)
        calc.minkans;
      (* Fu for pair *)
      let pair_tile = tile14.(div.pair_idx) in
      if is_dragon pair_tile
      then fu := !fu + 2
      else (
        if pair_tile = calc.bakaze then fu := !fu + 2;
        if pair_tile = calc.jikaze then fu := !fu + 2);
      (* Base case handling *)
      if !fu = 20
      then (
        (* No fu from melds yet *)
        let has_pinfu = check_pinfu calc div tile14 in
        if not calc.is_menzen
        then 30 (* open with no fu = 30 *)
        else if has_pinfu
        then if calc.is_ron then 30 else 20 (* pinfu ron = 30, pinfu tsumo = 20 *)
        else if calc.is_ron
        then 40 (* closed ron with no sequences = 40 *)
        else 30 (* closed tsumo with no sequences = 30 *))
      else (
        (* Add tsumo/ron bonus *)
        if not calc.is_ron
        then fu := !fu + 2 (* tsumo bonus *)
        else if calc.is_menzen
        then fu := !fu + 10 (* menzen ron bonus *)
        else ();
        (* Add wait fu if not pinfu *)
        if not (check_pinfu calc div tile14)
        then
          if pair_tile = calc.winning_tile
          then fu := !fu + 2 (* tanki wait *)
          else (
            (* Check for kanchan/penchan wait *)
            let is_kanchan_penchan =
              Array.exists
                (fun s ->
                   s + 1 = calc.winning_tile
                   (* middle wait *)
                   || (s mod 9 = 0 && s + 2 = calc.winning_tile)
                   ||
                   (* left penchan *)
                   (s mod 9 = 6 && s = calc.winning_tile)
                   (* right penchan *))
                (Array.map (fun idx -> tile14.(idx)) div.shuntsu_idxs)
            in
            if is_kanchan_penchan then fu := !fu + 2);
        (* Round up to nearest 10 *)
        ((!fu - 1) / 10 * 10) + 10))
  with
  | Invalid_argument msg ->
    Printf.eprintf "calc_fu_for_div crashed: %s\n" msg;
    Printf.eprintf
      "Div: pair_idx=%d, kotsu=[%s], shuntsu=[%s]\n"
      div.pair_idx
      (String.concat ";" (List.map string_of_int (Array.to_list div.kotsu_idxs)))
      (String.concat ";" (List.map string_of_int (Array.to_list div.shuntsu_idxs)));
    Printf.eprintf
      "Tile14 (len=%d): [%s]\n"
      (Array.length tile14)
      (String.concat ";" (List.map string_of_int (Array.to_list tile14)));
    flush stderr;
    raise (Invalid_argument msg)
;;

(** [check_agari tiles34] checks if the hand is winning and returns fu/han.

    Returns the maximum fu/han among all possible divisions.
    Uses minimal context - assumes menzen ron with no wind/dragon bonuses. *)
let check_agari (tiles34 : int array) : agari_result option =
  if not (is_agari tiles34)
  then None
  else (
    let divs = divide_tiles_table tiles34 in
    if Array.length divs = 0
    then None
    else (
      (* Create a minimal calculator context *)
      let tile14, _key = get_tile14_and_key tiles34 in
      (* Find the winning tile - for now, use the first tile that appears *)
      (* In a real implementation, this would be passed as a parameter *)
      let winning_tile = if Array.length tile14 > 0 then tile14.(0) else 0 in
      let calc =
        { tehai = tiles34
        ; winning_tile
        ; bakaze = 27
        ; (* E - shouldn't match anything *)
          jikaze = 27
        ; (* E - shouldn't match anything *)
          is_menzen = true
        ; is_ron = true
        ; chis = []
        ; pons = []
        ; minkans = []
        ; ankans = []
        }
      in
      (* Find best result across all divisions *)
      let best_result = ref None in
      Array.iter
        (fun div ->
           (* Calculate fu for this division *)
           let fu = calc_fu_for_div calc div tile14 in
           (* Calculate basic han from div flags *)
           let han = ref 0 in
           if div.has_chitoi then han := !han + 2;
           if div.has_ryanpeikou
           then han := !han + 3
           else if div.has_ipeikou
           then han := !han + 1;
           if div.has_chuuren then han := !han + 13 (* yakuman *);
           if div.has_ittsuu then han := !han + 2;
           (* Check for basic yaku *)
           (* ... (this would need the full yaku detection) *)
           if !han > 0
           then (
             let result = Normal { fu; han = !han } in
             best_result
             := match !best_result with
                | None -> Some result
                | Some r1 -> Some (if compare_agari r1 result >= 0 then r1 else result)))
        divs;
      !best_result))
;;

(** [search_yakus calc] searches for yaku given full game context.

    Returns the maximum han/yakuman result among all possible divisions. *)
let search_yakus (calc : agari_calculator) : agari_result option =
  try
    (* Check kokushi (thirteen orphans) first - special pattern *)
    if calc.is_menzen && Shanten.calc_kokushi calc.tehai < 0
    then Some (Yakuman 1)
    else (
      let divs = divide_tiles_table calc.tehai in
      if Array.length divs = 0
      then None
      else (
        (* For each division, check for yakus and return the best result *)
        let best_result = ref None in
        Array.iter
          (fun div ->
             (* Check special patterns from div flags *)
             let has_chitoi = div.has_chitoi in
             let has_chuuren = div.has_chuuren in
             (* Skip if menzen requirement violated *)
             if (not calc.is_menzen) && has_chitoi
             then ()
             else (
               let han = ref 0 in
               let yakuman = ref 0 in
               (* Chuuren poutou (nine gates) - yakuman *)
               if has_chuuren && calc.is_menzen then yakuman := !yakuman + 1;
               (* Chitoi (seven pairs) *)
               if has_chitoi then han := !han + 2;
               (* Ryanpeikou (two consecutive runs) *)
               if div.has_ryanpeikou && calc.is_menzen then han := !han + 3;
               (* Analyze tiles for other yaku *)
               let tile14, _key = get_tile14_and_key calc.tehai in
               (* Get pair tile *)
               if div.pair_idx >= Array.length tile14
               then (
                 Printf.eprintf
                   "search_yakus: pair_idx %d out of bounds (len %d)\n"
                   div.pair_idx
                   (Array.length tile14);
                 flush stderr;
                 failwith "pair_idx out of bounds");
               let pair_tile = tile14.(div.pair_idx) in
               (* Build lists of melds *)
               let menzen_kotsu =
                 Array.map
                   (fun idx ->
                      if idx >= Array.length tile14
                      then (
                        Printf.eprintf
                          "search_yakus: kotsu_idx %d out of bounds (len %d)\n"
                          idx
                          (Array.length tile14);
                        flush stderr;
                        failwith "kotsu_idx out of bounds");
                      tile14.(idx))
                   div.kotsu_idxs
               in
               let menzen_shuntsu =
                 Array.map
                   (fun idx ->
                      if idx >= Array.length tile14
                      then (
                        Printf.eprintf
                          "search_yakus: shuntsu_idx %d out of bounds (len %d)\n"
                          idx
                          (Array.length tile14);
                        flush stderr;
                        failwith "shuntsu_idx out of bounds");
                      tile14.(idx))
                   div.shuntsu_idxs
               in
               (* Check pinfu (all sequences) *)
               let has_pinfu = check_pinfu calc div tile14 in
               if has_pinfu then han := !han + 1;
               (* All kotsu/kantsu *)
               let all_kotsu_kantsu =
                 Array.to_list menzen_kotsu @ calc.pons @ calc.minkans @ calc.ankans
               in
               (* All shuntsu *)
               let all_shuntsu = Array.to_list menzen_shuntsu @ calc.chis in
               (* Check tanyao (all simples) *)
               let check_tanyao () =
                 let is_simple tile =
                   let kind = tile / 9 in
                   let num = tile mod 9 in
                   kind < 3 && num > 0 && num < 8
                 in
                 if has_chitoi
                 then Array.for_all is_simple tile14
                 else
                   List.for_all (fun t -> is_simple t && is_simple (t + 2)) all_shuntsu
                   && List.for_all is_simple (all_kotsu_kantsu @ [ pair_tile ])
               in
               if check_tanyao () then han := !han + 1;
               (* Check toitoi (all triplets) *)
               let has_toitoi =
                 (not has_chitoi) && Array.length menzen_shuntsu = 0 && calc.chis = []
               in
               if has_toitoi then han := !han + 2;
               (* Check iipeikou (one consecutive run) *)
               if not has_chitoi
               then
                 if div.has_ipeikou && calc.is_menzen && not div.has_ryanpeikou
                 then
                   han := !han + 1
                   (* Fallback check for iipeikou when ankans are present *)
                 else if
                   calc.ankans <> [] && calc.is_menzen && Array.length menzen_shuntsu >= 2
                 then (
                   let shuntsu_marks = Array.make 3 0 in
                   let has_ipeikou =
                     Array.exists
                       (fun t ->
                          let kind = t / 9 in
                          let num = t mod 9 in
                          if kind < 3
                          then (
                            let mark = shuntsu_marks.(kind) in
                            if (mark lsr num) land 1 = 1
                            then true
                            else (
                              shuntsu_marks.(kind) <- mark lor (1 lsl num);
                              false))
                          else false)
                       menzen_shuntsu
                   in
                   if has_ipeikou then han := !han + 1);
               (* Check ittsuu (pure straight 123-456-789) *)
               if not has_chitoi
               then
                 if calc.is_menzen && div.has_ittsuu
                 then han := !han + 2
                 else if calc.chis = [] && div.has_ittsuu
                 then han := !han + 1 (* Fallback check for ittsuu *)
                 else if Array.length menzen_shuntsu + List.length calc.chis >= 3
                 then (
                   let kinds = Array.make 3 0 in
                   Array.iter
                     (fun s ->
                        let kind = s / 9 in
                        if kind < 3
                        then (
                          let num = s mod 9 in
                          match num with
                          | 0 -> kinds.(kind) <- kinds.(kind) lor 0b001 (* 123 *)
                          | 3 -> kinds.(kind) <- kinds.(kind) lor 0b010 (* 456 *)
                          | 6 -> kinds.(kind) <- kinds.(kind) lor 0b100 (* 789 *)
                          | _ -> ()))
                     (Array.of_list all_shuntsu);
                   if Array.exists (fun k -> k = 0b111) kinds then han := !han + 1);
               (* Check honitsu/chinitsu (one suit) *)
               let check_isou () =
                 let kinds = ref None in
                 let has_jihai = ref false in
                 let is_single_kind = ref true in
                 let check_tile tile =
                   let kind = tile / 9 in
                   if kind >= 3
                   then has_jihai := true
                   else (
                     match !kinds with
                     | None -> kinds := Some kind
                     | Some k when k <> kind -> is_single_kind := false
                     | _ -> ())
                 in
                 if has_chitoi
                 then Array.iter check_tile tile14
                 else (
                   List.iter check_tile all_shuntsu;
                   List.iter check_tile all_kotsu_kantsu;
                   check_tile pair_tile);
                 !kinds, !has_jihai, !is_single_kind
               in
               let kinds, has_jihai, is_single_kind = check_isou () in
               (match kinds with
                | None ->
                  (* All honors - tsuuiisou *)
                  yakuman := !yakuman + 1
                | Some _ when is_single_kind ->
                  let h = if has_jihai then 2 else 5 in
                  han := !han + h + if calc.is_menzen then 1 else 0
                | _ -> ());
               (* Check sanshoku (three colors) *)
               let check_sanshoku_shuntsu () =
                 if List.length all_shuntsu >= 3
                 then (
                   let counter = Array.make 9 0 in
                   List.iter
                     (fun s ->
                        let kind = s / 9 in
                        let num = s mod 9 in
                        if kind < 3 && num < 9
                        then counter.(num) <- counter.(num) lor (1 lsl kind))
                     all_shuntsu;
                   Array.exists (fun c -> c = 0b111) counter)
                 else false
               in
               let check_sanshoku_kotsu () =
                 let counter = Array.make 9 0 in
                 List.iter
                   (fun k ->
                      let kind = k / 9 in
                      let num = k mod 9 in
                      if kind < 3 then counter.(num) <- counter.(num) lor (1 lsl kind))
                   all_kotsu_kantsu;
                 Array.exists (fun c -> c = 0b111) counter
               in
               if check_sanshoku_shuntsu ()
               then han := !han + if calc.is_menzen then 2 else 1
               else if check_sanshoku_kotsu ()
               then han := !han + 2;
               (* Check sanankou/suuankou - use winning_tile_makes_minkou *)
               let makes_minkou = winning_tile_makes_minkou calc div tile14 in
               let ankou_count =
                 Array.length menzen_kotsu
                 + List.length calc.ankans
                 - if makes_minkou then 1 else 0
               in
               (* Check suuankou (four concealed triplets) - yakuman *)
               if ankou_count = 4
               then
                 yakuman := !yakuman + 1 (* Check sanankou (three concealed triplets) *)
               else if ankou_count = 3
               then han := !han + 2;
               (* Check sankantsu/suukantsu *)
               let kans_count = List.length calc.ankans + List.length calc.minkans in
               if kans_count = 4
               then yakuman := !yakuman + 1
               else if kans_count = 3
               then han := !han + 2;
               (* Check honor yaku (wind/dragon triplets) *)
               let has_jihai_triplets = Array.make 7 false in
               List.iter
                 (fun k ->
                    if k >= 37
                    then (
                      Printf.eprintf
                        "search_yakus: invalid tile %d in all_kotsu_kantsu\n"
                        k;
                      flush stderr);
                    if k >= Tiles.tile_id_E
                    then (
                      let idx = k - Tiles.tile_id_E in
                      if idx >= 7
                      then (
                        Printf.eprintf
                          "search_yakus: jihai idx %d out of bounds (k=%d)\n"
                          idx
                          k;
                        flush stderr;
                        failwith "jihai idx out of bounds");
                      has_jihai_triplets.(idx) <- true))
                 all_kotsu_kantsu;
               (* Bakaze/Jikaze *)
               if calc.bakaze >= Tiles.tile_id_E
               then (
                 let idx = calc.bakaze - Tiles.tile_id_E in
                 if idx >= 7
                 then (
                   Printf.eprintf
                     "search_yakus: bakaze idx %d out of bounds (bakaze=%d)\n"
                     idx
                     calc.bakaze;
                   flush stderr;
                   failwith "bakaze idx out of bounds");
                 if has_jihai_triplets.(idx) then han := !han + 1);
               if calc.jikaze >= Tiles.tile_id_E
               then (
                 let idx = calc.jikaze - Tiles.tile_id_E in
                 if idx >= 7
                 then (
                   Printf.eprintf
                     "search_yakus: jikaze idx %d out of bounds (jikaze=%d)\n"
                     idx
                     calc.jikaze;
                   flush stderr;
                   failwith "jikaze idx out of bounds");
                 if has_jihai_triplets.(idx) then han := !han + 1);
               (* Dragon triplets *)
               let dragons = ref 0 in
               if has_jihai_triplets.(Tiles.tile_id_P - Tiles.tile_id_E)
               then (
                 dragons := !dragons + 1;
                 han := !han + 1);
               (* P *)
               if has_jihai_triplets.(Tiles.tile_id_F - Tiles.tile_id_E)
               then (
                 dragons := !dragons + 1;
                 han := !han + 1);
               (* F *)
               if has_jihai_triplets.(Tiles.tile_id_C - Tiles.tile_id_E)
               then (
                 dragons := !dragons + 1;
                 han := !han + 1);
               (* C *)

               (* Daisangen (three dragons) - yakuman *)
               if !dragons = 3
               then yakuman := !yakuman + 1 (* Shousangen (two dragons + pair) *)
               else if !dragons = 2 && is_dragon pair_tile
               then han := !han + 2;
               (* Big/little four winds *)
               let winds = ref 0 in
               for i = 0 to 3 do
                 if has_jihai_triplets.(i) then incr winds
               done;
               if !winds = 4
               then yakuman := !yakuman + 1
               else if !winds = 3 && is_wind pair_tile
               then yakuman := !yakuman + 1;
               (* Check chanta/junchan (terminals/honors in all melds) *)
               let check_yaokyuu tile =
                 let kind = tile / 9 in
                 let num = tile mod 9 in
                 kind >= 3 || num = 0 || num = 8
               in
               let has_yaokyuu_everywhere =
                 if has_chitoi
                 then Array.for_all check_yaokyuu tile14
                 else
                   List.for_all check_yaokyuu (all_kotsu_kantsu @ [ pair_tile ])
                   && (all_shuntsu = []
                       || List.for_all
                            (fun s ->
                               let num = s mod 9 in
                               num = 0 || num = 6)
                            all_shuntsu)
               in
               if has_yaokyuu_everywhere
               then (
                 let all_shuntsu_yaokyuu =
                   List.for_all
                     (fun s ->
                        let num = s mod 9 in
                        num = 0 || num = 6)
                     all_shuntsu
                 in
                 (* Check if has honors *)
                 let has_any_jihai =
                   List.exists (fun k -> k >= 27) (all_kotsu_kantsu @ [ pair_tile ])
                 in
                 if has_chitoi || has_toitoi
                 then
                   if has_any_jihai
                   then han := !han + 2 (* Honroto *)
                   else yakuman := !yakuman + 1 (* Chinroto *)
                 else if all_shuntsu_yaokyuu
                 then (
                   let h = if has_any_jihai then 1 else 2 in
                   han := !han + h + if calc.is_menzen then 1 else 0));
               (* Determine result - only if we have yaku or yakuman *)
               if !yakuman > 0
               then (
                 let result = Yakuman !yakuman in
                 best_result
                 := match !best_result with
                    | None -> Some result
                    | Some r1 ->
                      Some (if compare_agari r1 result >= 0 then r1 else result))
               else if !han > 0
               then (
                 (* Calculate fu for this division *)
                 let fu = calc_fu_for_div calc div tile14 in
                 let result = Normal { fu; han = !han } in
                 best_result
                 := match !best_result with
                    | None -> Some result
                    | Some r1 ->
                      Some (if compare_agari r1 result >= 0 then r1 else result))))
          divs;
        !best_result))
  with
  | Invalid_argument msg ->
    Printf.eprintf "search_yakus crashed: %s\n" msg;
    flush stderr;
    raise (Invalid_argument msg)
;;

(** [agari calc additional_hans doras] calculates final point result.

    Combines yaku search with additional hans (riichi, tsumo, etc.) and doras.
    Returns None if no yaku and no additional_hans. *)
let agari (calc : agari_calculator) (additional_hans : int) (doras : int)
  : agari_result option
  =
  match search_yakus calc with
  | Some (Normal { fu; han }) -> Some (Normal { fu; han = han + additional_hans + doras })
  | Some (Yakuman n) -> Some (Yakuman n)
  | None ->
    if additional_hans + doras = 0
    then None
    else if additional_hans + doras >= 5
    then Some (Normal { fu = 0; han = additional_hans + doras })
    else (
      (* Calculate fu without yaku *)
      let divs = divide_tiles_table calc.tehai in
      if Array.length divs = 0
      then None
      else (
        (* Calculate maximum fu across all divisions *)
        let tile14, _key = get_tile14_and_key calc.tehai in
        let max_fu =
          Array.fold_left
            (fun max_fu div ->
               let fu = calc_fu_for_div calc div tile14 in
               max max_fu fu)
            0
            divs
        in
        if max_fu = 0
        then None
        else Some (Normal { fu = max_fu; han = additional_hans + doras })))
;;

(** [point result is_oya] calculates point from agari result *)
let point (result : agari_result) (is_oya : bool) : Point.point =
  match result with
  | Normal { fu; han } -> Point.calc is_oya fu han
  | Yakuman n -> Point.yakuman is_oya n
;;

(** Check if ankan is allowed after riichi.
    tehai must already contain the 4th tile.
    If strict is false, it only checks if shanten and waits are preserved (Tenhou rule).
*)
let check_ankan_after_riichi
      (tehai : int array)
      (len_div3 : int)
      (tile : int)
      (strict : bool)
  : bool
  =
  let tile_idx = Tiles.deaka tile in
  if tehai.(tile_idx) <> 4
  then false
  else if tile_idx >= Tiles.tile_id_E
  then true (* Honors can always be ankan'd if we have 4 *)
  else (
    (* Create tehai before drawing the 4th tile (assuming it was drawn) *)
    let tehai_before = Array.copy tehai in
    tehai_before.(tile_idx) <- 3;
    (* Check all waits of the original hand *)
    let valid = ref true in
    for t = 0 to 33 do
      if !valid && tehai_before.(t) < 4
      then (
        tehai_before.(t) <- tehai_before.(t) + 1;
        if Shanten.calc_all tehai_before len_div3 = -1
        then
          (* Found a wait 't' *)
          if t = tile_idx
          then valid := false (* Cannot kan a waited tile *)
          else (
            (* Check if the hand after ankan can also win with the wait tile *)
            let tehai_after = Array.copy tehai in
            tehai_after.(tile_idx) <- 0;
            (* Remove 4 tiles for ankan *)
            tehai_after.(t) <- tehai_after.(t) + 1;
            (* Add winning tile *)

            (* Check if still winning *)
            (* Note: We only check if it is still agari (shanten = -1).
               We assume ankan doesn't change len_div3 for shanten calc purposes here?
               Actually ankan reduces len_div3 by 1. *)
            if Shanten.calc_all tehai_after (len_div3 - 1) <> -1
            then valid := false
            else if strict
            then (
              (* Strict check: compare number of divisions/shapes *)
              let divs_before = divide_tiles_table tehai_before in
              let divs_after = divide_tiles_table tehai_after in
              if Array.length divs_before <> Array.length divs_after then valid := false));
        tehai_before.(t) <- tehai_before.(t) - 1)
    done;
    !valid)
;;
