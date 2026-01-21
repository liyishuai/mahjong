open State
open Tiles

let action_space = 46

let obs_shape version =
  match version with
  | 1 -> 938, 34
  | 2 -> 942, 34
  | 3 -> 934, 34
  | 4 -> 1012, 34
  | _ -> failwith "invalid version"

type obs = {
  features : float array;
  mask : bool array;
}

type context = {
  _state : player_state;
  arr : float array;
  rows : int;
  cols : int;
  mask : bool array;
  mutable idx : int;
  _at_kan_select : bool;
  version : int;
}

let create_context state version at_kan_select =
  let rows, cols = obs_shape version in
  {
    _state = state;
    arr = Array.make (rows * cols) 0.0;
    rows;
    cols;
    mask = Array.make action_space false;
    idx = 0;
    _at_kan_select = at_kan_select;
    version;
  }

let assign_row ctx row_offset col value =
  if ctx.idx + row_offset < ctx.rows && col < ctx.cols then
    ctx.arr.((ctx.idx + row_offset) * ctx.cols + col) <- value

let fill_row ctx row_offset value =
  if ctx.idx + row_offset < ctx.rows then
    for j = 0 to ctx.cols - 1 do
      ctx.arr.((ctx.idx + row_offset) * ctx.cols + j) <- value
    done

let assign_rows ctx row_offset col n value =
  for i = 0 to n - 1 do
    assign_row ctx (row_offset + i) col value
  done

let fill_rows ctx row_offset n value =
  for i = 0 to n - 1 do
    fill_row ctx (row_offset + i) value
  done

type integer_encoder = {
  n : int;
  cap : int;
  one_hot : bool;
  rescale : bool;
  rbf_intervals : int option;
}

let encode_int ctx enc =
  let n = min enc.n enc.cap in
  match ctx.version with
  | 1 ->
      fill_rows ctx 0 n 1.0;
      ctx.idx <- ctx.idx + enc.cap
  | 2 | 3 ->
      if enc.one_hot then (
        fill_row ctx n 1.0;
        ctx.idx <- ctx.idx + enc.cap + 1
      );
      if enc.rescale then (
        let v = float_of_int n /. float_of_int enc.cap in
        fill_row ctx 0 v;
        ctx.idx <- ctx.idx + 1
      );
      (match enc.rbf_intervals with
      | Some intervals ->
          let interval_size = float_of_int enc.cap /. float_of_int intervals in
          for i = 1 to intervals - 1 do
            let x = float_of_int enc.n in
            let mu = float_of_int i *. interval_size in
            let sigma = interval_size in
            let v = exp (-. (x -. mu) ** 2.0 /. (2.0 *. sigma ** 2.0)) in
            fill_row ctx (i - 1) v
          done;
          ctx.idx <- ctx.idx + intervals - 1
      | None -> ())
  | 4 ->
      if enc.one_hot then (
        fill_row ctx n 1.0;
        ctx.idx <- ctx.idx + enc.cap + 1
      );
      if enc.rescale then (
        let v = float_of_int n /. float_of_int enc.cap in
        fill_row ctx 0 v;
        ctx.idx <- ctx.idx + 1
      )
  | _ -> assert false

let encode_tile_set ctx tiles =
  List.iter (fun tile ->
    let tid = deaka tile in
    if tid < ctx.cols then (
      let rec find_row r =
        if r >= 4 then ()
        else if ctx.arr.((ctx.idx + r) * ctx.cols + tid) = 0.0 then
          assign_row ctx r tid 1.0
        else find_row (r + 1)
      in
      find_row 0
    )
  ) tiles;
  ctx.idx <- ctx.idx + 4

let encode_self_kawa ctx item_opt =
  match item_opt with
  | Some item ->
      let tid = deaka item.sutehai.tile in
      assign_row ctx 0 tid 1.0;
      if is_aka item.sutehai.tile then fill_row ctx 1 1.0;
      if item.sutehai.is_tedashi then fill_row ctx 2 1.0;
      if item.sutehai.is_riichi then fill_row ctx 3 1.0;
      ctx.idx <- ctx.idx + 4
  | None ->
      ctx.idx <- ctx.idx + 4

let encode_kawa ctx item_opt =
  match item_opt with
  | Some item ->
      let tid = deaka item.sutehai.tile in
      assign_row ctx 0 tid 1.0;
      if is_aka item.sutehai.tile then fill_row ctx 1 1.0;
      if item.sutehai.is_tedashi then fill_row ctx 2 1.0;
      if item.sutehai.is_riichi then fill_row ctx 3 1.0;
      if item.sutehai.is_dora then fill_row ctx 4 1.0;
      (match item.chi_pon with
      | Some _ -> fill_row ctx 5 1.0
      | None -> ());
      if item.kan <> [] then fill_row ctx 6 1.0;
      ctx.idx <- ctx.idx + 8
  | None ->
      ctx.idx <- ctx.idx + 8

let encode_obs state version at_kan_select =
  let ctx = create_context state version at_kan_select in
  
  (* Tehai *)
  for tid = 0 to 33 do
    let count = state.tehai.(tid) in
    if count > 0 then
      assign_rows ctx 0 tid count 1.0
  done;
  ctx.idx <- ctx.idx + 4;

  (* Akas in hand *)
  for i = 0 to 2 do
    if state.akas_in_hand.(i) then fill_row ctx i 1.0
  done;
  ctx.idx <- ctx.idx + 3;

  (* Scores *)
  for i = 0 to 3 do
    let score = state.scores.(i) in
    let v = float_of_int (max 0 (min score 100000)) /. 100000.0 in
    fill_row ctx 0 v;
    ctx.idx <- ctx.idx + 1;
    (match version with
    | 2 | 3 ->
        encode_int ctx { n = score / 100; cap = 500; one_hot = false; rescale = false; rbf_intervals = Some 10 }
    | 4 ->
        let v2 = float_of_int (max 0 (min score 30000)) /. 30000.0 in
        fill_row ctx 0 v2;
        ctx.idx <- ctx.idx + 1
    | _ -> ())
  done;

  (* Rank *)
  fill_row ctx state.rank 1.0;
  ctx.idx <- ctx.idx + 4;

  (* Kyoku *)
  (match version with
  | 1 -> fill_rows ctx 0 state.kyoku 1.0
  | _ -> fill_row ctx state.kyoku 1.0);
  ctx.idx <- ctx.idx + 4;

  (* Honba and Kyotaku *)
  let cap = if version = 1 || version = 4 then 10 else 6 in
  encode_int ctx { n = state.honba; cap; one_hot = false; rescale = (version = 4); rbf_intervals = Some 3 };
  encode_int ctx { n = state.kyotaku; cap; one_hot = false; rescale = (version = 4); rbf_intervals = Some 3 };

  (* Winds *)
  assign_row ctx 0 (deaka state.bakaze) 1.0;
  assign_row ctx 1 (deaka state.jikaze) 1.0;
  ctx.idx <- ctx.idx + 2;

  if version >= 2 then (
    let n = (if deaka state.bakaze = tile_id_E then 0 else 1) * 4 + state.kyoku in
    encode_int ctx { n; cap = 7; one_hot = false; rescale = true; rbf_intervals = None }
  );

  (* Dora indicators *)
  encode_tile_set ctx state.dora_indicators;

  (* Self Kawa *)
  let self_kawa = List.rev state.kawa.(0) in (* kawa is newest first *)
  let self_kawa_len = List.length self_kawa in
  let rec take n l = if n <= 0 then [] else match l with [] -> [] | h::t -> h :: take (n-1) t in
  
  List.iter (encode_self_kawa ctx) (take 6 self_kawa);
  ctx.idx <- ctx.idx + (max 0 (6 - self_kawa_len)) * 4;

  let rev_kawa = List.rev self_kawa in
  List.iter (encode_self_kawa ctx) (take 18 rev_kawa);
  ctx.idx <- ctx.idx + (max 0 (18 - self_kawa_len)) * 4;

  let max_kawa_len = ref 0 in
  for i = 0 to 3 do max_kawa_len := max !max_kawa_len (List.length state.kawa.(i)) done;
  let max_kawa_len = !max_kawa_len in

  if version >= 3 then (
    List.iteri (fun turn item_opt ->
      match item_opt with
      | Some item ->
          let tid = deaka item.sutehai.tile in
          let v = exp (-. 0.2 *. float_of_int (max_kawa_len - 1 - turn)) in
          assign_row ctx 0 tid v
      | None -> ()
    ) self_kawa;
    ctx.idx <- ctx.idx + 1
  );

  (* Others Kawa *)
  for i = 1 to 3 do
    let player_kawa = List.rev state.kawa.(i) in
    let player_kawa_len = List.length player_kawa in
    List.iter (encode_kawa ctx) (take 6 player_kawa);
    ctx.idx <- ctx.idx + (max 0 (6 - player_kawa_len)) * 8;

    let rev_pkawa = List.rev player_kawa in
    List.iter (encode_kawa ctx) (take 18 rev_pkawa);
    ctx.idx <- ctx.idx + (max 0 (18 - player_kawa_len)) * 8;

    match version with
    | 2 ->
        List.iteri (fun turn item_opt ->
          match item_opt with
          | Some item ->
              let row = min (turn / 6) 2 in
              let tid = deaka item.sutehai.tile in
              assign_row ctx row tid 1.0;
              if item.sutehai.is_tedashi then assign_row ctx (3 + row) tid 1.0
          | None -> ()
        ) player_kawa;
        ctx.idx <- ctx.idx + 6
    | 3 | 4 ->
        List.iteri (fun turn item_opt ->
          match item_opt with
          | Some item ->
              let tid = deaka item.sutehai.tile in
              let v = exp (-. 0.2 *. float_of_int (max_kawa_len - 1 - turn)) in
              assign_row ctx 0 tid v;
              if item.sutehai.is_tedashi then assign_row ctx 1 tid v;
              if item.sutehai.is_riichi then assign_row ctx 2 tid v
          | None -> ()
        ) player_kawa;
        ctx.idx <- ctx.idx + 3
    | _ -> ()
  done;

  (* Tiles left *)
  let v = float_of_int state.tiles_left /. 69.0 in
  fill_row ctx 0 v;
  ctx.idx <- ctx.idx + 1;

  (* Doras owned *)
  for i = 0 to 3 do
    encode_int ctx { n = state.doras_owned.(i); cap = 12; one_hot = false; rescale = true; rbf_intervals = Some 3 }
  done;

  (* Doras unseen *)
  let doras_unseen = List.length state.dora_indicators * 4 + 3 - state.doras_seen in
  encode_int ctx { n = doras_unseen; cap = 5 * 4 + 3; one_hot = false; rescale = true; rbf_intervals = Some 4 };

  (* Kawa Overview *)
  for i = 0 to 3 do
    encode_tile_set ctx state.kawa_overview.(i)
  done;

  (* Fuuro Overview *)
  for p = 0 to 3 do
    let player_fuuro = state.fuuro_overview.(p) in
    List.iter (fun meld ->
      List.iter (fun tile ->
        let tid = deaka tile in
        let rec find_row r =
          if r >= 4 then ()
          else if ctx.arr.((ctx.idx + r) * ctx.cols + tid) = 0.0 then
            assign_row ctx r tid 1.0
          else find_row (r + 1)
        in
        find_row 0;
        if is_aka tile then fill_row ctx 4 1.0
      ) meld;
      ctx.idx <- ctx.idx + 5
    ) player_fuuro;
    ctx.idx <- ctx.idx + (4 - List.length player_fuuro) * 5
  done;

  (* Ankan Overview *)
  for p = 0 to 3 do
    List.iter (fun tid -> assign_row ctx 0 tid 1.0) state.ankan_overview.(p);
    ctx.idx <- ctx.idx + 1
  done;

  if version >= 2 then (
    (* Tiles Seen *)
    for tid = 0 to 33 do
      assign_row ctx 0 tid (float_of_int state.tiles_seen.(tid) /. 4.0)
    done;
    ctx.idx <- ctx.idx + 1;

    (* Last Tedashis *)
    for i = 1 to 3 do
      (match state.last_tedashis.(i) with
      | Some sute ->
          let tid = deaka sute.tile in
          assign_row ctx 0 tid 1.0;
          if is_aka sute.tile then fill_row ctx 1 1.0;
          if sute.is_dora then fill_row ctx 2 1.0
      | None -> ());
      ctx.idx <- ctx.idx + 3
    done;

    (* Riichi Sutehais - placeholder since not in state yet, but we have kawa_item *)
    (* In state.ml, we can find riichi_sutehai from kawa if needed *)
    for i = 1 to 3 do
      let found = ref false in
      List.iter (function 
        | Some item when item.sutehai.is_riichi ->
            let tid = deaka item.sutehai.tile in
            assign_row ctx 0 tid 1.0;
            if is_aka item.sutehai.tile then fill_row ctx 1 1.0;
            if item.sutehai.is_dora then fill_row ctx 2 1.0;
            found := true
        | _ -> ()
      ) state.kawa.(i);
      ctx.idx <- ctx.idx + 3
    done
  );

  (* Riichi status *)
  for i = 1 to 3 do
    if state.riichi_declared.(i) then fill_row ctx 0 1.0;
    ctx.idx <- ctx.idx + 1
  done;
  for i = 1 to 3 do
    if state.riichi_accepted.(i) then fill_row ctx 0 1.0;
    ctx.idx <- ctx.idx + 1
  done;

  (* Waits *)
  for tid = 0 to 33 do
    if state.waits.(tid) then assign_row ctx 0 tid 1.0
  done;
  ctx.idx <- ctx.idx + 1;

  if state.at_furiten then fill_row ctx 0 1.0;
  ctx.idx <- ctx.idx + 1;

  (* Shanten *)
  encode_int ctx { n = state.shanten; cap = 6; one_hot = true; rescale = false; rbf_intervals = None };

  if state.riichi_accepted.(0) then fill_row ctx 0 1.0;
  ctx.idx <- ctx.idx + 1;

  if at_kan_select then fill_row ctx 0 1.0;
  ctx.idx <- ctx.idx + 1;

  (* Action Mask *)
  let cans = state.last_cans in
  if can_pass cans then (
    match state.last_kawa_tile with
    | Some tile ->
        let tid = deaka tile in
        assign_row ctx 0 tid 1.0;
        if is_aka tile then fill_row ctx 1 1.0;
        if state.dora_factor.(tid) > 0 then fill_row ctx 2 1.0;
        
        if not at_kan_select then ctx.mask.(action_space - 1) <- true
        else if cans.can_daiminkan then ctx.mask.(tid) <- true
    | None -> ()
  );
  ctx.idx <- ctx.idx + 3;

  if cans.can_discard then (
    let discards = discard_candidates_aka state in
    Array.iteri (fun tid b ->
      if b then (
        let deaka_tid = deaka tid in
        assign_row ctx 0 deaka_tid 1.0;
        if not at_kan_select then ctx.mask.(tid) <- true
      )
    ) discards;

    for tid = 0 to 33 do
      if state.keep_shanten_discards.(tid) then assign_row ctx 1 tid 1.0;
      if state.next_shanten_discards.(tid) then assign_row ctx 2 tid 1.0;
    done;

    if state.shanten <= 1 then (
      let unconditional = discard_candidates_with_unconditional_tenpai_aka state in
      Array.iteri (fun tid b -> if b then assign_row ctx 3 (deaka tid) 1.0) unconditional
    );

    if state.riichi_declared.(0) then fill_row ctx 4 1.0
  );
  ctx.idx <- ctx.idx + 5;

  if cans.can_riichi then (
    fill_row ctx 0 1.0;
    if not at_kan_select then ctx.mask.(37) <- true
  );
  ctx.idx <- ctx.idx + 1;

  if cans.can_chi_low then (
    fill_row ctx 0 1.0;
    if not at_kan_select then ctx.mask.(38) <- true
  );
  if cans.can_chi_mid then (
    fill_row ctx 1 1.0;
    if not at_kan_select then ctx.mask.(39) <- true
  );
  if cans.can_chi_high then (
    fill_row ctx 2 1.0;
    if not at_kan_select then ctx.mask.(40) <- true
  );
  ctx.idx <- ctx.idx + 3;

  if cans.can_pon then (
    fill_row ctx 0 1.0;
    if not at_kan_select then ctx.mask.(41) <- true
  );
  ctx.idx <- ctx.idx + 1;

  if cans.can_daiminkan then (
    fill_row ctx 0 1.0;
    if not at_kan_select then ctx.mask.(42) <- true
  );
  ctx.idx <- ctx.idx + 1;

  if cans.can_ankan then (
    List.iter (fun tid ->
      assign_row ctx 0 tid 1.0;
      if at_kan_select then ctx.mask.(tid) <- true
    ) state.ankan_candidates;
    if not at_kan_select then ctx.mask.(42) <- true
  );
  ctx.idx <- ctx.idx + 1;

  if cans.can_kakan then (
    List.iter (fun tid ->
      assign_row ctx 0 tid 1.0;
      if at_kan_select then ctx.mask.(tid) <- true
    ) state.kakan_candidates;
    if not at_kan_select then ctx.mask.(42) <- true
  );
  ctx.idx <- ctx.idx + 1;

  if can_agari cans then (
    fill_row ctx 0 1.0;
    if not at_kan_select then ctx.mask.(43) <- true
  );
  ctx.idx <- ctx.idx + 1;

  if cans.can_ryukyoku then (
    fill_row ctx 0 1.0;
    if not at_kan_select then ctx.mask.(44) <- true
  );
  ctx.idx <- ctx.idx + 1;

  (* Version 4 SP features would go here *)
  
  { features = ctx.arr; mask = ctx.mask }
