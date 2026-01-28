open Base

(* Block: sparse representation for efficient add/sub operations *)
type block = {
  add_ops : (int * int) array;  (* Array of (index, delta) for indexed access *)
  tiles : int list;             (* Tile indices - only mapped once, never indexed *)
}

let create_sets () =
  let sets = Array.create ~len:(21 + 34) { add_ops = [||]; tiles = [] } in
  let idx = ref 0 in
  (* Shuntsu: 7 per suit × 3 suits = 21 *)
  List.iter [0; 9; 18] ~f:(fun start ->
    for i = start to start + 6 do
      sets.(!idx) <- {
        add_ops = [| (i, 1); (i+1, 1); (i+2, 1) |];
        tiles = [i; i+1; i+2];
      };
      Int.incr idx
    done
  );
  (* Koutsu: 34 *)
  for i = 0 to 33 do
    sets.(!idx) <- {
      add_ops = [| (i, 3) |];
      tiles = [i; i; i];
    };
    Int.incr idx
  done;
  sets

let create_heads () =
  Array.init 34 ~f:(fun i -> {
    add_ops = [| (i, 2) |];
    tiles = [i; i];
  })

(* Pattern: decomposition as int list list *)
module PatternSet = Stdlib.Set.Make(struct
  type t = int list list
  let compare = Stdlib.compare
end)

(* Cache type: hand -> set of decompositions *)
type cache_t = (string, PatternSet.t) Hashtbl.t

(* Work buffer for register function - avoids repeated allocation *)
type work_buf = {
  map_arr : int array;
  total : int array;
}

let create_work_buf () = {
  map_arr = Array.create ~len:34 (-1);
  total = Array.create ~len:34 0;
}

(* Build abstract hand key, returns (valid, key_string) *)
let build_key (total : int array) (map_arr : int array) : bool * string =
  let buf = Buffer.create 32 in

  (* Check validity and build key simultaneously *)
  let valid = ref true in
  let current_idx = ref 0 in
  let need_sep = ref true in  (* Persistent across suits, like C++ *)

  let process_suit start_tile end_tile =
    if not !valid then ()
    else begin
      for i = start_tile to end_tile do
        let c = total.(i) in
        if c > 4 then valid := false
        else if c > 0 then begin
          (* Only add comma if needed AND buffer not empty *)
          if !need_sep && Buffer.length buf > 0 then Buffer.add_char buf ',';
          Buffer.add_char buf (Char.of_int_exn (Char.to_int '0' + c));
          map_arr.(i) <- !current_idx;
          Int.incr current_idx;
          need_sep := false
        end else begin
          map_arr.(i) <- -1;
          need_sep := true
        end
      done;
      need_sep := true  (* Set to true after each suit, like C++ line 24 *)
    end
  in

  (* Process suits: manzu, pinzu, souzu *)
  process_suit 0 8;
  process_suit 9 17;
  process_suit 18 26;

  (* Process honors (each separated) *)
  if !valid then begin
    for i = 27 to 33 do
      let c = total.(i) in
      if c > 4 then valid := false
      else if c > 0 then begin
        if Buffer.length buf > 0 then Buffer.add_char buf ',';
        Buffer.add_char buf (Char.of_int_exn (Char.to_int '0' + c));
        map_arr.(i) <- !current_idx;
        Int.incr current_idx
      end else
        map_arr.(i) <- -1
    done
  end;

  (!valid, Buffer.contents buf)

(* Convert blocks to pattern: int list list *)
let blocks_to_pattern (blocks : block list) (map_arr : int array) : int list list =
  List.map blocks ~f:(fun b ->
    List.map b.tiles ~f:(fun tile_idx -> map_arr.(tile_idx))
  )

let register (blocks : block list) (wb : work_buf) (cache : cache_t) : bool =
  let valid, key = build_key wb.total wb.map_arr in
  if not valid then false
  else begin
    let pattern = blocks_to_pattern blocks wb.map_arr in
    Hashtbl.update cache key ~f:(function
      | None -> PatternSet.singleton pattern
      | Some ps -> PatternSet.add pattern ps
    );
    true
  end

let add_counts (total : int array) (block : block) =
  Array.iter block.add_ops ~f:(fun (idx, delta) ->
    total.(idx) <- total.(idx) + delta
  )

let sub_counts (total : int array) (block : block) =
  Array.iter block.add_ops ~f:(fun (idx, delta) ->
    total.(idx) <- total.(idx) - delta
  )

(* Generate chiitoi (seven pairs) patterns *)
let generate_chiitoi (cache : cache_t) =
  (* Pattern: 7 pairs, each pair is [i; i] *)
  let pattern = List.init 7 ~f:(fun i -> [i; i]) in

  (* Generate all possible abstract hands for 7 pairs *)
  (* Each pair is "2", separated by ',' or not *)
  for bits = 0 to 63 do  (* 2^6 = 64 combinations of separators *)
    let buf = Buffer.create 16 in
    Buffer.add_char buf '2';
    for i = 0 to 5 do
      if bits land (1 lsl i) <> 0 then Buffer.add_char buf ',';
      Buffer.add_char buf '2'
    done;
    let key = Buffer.contents buf in
    Hashtbl.update cache key ~f:(function
      | None -> PatternSet.singleton pattern
      | Some ps -> PatternSet.add pattern ps
    )
  done

let generate_cache () =
  let sets = create_sets () in
  let heads = create_heads () in
  let num_sets = Array.length sets in

  let num_domains =
    try Int.of_string (Unix.getenv "DOMAINS")
    with _ -> Domain.recommended_domain_count ()
  in
  Stdio.printf "Using %d domains\n%!" num_domains;

  let pool = Domainslib.Task.setup_pool ~num_domains:(num_domains - 1) () in

  let process_head head_idx =
    let head = heads.(head_idx) in
    let local_cache : cache_t = Hashtbl.create (module String) in
    let wb = create_work_buf () in

    add_counts wb.total head;
    (* Register head alone, then add sets incrementally *)
    if register [head] wb local_cache then begin
      let rec loop_sets depth start_idx (blocks : block list) =
        if depth = 4 then
          ignore (register (List.rev blocks) wb local_cache)
        else begin
          for i = start_idx to num_sets - 1 do
            let s = sets.(i) in
            add_counts wb.total s;
            (* Register at each depth like C++ *)
            if register (s :: blocks) wb local_cache then
              loop_sets (depth + 1) i (s :: blocks);
            sub_counts wb.total s
          done
        end
      in
      loop_sets 0 0 [head]
    end;
    sub_counts wb.total head;
    local_cache
  in

  let results =
    Domainslib.Task.run pool (fun () ->
      List.init 34 ~f:(fun i ->
        Domainslib.Task.async pool (fun () -> process_head i))
      |> List.map ~f:(Domainslib.Task.await pool)
    )
  in
  Domainslib.Task.teardown_pool pool;

  (* Merge results *)
  let final_cache : cache_t = Hashtbl.create (module String) in

  (* Add chiitoi patterns first *)
  generate_chiitoi final_cache;

  (* Merge parallel results *)
  List.iter results ~f:(fun local ->
    Hashtbl.iteri local ~f:(fun ~key ~data ->
      Hashtbl.update final_cache key ~f:(function
        | None -> data
        | Some ps -> PatternSet.union ps data
      )
    )
  );

  final_cache

(* Escape string for OCaml literal *)
let escape_string (s : string) : string =
  let buf = Buffer.create (String.length s * 2) in
  String.iter s ~f:(fun c ->
    match c with
    | '\\' -> Buffer.add_string buf "\\\\"
    | '"' -> Buffer.add_string buf "\\\""
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c when Char.to_int c >= 32 && Char.to_int c <= 126 ->
      Buffer.add_char buf c
    | c ->
      Printf.bprintf buf "\\x%02x" (Char.to_int c)
  );
  Buffer.contents buf

(* Generate tenpai cache by reducing one tile from each winning hand *)
module StringSet = Stdlib.Set.Make(String)

let reduce_tile (hand : string) : StringSet.t =
  let result = ref StringSet.empty in
  for i = 0 to String.length hand - 1 do
    let c = hand.[i] in
    if Char.(c <> ',') then begin
      let h = Bytes.of_string hand in
      if Char.(c > '1') then begin
        (* Decrement count *)
        Bytes.set h i (Char.of_int_exn (Char.to_int c - 1));
        result := StringSet.add (Bytes.to_string h) !result
      end else begin
        (* Replace '1' with ',' and clean up *)
        Bytes.set h i ',';
        let rec clean_commas s =
          let len = String.length s in
          if len = 0 then s
          else if Char.(s.[0] = ',') then clean_commas (String.sub s ~pos:1 ~len:(len - 1))
          else if Char.(s.[len - 1] = ',') then clean_commas (String.sub s ~pos:0 ~len:(len - 1))
          else begin
            let rec find_double_comma j =
              if j + 1 >= len then None
              else if Char.(s.[j] = ',' && s.[j + 1] = ',') then Some j
              else find_double_comma (j + 1)
            in
            match find_double_comma 0 with
            | None -> s
            | Some j ->
              String.sub s ~pos:0 ~len:j ^ String.sub s ~pos:(j + 1) ~len:(len - j - 1)
              |> clean_commas
          end
        in
        result := StringSet.add (clean_commas (Bytes.to_string h)) !result
      end
    end
  done;
  !result

let generate_tenpai_cache (win_cache : cache_t) : StringSet.t =
  let tenpai = ref StringSet.empty in
  Hashtbl.iter_keys win_cache ~f:(fun hand ->
    let reduced = reduce_tile hand in
    StringSet.iter (fun h -> tenpai := StringSet.add h !tenpai) reduced
  );
  !tenpai

let main () =
  let t0 = Unix.gettimeofday () in
  let cache = generate_cache () in
  let t1 = Unix.gettimeofday () in
  Stdio.printf "Win cache generation took %.2f seconds\n%!" (t1 -. t0);

  let t2 = Unix.gettimeofday () in
  let tenpai = generate_tenpai_cache cache in
  let t3 = Unix.gettimeofday () in
  Stdio.printf "Tenpai cache generation took %.2f seconds\n%!" (t3 -. t2);

  let total_patterns = ref 0 in
  let out = Stdio.Out_channel.create "win_cache_data.ml" in

  Stdio.Out_channel.fprintf out "open Base\n\n";
  Stdio.Out_channel.fprintf out "(* Auto-generated win cache data *)\n";
  Stdio.Out_channel.fprintf out "(* DO NOT EDIT - generated by win_cache_generator *)\n";
  Stdio.Out_channel.fprintf out "(* Win cache: %.2f seconds, Tenpai cache: %.2f seconds *)\n\n" (t1 -. t0) (t3 -. t2);

  (* Helper to format int list *)
  let fmt_int_list lst =
    "[" ^ String.concat ~sep:"; " (List.map lst ~f:Int.to_string) ^ "]"
  in
  (* Helper to format int list list *)
  let fmt_pattern pattern =
    "[" ^ String.concat ~sep:"; " (List.map pattern ~f:fmt_int_list) ^ "]"
  in

  (* Output win cache *)
  Stdio.Out_channel.fprintf out "let win_cache_data : (string * int list list list) list = [\n";

  Hashtbl.iteri cache ~f:(fun ~key ~data ->
    let patterns = PatternSet.elements data in
    total_patterns := !total_patterns + List.length patterns;

    Stdio.Out_channel.fprintf out "  (\"%s\",\n" (escape_string key);
    Stdio.Out_channel.fprintf out "   [\n";
    List.iter patterns ~f:(fun pattern ->
      Stdio.Out_channel.fprintf out "     %s;\n" (fmt_pattern pattern)
    );
    Stdio.Out_channel.fprintf out "   ]);\n"
  );

  Stdio.Out_channel.fprintf out "]\n\n";

  (* Output tenpai cache *)
  Stdio.Out_channel.fprintf out "let tenpai_cache_data : string list = [\n";
  StringSet.iter (fun hand ->
    Stdio.Out_channel.fprintf out "  \"%s\";\n" (escape_string hand)
  ) tenpai;
  Stdio.Out_channel.fprintf out "]\n\n";

  (* Output cache constructors *)
  Stdio.Out_channel.fprintf out "let create_win_cache () : (string, int list list list) Stdlib.Hashtbl.t =\n";
  Stdio.Out_channel.fprintf out "  let cache = Stdlib.Hashtbl.create (List.length win_cache_data) in\n";
  Stdio.Out_channel.fprintf out "  List.iter win_cache_data ~f:(fun (key, patterns) ->\n";
  Stdio.Out_channel.fprintf out "    Stdlib.Hashtbl.add cache key patterns\n";
  Stdio.Out_channel.fprintf out "  );\n";
  Stdio.Out_channel.fprintf out "  cache\n\n";

  Stdio.Out_channel.fprintf out "let create_tenpai_cache () : (string, unit) Stdlib.Hashtbl.t =\n";
  Stdio.Out_channel.fprintf out "  let cache = Stdlib.Hashtbl.create (List.length tenpai_cache_data) in\n";
  Stdio.Out_channel.fprintf out "  List.iter tenpai_cache_data ~f:(fun key ->\n";
  Stdio.Out_channel.fprintf out "    Stdlib.Hashtbl.add cache key ()\n";
  Stdio.Out_channel.fprintf out "  );\n";
  Stdio.Out_channel.fprintf out "  cache\n";

  Stdio.Out_channel.close out;

  Stdio.printf "Generated win_cache_data.ml:\n";
  Stdio.printf "  - Win cache: %d keys, %d patterns\n" (Hashtbl.length cache) !total_patterns;
  Stdio.printf "  - Tenpai cache: %d keys\n" (StringSet.cardinal tenpai)

let () = main ()
