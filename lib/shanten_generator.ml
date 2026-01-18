(** Table generation for shanten calculation.

    Port of mkind.cpp from shanten-number by tomohxx.
    Generates binary lookup tables for fast shanten calculation.

    Uses OCaml 5 domainslib for parallel processing.

    Original: https://github.com/tomohxx/shanten-number *)

(** Meld type: triplet (pon) or sequence (chi) *)
type meld =
  | Triplet of int  (** Three of the same tile *)
  | Sequence of int  (** Three consecutive tiles, starting at index *)

(** [get_all_melds length] returns all possible melds for a suit of given length.
    For suits (length=9): includes triplets and sequences.
    For honors (length=7): includes only triplets. *)
let get_all_melds (length : int) : meld array =
  let melds = ref [] in
  (* Add triplets for each tile *)
  for i = 0 to length - 1 do
    melds := Triplet i :: !melds
  done;
  (* Add sequences for suits (not honors) *)
  if length = 9 then
    for i = 0 to 6 do
      melds := Sequence i :: !melds
    done;
  Array.of_list (List.rev !melds)

(** [is_valid_target tiles] checks if target configuration has no tile > 4 *)
let is_valid_target (tiles : int array) : bool =
  Array.for_all (fun c -> c <= 4) tiles

(** [calc_distance current target] calculates tiles needed to reach target.
    Returns sum of max(target[i] - current[i], 0) for all i. *)
let calc_distance (current : int array) (target : int array) : int =
  let rec loop i acc =
    if i >= Array.length current then acc
    else
      let needed = max 0 (target.(i) - current.(i)) in
      loop (i + 1) (acc + needed)
  in
  loop 0 0

(** [add_meld meld target] adds a meld to the target configuration *)
let add_meld (meld : meld) (target : int array) : unit =
  match meld with
  | Triplet idx -> target.(idx) <- target.(idx) + 3
  | Sequence idx ->
      target.(idx) <- target.(idx) + 1;
      target.(idx + 1) <- target.(idx + 1) + 1;
      target.(idx + 2) <- target.(idx + 2) + 1

(** [remove_meld meld target] removes a meld from the target configuration *)
let remove_meld (meld : meld) (target : int array) : unit =
  match meld with
  | Triplet idx -> target.(idx) <- target.(idx) - 3
  | Sequence idx ->
      target.(idx) <- target.(idx) - 1;
      target.(idx + 1) <- target.(idx + 1) - 1;
      target.(idx + 2) <- target.(idx + 2) - 1

(** DFS search for minimum shanten.
    [dfs current target m min_mid melds sht] searches for minimum distance
    by trying all pairs and recursively adding melds. *)
let rec dfs (current : int array) (target : int array) (m : int)
    (min_mid : int) (melds : meld array) (sht : int array) : unit =
  (* Try adding a pair at each position *)
  for tid = 0 to Array.length current - 1 do
    target.(tid) <- target.(tid) + 2;

    if is_valid_target target then (
      let distance = calc_distance current target in
      (* Update best shanten for this meld count *)
      let idx = m + 5 in
      if distance < sht.(idx) then sht.(idx) <- distance
    );

    target.(tid) <- target.(tid) - 2
  done;

  (* Try adding melds if we haven't reached max melds *)
  if m < 4 then
    for mid = min_mid to Array.length melds - 1 do
      add_meld melds.(mid) target;

      if is_valid_target target then (
        let distance = calc_distance current target in
        (* Update shanten for m+1 melds *)
        let idx = m + 1 in
        if distance < sht.(idx) then sht.(idx) <- distance;

        (* Continue searching if we might improve the result *)
        if distance < sht.(9) then
          dfs current target (m + 1) mid melds sht
      );

      remove_meld melds.(mid) target
    done

(** [calc_section_shanten hand melds] calculates shanten array for one hand section.
    Returns array of 10 elements where sht[m] = min tiles needed with m melds. *)
let calc_section_shanten (hand : int array) (melds : meld array) : int array =
  let max_sht = 14 in
  let sht = Array.make 10 max_sht in
  sht.(0) <- 0;  (* No melds, no tiles needed *)
  dfs hand (Array.make (Array.length hand) 0) 0 0 melds sht;
  sht

(** [list_slice start len lst] returns a sublist of lst starting at start with length len. *)
let list_slice start len lst =
  let rec skip n lst =
    if n <= 0 then lst
    else match lst with
    | [] -> []
    | _ :: tl -> skip (n - 1) tl
  in
  let rec take n lst acc =
    if n <= 0 || lst = [] then List.rev acc
    else
      match lst with
      | hd :: tl -> take (n - 1) tl (hd :: acc)
      | [] -> List.rev acc
  in
  take len (skip start lst) []

(** [generate_hands length] generates all 5^n hand configurations.
    Returns list sorted by hash. *)
let generate_hands (length : int) : (int array * int) list =
  (* Generate all 5^n combinations *)
  let rec gen pos current acc =
    if pos >= length then
      (Array.copy current, 0) :: acc
    else
      let rec add_tile count acc =
        if count > 4 then acc
        else (
          current.(pos) <- count;
          let acc' = gen (pos + 1) current acc in
          add_tile (count + 1) acc'
        )
      in
      add_tile 0 acc
  in
  let result = gen 0 (Array.make length 0) [] in
  (* Sort by hash for efficient table lookup *)
  List.sort (fun a b ->
    let hash_a = Array.fold_left (fun h c -> h * 5 + c) 0 (fst a) in
    let hash_b = Array.fold_left (fun h c -> h * 5 + c) 0 (fst b) in
    compare hash_a hash_b) result

(** [write_table filename length] generates and writes a shanten table.
    For each possible hand configuration, calculates shanten array and writes to file.
    Uses parallel processing with domainslib. *)
let write_table (filename : string) (length : int) : unit =
  let melds = get_all_melds length in
  let hands = generate_hands length in

  Printf.printf "Generating %s...\n" filename;
  Printf.printf "  Hand configurations: %d\n" (List.length hands);
  Printf.printf "  Table size: %d entries x 10 bytes = %.2f MB\n"
    (List.length hands)
    ((float_of_int (List.length hands * 10)) /. 1024.0 /. 1024.0);

  (* Open output channel *)
  let oc = open_out_bin filename in

  (* Calculate shanten for each hand in parallel using domainslib *)
  let num_domains = max 1 (List.length Processor.Topology.t) in
  Printf.printf "  Using %d parallel domains\n" num_domains;

  (* Split hands into chunks for parallel processing *)
  let chunk_size = (List.length hands + num_domains - 1) / num_domains in

  let results = Array.make num_domains [||] in

  (* Process chunks in parallel *)
  let domains =
    Array.init num_domains (fun i ->
      let start = i * chunk_size in
      let hands_chunk =
        if i = num_domains - 1 then
          list_slice start (List.length hands - start) hands
        else
          list_slice start chunk_size hands
      in
      Domain.spawn (fun () ->
        let chunk_results =
          Array.map
            (fun (hand, _hash) ->
              calc_section_shanten hand melds)
            (Array.of_list hands_chunk)
        in
        results.(i) <- chunk_results;
        ()
      ))
  in

  (* Wait for all domains to complete *)
  Array.iter Domain.join domains;

  (* Write results in original order *)
  Array.iter (fun chunk_results ->
    Array.iter (fun sht ->
      Array.iter (fun v -> output_byte oc v) sht)
    chunk_results) results;

  close_out oc;
  Printf.printf "  Done: %s\n%!" filename

let generate_suit_tables () : unit =
  (* Generate index_s.bin for suits (9 tiles) *)
  write_table "index_s.bin" 9

let generate_honor_tables () : unit =
  (* Generate index_h.bin for honors (7 tiles) *)
  write_table "index_h.bin" 7

let () =
  generate_suit_tables ();
  generate_honor_tables ();

  Printf.printf "\nAll tables generated successfully!\n"
