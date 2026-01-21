(** Shanten (progress to win) calculation.

    Port of the table-based algorithm from shanten-number by tomohxx.
    Original: https://github.com/tomohxx/shanten-number

    This module provides a clean, functional API for calculating how many
    tiles away from winning a mahjong hand is. Uses precomputed lookup
    tables for O(1) performance.

    {1 Usage}

    {[
      (* Calculate shanten for a hand *)
      let tiles34 = (* your 34-tile array *) in
      let shanten = Shanten.calc_normal tiles34 4 in
      Printf.printf "Shanten: %s\n" (Shanten.to_string shanten);
    ]}

    {1 Hand Forms}

    - Normal: Standard form with 4 melds and 1 pair
    - Chitoi: Seven pairs form
    - Kokushi: Thirteen orphans form *)

(** {1 Public Types} *)

(** Shanten calculation mode *)
type shanten_mode =
  | Normal (** Standard hand form (4 melds + 1 pair) *)
  | Chitoi (** Seven pairs *)
  | Kokushi (** Thirteen orphans *)

(** Shanten result *)
type t = int
(** Negative values: hand is already winning
       0: tenpai (one tile from winning)
       Positive: tiles away from tenpai *)

(** Initialization errors *)
type init_error =
  | TableFileNotFound of string (** Table file not found *)
  | InvalidTableData of string (** Table file is corrupted *)
  | IoError of string (** I/O error reading table *)

exception InitError of init_error

(** {1 Private Types} *)

(** 10 values: indices 0-4 for 0-4 melds, indices 5-9 for pair calculations *)
type table_entry = int array

type tables =
  { suit_table : table_entry array (** Size: 5^9 = 1,953,125 *)
  ; honor_table : table_entry array (** Size: 5^7 = 78,125 *)
  }

(** {1 Hash Functions} *)

(** [hash_suit tiles offset] computes a base-5 hash for a 9-tile suit section.

    Each tile count (0-4) becomes a digit in base-5 encoding.
    Result is in range [0, 5^9 - 1] = [0, 1953124].

    @param tiles Full 34-tile array
    @param offset Starting index (0 for man, 9 for pin, 18 for sou)
    @return Hash key for suit table lookup *)
let hash_suit (tiles : int array) (offset : int) : int =
  let rec loop i acc =
    if i >= offset + 9 then acc else loop (i + 1) ((acc * 5) + tiles.(i))
  in
  loop offset 0
;;

(** [hash_honor tiles offset] computes a base-5 hash for a 7-tile honor section.

    Each tile count (0-4) becomes a digit in base-5 encoding.
    Result is in range [0, 5^7 - 1] = [0, 78124].

    @param tiles Full 34-tile array
    @param offset Starting index (typically 27 for honors)
    @return Hash key for honor table lookup *)
let hash_honor (tiles : int array) (offset : int) : int =
  let rec loop i acc =
    if i >= offset + 7 then acc else loop (i + 1) ((acc * 5) + tiles.(i))
  in
  loop offset 0
;;

(** {1 Combination Functions} *)

(** [combine_with_melds lhs rhs m] combines two shanten arrays with full convolution.

    This combines table entries considering all possible ways to distribute
    melds between two tile sections.

    Algorithm: For each target meld count j, find the minimum shanten across
    all ways to split j melds between lhs and rhs sections.

    @param lhs Left-hand side shanten array (10 values)
    @param rhs Right-hand side shanten array (10 values)
    @param m Maximum melds to consider (typically 4)
    @return New shanten array with combined values *)
let combine_with_melds (lhs : int array) (rhs : int array) (m : int) : int array =
  let result = Array.copy lhs in
  (* Combine meld counts [5..m+5] (pair calculations) *)
  for j = m + 5 downto 5 do
    (* Start with direct splits: j+0 and 0+j *)
    let rec find_best k best =
      if k >= j
      then best
      else (
        let v1 = lhs.(k) + rhs.(j - k) in
        let v2 = lhs.(j - k) + rhs.(k) in
        find_best (k + 1) (min best (min v1 v2)))
    in
    let initial = min (lhs.(j) + rhs.(0)) (lhs.(0) + rhs.(j)) in
    result.(j) <- find_best 5 initial
  done;
  (* Combine meld counts [0..m] (standard calculations) *)
  for j = m downto 0 do
    (* Start with direct split: j+0 *)
    let rec find_best k best =
      if k >= j
      then best
      else (
        let v = lhs.(k) + rhs.(j - k) in
        find_best (k + 1) (min best v))
    in
    let initial = lhs.(j) + rhs.(0) in
    result.(j) <- find_best 0 initial
  done;
  result
;;

(** [combine_honor_only lhs rhs m] adds honors section with optimized calculation.

    This is a simplified version that only computes the final result
    position (m+5), since honor tiles don't form sequences.

    @param lhs Shanten array (10 values)
    @param rhs Honor section shanten array (10 values)
    @param m Maximum melds to consider
    @return New shanten array with honor section combined *)
let combine_honor_only (lhs : int array) (rhs : int array) (m : int) : int array =
  let result = Array.copy lhs in
  let j = m + 5 in
  (* Start with direct splits: j+0 and 0+j *)
  let rec find_best k best =
    if k >= j
    then best
    else (
      let v1 = lhs.(k) + rhs.(j - k) in
      let v2 = lhs.(j - k) + rhs.(k) in
      find_best (k + 1) (min best (min v1 v2)))
  in
  let initial = min (lhs.(j) + rhs.(0)) (lhs.(0) + rhs.(j)) in
  result.(j) <- find_best 5 initial;
  result
;;

(** {1 Lazy Table Initialization} *)

let shanten_tables : tables Lazy.t =
  lazy
    ((* Try to find table files in multiple possible locations *)
     let open_file filename =
       try Some (open_in_bin filename) with
       | Sys_error _ -> None
     in
     let find_file basename =
       match
         List.find_map
           open_file
           [ basename; "../lib/" ^ basename; "_build/default/lib/" ^ basename ]
       with
       | Some ic -> ic
       | None -> raise (Sys_error (basename ^ ": No such file or directory"))
     in
     let load_table basename size =
       let ic = find_file basename in
       try
         let table = Array.make size [||] in
         for i = 0 to size - 1 do
           let entry = Array.make 10 0 in
           for j = 0 to 9 do
             entry.(j) <- input_byte ic
           done;
           table.(i) <- entry
         done;
         close_in ic;
         table
       with
       | exn ->
         close_in ic;
         raise
           (InitError
              (IoError
                 (Printf.sprintf
                    "Failed to read %s: %s"
                    basename
                    (Printexc.to_string exn))))
     in
     let suit_table = load_table "index_s.bin" 1953125 in
     let honor_table = load_table "index_h.bin" 78125 in
     { suit_table; honor_table })
;;

(** {1 Calculation Functions} *)

(** [calc_normal_internal tables tiles34 len_div3] calculates shanten for normal form.

    @param tables Loaded shanten tables
    @param tiles34 34-element tile count array
    @param len_div3 Number of complete melds (must be 0-4)
    @return Shanten value for normal form *)
let calc_normal_internal (tables : tables) (tiles34 : int array) (len_div3 : int) : int =
  let suit_table = tables.suit_table in
  let honor_table = tables.honor_table in
  let m = len_div3 in
  if m < 0 || m > 4 then (
    let count = Array.fold_left (+) 0 tiles34 in
    Printf.eprintf "DEBUG: calc_normal_internal m=%d count=%d\n" m count;
    failwith (Printf.sprintf "len_div3 %d out of bounds (0-4)" m)
  );
  (* Hash each tile section *)
  let hash_man = hash_suit tiles34 0 in
  let hash_pin = hash_suit tiles34 9 in
  let hash_sou = hash_suit tiles34 18 in
  let hash_honor = hash_honor tiles34 27 in
  (* Look up table entries with bounds checks *)
  let get_entry table hash name =
    if hash < 0 || hash >= Array.length table
    then failwith (Printf.sprintf "hash %d out of bounds for %s table" hash name)
    else table.(hash)
  in
  let entry_man = get_entry suit_table hash_man "suit" in
  let entry_pin = get_entry suit_table hash_pin "suit" in
  let entry_sou = get_entry suit_table hash_sou "suit" in
  let entry_honor = get_entry honor_table hash_honor "honor" in
  (* Combine sections: man + pin + sou + honors *)
  let combined_man_pin = combine_with_melds entry_man entry_pin m in
  let combined_three_suits = combine_with_melds combined_man_pin entry_sou m in
  (* Add honors (simplified calculation) *)
  let result = combine_honor_only combined_three_suits entry_honor m in
  (* Get final shanten value at position m+5, subtract 1 *)
  if m + 5 >= Array.length result
  then failwith (Printf.sprintf "m+5 %d out of bounds for result" (m + 5));
  result.(m + 5) - 1
;;

(** [count_pairs_and_kinds tiles34] counts pairs and unique tile types.

    @param tiles34 34-element tile count array
    @return (number_of_pairs, number_of_unique_kinds) *)
let count_pairs_and_kinds (tiles34 : int array) : int * int =
  Array.fold_left
    (fun (pairs, kinds) count ->
       if count > 0
       then (
         let new_kinds = kinds + 1 in
         let new_pairs = if count >= 2 then pairs + 1 else pairs in
         new_pairs, new_kinds)
       else pairs, kinds)
    (0, 0)
    tiles34
;;

(** [calc_chitoi tiles34] calculates shanten for seven pairs form.

    Formula: shanten = 7 - pairs + max(0, 7 - kinds) - 1

    @param tiles34 34-element tile count array
    @return Shanten value for seven pairs form *)
let calc_chitoi (tiles34 : int array) : int =
  let pairs, kinds = count_pairs_and_kinds tiles34 in
  let redundant = max 0 (7 - kinds) in
  7 - pairs + redundant - 1
;;

(** Terminal and honor tile indices for Kokushi *)
let terminals_and_honors = [| 0; 8; 9; 17; 18; 26; 27; 28; 29; 30; 31; 32; 33 |]
(* 1m,9m,1p,9p,1s,9s,E,S,W,N,P,F,C *)

(** [count_unique_and_has_pair tiles34] counts unique terminals/honors and checks for pair.

    @param tiles34 34-element tile count array
    @return (unique_count, has_pair) *)
let count_unique_and_has_pair (tiles34 : int array) : int * bool =
  Array.fold_left
    (fun (unique, has_pair) idx ->
       let count = tiles34.(idx) in
       let new_unique = if count > 0 then unique + 1 else unique in
       let new_has_pair = has_pair || count >= 2 in
       new_unique, new_has_pair)
    (0, false)
    terminals_and_honors
;;

(** [calc_kokushi tiles34] calculates shanten for thirteen orphans form.

    Formula: shanten = 14 - unique_kinds - (1 if has_pair else 0) - 1

    @param tiles34 34-element tile count array
    @return Shanten value for thirteen orphans form *)
let calc_kokushi (tiles34 : int array) : int =
  let unique_kinds, has_pair = count_unique_and_has_pair tiles34 in
  let pair_bonus = if has_pair then 1 else 0 in
  14 - unique_kinds - pair_bonus - 1
;;

(** {1 Public API} *)

(** [calc_normal tiles34 len_div3] calculates shanten for normal hand form.

    @param tiles34 Array of 34 tile counts
    @param len_div3 Number of complete melds (tile_count / 3), must be in [0, 4]
    @return Shanten number where:
            - negative values mean the hand is already winning
            - 0 means tenpai (one tile away from winning)
            - positive values indicate how many tiles away from tenpai *)
let calc_normal (tiles34 : int array) (len_div3 : int) : t =
  let tables = Lazy.force shanten_tables in
  calc_normal_internal tables tiles34 len_div3
;;

(** [calc_chitoi tiles34] calculates shanten for seven pairs form.

    This function calculates:
    - Number of unique tile types present
    - Number of pairs available
    - Shanten = 7 - pairs + max(0, 7 - kinds) - 1

    @param tiles34 Array of 34 tile counts
    @return Shanten number (0 = tenpai, -1 = winning with 7 pairs) *)
let calc_chitoi_public (tiles34 : int array) : t = calc_chitoi tiles34

(** [calc_kokushi tiles34] calculates shanten for thirteen orphans form.

    This function:
    - Counts unique terminal/honor tiles present (1m,9m,1p,9p,1s,9s,E,S,W,N,P,F,C)
    - Checks if any terminal/honor has a pair
    - Calculates shanten = 14 - unique_kinds - has_pair

    @param tiles34 Array of 34 tile counts
    @return Shanten number (0 = tenpai with pair, -1 = winning with 13 unique) *)
let calc_kokushi_public (tiles34 : int array) : t = calc_kokushi tiles34

(** [calc_all tiles34 len_div3] calculates minimum shanten across all forms.

    This function computes the shanten for all three hand forms and returns
    the minimum value, representing the closest path to winning.

    @param tiles34 Array of 34 tile counts
    @param len_div3 Number of complete melds (typically 4 for a 13-tile hand)
    @return Minimum shanten number across Normal, Chitoi, and Kokushi forms *)
let calc_all (tiles34 : int array) (len_div3 : int) : t =
  let shanten_normal = calc_normal tiles34 len_div3 in
  if shanten_normal <= 0 || len_div3 < 4
  then shanten_normal
  else (
    let shanten_chitoi = calc_chitoi tiles34 in
    let shanten = min shanten_normal shanten_chitoi in
    if shanten > 0 then min shanten (calc_kokushi tiles34) else shanten)
;;

(** [calc tiles34 len_div3 mode] calculates shanten for a specific mode only.

    @param tiles34 Array of 34 tile counts
    @param len_div3 Number of complete melds (only used for Normal mode)
    @param mode Which hand form to calculate for
    @return Shanten number for the specified mode *)
let calc (tiles34 : int array) (len_div3 : int) (mode : shanten_mode) : t =
  match mode with
  | Normal -> calc_normal tiles34 len_div3
  | Chitoi -> calc_chitoi_public tiles34
  | Kokushi -> calc_kokushi_public tiles34
;;

(** {1 Utility Functions} *)

(** [to_string shanten] converts shanten value to human-readable string.

    @param shanten Shanten value
    @return String description ("Winning", "Tenpai", "Shanten: 2", etc.) *)
let to_string (shanten : t) : string =
  if shanten < 0
  then "Winning"
  else if shanten = 0
  then "Tenpai"
  else Printf.sprintf "Shanten: %d" shanten
;;
