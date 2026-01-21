(** Agari lookup table generator.

    Generates a lookup table mapping bit-encoded tile patterns to their valid
    meld decompositions for mahjong winning hand detection.

    Output format: space-separated integers (key followed by decomposition values).
    Each line represents one unique tile pattern with all possible ways to
    decompose it into melds (刻子/順子) and a pair (雀頭). *)

(** Module for permutation generation *)
module Perms = struct
  (** [remove_at idx arr] returns array with element at idx removed *)
  let remove_at (idx : int) (arr : 'a array) : 'a array =
    Array.concat
      [ Array.sub arr 0 idx; Array.sub arr (idx + 1) (Array.length arr - idx - 1) ]
  ;;

  (** [find_index x arr] returns the first index of x in arr *)
  let find_index (x : 'a) (arr : 'a array) : int =
    let rec loop i =
      if i >= Array.length arr
      then raise Not_found
      else if arr.(i) = x
      then i
      else loop (i + 1)
    in
    loop 0
  ;;

  (** [perms arr] generates all unique permutations of array elements.
      Recursively generates permutations by selecting each unique element
      as the first element and permuting the remainder. *)
  let rec perms (arr : int array array) : int array array list =
    if Array.length arr = 0
    then [ [||] ]
    else
      (* Collect unique elements using a Set *)
      let module StringSet = Set.Make (String) in
      let seen = ref StringSet.empty in
      let uniq_elems = ref [] in
      Array.iter
        (fun x ->
           let key = String.concat "," (Array.to_list (Array.map string_of_int x)) in
           if not (StringSet.mem key !seen)
           then (
             seen := StringSet.add key !seen;
             uniq_elems := x :: !uniq_elems))
        arr;
      let uniq_arr = Array.of_list (List.rev !uniq_elems) in
      Array.fold_left
        (fun acc h ->
           let tmp = Array.copy arr in
           let idx = find_index h tmp in
           let tmp' = remove_at idx tmp in
           let sub_perms = perms tmp' in
           let new_perms = List.map (fun t -> Array.concat [ [| h |]; t ]) sub_perms in
           acc @ new_perms)
        []
        uniq_arr
  ;;
end

(** Module for pattern generation *)
module Pattern = struct
  (** Remove all zeros from array *)
  let remove_zeros (arr : int array) : int array =
    Array.of_list (List.filter (fun x -> x <> 0) (Array.to_list arr))
  ;;

  (** Check if array has any element > 4 *)
  let has_too_large (arr : int array) : bool = Array.exists (fun v -> v > 4) arr

  (** [ptn a] generates all valid tile patterns including overlapping configurations.

      Given meld groups (e.g., [[1,1,1], [3], [2]]), generates:
      1. Non-overlapping patterns: all permutations of the groups
      2. Overlapping patterns: groups combined with different shifts/overlaps

      Filters out invalid patterns (tiles > 4, length > 9) and deduplicates. *)
  let rec ptn (a : int array array) : int array array list =
    if Array.length a = 1
    then [ a ]
    else (
      let ret = ref [] in
      (* Non-overlapping patterns (permutations) *)
      let perms = Perms.perms a in
      ret := perms @ !ret;
      (* Overlapping patterns *)
      let module StringSet = Set.Make (String) in
      let h1 = ref StringSet.empty in
      for i = 0 to Array.length a - 1 do
        for j = i + 1 to Array.length a - 1 do
          (* Create key for deduplication *)
          let key =
            Printf.sprintf
              "[%s],0,[%s]"
              (String.concat "," (Array.to_list (Array.map string_of_int a.(i))))
              (String.concat "," (Array.to_list (Array.map string_of_int a.(j))))
          in
          if not (StringSet.mem key !h1)
          then (
            h1 := StringSet.add key !h1;
            let h2 = ref StringSet.empty in
            (* Overlap a.(i) and a.(j) with different shifts *)
            for k = 0 to Array.length a.(i) + Array.length a.(j) do
              (* Create padded array *)
              let zeros_before = Array.make (Array.length a.(j)) 0 in
              let zeros_after = Array.make (Array.length a.(j)) 0 in
              let t = Array.concat [ zeros_before; a.(i); zeros_after ] in
              (* Add a.(j) at offset k *)
              for m = 0 to Array.length a.(j) - 1 do
                t.(k + m) <- t.(k + m) + a.(j).(m)
              done;
              (* Remove zeros *)
              let t = remove_zeros t in
              (* Filter: no value > 4 *)
              if not (has_too_large t)
              then
                (* Filter: length <= 9 *)
                if Array.length t <= 9
                then (
                  (* Deduplicate *)
                  let t_str =
                    String.concat "," (Array.to_list (Array.map string_of_int t))
                  in
                  if not (StringSet.mem t_str !h2)
                  then (
                    h2 := StringSet.add t_str !h2;
                    (* Create remainder: a without elements i and j *)
                    let t2 = ref [] in
                    for idx = 0 to Array.length a - 1 do
                      if idx <> i && idx <> j then t2 := a.(idx) :: !t2
                    done;
                    let t2 = Array.of_list (List.rev !t2) in
                    (* Recursive call *)
                    let sub_patterns = ptn (Array.concat [ [| t |]; t2 ]) in
                    ret := !ret @ sub_patterns))
            done)
        done
      done;
      !ret)
  ;;
end

(** Module for key calculation *)
module Key = struct
  (** [calc_key a] calculates bit-packed integer key for a tile pattern.

      Encoding scheme:
      - Count 1: no bits (implicit)
      - Count 2: 0b11 (2 bits)
      - Count 3: 0b1111 (4 bits)
      - Count 4: 0b111111 (6 bits)
      - Group separator: 0b1 (1 bit)

      Each tile group is encoded sequentially with separators between groups. *)
  let calc_key (a : int array array) : int =
    let ret = ref 0 in
    let len = ref (-1) in
    for i = 0 to Array.length a - 1 do
      let b = a.(i) in
      for j = 0 to Array.length b - 1 do
        len := !len + 1;
        match b.(j) with
        | 2 ->
          ret := !ret lor (0b11 lsl !len);
          len := !len + 2
        | 3 ->
          ret := !ret lor (0b1111 lsl !len);
          len := !len + 4
        | 4 ->
          ret := !ret lor (0b111111 lsl !len);
          len := !len + 6
        | _ -> ()
      done;
      ret := !ret lor (0b1 lsl !len);
      len := !len + 1
    done;
    !ret
  ;;
end

(** Module for meld decomposition *)
module Decompose = struct
  (** Deep copy a 2D array *)
  let deep_copy (arr : int array array) : int array array = Array.map Array.copy arr

  (** Flatten 2D array to 1D *)
  let flatten (arr : int array array) : int array = Array.concat (Array.to_list arr)

  (** Check if all elements are zero *)
  let all_zero (arr : int array) : bool = Array.for_all (fun x -> x = 0) arr

  (** Get unique elements from list *)
  let unique_list (lst : int list) : int list =
    let module S = Set.Make (Int) in
    S.elements (List.fold_left (fun s x -> S.add x s) S.empty lst)
  ;;

  (** [find_hai_pos a] finds all valid meld decompositions for a tile pattern.

      For each possible pair (雀頭), attempts to extract melds in two orderings:
      - Kotsu-first (刻子 before 順子)
      - Shuntsu-first (順子 before 刻子)

      Both orderings are tried because they can yield different valid decompositions
      that affect yaku scoring.

      Returns list of encoded decomposition values with bit layout:
      - Bits 0-2:   kotsu count (0-4)
      - Bits 3-5:   shuntsu count (0-4)
      - Bits 6-9:   pair position (1-13)
      - Bits 10+:   meld positions (4 bits each)
      - Bit 26:     seven pairs flag (七対子)
      - Bit 27:     nine gates flag (九蓮宝燈)
      - Bit 28:     pure straight flag (一気通貫)
      - Bit 29:     two consecutive runs flag (二盃口)
      - Bit 30:     one consecutive run flag (一盃口) *)
  let find_hai_pos (a : int array array) : int list =
    let ret_array = ref [] in
    let p_atama = ref 0 in
    (* Try each position as pair (head) *)
    for i = 0 to Array.length a - 1 do
      for j = 0 to Array.length a.(i) - 1 do
        if a.(i).(j) >= 2
        then
          (* Try both kotsu-first and shuntsu-first orderings *)
          for kotsu_shuntsu = 0 to 1 do
            let t = deep_copy a in
            t.(i).(j) <- t.(i).(j) - 2;
            let p = ref 0 in
            let p_kotsu = ref [] in
            let p_shuntsu = ref [] in
            (* Extract melds *)
            for k = 0 to Array.length t - 1 do
              for m = 0 to Array.length t.(k) - 1 do
                if kotsu_shuntsu = 0
                then (
                  (* Kotsu first *)
                  (* Extract kotsu *)
                  if t.(k).(m) >= 3
                  then (
                    t.(k).(m) <- t.(k).(m) - 3;
                    p_kotsu := !p :: !p_kotsu);
                  (* Extract shuntsu *)
                  while
                    Array.length t.(k) - m >= 3
                    && t.(k).(m) >= 1
                    && t.(k).(m + 1) >= 1
                    && t.(k).(m + 2) >= 1
                  do
                    t.(k).(m) <- t.(k).(m) - 1;
                    t.(k).(m + 1) <- t.(k).(m + 1) - 1;
                    t.(k).(m + 2) <- t.(k).(m + 2) - 1;
                    p_shuntsu := !p :: !p_shuntsu
                  done)
                else (
                  (* Shuntsu first *)
                  (* Extract shuntsu *)
                  while
                    Array.length t.(k) - m >= 3
                    && t.(k).(m) >= 1
                    && t.(k).(m + 1) >= 1
                    && t.(k).(m + 2) >= 1
                  do
                    t.(k).(m) <- t.(k).(m) - 1;
                    t.(k).(m + 1) <- t.(k).(m + 1) - 1;
                    t.(k).(m + 2) <- t.(k).(m + 2) - 1;
                    p_shuntsu := !p :: !p_shuntsu
                  done;
                  (* Extract kotsu *)
                  if t.(k).(m) >= 3
                  then (
                    t.(k).(m) <- t.(k).(m) - 3;
                    p_kotsu := !p :: !p_kotsu));
                p := !p + 1
              done
            done;
            (* Check if valid winning hand (all tiles used) *)
            let flat = flatten t in
            if all_zero flat
            then (
              (* Reverse lists to restore insertion order *)
              let p_kotsu = List.rev !p_kotsu in
              let p_shuntsu = List.rev !p_shuntsu in
              (* Calculate base value *)
              let ret =
                ref
                  (List.length p_kotsu + (List.length p_shuntsu lsl 3) + (!p_atama lsl 6))
              in
              (* Encode meld positions *)
              let len = ref 10 in
              List.iter
                (fun x ->
                   ret := !ret lor (x lsl !len);
                   len := !len + 4)
                p_kotsu;
              List.iter
                (fun x ->
                   ret := !ret lor (x lsl !len);
                   len := !len + 4)
                p_shuntsu;
              (* Nine gates flag (chuuren) *)
              if Array.length a = 1
              then
                if
                  a = [| [| 4; 1; 1; 1; 1; 1; 1; 1; 3 |] |]
                  || a = [| [| 3; 2; 1; 1; 1; 1; 1; 1; 3 |] |]
                  || a = [| [| 3; 1; 2; 1; 1; 1; 1; 1; 3 |] |]
                  || a = [| [| 3; 1; 1; 2; 1; 1; 1; 1; 3 |] |]
                  || a = [| [| 3; 1; 1; 1; 2; 1; 1; 1; 3 |] |]
                  || a = [| [| 3; 1; 1; 1; 1; 2; 1; 1; 3 |] |]
                  || a = [| [| 3; 1; 1; 1; 1; 1; 2; 1; 3 |] |]
                  || a = [| [| 3; 1; 1; 1; 1; 1; 1; 2; 3 |] |]
                  || a = [| [| 3; 1; 1; 1; 1; 1; 1; 1; 4 |] |]
                then ret := !ret lor (1 lsl 27);
              (* Pure straight flag (ittsuu) *)
              if Array.length a <= 3 && List.length p_shuntsu >= 3
              then (
                let p_ikki = ref 0 in
                for b_idx = 0 to Array.length a - 1 do
                  let b = a.(b_idx) in
                  if Array.length b = 9
                  then (
                    let b_ikki1 = ref false in
                    let b_ikki2 = ref false in
                    let b_ikki3 = ref false in
                    List.iter
                      (fun x_ikki ->
                         if x_ikki = !p_ikki then b_ikki1 := true;
                         if x_ikki = !p_ikki + 3 then b_ikki2 := true;
                         if x_ikki = !p_ikki + 6 then b_ikki3 := true)
                      p_shuntsu;
                    if !b_ikki1 && !b_ikki2 && !b_ikki3 then ret := !ret lor (1 lsl 28));
                  p_ikki := !p_ikki + Array.length b
                done);
              (* Two consecutive runs flag (ryanpeikou) *)
              if
                List.length p_shuntsu = 4
                &&
                match p_shuntsu with
                | [ a; b; c; d ] -> a = b && c = d
                | _ -> false
              then ret := !ret lor (1 lsl 29) (* One consecutive run flag (ipeikou) *)
              else if
                List.length p_shuntsu >= 2
                && List.length p_kotsu + List.length p_shuntsu = 4
              then (
                let uniq_count = List.length (unique_list p_shuntsu) in
                if List.length p_shuntsu - uniq_count >= 1 then ret := !ret lor (1 lsl 30));
              ret_array := !ret :: !ret_array)
          done;
        p_atama := !p_atama + 1
      done
    done;
    (* Check for seven pairs (chitoi) *)
    if List.length !ret_array > 0
    then (
      let module
        (* Remove duplicates while preserving order *)
        IntSet =
        Set.Make (Int)
      in
      let seen = ref IntSet.empty in
      let result = ref [] in
      List.iter
        (fun x ->
           if not (IntSet.mem x !seen)
           then (
             seen := IntSet.add x !seen;
             result := x :: !result))
        (List.rev !ret_array);
      List.rev !result)
    else (
      let flat = flatten a in
      let total = Array.fold_left ( + ) 0 flat in
      if total = 14 && Array.for_all (fun x -> x = 2) flat then [ 1 lsl 26 ] else [])
  ;;
end

(** Output helpers *)
module Output = struct
  (** Write a 32-bit integer in little-endian format *)
  let write_u32_le (oc : out_channel) (n : int) : unit =
    output_byte oc (n land 0xFF);
    output_byte oc ((n lsr 8) land 0xFF);
    output_byte oc ((n lsr 16) land 0xFF);
    output_byte oc ((n lsr 24) land 0xFF)
  ;;

  (** Write entries in little-endian binary format *)
  let write_binary (oc : out_channel) (entries : (int * int list) list) : unit =
    List.iter
      (fun (key, values) ->
         write_u32_le oc key;
         output_byte oc (List.length values);
         List.iter (write_u32_le oc) values)
      entries
  ;;
end

(** Main program *)
let () =
  (* List to preserve order of patterns *)
  let entries : (int * int list) list ref = ref [] in
  (* Helper to add pattern to table *)
  let add_pattern (pattern : int array array) : unit =
    let key = Key.calc_key pattern in
    let values = Decompose.find_hai_pos pattern in
    if List.length values > 0
    then
      (* Check if key already exists *)
      if not (List.exists (fun (k, _) -> k = key) !entries)
      then entries := (key, values) :: !entries
  in
  (* Generate chitoi patterns (seven pairs) *)
  let chitoi =
    Pattern.ptn [| [| 2 |]; [| 2 |]; [| 2 |]; [| 2 |]; [| 2 |]; [| 2 |]; [| 2 |] |]
  in
  let chitoi =
    List.filter
      (fun x ->
         let flat = Decompose.flatten x in
         Array.for_all (fun y -> y = 2) flat)
      chitoi
  in
  (* Process all patterns *)
  List.iter
    add_pattern
    (List.concat
       [ Pattern.ptn
           [| [| 1; 1; 1 |]; [| 1; 1; 1 |]; [| 1; 1; 1 |]; [| 1; 1; 1 |]; [| 2 |] |]
       ; Pattern.ptn [| [| 1; 1; 1 |]; [| 1; 1; 1 |]; [| 1; 1; 1 |]; [| 3 |]; [| 2 |] |]
       ; Pattern.ptn [| [| 1; 1; 1 |]; [| 1; 1; 1 |]; [| 3 |]; [| 3 |]; [| 2 |] |]
       ; Pattern.ptn [| [| 1; 1; 1 |]; [| 3 |]; [| 3 |]; [| 3 |]; [| 2 |] |]
       ; Pattern.ptn [| [| 3 |]; [| 3 |]; [| 3 |]; [| 3 |]; [| 2 |] |]
       ; chitoi
       ; Pattern.ptn [| [| 1; 1; 1 |]; [| 1; 1; 1 |]; [| 1; 1; 1 |]; [| 2 |] |]
       ; Pattern.ptn [| [| 1; 1; 1 |]; [| 1; 1; 1 |]; [| 3 |]; [| 2 |] |]
       ; Pattern.ptn [| [| 1; 1; 1 |]; [| 3 |]; [| 3 |]; [| 2 |] |]
       ; Pattern.ptn [| [| 3 |]; [| 3 |]; [| 3 |]; [| 2 |] |]
       ; Pattern.ptn [| [| 1; 1; 1 |]; [| 1; 1; 1 |]; [| 2 |] |]
       ; Pattern.ptn [| [| 1; 1; 1 |]; [| 3 |]; [| 2 |] |]
       ; Pattern.ptn [| [| 3 |]; [| 3 |]; [| 2 |] |]
       ; Pattern.ptn [| [| 1; 1; 1 |]; [| 2 |] |]
       ; Pattern.ptn [| [| 3 |]; [| 2 |] |]
       ; Pattern.ptn [| [| 2 |] |]
       ]);
  (* Reverse to get correct insertion order (we prepended) *)
  let entries = List.rev !entries in
  (* Output binary format to agari.bin *)
  let oc = open_out_bin "agari.bin" in
  Output.write_binary oc entries;
  close_out oc
;;
