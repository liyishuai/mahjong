(** Hand format conversions.

    Note that all functions in this module that take or produce strings are dealing
    with tenhou.net/2 format tile description (like "0m 123z") instead of mjai (like
    "5mr ESW").

    In tenhou.net format:
    - "0" represents the red five (aka dora)
    - Numbers 1-9 followed by m/p/s/z for suit
    - Spaces are allowed
*)

open Tiles

(** {1 Parsing functions} *)

(** Parse a tenhou.net format string into a 37-count array (including red fives).
    @param s Tenhou.net format string (e.g., "22334450m234p2s3s4s")
    @return Ok array of 37 counts (index 34-36 are red fives), or Error with message *)
let hand_with_aka (s : string) : (int array, string) result =
  (* Check ASCII only *)
  if not (String.for_all (fun c -> Char.code c < 128) s) then
    Error "hand contains non-ASCII content"
  else
    let ret = Array.make 37 0 in
    let stack = ref [] in

    try
      String.iter
        (fun c ->
          match c with
          | '0' .. '9' ->
              stack := (Char.code c - Char.code '0') :: !stack
          | 'm' | 'p' | 's' | 'z' ->
              let kind =
                match c with
                | 'm' -> 0
                | 'p' -> 1
                | 's' -> 2
                | 'z' -> 3
                | _ -> assert false
              in
              List.iter
                (fun t ->
                  let idx =
                    if t = 0 then
                      (* Red five *)
                      (match c with
                       | 'm' -> tile_id_5mr
                       | 'p' -> tile_id_5pr
                       | 's' -> tile_id_5sr
                       | _ -> raise (Invalid_argument "unexpected byte with red five"))
                    else kind * 9 + t - 1
                  in
                  ret.(idx) <- ret.(idx) + 1)
                !stack;
              stack := []
          | ' ' | '\t' | '\n' -> ()
          | _ ->
              raise
                (Invalid_argument
                   (Printf.sprintf "unexpected character '%c'" c)))
        s;

      (* Check for incomplete numbers at end *)
      if !stack <> [] then
        raise (Invalid_argument "incomplete number at end of string");

      Ok ret
    with Invalid_argument msg -> Error msg

(** Parse a tenhou.net format string into a 34-count array (without red fives).
    Red fives are merged with normal fives.
    @param s Tenhou.net format string
    @return Ok array of 34 counts, or Error with message *)
let hand (s : string) : (int array, string) result =
  match hand_with_aka s with
  | Error _ as e -> e
  | Ok hand37 ->
      let ret = Array.make 34 0 in
      (* Copy counts, merging red fives *)
      for i = 0 to 33 do
        ret.(i) <- hand37.(i)
      done;
      (* Merge red fives *)
      ret.(tile_id_5m) <- ret.(tile_id_5m) + hand37.(tile_id_5mr);
      ret.(tile_id_5p) <- ret.(tile_id_5p) + hand37.(tile_id_5pr);
      ret.(tile_id_5s) <- ret.(tile_id_5s) + hand37.(tile_id_5sr);
      Ok ret

(** {1 Conversion functions} *)

(** Convert a 37-count array (with red fives) to an array of tile IDs.
    Each count expands to that many tile values. *)
let tile37_to_array (tiles37 : int array) : int array =
  let size = Array.fold_left ( + ) 0 tiles37 in
  let ret = Array.make size (tile_id_unknown) in
  let pos = ref 0 in
  Array.iteri
    (fun tid count ->
      if count > 0 then
        for _i = 1 to count do
          ret.(!pos) <- tid;
          incr pos
        done)
    tiles37;
  ret

(** Convert a 34-count array (without red fives) to an array of tile IDs.
    Each count expands to that many tile values. *)
let tile34_to_array (tiles34 : int array) : int array =
  let size = Array.fold_left ( + ) 0 tiles34 in
  let ret = Array.make size (tile_id_unknown) in
  let pos = ref 0 in
  Array.iteri
    (fun tid count ->
      if count > 0 then
        for _i = 1 to count do
          ret.(!pos) <- tid;
          incr pos
        done)
    tiles34;
  ret

(** Convert a 34-count array to tenhou.net format string.
    @param tiles34 Array of 34 tile counts
    @param aka Triple of booleans indicating presence of red fives (man, pin, sou)
    @return Tenhou.net format string *)
let tiles_to_string (tiles34 : int array) (aka : bool array) : string =
  if Array.length aka <> 3 then
    invalid_arg "aka array must have exactly 3 elements";

  let parts = ref [] in

  (* Process suited tiles (m, p, s) *)
  for kind = 0 to 2 do
    let partial = ref "" in
    for num = 0 to 8 do
      let count = tiles34.(kind * 9 + num) in
      if count > 0 then
        let literal_num = num + 1 in
        let suffix =
          if literal_num = 5 && aka.(kind) then
            (* Red five: use '0' and one fewer normal 5 *)
            "0" ^ String.make (count - 1) (char_of_int (Char.code '0' + literal_num))
          else String.make count (char_of_int (Char.code '0' + literal_num))
        in
        partial := !partial ^ suffix
    done;

    if !partial <> "" then
      let suffix =
        match kind with
        | 0 -> "m"
        | 1 -> "p"
        | 2 -> "s"
        | _ -> assert false
      in
      parts := (!partial ^ suffix) :: !parts
  done;

  (* Process honor tiles *)
  let jihai = ref "" in
  for num = 0 to 6 do
    let count = tiles34.(27 + num) in
    if count > 0 then
      jihai := !jihai ^ String.make count (char_of_int (Char.code '0' + num + 1))
  done;

  (* Join parts *)
  let suhai_parts = List.rev !parts in
  match !jihai with
  | "" -> String.concat " " suhai_parts
  | j ->
      match suhai_parts with
      | [] -> j ^ "z"
      | _ -> String.concat " " suhai_parts ^ " " ^ j ^ "z"
