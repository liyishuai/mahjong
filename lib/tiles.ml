(* 38 tile IDs: 0-36 for normal tiles, 37 for unknown
   Mapping:
   0-8: 1m-9m, 9-17: 1p-9p, 18-26: 1s-9s
   27-30: E,S,W,N, 31-33: P,F,C
   34-36: 5mr,5pr,5sr (red fives)
   37: ? (unknown)
*)

(** Chi sequence types for chii (sequence meld) representations.
    These types represent which position a tile was taken from when forming
    a chii (sequence meld of three consecutive tiles). *)
type chi_type =
  | Low  (** Lower sequence: ABC from ABC *)
  | Mid  (** Middle sequence: ABC from A[BC]D *)
  | High (** Upper sequence: ABC from [AB]CD *)

type tile = int

type invalid_tile =
  | Number of int
  | String of string

(* Internal tile ID constants *)
let tile_id_1m = 0
let tile_id_2m = 1
let tile_id_3m = 2
let tile_id_4m = 3
let tile_id_5m = 4
let tile_id_6m = 5
let tile_id_7m = 6
let tile_id_8m = 7
let tile_id_9m = 8
let tile_id_1p = 9
let tile_id_2p = 10
let tile_id_3p = 11
let tile_id_4p = 12
let tile_id_5p = 13
let tile_id_6p = 14
let tile_id_7p = 15
let tile_id_8p = 16
let tile_id_9p = 17
let tile_id_1s = 18
let tile_id_2s = 19
let tile_id_3s = 20
let tile_id_4s = 21
let tile_id_5s = 22
let tile_id_6s = 23
let tile_id_7s = 24
let tile_id_8s = 25
let tile_id_9s = 26
let tile_id_E = 27
let tile_id_S = 28
let tile_id_W = 29
let tile_id_N = 30
let tile_id_P = 31
let tile_id_F = 32
let tile_id_C = 33
let tile_id_5mr = 34
let tile_id_5pr = 35
let tile_id_5sr = 36
let tile_id_unknown = 37

let mjai_pai_strings =
  [| "1m"; "2m"; "3m"; "4m"; "5m"; "6m"; "7m"; "8m"; "9m"; "1p"; "2p"; "3p"; "4p";
     "5p"; "6p"; "7p"; "8p"; "9p"; "1s"; "2s"; "3s"; "4s"; "5s"; "6s"; "7s"; "8s";
     "9s"; "E"; "S"; "W"; "N"; "P"; "F"; "C"; "5mr"; "5pr"; "5sr"; "?" |]

let mjai_pai_strings_len = Array.length mjai_pai_strings

let discard_priorities =
  [| 6; 5; 4; 3; 2; 3; 4; 5; 6; (* m *)
     6; 5; 4; 3; 2; 3; 4; 5; 6; (* p *)
     6; 5; 4; 3; 2; 3; 4; 5; 6; (* s *)
     7; 7; 7; 7; 7; 7; 7; (* z *)
     1; 1; 1; (* aka *)
     0 (* unknown *)
  |]

(* Build string to tile ID mapping *)
let string_to_tile_map =
  let tbl = Hashtbl.create 64 in
  Array.iteri (fun id s -> Hashtbl.add tbl s id) mjai_pai_strings;
  tbl

(* Helper to format error message *)
let string_of_invalid_tile = function
  | Number n -> Printf.sprintf "invalid tile number: %d" n
  | String s -> Printf.sprintf "invalid tile string: \"%s\"" s

(* Conversion functions *)
let tile_of_int (v : int) : (tile, invalid_tile) result =
  if v >= 0 && v < mjai_pai_strings_len then Ok v else Error (Number v)

let tile_of_int_exn (v : int) : tile =
  match tile_of_int v with
  | Ok tile -> tile
  | Error e -> invalid_arg (string_of_invalid_tile e)

let tile_of_string (s : string) : (tile, invalid_tile) result =
  try
    match Hashtbl.find string_to_tile_map s with
    | id -> Ok id
    | exception Not_found -> Error (String s)
  with Not_found -> Error (String s)

let int_of_tile (t : tile) : int = t

let string_of_tile (t : tile) : string =
  mjai_pai_strings.(t)

(* Tile properties *)
let deaka (t : tile) : tile =
  match t with
  | 34 -> (* 5mr *) 4 (* 5m *)
  | 35 -> (* 5pr *) 13 (* 5p *)
  | 36 -> (* 5sr *) 22 (* 5s *)
  | _ -> t

let akaize (t : tile) : tile =
  match t with
  | 4 -> (* 5m *) 34 (* 5mr *)
  | 13 -> (* 5p *) 35 (* 5pr *)
  | 22 -> (* 5s *) 36 (* 5sr *)
  | _ -> t

let is_aka (t : tile) : bool =
  t = 34 || t = 35 || t = 36

let is_jihai (t : tile) : bool =
  t >= 27 && t <= 33

let is_yaokyuu (t : tile) : bool =
  (* Terminals: 1m,9m,1p,9p,1s,9s and all honors *)
  t = 0 || t = 8 || t = 9 || t = 17 || t = 18 || t = 26 || t >= 27 && t <= 33

let is_unknown (t : tile) : bool =
  t >= tile_id_unknown

(* Navigation *)
let next (t : tile) : tile =
  if is_unknown t then t
  else
    let tile = deaka t in
    let kind = tile / 9 in
    let num = tile mod 9 in
    if kind < 3 then
      kind * 9 + (num + 1) mod 9
    else if num < 4 then
      3 * 9 + (num + 1) mod 4
    else
      3 * 9 + 4 + (num - 4 + 1) mod 3

let prev (t : tile) : tile =
  if is_unknown t then t
  else
    let tile = deaka t in
    let kind = tile / 9 in
    let num = tile mod 9 in
    if kind < 3 then
      kind * 9 + (num + 9 - 1) mod 9
    else if num < 4 then
      3 * 9 + (num + 4 - 1) mod 4
    else
      3 * 9 + 4 + (num - 4 + 3 - 1) mod 3

let augment (t : tile) : tile =
  if is_unknown t then t
  else
    let tile = deaka t in
    let tid = tile in
    let kind = tid / 9 in
    let ret =
      match kind with
      | 0 -> tid + 9 (* m -> p *)
      | 1 -> tid - 9 (* p -> m *)
      | _ -> tile (* s and honors stay the same *)
    in
    if is_aka t then akaize ret else ret

(* Comparison for discard priority *)
let cmp_discard_priority (t1 : tile) (t2 : tile) : int =
  let p1 = discard_priorities.(t1) in
  let p2 = discard_priorities.(t2) in
  if p1 <> p2 then compare p1 p2 else compare t2 t1

(* Default tile (unknown) *)
let default () : tile = tile_id_unknown
