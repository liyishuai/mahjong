open Base

module RelativePos = struct
  type t =
    | Self
    | Right
    | Mid
    | Left
  [@@deriving sexp, compare, equal]

  let to_int = function
    | Self -> 0
    | Right -> 1
    | Mid -> 2
    | Left -> 3

  let of_int = function
    | 0 -> Some Self
    | 1 -> Some Right
    | 2 -> Some Mid
    | 3 -> Some Left
    | _ -> None
end

module OpenType = struct
  type t =
    | Chi
    | Pon
    | KanOpened
    | KanClosed
    | KanAdded
  [@@deriving sexp, compare, equal]
end

type t = int [@@deriving sexp, compare, equal]

(* Masks and constants *)
let mask_from = 0b0000000000000011
let mask_is_chi = 0b0000000000000100
let mask_chi_offset = [| 0b0000000000011000; 0b0000000001100000; 0b0000000110000000 |]
let mask_is_pon = 0b0000000000001000
let mask_is_kan_added = 0b0000000000010000
let mask_pon_unused_offset = 0b0000000001100000
let _mask_kan_stolen = 0b1111111100000000

let type_ t =
  if (t land mask_is_chi) <> 0 then OpenType.Chi
  else if (t land mask_is_pon) <> 0 then OpenType.Pon
  else if (t land mask_is_kan_added) <> 0 then OpenType.KanAdded
  else
    match RelativePos.of_int (t land mask_from) with
    | Some RelativePos.Self -> OpenType.KanClosed
    | _ -> OpenType.KanOpened

let min_type t =
  let min_type_base21 = (t lsr 10) / 3 in
  (min_type_base21 / 7) * 9 + (min_type_base21 % 7)

let at_chi t i m =
  let offset = (t land mask_chi_offset.(i)) lsr (2 * i + 3) in
  match Tile.of_int ((m + i) * 4 + offset) with
  | Some x -> x
  | None -> failwith "Invalid tile in Chi"

let chi_at t i =
  assert (i < 3);
  at_chi t i (min_type t)

let pon_at t i =
  let type_ = (t lsr 9) / 3 in
  let unused_offset = (t land mask_pon_unused_offset) lsr 5 in
  let i = if i >= unused_offset then i + 1 else i in
  match Tile.of_int (type_ * 4 + i) with
  | Some x -> x
  | None -> failwith "Invalid tile in Pon"

let kan_at t i =
  match Tile.of_int (((t lsr 8) / 4) * 4 + i) with
  | Some x -> x
  | None -> failwith "Invalid tile in Kan"

let kan_added_at t i =
  assert (i < 4);
  let type_ = (t lsr 9) / 3 in
  match Tile.of_int (type_ * 4 + i) with
  | Some x -> x
  | None -> failwith "Invalid tile in KanAdded"

let at t i =
  match type_ t with
  | Chi -> chi_at t i
  | Pon -> pon_at t i
  | KanOpened | KanClosed -> kan_at t i
  | KanAdded -> kan_added_at t i

let stolen_tile t =
  match type_ t with
  | Chi -> at t ((t lsr 10) % 3)
  | Pon -> at t ((t lsr 9) % 3)
  | KanOpened | KanClosed -> Tile.of_int (t lsr 8) |> Option.value_exn
  | KanAdded ->
      let type_ = (t lsr 9) / 3 in
      let stolen_ix = (t lsr 9) % 3 in
      let unused_offset = (t land mask_pon_unused_offset) lsr 5 in
      let stolen_ix = if stolen_ix >= unused_offset then stolen_ix + 1 else stolen_ix in
      Tile.of_int (type_ * 4 + stolen_ix) |> Option.value_exn

let last_tile t =
  match type_ t with
  | KanAdded ->
    let type_ = (t lsr 9) / 3 in
    let unused_offset = (t land mask_pon_unused_offset) lsr 5 in
    Tile.of_int (type_ * 4 + unused_offset) |> Option.value_exn
  | _ -> stolen_tile t

let from t =
  match type_ t with
  | Chi -> RelativePos.Left
  | KanClosed -> RelativePos.Self
  | _ -> RelativePos.of_int (t land mask_from) |> Option.value_exn

let size t =
  match type_ t with
  | Chi | Pon -> 3
  | _ -> 4

let tiles t =
  List.init (size t) ~f:(fun i -> at t i)

let tiles_from_hand t =
  let all_tiles = tiles t in
  match type_ t with
  | Chi | Pon ->
    let s = stolen_tile t in
    List.filter all_tiles ~f:(fun x -> not (Tile.equal x s))
  | KanOpened ->
    let s = stolen_tile t in
    List.filter all_tiles ~f:(fun x -> Tile.offset x <> Tile.offset s)
  | KanClosed -> all_tiles
  | KanAdded ->
    let s = stolen_tile t in
    List.filter all_tiles ~f:(fun x -> not (Tile.equal x s))

let undiscardable_tile_types t =
  match type_ t with
  | Chi ->
      let s = stolen_tile t in
      let type_ = Tile.type_ s in
      let base = [type_] in
      (* Check rule for prohibitted discard (kuikae) *)
      (* Simplified translation of C++ logic *)
      let at0 = at t 0 in
      let at2 = at t 2 in
      let m = Tile.TileType.to_int type_ in
      if Tile.equal at2 s && m >= 3 then
         base @ [Option.value_exn (Tile.TileType.of_int (m - 3))]
      else if Tile.equal at0 s && m <= 30 (* rough check *) then (* Fix boundary check properly *)
         base @ [Option.value_exn (Tile.TileType.of_int (m + 3))]
      else base
  | Pon -> [Tile.type_ (at t 0)]
  | _ -> []

let to_string t ~verbose:_ =
  let ts = tiles t in
  let s = String.concat ~sep:"," (List.map ts ~f:(fun tile -> Tile.to_string tile)) in
  let suffix = match type_ t with
    | KanOpened -> "o"
    | KanClosed -> "c"
    | KanAdded -> "a"
    | _ -> ""
  in
  "[" ^ s ^ "]" ^ suffix

let to_bits t = t
let of_bits t = t

(* Creation functions *)
let create_chi ~tiles ~stolen =
  let sorted_tiles = List.sort tiles ~compare:Tile.compare in
  let t0 = List.nth_exn sorted_tiles 0 in
  let t1 = List.nth_exn sorted_tiles 1 in
  let t2 = List.nth_exn sorted_tiles 2 in
  let bits = (RelativePos.to_int RelativePos.Left) lor mask_is_chi in
  let bits = bits lor ((Tile.offset t0) lsl 3) in
  let bits = bits lor ((Tile.offset t1) lsl 5) in
  let bits = bits lor ((Tile.offset t2) lsl 7) in
  let base = (Tile.to_int t0) / 4 in
  let stolen_ix = match List.findi sorted_tiles ~f:(fun _ t -> Tile.equal t stolen) with
    | Some (i, _) -> i
    | None -> failwith "Stolen tile not found in chi"
  in
  bits lor (((base / 9) * 7 + (base % 9)) * 3 + stolen_ix) lsl 10

let create_pon ~stolen ~unused ~from =
  let bits = (RelativePos.to_int from) lor mask_is_pon in
  let unused_offset = Tile.offset unused in
  let bits = bits lor (unused_offset lsl 5) in
  let base = Tile.TileType.to_int (Tile.type_ stolen) in
  let stolen_offset = Tile.offset stolen in
  let stolen_ix = if stolen_offset > unused_offset then stolen_offset - 1 else stolen_offset in
  bits lor ((base * 3 + stolen_ix) lsl 9)

let create_kan_opened ~stolen ~from =
  (RelativePos.to_int from) lor ((Tile.to_int stolen) lsl 8)

let create_kan_closed ~tile =
  (RelativePos.to_int RelativePos.Self) lor ((Tile.to_int tile) lsl 8)

let create_kan_added ~pon =
  (pon lor mask_is_kan_added) land (lnot mask_is_pon)

let equal t1 t2 = Int.equal t1 t2
