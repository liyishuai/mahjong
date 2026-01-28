open Base

module AbsolutePos = struct
  type t =
    | InitEast  (* 0 *)
    | InitSouth (* 1 *)
    | InitWest  (* 2 *)
    | InitNorth (* 3 *)
  [@@deriving sexp, compare, equal, enumerate]

  let to_int = function
    | InitEast -> 0
    | InitSouth -> 1
    | InitWest -> 2
    | InitNorth -> 3

  let of_int = function
    | 0 -> Some InitEast
    | 1 -> Some InitSouth
    | 2 -> Some InitWest
    | 3 -> Some InitNorth
    | _ -> None
end

module ActionType = struct
  type t =
    | Discard
    | Tsumogiri
    | Riichi
    | Tsumo
    | Ron
    | Chi
    | Pon
    | KanOpened
    | KanClosed
    | KanAdded
    | No
    | NineTiles
    | Dummy
  [@@deriving sexp, compare, equal, enumerate]
end

type t = {
  type_ : ActionType.t;
  who : AbsolutePos.t;
  tile : Tile.t option;
  open_ : Open.t option;
}
[@@deriving sexp, equal]

(* Creation functions *)
let create_discard ~who ~tile =
  assert (Tile.of_int tile |> Option.is_some);
  { type_ = Discard; who; tile = Some tile; open_ = None }

let create_tsumogiri ~who ~tile =
  assert (Tile.of_int tile |> Option.is_some);
  { type_ = Tsumogiri; who; tile = Some tile; open_ = None }

let create_riichi ~who =
  { type_ = Riichi; who; tile = None; open_ = None }

let create_tsumo ~who ~tile =
  assert (Tile.of_int tile |> Option.is_some);
  { type_ = Tsumo; who; tile = Some tile; open_ = None }

let create_ron ~who ~tile =
  assert (Tile.of_int tile |> Option.is_some);
  { type_ = Ron; who; tile = Some tile; open_ = None }

let create_open ~who ~open_ =
  { type_ = begin match Open.type_ open_ with
    | Open.OpenType.Chi -> ActionType.Chi
    | Open.OpenType.Pon -> ActionType.Pon
    | Open.OpenType.KanOpened -> ActionType.KanOpened
    | Open.OpenType.KanClosed -> ActionType.KanClosed
    | Open.OpenType.KanAdded -> ActionType.KanAdded
  end; who; tile = None; open_ = Some open_ }

let create_no ~who =
  { type_ = No; who; tile = None; open_ = None }

let create_nine_tiles ~who =
  { type_ = NineTiles; who; tile = None; open_ = None }

let create_dummy ~who =
  { type_ = Dummy; who; tile = None; open_ = None }

let type_ t = t.type_
let who t = t.who
let tile t = t.tile
let open_ t = t.open_

(* Validation *)
let is_valid t =
  (* Check who is in valid range *)
  let who_valid = match t.who with
    | InitEast | InitSouth | InitWest | InitNorth -> true
  in
  if not who_valid then false else
  match t.type_ with
  | ActionType.Discard | ActionType.Tsumogiri | ActionType.Tsumo | ActionType.Ron ->
      (match t.tile with
       | Some tile -> Tile.of_int tile |> Option.is_some
       | None -> false)
      && Option.is_none t.open_
  | ActionType.Chi | ActionType.Pon | ActionType.KanOpened | ActionType.KanClosed | ActionType.KanAdded ->
      Option.is_none t.tile && Option.is_some t.open_
  | ActionType.Riichi | ActionType.NineTiles | ActionType.No | ActionType.Dummy ->
      Option.is_none t.tile && Option.is_none t.open_

(* Encode helper functions to avoid complex nesting *)
let encode_discard tile =
  if not (Tile.is_red_five tile)
  then Tile.TileType.to_int (Tile.type_ tile)
  else
    match Tile.type_ tile with
    | Tile.TileType.M5 -> 34
    | Tile.TileType.P5 -> 35
    | Tile.TileType.S5 -> 36
    | _ -> assert false

let encode_tsumogiri tile =
  if not (Tile.is_red_five tile)
  then Tile.TileType.to_int (Tile.type_ tile) + 37
  else
    match Tile.type_ tile with
    | Tile.TileType.M5 -> 71
    | Tile.TileType.P5 -> 72
    | Tile.TileType.S5 -> 73
    | _ -> assert false

let encode_chi tiles =
  if not (List.exists tiles ~f:Tile.is_red_five)
  then begin
    let tile0 = List.nth_exn tiles 0 in
    let base = Tile.num tile0 - 1 in
    match Tile.type_ tile0 with
    | Tile.TileType.M1 | Tile.TileType.M2 | Tile.TileType.M3 | Tile.TileType.M4
    | Tile.TileType.M5 | Tile.TileType.M6 | Tile.TileType.M7 | Tile.TileType.M8 | Tile.TileType.M9 ->
        base + 74
    | Tile.TileType.P1 | Tile.TileType.P2 | Tile.TileType.P3 | Tile.TileType.P4
    | Tile.TileType.P5 | Tile.TileType.P6 | Tile.TileType.P7 | Tile.TileType.P8 | Tile.TileType.P9 ->
        base + 81
    | _ ->
        (* Souzu *)
        base + 88
  end else begin
    let tile0 = List.nth_exn tiles 0 in
    match Tile.type_ tile0 with
    | Tile.TileType.M3 -> 95
    | Tile.TileType.M4 -> 96
    | Tile.TileType.M5 -> 97
    | Tile.TileType.P3 -> 98
    | Tile.TileType.P4 -> 99
    | Tile.TileType.P5 -> 100
    | Tile.TileType.S3 -> 101
    | Tile.TileType.S4 -> 102
    | Tile.TileType.S5 -> 103
    | _ -> assert false
  end

let encode_pon tiles =
  if not (List.exists tiles ~f:Tile.is_red_five)
  then
    let tile0 = List.nth_exn tiles 0 in
    Tile.TileType.to_int (Tile.type_ tile0) + 104
  else
    let tile0 = List.nth_exn tiles 0 in
    match Tile.type_ tile0 with
    | Tile.TileType.M5 -> 138
    | Tile.TileType.P5 -> 139
    | Tile.TileType.S5 -> 140
    | _ -> assert false

let encode_kan tiles =
  let tile0 = List.nth_exn tiles 0 in
  Tile.TileType.to_int (Tile.type_ tile0) + 141

(* Encode action to 0-180 code *)
let encode t =
  match t.type_ with
  | ActionType.Discard ->
      let tile = Option.value_exn t.tile in
      encode_discard tile
  | ActionType.Tsumogiri ->
      let tile = Option.value_exn t.tile in
      encode_tsumogiri tile
  | ActionType.Chi ->
      let open_ = Option.value_exn t.open_ in
      let tiles = Open.tiles open_ in
      encode_chi tiles
  | ActionType.Pon ->
      let open_ = Option.value_exn t.open_ in
      let tiles = Open.tiles open_ in
      encode_pon tiles
  | ActionType.KanOpened | ActionType.KanClosed | ActionType.KanAdded ->
      let open_ = Option.value_exn t.open_ in
      let tiles = Open.tiles open_ in
      encode_kan tiles
  | ActionType.Tsumo -> 175
  | ActionType.Ron -> 176
  | ActionType.Riichi -> 177
  | ActionType.NineTiles -> 178
  | ActionType.No -> 179
  | ActionType.Dummy -> 180

(* Decode action from code using legal actions *)
let rec decode ~code ~legal_actions =
  match legal_actions with
  | [] -> failwith "Action not found in legal actions"
  | action :: rest ->
      if encode action = code then action
      else decode ~code ~legal_actions:rest

let equal a b =
  phys_equal a b ||
  (AbsolutePos.equal a.who b.who &&
   ActionType.equal a.type_ b.type_ &&
   Option.equal Tile.equal a.tile b.tile &&
   Option.equal Open.equal a.open_ b.open_)
