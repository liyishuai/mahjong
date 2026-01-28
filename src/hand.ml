open Base

module HandStage = struct
  type t =
    | AfterDiscards
    | AfterDraw
    | AfterDrawAfterKan
    | AfterRiichi
    | AfterTsumo
    | AfterTsumoAfterKan
    | AfterRon
    | AfterChi
    | AfterPon
    | AfterKanOpened
    | AfterKanClosed
    | AfterKanAdded
  [@@deriving sexp, compare, equal]
end

type t = {
  closed_tiles : Tile.t Set.Poly.t;
  opens : Open.t list;
  undiscardable_tiles : Tile.t Set.Poly.t;
  last_tile_added : Tile.t option;
  stage : HandStage.t;
  under_riichi : bool;
  double_riichi : bool;
}

let create tiles =
  assert (List.length tiles = 13);
  {
    closed_tiles = Set.Poly.of_list tiles;
    opens = [];
    undiscardable_tiles = Set.Poly.empty;
    last_tile_added = None;
    stage = HandStage.AfterDiscards;
    under_riichi = false;
    double_riichi = false;
  }

let stage t = t.stage
let closed_tiles t = Set.Poly.to_list t.closed_tiles
let opens t = t.opens
let last_tile_added t = t.last_tile_added
let is_under_riichi t = t.under_riichi

let is_menzen t =
  List.for_all t.opens ~f:(fun o ->
      match Open.type_ o with
      | KanClosed -> true
      | _ -> false)

let size_closed t = Set.Poly.length t.closed_tiles

let size_opened t =
  List.fold t.opens ~init:0 ~f:(fun acc o -> acc + Open.size o)

let size t = size_closed t + size_opened t

let draw t tile =
  assert (
    match t.stage with
    | AfterDiscards | AfterKanOpened | AfterKanClosed | AfterKanAdded -> true
    | _ -> false);
  assert (not (Set.Poly.mem t.closed_tiles tile));
  (* Check if tile is in opens *)
  let in_opens =
    List.exists t.opens ~f:(fun o ->
        List.exists (Open.tiles o) ~f:(fun ot -> Tile.equal ot tile))
  in
  assert (not in_opens);
  let new_stage =
    match t.stage with
    | AfterDiscards -> HandStage.AfterDraw
    | _ -> HandStage.AfterDrawAfterKan
  in
  {
    t with
    closed_tiles = Set.Poly.add t.closed_tiles tile;
    stage = new_stage;
    last_tile_added = Some tile;
  }

let discard t tile =
  assert (Set.Poly.mem t.closed_tiles tile);
  assert (not (Set.Poly.mem t.undiscardable_tiles tile));
  let tsumogiri =
    match t.last_tile_added with
    | Some l -> Tile.equal l tile
    | None -> false
  in
  let new_hand =
    {
      t with
      closed_tiles = Set.Poly.remove t.closed_tiles tile;
      undiscardable_tiles = Set.Poly.empty;
      stage = HandStage.AfterDiscards;
      last_tile_added = None;
    }
  in
  (new_hand, tsumogiri)

let apply_chi t open_ =
  assert (Open.OpenType.equal (Open.type_ open_) Open.OpenType.Chi);
  let tiles_from_hand = Open.tiles_from_hand open_ in
  let closed_tiles =
    List.fold tiles_from_hand ~init:t.closed_tiles ~f:Set.Poly.remove
  in
  let undiscardable_types = Open.undiscardable_tile_types open_ in
  let undiscardable_tiles =
    Set.Poly.filter closed_tiles ~f:(fun tile ->
        List.exists undiscardable_types ~f:(fun tt ->
            Tile.TileType.equal (Tile.type_ tile) tt))
  in
  {
    t with
    closed_tiles;
    undiscardable_tiles;
    opens = t.opens @ [ open_ ];
    stage = AfterChi;
    last_tile_added = Some (Open.last_tile open_);
  }

let apply_pon t open_ =
  assert (Open.OpenType.equal (Open.type_ open_) Open.OpenType.Pon);
  let tiles_from_hand = Open.tiles_from_hand open_ in
  let closed_tiles =
    List.fold tiles_from_hand ~init:t.closed_tiles ~f:Set.Poly.remove
  in
  let undiscardable_types = Open.undiscardable_tile_types open_ in
  let undiscardable_tiles =
    Set.Poly.filter closed_tiles ~f:(fun tile ->
        List.exists undiscardable_types ~f:(fun tt ->
            Tile.TileType.equal (Tile.type_ tile) tt))
  in
  {
    t with
    closed_tiles;
    undiscardable_tiles;
    opens = t.opens @ [ open_ ];
    stage = AfterPon;
    last_tile_added = Some (Open.last_tile open_);
  }

let apply_kan_opened t open_ =
  assert (Open.OpenType.equal (Open.type_ open_) Open.OpenType.KanOpened);
  let tiles_from_hand = Open.tiles_from_hand open_ in
  let closed_tiles =
    List.fold tiles_from_hand ~init:t.closed_tiles ~f:Set.Poly.remove
  in
  {
    t with
    closed_tiles;
    undiscardable_tiles = Set.Poly.empty;
    opens = t.opens @ [ open_ ];
    stage = AfterKanOpened;
    last_tile_added = Some (Open.last_tile open_);
  }

let apply_kan_closed t open_ =
  assert (Open.OpenType.equal (Open.type_ open_) Open.OpenType.KanClosed);
  let tiles_from_hand = Open.tiles_from_hand open_ in
  let closed_tiles =
    List.fold tiles_from_hand ~init:t.closed_tiles ~f:Set.Poly.remove
  in
  {
    t with
    closed_tiles;
    undiscardable_tiles = Set.Poly.empty;
    opens = t.opens @ [ open_ ];
    stage = AfterKanClosed;
    last_tile_added = Some (Open.last_tile open_);
  }

let apply_kan_added t open_ =
  assert (Open.OpenType.equal (Open.type_ open_) Open.OpenType.KanAdded);
  let last_tile = Open.last_tile open_ in
  assert (Set.Poly.mem t.closed_tiles last_tile);
  let closed_tiles = Set.Poly.remove t.closed_tiles last_tile in
  let stolen = Open.stolen_tile open_ in
  let opens =
    List.map t.opens ~f:(fun o ->
        if Open.OpenType.equal (Open.type_ o) Open.OpenType.Pon && Tile.equal (Open.stolen_tile o) stolen
        then open_
        else o)
  in
  {
    t with
    closed_tiles;
    opens;
    undiscardable_tiles = Set.Poly.empty;
    stage = AfterKanAdded;
    last_tile_added = Some last_tile;
  }

let apply_open t open_ =
  match Open.type_ open_ with
  | Chi -> apply_chi t open_
  | Pon -> apply_pon t open_
  | KanOpened -> apply_kan_opened t open_
  | KanClosed -> apply_kan_closed t open_
  | KanAdded -> apply_kan_added t open_

let to_string t ~verbose =
  let closed_str =
    List.map (Set.Poly.to_list t.closed_tiles) ~f:Tile.to_string
    |> String.concat ~sep:","
  in
  let opens_str =
    List.map t.opens ~f:(Open.to_string ~verbose) |> String.concat ~sep:","
  in
  if String.is_empty opens_str
  then closed_str
  else closed_str ^ "," ^ opens_str
