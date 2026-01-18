(** MJAI protocol event types and handling.

    Based on the MJAI protocol defined at:
    https://gimite.net/pukiwiki/index.php?Mjai%20%E9%BA%BB%E9%9B%80AI%E5%AF%BE%E6%88%A6%E3%82%B5%E3%83%BC%E3%83%90
*)

(** Out-of-bound error for actor validation *)
exception Out_of_bound of int

(** MJAI event type representing game actions and state changes *)
type event =
  | None
  | Start_game of {
      names : string array;  (* 4 player names *)
      seed : (int * int) option;  (* Optional (nonce, key) for RNG *)
    }
  | Start_kyoku of {
      bakaze : int;  (* Round wind tile (0-33) *)
      dora_marker : int;  (* Dora indicator tile *)
      kyoku : int;  (* Kyoku number (1-4) *)
      honba : int;  (* Honba counter *)
      kyotaku : int;  (* Riichi stick counter *)
      oya : int;  (* Dealer player (0-3) *)
      scores : int array;  (* 4 player scores *)
      tehais : int array array;  (* 4x13 starting hands *)
    }
  | Tsumo of {
      actor : int;  (* Player (0-3) *)
      pai : int;  (* Drawn tile *)
    }
  | Dahai of {
      actor : int;  (* Player (0-3) *)
      pai : int;  (* Discarded tile *)
      tsumogiri : bool;  (* Whether it's a draw-discard *)
    }
  | Chi of {
      actor : int;  (* Player calling chi *)
      target : int;  (* Player being called from *)
      pai : int;  (* Tile being called *)
      consumed : int array;  (* 2 tiles consumed *)
    }
  | Pon of {
      actor : int;
      target : int;
      pai : int;
      consumed : int array;  (* 2 tiles consumed *)
    }
  | Daiminkan of {
      actor : int;
      target : int;
      pai : int;
      consumed : int array;  (* 3 tiles consumed *)
    }
  | Kakan of {
      actor : int;
      pai : int;
      consumed : int array;  (* 3 tiles from existing pon *)
    }
  | Ankan of {
      actor : int;
      consumed : int array;  (* 4 concealed tiles *)
    }
  | Dora of {
      dora_marker : int;  (* New dora indicator *)
    }
  | Reach of {
      actor : int;
    }
  | Reach_accepted of {
      actor : int;
    }
  | Hora of {
      actor : int;  (* Winner *)
      target : int;  (* Target (self for tsumo) *)
      deltas : int array option;  (* Optional score deltas *)
      ura_markers : int array option;  (* Optional ura dora indicators *)
    }
  | Ryukyoku of {
      deltas : int array option;  (* Optional score deltas *)
    }
  | End_kyoku
  | End_game

(** Metadata for recording AI decision information *)
type metadata = {
  q_values : float array option;
  mask_bits : int option;
  is_greedy : bool option;
  batch_size : int option;
  eval_time_ns : int option;
  shanten : int option;
  at_furiten : bool option;
  kan_select : metadata option;
}

(** Event with optional metadata *)
type event_ext = {
  event : event;
  meta : metadata option;
}

(** Event with can_act flag *)
type event_with_can_act = {
  event : event;
  can_act : bool option;
}

(** Get the actor from an event, if applicable *)
let actor (ev : event) : int option =
  match ev with
  | Tsumo { actor; _ }
  | Dahai { actor; _ }
  | Chi { actor; _ }
  | Pon { actor; _ }
  | Daiminkan { actor; _ }
  | Kakan { actor; _ }
  | Ankan { actor; _ }
  | Reach { actor; _ }
  | Reach_accepted { actor; _ }
  | Hora { actor; _ } -> Some actor
  | _ -> None

(** Check if event is an in-game announcement *)
let is_in_game_announce (ev : event) : bool =
  match ev with
  | Reach_accepted _ | Dora _ | Hora _ -> true
  | _ -> false

(** Augment tiles in event by swapping red dora representation *)
let augment (ev : event) : event =
  let swap_tile t = Tiles.augment t in
  match ev with
  | Start_kyoku { bakaze; dora_marker; kyoku; honba; kyotaku; oya; scores; tehais } ->
      Start_kyoku {
        bakaze = swap_tile bakaze;
        dora_marker = swap_tile dora_marker;
        kyoku; honba; kyotaku; oya; scores;
        tehais = Array.map (Array.map swap_tile) tehais;
      }
  | Tsumo { actor; pai } ->
      Tsumo { actor; pai = swap_tile pai }
  | Dahai { actor; pai; tsumogiri } ->
      Dahai { actor; pai = swap_tile pai; tsumogiri }
  | Chi { actor; target; pai; consumed } ->
      Chi { actor; target; pai = swap_tile pai; consumed = Array.map swap_tile consumed }
  | Pon { actor; target; pai; consumed } ->
      Pon { actor; target; pai = swap_tile pai; consumed = Array.map swap_tile consumed }
  | Daiminkan { actor; target; pai; consumed } ->
      Daiminkan { actor; target; pai = swap_tile pai; consumed = Array.map swap_tile consumed }
  | Kakan { actor; pai; consumed } ->
      Kakan { actor; pai = swap_tile pai; consumed = Array.map swap_tile consumed }
  | Ankan { actor; consumed } ->
      Ankan { actor; consumed = Array.map swap_tile consumed }
  | Dora { dora_marker } ->
      Dora { dora_marker = swap_tile dora_marker }
  | Hora { actor; target; deltas; ura_markers } ->
      Hora { actor; target; deltas;
             ura_markers = Option.map (Array.map swap_tile) ura_markers }
  | ev -> ev

(** Create an EventExt without metadata *)
let no_meta (event : event) : event_ext =
  { event; meta = None }

(** JSON serialization and deserialization *)
module Json = struct
  open Yojson.Safe
  open Yojson.Safe.Util

  (** Validate actor is in range 0-3 *)
  let validate_actor (n : int) : int =
    if n < 0 || n > 3 then raise (Out_of_bound n);
    n

  (** Validate kyoku is in range 1-4 *)
  let validate_kyoku (n : int) : int =
    if n < 1 || n > 4 then raise (Out_of_bound n);
    n

  (** Convert tile string to tile index *)
  let tile_of_json (j : Yojson.Safe.t) : int =
    match j with
    | `String s -> (
        match Tiles.tile_of_string s with
        | Ok tile -> Tiles.int_of_tile tile
        | Error e -> failwith (Tiles.string_of_invalid_tile e)
      )
    | _ -> failwith "Expected string for tile"

  (** Convert tile index to tile string *)
  let tile_to_json (t : int) : Yojson.Safe.t =
    `String (Tiles.string_of_tile t)

  (** Convert array of tiles to JSON *)
  let tiles_to_json (tiles : int array) : Yojson.Safe.t =
    `List (Array.to_list (Array.map tile_to_json tiles))

  (** Parse event from JSON *)
  let event_of_json (j : Yojson.Safe.t) : event =
    let typ = member "type" j |> to_string in
    match typ with
    | "none" -> None
    | "start_game" ->
        let names =
          member "names" j |> to_list |> List.map to_string |> Array.of_list
        in
        let seed =
          try
            let seed_arr = member "seed" j |> to_list in
            match seed_arr with
            | [n; k] -> Some (to_int n, to_int k)
            | _ -> Option.none
          with _ -> Option.none
        in
        Start_game { names; seed }
    | "start_kyoku" ->
        Start_kyoku {
          bakaze = member "bakaze" j |> tile_of_json;
          dora_marker = member "dora_marker" j |> tile_of_json;
          kyoku = member "kyoku" j |> to_int |> validate_kyoku;
          honba = member "honba" j |> to_int;
          kyotaku = member "kyotaku" j |> to_int;
          oya = member "oya" j |> to_int |> validate_actor;
          scores = member "scores" j |> to_list |> List.map to_int |> Array.of_list;
          tehais = member "tehais" j |> to_list
                   |> List.map (fun tehaiJson -> tehaiJson |> to_list |> List.map tile_of_json |> Array.of_list)
                   |> Array.of_list;
        }
    | "tsumo" ->
        Tsumo {
          actor = member "actor" j |> to_int |> validate_actor;
          pai = member "pai" j |> tile_of_json;
        }
    | "dahai" ->
        Dahai {
          actor = member "actor" j |> to_int |> validate_actor;
          pai = member "pai" j |> tile_of_json;
          tsumogiri = member "tsumogiri" j |> to_bool;
        }
    | "chi" ->
        Chi {
          actor = member "actor" j |> to_int |> validate_actor;
          target = member "target" j |> to_int |> validate_actor;
          pai = member "pai" j |> tile_of_json;
          consumed = member "consumed" j |> to_list |> List.map tile_of_json |> Array.of_list;
        }
    | "pon" ->
        Pon {
          actor = member "actor" j |> to_int |> validate_actor;
          target = member "target" j |> to_int |> validate_actor;
          pai = member "pai" j |> tile_of_json;
          consumed = member "consumed" j |> to_list |> List.map tile_of_json |> Array.of_list;
        }
    | "daiminkan" ->
        Daiminkan {
          actor = member "actor" j |> to_int |> validate_actor;
          target = member "target" j |> to_int |> validate_actor;
          pai = member "pai" j |> tile_of_json;
          consumed = member "consumed" j |> to_list |> List.map tile_of_json |> Array.of_list;
        }
    | "kakan" ->
        Kakan {
          actor = member "actor" j |> to_int |> validate_actor;
          pai = member "pai" j |> tile_of_json;
          consumed = member "consumed" j |> to_list |> List.map tile_of_json |> Array.of_list;
        }
    | "ankan" ->
        Ankan {
          actor = member "actor" j |> to_int |> validate_actor;
          consumed = member "consumed" j |> to_list |> List.map tile_of_json |> Array.of_list;
        }
    | "dora" ->
        Dora {
          dora_marker = member "dora_marker" j |> tile_of_json;
        }
    | "reach" ->
        Reach {
          actor = member "actor" j |> to_int |> validate_actor;
        }
    | "reach_accepted" ->
        Reach_accepted {
          actor = member "actor" j |> to_int |> validate_actor;
        }
    | "hora" ->
        Hora {
          actor = member "actor" j |> to_int |> validate_actor;
          target = member "target" j |> to_int |> validate_actor;
          deltas = (try Some (member "deltas" j |> to_list |> List.map to_int |> Array.of_list) with _ -> Option.none);
          ura_markers = (try Some (member "ura_markers" j |> to_list |> List.map tile_of_json |> Array.of_list) with _ -> Option.none);
        }
    | "ryukyoku" ->
        Ryukyoku {
          deltas = (try Some (member "deltas" j |> to_list |> List.map to_int |> Array.of_list) with _ -> Option.none);
        }
    | "end_kyoku" -> End_kyoku
    | "end_game" -> End_game
    | _ -> failwith (Printf.sprintf "Unknown event type: %s" typ)

  (** Convert event to JSON *)
  let event_to_json (ev : event) : Yojson.Safe.t =
    match ev with
    | None -> `Assoc [("type", `String "none")]
    | Start_game { names; seed } ->
        let fields = [("type", `String "start_game");
                      ("names", `List (Array.to_list (Array.map (fun n -> `String n) names)))] in
        let fields = match seed with
          | Some (n, k) -> fields @ [("seed", `List [`Int n; `Int k])]
          | Option.None -> fields
        in
        `Assoc fields
    | Start_kyoku { bakaze; dora_marker; kyoku; honba; kyotaku; oya; scores; tehais } ->
        `Assoc [
          ("type", `String "start_kyoku");
          ("bakaze", tile_to_json bakaze);
          ("dora_marker", tile_to_json dora_marker);
          ("kyoku", `Int kyoku);
          ("honba", `Int honba);
          ("kyotaku", `Int kyotaku);
          ("oya", `Int oya);
          ("scores", `List (Array.to_list (Array.map (fun s -> `Int s) scores)));
          ("tehais", `List (Array.to_list (Array.map (fun tehai ->
            `List (Array.to_list (Array.map tile_to_json tehai))
          ) tehais)));
        ]
    | Tsumo { actor; pai } ->
        `Assoc [("type", `String "tsumo"); ("actor", `Int actor); ("pai", tile_to_json pai)]
    | Dahai { actor; pai; tsumogiri } ->
        `Assoc [("type", `String "dahai"); ("actor", `Int actor); ("pai", tile_to_json pai); ("tsumogiri", `Bool tsumogiri)]
    | Chi { actor; target; pai; consumed } ->
        `Assoc [("type", `String "chi"); ("actor", `Int actor); ("target", `Int target); ("pai", tile_to_json pai); ("consumed", tiles_to_json consumed)]
    | Pon { actor; target; pai; consumed } ->
        `Assoc [("type", `String "pon"); ("actor", `Int actor); ("target", `Int target); ("pai", tile_to_json pai); ("consumed", tiles_to_json consumed)]
    | Daiminkan { actor; target; pai; consumed } ->
        `Assoc [("type", `String "daiminkan"); ("actor", `Int actor); ("target", `Int target); ("pai", tile_to_json pai); ("consumed", tiles_to_json consumed)]
    | Kakan { actor; pai; consumed } ->
        `Assoc [("type", `String "kakan"); ("actor", `Int actor); ("pai", tile_to_json pai); ("consumed", tiles_to_json consumed)]
    | Ankan { actor; consumed } ->
        `Assoc [("type", `String "ankan"); ("actor", `Int actor); ("consumed", tiles_to_json consumed)]
    | Dora { dora_marker } ->
        `Assoc [("type", `String "dora"); ("dora_marker", tile_to_json dora_marker)]
    | Reach { actor } ->
        `Assoc [("type", `String "reach"); ("actor", `Int actor)]
    | Reach_accepted { actor } ->
        `Assoc [("type", `String "reach_accepted"); ("actor", `Int actor)]
    | Hora { actor; target; deltas; ura_markers } ->
        let fields = [("type", `String "hora"); ("actor", `Int actor); ("target", `Int target)] in
        let fields = match deltas with
          | Some d -> fields @ [("deltas", `List (Array.to_list (Array.map (fun v -> `Int v) d)))]
          | Option.None -> fields
        in
        let fields = match ura_markers with
          | Some u -> fields @ [("ura_markers", tiles_to_json u)]
          | Option.None -> fields
        in
        `Assoc fields
    | Ryukyoku { deltas } ->
        let fields = [("type", `String "ryukyoku")] in
        let fields = match deltas with
          | Some d -> fields @ [("deltas", `List (Array.to_list (Array.map (fun v -> `Int v) d)))]
          | Option.None -> fields
        in
        `Assoc fields
    | End_kyoku -> `Assoc [("type", `String "end_kyoku")]
    | End_game -> `Assoc [("type", `String "end_game")]

  (** Parse event from JSON string *)
  let event_from_string (s : string) : event =
    from_string s |> event_of_json

  (** Convert event to JSON string *)
  let event_to_string (ev : event) : string =
    event_to_json ev |> Yojson.Safe.to_string
end
