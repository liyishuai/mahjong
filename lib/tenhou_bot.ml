(** Tenhou bot client *)

open Tiles
open State
open Tenhou_protocol

(** Bot configuration *)
type bot_config =
  { username : string
  ; auth_token : string
  ; server : string
  ; port : int
  ; lobby : int  (** 0 = general, 1 = upper dan, etc. *)
  ; game_type : int  (** Bit flags for game settings *)
  }

(** Default Tenhou server settings *)
let default_config : bot_config =
  { username = "NoName"
  ; auth_token = ""
  ; server = "133.242.10.78"
  ; port = 10080
  ; lobby = 0
  ; game_type = 9  (* 4-player, no red, no kuitan *)
  }

(** Bot state *)
type bot_state =
  { config : bot_config
  ; game_state : game_state option
  ; seat : int  (** Our seat (0-3) *)
  ; hand : int array  (** Current hand as Tenhou tile codes *)
  ; discards : int array array  (** Each player's discards *)
  ; is_riichi : bool array  (** Which players are in riichi *)
  ; points : int array  (** Current points *)
  ; round_wind : int  (** 0=East, 1=South, 2=West, 3=North *)
  ; round_num : int  (** Round number within wind *)
  ; honba : int
  ; last_draw : int option  (** Last drawn tile *)
  ; last_discard : (int * int) option  (** Last discard: player, tile *)
  ; can_call : bool  (** Whether we can make a call *)
  ; pending_action : pending_action option
  }

and pending_action =
  | PendingDiscard
  | PendingCall of call_options
  | PendingRiichi

and call_options =
  { can_chi : int array list  (** Possible chi combinations *)
  ; can_pon : int array option  (** Pon tiles if available *)
  ; can_kan : int array option  (** Kan tiles if available *)
  ; can_ron : bool
  }

(** Initialize bot state *)
let init_bot_state (config : bot_config) : bot_state =
  { config
  ; game_state = None
  ; seat = 0
  ; hand = [||]
  ; discards = Array.init 4 (fun _ -> [||])
  ; is_riichi = [|false; false; false; false|]
  ; points = [|25000; 25000; 25000; 25000|]
  ; round_wind = 0
  ; round_num = 0
  ; honba = 0
  ; last_draw = None
  ; last_discard = None
  ; can_call = false
  ; pending_action = None
  }

(** Convert Tenhou hand to internal representation *)
let hand_of_tenhou (codes : int array) : tile array =
  Array.map tile_of_tenhou_tile codes

(** Find tile index in hand *)
let find_tile_index (hand : int array) (tile : int) : int option =
  let rec find i =
    if i >= Array.length hand then None
    else if hand.(i) = tile then Some i
    else find (i + 1)
  in
  find 0

(** Remove tile from hand array *)
let remove_tile (hand : int array) (idx : int) : int array =
  Array.concat [
    Array.sub hand 0 idx;
    Array.sub hand (idx + 1) (Array.length hand - idx - 1)
  ]

(** Add tile to hand array *)
let add_tile (hand : int array) (tile : int) : int array =
  Array.append hand [|tile|]

(** Check if we can chi with the given tile *)
let check_chi_options (hand : int array) (tile : int) : int array list =
  let t = tile_of_tenhou_tile tile in
  match t with
  | Honor _ -> []  (* Cannot chi honors *)
  | Man n | Pin n | So n ->
      let num = int_of_number n in
      let same_suit = Array.to_list hand |> List.filter (fun code ->
        let t2 = tile_of_tenhou_tile code in
        match (t, t2) with
        | (Man _, Man _) | (Pin _, Pin _) | (So _, So _) -> true
        | _ -> false
      ) in
      let find_num target =
        List.find_opt (fun code ->
          let t2 = tile_of_tenhou_tile code in
          match t2 with
          | Man n2 | Pin n2 | So n2 -> int_of_number n2 = target
          | _ -> false
        ) same_suit
      in
      let options = ref [] in
      (* Check ABC pattern (tile is A) *)
      (match (find_num (num + 1), find_num (num + 2)) with
       | (Some t1, Some t2) -> options := [|t1; t2|] :: !options
       | _ -> ());
      (* Check BAC pattern (tile is middle) *)
      (match (find_num (num - 1), find_num (num + 1)) with
       | (Some t1, Some t2) -> options := [|t1; t2|] :: !options
       | _ -> ());
      (* Check CBA pattern (tile is C) *)
      (match (find_num (num - 2), find_num (num - 1)) with
       | (Some t1, Some t2) -> options := [|t1; t2|] :: !options
       | _ -> ());
      !options

(** Check if we can pon the given tile *)
let check_pon_option (hand : int array) (tile : int) : int array option =
  let same_tiles = Array.to_list hand |> List.filter (fun code ->
    let t1 = tile_of_tenhou_tile tile in
    let t2 = tile_of_tenhou_tile code in
    (* Same tile type, ignoring aka *)
    match (t1, t2) with
    | (Man n1, Man n2) | (Pin n1, Pin n2) | (So n1, So n2) ->
        int_of_number n1 = int_of_number n2
    | (Honor h1, Honor h2) -> h1 = h2
    | _ -> false
  ) in
  if List.length same_tiles >= 2 then
    (* Take first 2 elements *)
    let rec take n lst = match (n, lst) with
      | (0, _) | (_, []) -> []
      | (n, x :: xs) -> x :: take (n - 1) xs
    in
    Some (Array.of_list (take 2 same_tiles))
  else
    None

(** Check if we can kan the given tile *)
let check_kan_option (hand : int array) (tile : int) : int array option =
  let same_tiles = Array.to_list hand |> List.filter (fun code ->
    let t1 = tile_of_tenhou_tile tile in
    let t2 = tile_of_tenhou_tile code in
    match (t1, t2) with
    | (Man n1, Man n2) | (Pin n1, Pin n2) | (So n1, So n2) ->
        int_of_number n1 = int_of_number n2
    | (Honor h1, Honor h2) -> h1 = h2
    | _ -> false
  ) in
  if List.length same_tiles >= 3 then
    (* Take first 3 elements *)
    let rec take n lst = match (n, lst) with
      | (0, _) | (_, []) -> []
      | (n, x :: xs) -> x :: take (n - 1) xs
    in
    Some (Array.of_list (take 3 same_tiles))
  else
    None

(** Strategy interface - to be implemented by neural network *)
type strategy = {
  choose_discard : bot_state -> int;  (** Choose tile index to discard *)
  should_riichi : bot_state -> bool;  (** Should declare riichi? *)
  should_call : bot_state -> call_options -> [`Chi of int array | `Pon | `Kan | `Ron | `Pass];
  should_tsumo : bot_state -> bool;  (** Should declare tsumo? *)
}

(** Default random strategy *)
let random_strategy : strategy =
  { choose_discard = (fun state ->
      let len = Array.length state.hand in
      if len > 0 then Random.int len else 0)
  ; should_riichi = (fun _ -> Random.bool ())
  ; should_call = (fun _ _ -> `Pass)
  ; should_tsumo = (fun _ -> true)
  }

(** Process incoming message and return response *)
let process_message (state : bot_state) (strategy : strategy) (msg : tenhou_msg) 
    : bot_state * string option =
  match msg with
  | Helo _ ->
      (* Login successful, request to join game *)
      (state, Some (Printf.sprintf "<JOIN t=\"%d,%d\"/>" state.config.lobby state.config.game_type))
  
  | Rejoin (_t, _log, _step) ->
      (* Rejoin a game in progress *)
      (state, Some "<REJOIN />")
  
  | Go (_lobby, _game_type) ->
      (* Game found, wait for player info *)
      (state, Some "<GOK />")
  
  | Un names ->
      Printf.printf "Players: %s\n" (String.concat ", " (Array.to_list names));
      (state, None)
  
  | Taikyoku (_log_id, oya) ->
      Printf.printf "Game started, dealer: %d\n" oya;
      (state, None)
  
  | Init info ->
      Printf.printf "Round start: %d-%d, honba=%d\n" 
        (info.seed.(0) / 4) (info.seed.(0) mod 4 + 1) info.seed.(1);
      let new_state = { state with
        hand = info.hai;
        round_wind = info.seed.(0) / 4;
        round_num = info.seed.(0) mod 4;
        honba = info.seed.(1);
        points = Array.map (fun x -> x * 100) info.ten;
        is_riichi = [|false; false; false; false|];
        discards = Array.init 4 (fun _ -> [||]);
        last_draw = None;
        last_discard = None;
      } in
      (new_state, None)
  
  | Draw tile ->
      Printf.printf "Drew tile: %d (%s)\n" tile (string_of_tile (tile_of_tenhou_tile tile));
      let new_hand = add_tile state.hand tile in
      let new_state = { state with hand = new_hand; last_draw = Some tile } in
      (* Choose discard *)
      let discard_idx = strategy.choose_discard new_state in
      let discard_tile = new_state.hand.(discard_idx) in
      let final_hand = remove_tile new_state.hand discard_idx in
      let final_state = { new_state with hand = final_hand; last_draw = None } in
      (final_state, Some (encode_discard discard_tile))
  
  | Discard (player, tile) ->
      Printf.printf "Player %d discarded: %d (%s)\n" player tile (string_of_tile (tile_of_tenhou_tile tile));
      let new_discards = Array.copy state.discards in
      new_discards.(player) <- Array.append state.discards.(player) [|tile|];
      let new_state = { state with 
        discards = new_discards;
        last_discard = Some (player, tile);
      } in
      
      (* Check for call options if not our discard *)
      if player <> state.seat then begin
        let call_opts = {
          can_chi = if (player + 1) mod 4 = state.seat 
                    then check_chi_options state.hand tile 
                    else [];
          can_pon = check_pon_option state.hand tile;
          can_kan = check_kan_option state.hand tile;
          can_ron = false;  (* Would need tenpai check *)
        } in
        match strategy.should_call new_state call_opts with
        | `Pon ->
            (match call_opts.can_pon with
             | Some tiles -> (new_state, Some (encode_pon tiles player))
             | None -> (new_state, Some (encode_noop ())))
        | `Chi tiles ->
            (new_state, Some (encode_chi tiles))
        | `Kan ->
            (match call_opts.can_kan with
             | Some tiles -> (new_state, Some (encode_kan tiles 2))  (* Daiminkan *)
             | None -> (new_state, Some (encode_noop ())))
        | `Ron ->
            (new_state, Some (encode_ron ()))
        | `Pass ->
            (new_state, Some (encode_noop ()))
      end
      else
        (new_state, None)
  
  | Reach (player, step) ->
      Printf.printf "Player %d declared riichi (step %d)\n" player step;
      let new_riichi = Array.copy state.is_riichi in
      new_riichi.(player) <- true;
      ({ state with is_riichi = new_riichi }, None)
  
  | Agari info ->
      Printf.printf "Win! Winner: %d, from: %d\n" info.winner info.from_who;
      (state, None)
  
  | Ryuukyoku info ->
      Printf.printf "Draw game: %s\n" info.reason;
      (state, None)
  
  | Dora tile ->
      Printf.printf "New dora indicator: %d\n" tile;
      (state, None)
  
  | Call _info ->
      (* Handle call confirmation *)
      (state, None)
  
  | Bye ->
      Printf.printf "Disconnected\n";
      (state, None)
  
  | Prof ->
      (state, None)
  
  | Unknown xml ->
      Printf.printf "Unknown message: %s\n" xml;
      (state, None)

(** Main bot loop - to be called with actual network connection *)
let run_bot (config : bot_config) (strategy : strategy) 
    (recv : unit -> string) (send : string -> unit) : unit =
  (* Send login *)
  send (encode_helo config.username config.auth_token);
  
  let state = ref (init_bot_state config) in
  let running = ref true in
  
  while !running do
    let xml = recv () in
    if String.length xml = 0 then
      running := false
    else begin
      let msg = parse_message xml in
      let (new_state, response) = process_message !state strategy msg in
      state := new_state;
      match response with
      | Some resp -> send resp
      | None -> ()
    end
  done

(** Export game state for external AI *)
type export_state =
  { hand_tiles : int array
  ; discards : int array array
  ; riichi_status : bool array
  ; points : int array
  ; round_wind : int
  ; round_num : int
  ; honba : int
  ; seat : int
  ; last_draw : int option
  ; last_discard : (int * int) option
  }

let export_state (state : bot_state) : export_state =
  { hand_tiles = state.hand
  ; discards = state.discards
  ; riichi_status = state.is_riichi
  ; points = state.points
  ; round_wind = state.round_wind
  ; round_num = state.round_num
  ; honba = state.honba
  ; seat = state.seat
  ; last_draw = state.last_draw
  ; last_discard = state.last_discard
  }

(** Import decision from external AI *)
type ai_decision =
  | AIDiscard of int  (** Tile code to discard *)
  | AIRiichi of int  (** Tile code to discard for riichi *)
  | AIChi of int array  (** Two tile codes from hand *)
  | AIPon of int array  (** Two tile codes from hand *)
  | AIKan of int array  (** Three or four tile codes *)
  | AIRon
  | AITsumo
  | AIPass

(** Create strategy from external AI decision function *)
let strategy_from_ai (get_decision : export_state -> ai_decision) : strategy =
  { choose_discard = (fun state ->
      let exported = export_state state in
      match get_decision exported with
      | AIDiscard tile | AIRiichi tile ->
          (match find_tile_index state.hand tile with
           | Some idx -> idx
           | None -> 0)
      | _ -> Array.length state.hand - 1)
  ; should_riichi = (fun state ->
      let exported = export_state state in
      match get_decision exported with
      | AIRiichi _ -> true
      | _ -> false)
  ; should_call = (fun state _opts ->
      let exported = export_state state in
      match get_decision exported with
      | AIChi tiles -> `Chi tiles
      | AIPon _ -> `Pon
      | AIKan _ -> `Kan
      | AIRon -> `Ron
      | _ -> `Pass)
  ; should_tsumo = (fun state ->
      let exported = export_state state in
      match get_decision exported with
      | AITsumo -> true
      | _ -> false)
  }
