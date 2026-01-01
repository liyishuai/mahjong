(** Neural network interface for Mac-based AI training *)

(** Feature vector for neural network input *)
type nn_input =
  { (* Tile encoding: 34 tile types x 4 instances = 136 features per category *)
    hand_encoding : float array  (** One-hot encoding of hand tiles *)
  ; discard_encoding : float array array  (** Discards for each player *)
  ; dora_encoding : float array  (** Dora indicators *)
  
  (* Game state features *)
  ; round_wind : float array  (** One-hot [E,S,W,N] *)
  ; seat_wind : float array  (** One-hot [E,S,W,N] *)
  ; round_progress : float  (** 0.0 to 1.0 *)
  ; tiles_remaining : float  (** Normalized count *)
  
  (* Player state features *)  
  ; own_points : float  (** Normalized *)
  ; point_diffs : float array  (** Diff to each other player *)
  ; riichi_status : float array  (** 1.0 if player in riichi *)
  ; riichi_turns : float array  (** Turns since riichi for each player *)
  
  (* Call features *)
  ; melds : float array array  (** Encoding of open melds per player *)
  
  (* Strategic features *)
  ; shanten : float  (** Estimated shanten *)
  ; danger_tiles : float array  (** Danger level for each tile type *)
  ; safe_tiles : float array  (** Safety level for each tile type *)
  }

(** Neural network output - action probabilities *)
type nn_output =
  { discard_probs : float array  (** Probability for discarding each tile in hand *)
  ; riichi_prob : float  (** Probability to declare riichi *)
  ; chi_prob : float  (** Probability to chi *)
  ; pon_prob : float  (** Probability to pon *)
  ; kan_prob : float  (** Probability to kan *)
  ; tsumo_prob : float  (** Probability to tsumo *)
  ; ron_prob : float  (** Probability to ron *)
  ; pass_prob : float  (** Probability to pass *)
  }

(** Encode hand to one-hot representation *)
let encode_hand (tiles : int array) : float array =
  let encoding = Array.make 136 0.0 in
  Array.iter (fun tile ->
    if tile >= 0 && tile < 136 then
      encoding.(tile) <- 1.0
  ) tiles;
  encoding

(** Encode discards *)
let encode_discards (discards : int array) : float array =
  let encoding = Array.make 136 0.0 in
  Array.iter (fun tile ->
    if tile >= 0 && tile < 136 then
      encoding.(tile) <- encoding.(tile) +. 0.25  (* Stack up to 4 *)
  ) discards;
  encoding

(** Encode game state for neural network *)
let encode_state (state : Tenhou_bot.export_state) : nn_input =
  let encode_wind w = 
    let arr = Array.make 4 0.0 in
    if w >= 0 && w < 4 then arr.(w) <- 1.0;
    arr
  in
  
  { hand_encoding = encode_hand state.hand_tiles
  ; discard_encoding = Array.map encode_discards state.discards
  ; dora_encoding = Array.make 136 0.0  (* Would need dora info *)
  ; round_wind = encode_wind state.round_wind
  ; seat_wind = encode_wind state.seat
  ; round_progress = float_of_int state.round_num /. 8.0
  ; tiles_remaining = 1.0  (* Would need wall count *)
  ; own_points = float_of_int state.points.(state.seat) /. 100000.0
  ; point_diffs = Array.init 3 (fun i ->
      let other = (state.seat + 1 + i) mod 4 in
      float_of_int (state.points.(state.seat) - state.points.(other)) /. 50000.0
    )
  ; riichi_status = Array.map (fun r -> if r then 1.0 else 0.0) state.riichi_status
  ; riichi_turns = Array.make 4 0.0  (* Would need turn tracking *)
  ; melds = Array.make 4 [||]  (* Would need meld tracking *)
  ; shanten = 8.0  (* Would need shanten calculation *)
  ; danger_tiles = Array.make 34 0.0
  ; safe_tiles = Array.make 34 0.0
  }

(** Convert float array to JSON array string *)
let json_of_float_array (arr : float array) : string =
  "[" ^ String.concat "," (Array.to_list (Array.map string_of_float arr)) ^ "]"

(** Convert nn_input to JSON string for external consumption *)
let json_of_nn_input (input : nn_input) : string =
  Printf.sprintf {|{
  "hand_encoding": %s,
  "discard_encoding": [%s],
  "dora_encoding": %s,
  "round_wind": %s,
  "seat_wind": %s,
  "round_progress": %f,
  "tiles_remaining": %f,
  "own_points": %f,
  "point_diffs": %s,
  "riichi_status": %s,
  "riichi_turns": %s,
  "shanten": %f,
  "danger_tiles": %s,
  "safe_tiles": %s
}|}
    (json_of_float_array input.hand_encoding)
    (String.concat "," (Array.to_list (Array.map json_of_float_array input.discard_encoding)))
    (json_of_float_array input.dora_encoding)
    (json_of_float_array input.round_wind)
    (json_of_float_array input.seat_wind)
    input.round_progress
    input.tiles_remaining
    input.own_points
    (json_of_float_array input.point_diffs)
    (json_of_float_array input.riichi_status)
    (json_of_float_array input.riichi_turns)
    input.shanten
    (json_of_float_array input.danger_tiles)
    (json_of_float_array input.safe_tiles)

(** Parse nn_output from JSON string *)
let nn_output_of_json (json : string) : nn_output option =
  (* Simple JSON parsing for expected format *)
  let get_float key =
    let pattern = Printf.sprintf "\"%s\":" key in
    try
      let start = Str.search_forward (Str.regexp pattern) json 0 in
      let value_start = start + String.length pattern in
      let value_end = 
        try min (String.index_from json value_start ',') (String.index_from json value_start '}')
        with Not_found -> String.length json - 1
      in
      let value_str = String.trim (String.sub json value_start (value_end - value_start)) in
      Some (float_of_string value_str)
    with Not_found | Failure _ -> None
  in
  
  let get_float_array key =
    let pattern = Printf.sprintf "\"%s\":\\s*\\[" key in
    try
      let start = Str.search_forward (Str.regexp pattern) json 0 in
      let array_start = String.index_from json start '[' + 1 in
      let array_end = String.index_from json array_start ']' in
      let array_str = String.sub json array_start (array_end - array_start) in
      let values = String.split_on_char ',' array_str in
      Some (Array.of_list (List.map (fun s -> float_of_string (String.trim s)) values))
    with Not_found | Failure _ -> None
  in
  
  match (get_float_array "discard_probs", get_float "riichi_prob",
         get_float "chi_prob", get_float "pon_prob", get_float "kan_prob",
         get_float "tsumo_prob", get_float "ron_prob", get_float "pass_prob") with
  | (Some dp, Some rp, Some cp, Some pp, Some kp, Some tp, Some rop, Some pap) ->
      Some { discard_probs = dp
           ; riichi_prob = rp
           ; chi_prob = cp
           ; pon_prob = pp
           ; kan_prob = kp
           ; tsumo_prob = tp
           ; ron_prob = rop
           ; pass_prob = pap
           }
  | _ -> None

(** Convert nn_output to decision *)
let decision_of_nn_output (output : nn_output) (state : Tenhou_bot.export_state) : Tenhou_bot.ai_decision =
  (* Find best discard *)
  let best_discard_idx = ref 0 in
  let best_discard_prob = ref (-1.0) in
  Array.iteri (fun i p ->
    if p > !best_discard_prob && i < Array.length state.hand_tiles then begin
      best_discard_prob := p;
      best_discard_idx := i
    end
  ) output.discard_probs;
  
  (* Check for special actions *)
  if output.tsumo_prob > 0.9 then
    Tenhou_bot.AITsumo
  else if output.ron_prob > 0.9 then
    Tenhou_bot.AIRon
  else if output.riichi_prob > 0.7 && state.last_draw <> None then
    Tenhou_bot.AIRiichi state.hand_tiles.(!best_discard_idx)
  else if output.pon_prob > 0.6 then
    Tenhou_bot.AIPon [||]  (* Would need to select tiles *)
  else if output.chi_prob > 0.6 then
    Tenhou_bot.AIChi [||]  (* Would need to select tiles *)
  else if !best_discard_idx < Array.length state.hand_tiles then
    Tenhou_bot.AIDiscard state.hand_tiles.(!best_discard_idx)
  else
    Tenhou_bot.AIPass

(** Interface for external neural network process *)
type nn_interface =
  { send_state : nn_input -> unit
  ; recv_output : unit -> nn_output option
  ; close : unit -> unit
  }

(** Create file-based interface for communication with Python/Swift neural network *)
let create_file_interface (input_file : string) (output_file : string) : nn_interface =
  { send_state = (fun input ->
      let json = json_of_nn_input input in
      let oc = open_out input_file in
      output_string oc json;
      close_out oc)
  ; recv_output = (fun () ->
      if Sys.file_exists output_file then begin
        let ic = open_in output_file in
        let len = in_channel_length ic in
        let json = really_input_string ic len in
        close_in ic;
        Sys.remove output_file;  (* Clean up *)
        nn_output_of_json json
      end else
        None)
  ; close = (fun () ->
      if Sys.file_exists input_file then Sys.remove input_file;
      if Sys.file_exists output_file then Sys.remove output_file)
  }

(** Create strategy using external neural network *)
let strategy_from_nn (interface : nn_interface) : Tenhou_bot.strategy =
  let get_decision state =
    let input = encode_state state in
    interface.send_state input;
    (* Wait for response with timeout *)
    let rec wait_for_response attempts =
      if attempts <= 0 then
        (* Timeout - use random fallback *)
        Tenhou_bot.AIDiscard state.hand_tiles.(0)
      else
        match interface.recv_output () with
        | Some output -> decision_of_nn_output output state
        | None ->
            Unix.sleepf 0.01;  (* 10ms *)
            wait_for_response (attempts - 1)
    in
    wait_for_response 100  (* 1 second timeout *)
  in
  Tenhou_bot.strategy_from_ai get_decision

(** Training data collection *)
type training_sample =
  { input : nn_input
  ; action : Tenhou_bot.ai_decision
  ; reward : float  (** Based on game outcome *)
  }

(** Collect training samples from a game *)
let collect_training_data (samples : training_sample list ref) 
    (state : Tenhou_bot.export_state) (action : Tenhou_bot.ai_decision) : unit =
  let input = encode_state state in
  samples := { input; action; reward = 0.0 } :: !samples

(** Update rewards based on game outcome *)
let update_rewards (samples : training_sample list) (final_score : int) : training_sample list =
  let normalized_reward = float_of_int final_score /. 50000.0 in
  let discount = 0.99 in
  let rec update lst factor =
    match lst with
    | [] -> []
    | sample :: rest ->
        { sample with reward = normalized_reward *. factor } :: update rest (factor *. discount)
  in
  update (List.rev samples) 1.0

(** Save training data to JSON file *)
let save_training_data (samples : training_sample list) (filename : string) : unit =
  let oc = open_out filename in
  output_string oc "[\n";
  List.iteri (fun i sample ->
    if i > 0 then output_string oc ",\n";
    Printf.fprintf oc "  {\"input\": %s, \"reward\": %f}"
      (json_of_nn_input sample.input) sample.reward
  ) samples;
  output_string oc "\n]\n";
  close_out oc
