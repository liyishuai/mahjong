(** Policy module for action selection using neural networks *)

open State
open Simulation
open Training

(** Simple neural network with random weights *)
type simple_nn =
  { input_size : int
  ; hidden_size : int
  ; output_size : int
  ; w1 : float array array  (** Input -> Hidden weights *)
  ; b1 : float array  (** Hidden biases *)
  ; w2 : float array array  (** Hidden -> Output weights *)
  ; b2 : float array  (** Output biases *)
  }

(** Initialize network with random weights *)
let init_nn ~input_size ~hidden_size ~output_size : simple_nn =
  let random_weight () = (Random.float 2.0) -. 1.0 in
  let w1 = Array.init hidden_size (fun _ ->
    Array.init input_size (fun _ -> random_weight () *. 0.1)
  ) in
  let b1 = Array.init hidden_size (fun _ -> 0.0) in
  let w2 = Array.init output_size (fun _ ->
    Array.init hidden_size (fun _ -> random_weight () *. 0.1)
  ) in
  let b2 = Array.init output_size (fun _ -> 0.0) in
  { input_size; hidden_size; output_size; w1; b1; w2; b2 }

(** ReLU activation *)
let relu x = max 0.0 x

(** Softmax activation *)
let softmax (arr : float array) : float array =
  let max_val = Array.fold_left max neg_infinity arr in
  let exp_arr = Array.map (fun x -> exp (x -. max_val)) arr in
  let sum = Array.fold_left (+.) 0.0 exp_arr in
  if sum > 0.0 then
    Array.map (fun x -> x /. sum) exp_arr
  else
    Array.make (Array.length arr) (1.0 /. float_of_int (Array.length arr))

(** Forward pass through network *)
let forward (nn : simple_nn) (input : float array) : float array =
  (* Input -> Hidden *)
  let hidden = Array.mapi (fun i _ ->
    let sum = ref nn.b1.(i) in
    for j = 0 to nn.input_size - 1 do
      sum := !sum +. input.(j) *. nn.w1.(i).(j)
    done;
    relu !sum
  ) nn.b1 in

  (* Hidden -> Output *)
  let output = Array.mapi (fun i _ ->
    let sum = ref nn.b2.(i) in
    for j = 0 to nn.hidden_size - 1 do
      sum := !sum +. hidden.(j) *. nn.w2.(i).(j)
    done;
    !sum
  ) nn.b2 in

  softmax output

(** Convert features to flat input vector *)
let features_to_vector (features : features) : float array =
  Array.concat [
    features.hand_tiles;
    features.own_river;
    features.others_rivers;
    features.round_wind;
    features.seat_wind;
    [| features.round_num; features.honba; features.remaining_tiles |];
    [| features.num_furos; features.is_riichi; features.riichi_sticks |];
    [| features.own_points |];
    features.others_points;
    [| features.dora_in_hand |];
  ]

(** Neural network policy *)
type nn_policy =
  { mutable nn : simple_nn option
  ; hidden_size : int
  }

(** Create a new NN policy *)
let create_nn_policy ~hidden_size : nn_policy =
  { nn = None; hidden_size }

(** Lazy initialization of NN when first features are seen *)
let ensure_nn_initialized (policy : nn_policy) (input_size : int) (output_size : int) : unit =
  match policy.nn with
  | Some _ -> ()
  | None ->
      policy.nn <- Some (init_nn ~input_size ~hidden_size:policy.hidden_size ~output_size)

(** Select action using neural network *)
let nn_action (policy : nn_policy) (state : game_state) (actions : action list) : action =
  let num_actions = List.length actions in
  if num_actions = 0 then
    failwith "No valid actions"
  else if num_actions = 1 then
    List.hd actions
  else begin
    let player_idx = state.round.current_player in
    let features = extract_features state player_idx in
    let input = features_to_vector features in
    let input_size = Array.length input in

    (* Initialize NN if needed *)
    ensure_nn_initialized policy input_size num_actions;

    match policy.nn with
    | None -> List.hd actions  (* Fallback *)
    | Some nn ->
        (* Get action probabilities *)
        let probs = forward nn input in

        (* Sample action based on probabilities *)
        let r = Random.float 1.0 in
        let rec select_action actions probs acc idx =
          match (actions, probs) with
          | (a :: _, p :: _) when acc +. p >= r || idx = num_actions - 1 -> a
          | (_ :: as_, p :: ps) -> select_action as_ ps (acc +. p) (idx + 1)
          | _ -> List.hd actions  (* Fallback *)
        in
        select_action actions (Array.to_list probs) 0.0 0
  end

(** Create policy function from NN policy *)
let policy_function (policy : nn_policy) : game_state -> action list -> action =
  fun state actions -> nn_action policy state actions
