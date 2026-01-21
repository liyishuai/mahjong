(** Observation representation for RL agent.
    
    This module converts the game state into a feature matrix (observation)
    suitable for a neural network input.
*)

(** Action space size (46 actions) *)
val action_space : int

(** Get observation shape (rows, cols) for a given version *)
val obs_shape : int -> int * int

(** Observation data structure *)
type obs = {
  features : float array;  (* Flattened matrix of shape (rows, 34) *)
  mask : bool array;       (* Action mask of shape (action_space) *)
}

(** Encode player state into observation features and action mask.
    @param state The player state to encode
    @param version The encoding version (1-4)
    @param at_kan_select True if currently in kan selection sub-state
    @return The encoded observation
*)
val encode_obs : State.player_state -> int -> bool -> obs
