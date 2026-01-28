open Base

(** Win cache for checking winning hands and decompositions

    This module provides functions to:
    - Check if a hand is a winning hand (has)
    - Check if a hand is ready/tenpai (tenpai)
    - Get waiting tiles (machi)
    - Decompose a hand into sets and heads (sets_and_heads)
*)

(** Type for tile counts in a decomposed hand *)
type tile_count = (Tile.TileType.t, int) Hashtbl.t

(** Load the win cache (deprecated - cache is now embedded)
    @param path Path to the cache file (ignored)
    @return always returns true
*)
val load_cache : string -> bool

(** Load the win cache (deprecated - cache is now embedded)
    @param path Path to the cache file (ignored)
*)
val load_cache_from : string -> unit

(** Check if a hand is a winning hand
    @param counts Array of tile counts (length 34)
    @return true if the hand is a winning configuration
*)
val has : int array -> bool

(** Check if a hand is ready (tenpai)
    @param counts Array of tile counts (length 34)
    @return true if the hand is one tile away from winning
*)
val tenpai : int array -> bool

(** Get the list of waiting tiles for a ready hand
    @param counts Array of tile counts (length 34)
    @return List of tiles that complete the hand
*)
val machi : int array -> Tile.TileType.t list

(** Decompose a winning hand into sets and heads

    Returns all possible decompositions where:
    - Each set (mentsu) is a triplet (koutsu) or sequence (shuntsu)
    - Each head (jantou) is a pair

    @param counts Array of tile counts (length 34)
    @return List of (sets, heads) pairs for each valid decomposition
*)
val sets_and_heads : int array -> (tile_count list * tile_count list) list

(** Ensure the cache is loaded (call before using other functions) *)
val ensure_loaded : unit -> unit
