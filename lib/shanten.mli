(** Shanten (progress to win) calculation.

    Port of the table-based algorithm from shanten-number by tomohxx.
    Original: https://github.com/tomohxx/shanten-number

    Uses precomputed lookup tables for fast shanten calculation.

    Tables are automatically loaded on first access using lazy evaluation. *)

(** {1 Public Types} *)

(** Shanten calculation mode *)
type shanten_mode =
  | Normal (** Standard hand form (4 melds + 1 pair) *)
  | Chitoi (** Seven pairs *)
  | Kokushi (** Thirteen orphans *)

(** Shanten result type *)
type t = int
(** Negative values: hand is already winning
       0: tenpai (one tile from winning)
       Positive: tiles away from tenpai *)

(** Initialization errors *)
type init_error =
  | TableFileNotFound of string (** Table file not found *)
  | InvalidTableData of string (** Table file is corrupted *)
  | IoError of string (** I/O error reading table *)

(** Exception raised on table loading errors *)
exception InitError of init_error

(** {1 Calculation Functions} *)

(** [calc_normal tiles34 len_div3] calculates shanten for normal hand form.

    @param tiles34 Array of 34 tile counts
    @param len_div3 Number of complete melds (tile_count / 3), must be in [0, 4]
    @return Shanten number where:
            - negative values mean the hand is already winning
            - 0 means tenpai (one tile away from winning)
            - positive values indicate how many tiles away from tenpai

    This uses precomputed lookup tables for O(1) performance.
    Tables are automatically loaded on first access.

    @raise InitError if table files cannot be loaded *)
val calc_normal : int array -> int -> t

(** [calc_chitoi tiles34] calculates shanten for seven pairs form.

    @param tiles34 Array of 34 tile counts
    @return Shanten number (0 = tenpai, -1 = winning with 7 pairs)

    This function calculates:
    - Number of unique tile types present
    - Number of pairs available
    - Shanten = 7 - pairs + max(0, 7 - kinds) - 1 *)
val calc_chitoi : int array -> t

(** [calc_kokushi tiles34] calculates shanten for thirteen orphans form.

    @param tiles34 Array of 34 tile counts
    @return Shanten number (0 = tenpai with pair, -1 = winning with 13 unique)

    This function:
    - Counts unique terminal/honor tiles present (1m,9m,1p,9p,1s,9s,E,S,W,N,P,F,C)
    - Checks if any terminal/honor has a pair
    - Calculates shanten = 14 - unique_kinds - has_pair *)
val calc_kokushi : int array -> t

(** [calc_all tiles34 len_div3] calculates minimum shanten across all forms.

    @param tiles34 Array of 34 tile counts
    @param len_div3 Number of complete melds (typically 4 for a 13-tile hand)
    @return Minimum shanten number across Normal, Chitoi, and Kokushi forms

    This function computes the shanten for all three hand forms and returns
    the minimum value, representing the closest path to winning. *)
val calc_all : int array -> int -> t

(** [calc tiles34 len_div3 mode] calculates shanten for a specific mode only.

    @param tiles34 Array of 34 tile counts
    @param len_div3 Number of complete melds (only used for Normal mode)
    @param mode Which hand form to calculate for
    @return Shanten number for the specified mode *)
val calc : int array -> int -> shanten_mode -> t

(** {1 Utility Functions} *)

(** [to_string shanten] converts shanten value to human-readable string.

    @param shanten Shanten value
    @return String description ("Winning", "Tenpai", "Shanten: 2", etc.) *)
val to_string : t -> string
