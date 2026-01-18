(** Hand format conversions.

    Note that all functions in this module that take or produce strings are dealing
    with tenhou.net/2 format tile description (like "0m 123z") instead of mjai (like
    "5mr ESW").

    In tenhou.net format:
    - "0" represents the red five (aka dora)
    - Numbers 1-9 followed by m/p/s/z for suit
    - Spaces are allowed *)

(** {1 Parsing functions} *)

(** Parse a tenhou.net format string into a 37-count array (including red fives).
    @param s Tenhou.net format string (e.g., "22334450m234p2s3s4s")
    @return Ok array of 37 counts (index 34-36 are red fives), or Error with message *)
val hand_with_aka : string -> (int array, string) result

(** Parse a tenhou.net format string into a 34-count array (without red fives).
    Red fives are merged with normal fives.
    @param s Tenhou.net format string
    @return Ok array of 34 counts, or Error with message *)
val hand : string -> (int array, string) result

(** {1 Conversion functions} *)

(** Convert a 37-count array (with red fives) to an array of tile IDs.
    Each count expands to that many tile values. *)
val tile37_to_array : int array -> int array

(** Convert a 34-count array (without red fives) to an array of tile IDs.
    Each count expands to that many tile values. *)
val tile34_to_array : int array -> int array

(** Convert a 34-count array to tenhou.net format string.
    @param tiles34 Array of 34 tile counts
    @param aka Triple of booleans indicating presence of red fives (man, pin, sou)
    @return Tenhou.net format string *)
val tiles_to_string : int array -> bool array -> string
