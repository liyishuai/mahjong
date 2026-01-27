(** Agari (winning hand) detection and yaku calculation.

    Implementation provides both backtracking and lookup table methods.
    Backtracking: Native algorithm from mjalgorism.html.
    Table: Precomputed 9,362-entry lookup table for ~50x faster detection.
    Based on the algorithm by 山岡忠夫 (Yamaoka Tadao).
    See: http://hp.vector.co.jp/authors/VA046927/mjscore/mjalgorism.html
 *)

(** Result of agari detection *)
type agari_result =
  | Normal of
      { fu : int
      ; han : int
      } (** Normal win with fu and han values *)
  | Yakuman of int (** Yakuman with count (1-13+) *)

(** Comparison for agari_result (for testing) *)
val compare_agari : agari_result -> agari_result -> int

(** Pretty printer for agari_result *)
val pp_agari_result : Format.formatter -> agari_result -> unit

(** String conversion for agari_result *)
val string_of_agari_result : agari_result -> string

(** Yaku (scoring elements) - not yet fully implemented *)
type yaku =
  | Riichi
  | Double_Riichi
  | Ippatsu
  | Tsumo
  | Tsumo_Ron
  | Iipeikou
  | Ryanpeikou
  | Sanankou
  | Sankantsu
  | Suukantsu
  | Toitoi
  | Sanshoku
  | Shousangen
  | Daisangen
  | Honroto
  | Chinroto
  | Honitsu
  | Chinitsu
  | Haku
  | Hatsu
  | Chun
  | Jikaze
  | Bakaze
  | Akahai
  | Dora
  | Akadora
  | Chuuren
  | Kokushi
  | Suuankou
  | Tsuuiisou
  | Ryuuiisou

(** AgariCalculator input for calculating yaku *)
type agari_calculator =
  { tehai : int array (** 34-tile array including the winning tile (3n+2 total) *)
  ; winning_tile : int (** The winning tile index (0-33) *)
  ; bakaze : int (** Prevalent wind: 0=E, 1=S, 2=W, 3=N *)
  ; jikaze : int (** Seat wind: 0=E, 1=S, 2=W, 3=N *)
  ; is_menzen : bool (** True if fully concealed (menzen) *)
  ; is_ron : bool (** True if won by discard (false for tsumo) *)
  ; chis : int list (** Chi melds (open sequences) *)
  ; pons : int list (** Pon melds (open triplets) *)
  ; minkans : int list (** Minkan melds (open quads) *)
  ; ankans : int list (** Ankan melds (closed quads) *)
  }

(** [is_agari tiles34] checks if a hand is a winning hand.

    @param tiles34 Array of 34 tile counts
    @return true if the hand is a winning configuration *)
val is_agari : int array -> bool

(** [is_agari_ref tiles34] checks if a hand is a winning hand using straightforward algorithm.
    This is a clear reference implementation used for verification of optimized versions.

    @param tiles34 Array of 34 tile counts
    @return true if the hand is a winning configuration *)
val is_agari_ref : int array -> bool

(** Division result for winning hand *)
type div =
  { pair_idx : int
  ; kotsu_idxs : int array
  ; shuntsu_idxs : int array
  ; has_chitoi : bool
  ; has_chuuren : bool
  ; has_ittsuu : bool
  ; has_ryanpeikou : bool
  ; has_ipeikou : bool
  }

(** [divide_tiles_table tiles34] returns all possible divisions of a winning hand.

    @param tiles34 Array of 34 tile counts
    @return Array of possible divisions, empty if not winning *)
val divide_tiles_table : int array -> div array

(** [check_agari tiles34] checks if the hand is a winning hand.
    @param tiles34 Array of 34 tile counts (including winning tile, 3n+2 tiles total)
    @return Some agari_result if the hand wins, None otherwise *)
val check_agari : int array -> agari_result option

(** Search for yaku (not yet implemented) *)
val search_yakus : agari_calculator -> agari_result option

(** [agari calc additional_hans doras] calculates final point result.

    @param calc AgariCalculator containing the hand and game context
    @param additional_hans Hans from riichi, ippatsu, tsumo, etc. (not in search_yakus)
    @param doras Number of dora tiles found
    @return Some agari_result if the hand wins, None otherwise *)
val agari : agari_calculator -> int -> int -> agari_result option

(** [point result is_oya] calculates points from an agari_result.

    @param result The agari result (Normal with fu/han or Yakuman with count)
    @param is_oya true if the winner is the dealer
    @return Point structure with ron and tsumo payments *)
val point : agari_result -> bool -> Point.point

(** Check if an ankan is valid after declaring riichi.
    An ankan is valid if it does not change the waits of the hand.
    @param tehai Hand tiles (34-element array)
    @param len_div3 Number of targeting melds
    @param tile Tile index to ankan
    @param strict If true, also check for same waits composition (not just count)
    @return True if valid, false otherwise *)
val check_ankan_after_riichi : int array -> int -> int -> bool -> bool
