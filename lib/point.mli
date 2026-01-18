(** Point calculation for Riichi Mahjong scoring.

    Based on fu (basic points) and han (doubling) values,
    calculates ron and tsumo payments. *)

(** Point structure containing payment amounts *)
type point = {
  ron : int;       (** Points paid on ron (win by discard) *)
  tsumo_ko : int;  (** Points each non-dealer pays on tsumo *)
  tsumo_oya : int; (** Points dealer pays on tsumo (0 if winner is dealer) *)
}

(** Calculate points based on fu, han, and whether the winner is dealer.
    @param is_dealer true if the winner is the dealer (oya)
    @param fu basic points (20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 110)
    @param han doubling units (1-13+)
    @return Point structure with ron, tsumo_ko, and tsumo_oya values
    @raise Invalid_argument if the fu/han combination is impossible *)
val calc : bool -> int -> int -> point

(** Calculate yakuman (limit hand) points.
    @param is_dealer true if the winner is the dealer (oya)
    @param count number of yakuman (1-13+)
    @return Point structure *)
val yakuman : bool -> int -> point

(** Calculate total points paid on a tsumo win.
    @param p Point structure
    @param is_dealer true if the winner is the dealer (oya)
    @return Total points all players pay *)
val tsumo_total : point -> bool -> int
