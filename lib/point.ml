(** Point calculation for Riichi Mahjong scoring.

    Based on fu (basic points) and han (doubling) values,
    calculates ron and tsumo payments. *)

type point =
  { ron : int (** Points paid on ron (win by discard) *)
  ; tsumo_ko : int (** Points each non-dealer pays on tsumo *)
  ; tsumo_oya : int (** Points dealer pays on tsumo (0 if winner is dealer) *)
  }

(** Calculate points based on fu, han, and whether the winner is dealer.
    @param is_dealer true if the winner is the dealer (oya)
    @param fu basic points (20, 25, 30, 40, 50, 60, 70, 80, 90, 100, 110)
    @param han doubling units (1-13+)
    @return Point structure with ron, tsumo_ko, and tsumo_oya values
    @raise Invalid_argument if the fu/han combination is impossible *)
let calc (is_dealer : bool) (fu : int) (han : int) : point =
  let ron, tsumo_ko, tsumo_oya =
    if is_dealer
    then (
      (* Dealer (oya) winning *)
      match fu, han with
      (* 20/40/80 base *)
      | 20, 2 | 40, 1 -> 2000, 700, 0
      | 20, 3 | 40, 2 | 80, 1 -> 3900, 1300, 0
      | 20, 4 | 40, 3 | 80, 2 -> 7700, 2600, 0
      (* 25/50/100 base *)
      | 25, 2 | 50, 1 -> 2400, 800, 0
      | 25, 3 | 50, 2 | 100, 1 -> 4800, 1600, 0
      | 25, 4 | 50, 3 | 100, 2 -> 9600, 3200, 0
      (* 30/60 base *)
      | 30, 1 -> 1500, 500, 0
      | 30, 2 | 60, 1 -> 2900, 1000, 0
      | 30, 3 | 60, 2 -> 5800, 2000, 0
      | 30, 4 | 60, 3 -> 11600, 3900, 0
      (* 70 base *)
      | 70, 1 -> 3400, 1200, 0
      | 70, 2 -> 6800, 2300, 0
      (* 90 base *)
      | 90, 1 -> 4400, 1500, 0
      | 90, 2 -> 8700, 2900, 0
      (* 110 base (theoretical, not possible in real play) *)
      | 110, 1 -> 5300, 1800, 0
      | 110, 2 -> 10600, 3600, 0
      (* Mangan and above *)
      | _, 5 -> 12000, 4000, 0
      | fu, 4 when fu >= 40 -> 12000, 4000, 0
      | fu, 3 when fu >= 70 -> 12000, 4000, 0
      | _, 6 | _, 7 -> 18000, 6000, 0
      | _, 8 | _, 9 | _, 10 -> 24000, 8000, 0
      | _, 11 | _, 12 -> 36000, 12000, 0
      | _ when han >= 13 -> 48000, 16000, 0
      | _ ->
        invalid_arg (Printf.sprintf "impossible combination of %d fu and %d han" fu han))
    else (
      (* Non-dealer (ko) winning *)
      match fu, han with
      (* 20/40/80 base *)
      | 20, 2 | 40, 1 -> 1300, 400, 700
      | 20, 3 | 40, 2 | 80, 1 -> 2600, 700, 1300
      | 20, 4 | 40, 3 | 80, 2 -> 5200, 1300, 2600
      (* 25/50/100 base *)
      | 25, 2 | 50, 1 -> 1600, 400, 800
      | 25, 3 | 50, 2 | 100, 1 -> 3200, 800, 1600
      | 25, 4 | 50, 3 | 100, 2 -> 6400, 1600, 3200
      (* 30/60 base *)
      | 30, 1 -> 1000, 300, 500
      | 30, 2 | 60, 1 -> 2000, 500, 1000
      | 30, 3 | 60, 2 -> 3900, 1000, 2000
      | 30, 4 | 60, 3 -> 7700, 2000, 3900
      (* 70 base *)
      | 70, 1 -> 2300, 600, 1200
      | 70, 2 -> 4500, 1200, 2300
      (* 90 base *)
      | 90, 1 -> 2900, 800, 1500
      | 90, 2 -> 5800, 1500, 2900
      (* 110 base (theoretical, not possible in real play) *)
      | 110, 1 -> 3600, 900, 1800
      | 110, 2 -> 7100, 1800, 3600
      (* Mangan and above *)
      | _, 5 -> 8000, 2000, 4000
      | fu, 4 when fu >= 40 -> 8000, 2000, 4000
      | fu, 3 when fu >= 70 -> 8000, 2000, 4000
      | _, 6 | _, 7 -> 12000, 3000, 6000
      | _, 8 | _, 9 | _, 10 -> 16000, 4000, 8000
      | _, 11 | _, 12 -> 24000, 6000, 12000
      | _ when han >= 13 -> 32000, 8000, 16000
      | _ ->
        invalid_arg (Printf.sprintf "impossible combination of %d fu and %d han" fu han))
  in
  { ron; tsumo_ko; tsumo_oya }
;;

(** Calculate yakuman (limit hand) points.
    @param is_dealer true if the winner is the dealer (oya)
    @param count number of yakuman (1-13+)
    @return Point structure *)
let yakuman (is_dealer : bool) (count : int) : point =
  if is_dealer
  then { ron = 48000 * count; tsumo_ko = 16000 * count; tsumo_oya = 0 }
  else { ron = 32000 * count; tsumo_ko = 8000 * count; tsumo_oya = 16000 * count }
;;

(** Calculate total points paid on a tsumo win.
    @param p Point structure
    @param is_dealer true if the winner is the dealer (oya)
    @return Total points all players pay *)
let tsumo_total (p : point) (is_dealer : bool) : int =
  if is_dealer then p.tsumo_ko * 3 else (p.tsumo_ko * 2) + p.tsumo_oya
;;
