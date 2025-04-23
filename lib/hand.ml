open Tiles

type source =
  | Kami
  | Shimo
  | Toimen

type chiNums =
  | AkaLeft (* 067 *)
  | AkaRight (* 034 *)
  | ChiLeft of number (* aBC *)
  | ChiMid of number (* bAC *)
  | ChiRight of number (* cAB *)

type chi =
  | ChiMan of chiNums
  | ChiSo of chiNums
  | ChiPin of chiNums

type furo =
  | Chi of chi
  | Pon of tile * source
  | Minkan of tile * source
  | Kakan of tile * source

type hand =
  { tiles : tile array
  ; furos : furo array
  ; ankan : tile array
  }
