open Base

module Yaku = struct
  type t =
    | FullyConcealedHand
    | Riichi
    | Ippatsu
    | RobbingKan
    | AfterKan
    | BottomOfTheSea
    | BottomOfTheRiver
    | Pinfu
    | AllSimples
    | PureDoubleChis
    | SeatWindEast
    | SeatWindSouth
    | SeatWindWest
    | SeatWindNorth
    | PrevalentWindEast
    | PrevalentWindSouth
    | PrevalentWindWest
    | PrevalentWindNorth
    | WhiteDragon
    | GreenDragon
    | RedDragon
    | DoubleRiichi
    | SevenPairs
    | OutsideHand
    | PureStraight
    | MixedTripleChis
    | TriplePons
    | ThreeKans
    | AllPons
    | ThreeConcealedPons
    | LittleThreeDragons
    | AllTermsAndHonours
    | TwicePureDoubleChis
    | TerminalsInAllSets
    | HalfFlush
    | FullFlush
    | BlessingOfMan
    | BlessingOfHeaven
    | BlessingOfEarth
    | BigThreeDragons
    | FourConcealedPons
    | CompletedFourConcealedPons
    | AllHonours
    | AllGreen
    | AllTerminals
    | NineGates
    | PureNineGates
    | ThirteenOrphans
    | CompletedThirteenOrphans
    | BigFourWinds
    | LittleFourWinds
    | FourKans
    | Dora
    | ReversedDora
    | RedDora
  [@@deriving sexp, compare, equal, hash]
end

module Wind = struct
  type t =
    | East
    | South
    | West
    | North
  [@@deriving sexp, compare, equal]
end

module WinStateInfo = struct
  type t = {
    seat_wind : Wind.t;
    prevalent_wind : Wind.t;
    is_bottom : bool;
    is_ippatsu : bool;
    is_first_tsumo : bool;
    is_dealer : bool;
    is_robbing_kan : bool;
    dora : (Tile.TileType.t, int) Hashtbl.t; 
    reversed_dora : (Tile.TileType.t, int) Hashtbl.t;
  }
end

module WinHandInfo = struct
  type t = {
    closed_tiles : Tile.t list;
    opens : Open.t list;
    closed_tile_types : (Tile.TileType.t, int) Hashtbl.t;
    all_tile_types : (Tile.TileType.t, int) Hashtbl.t;
    win_tile : Tile.t option;
    stage : Hand.HandStage.t;
    under_riichi : bool;
    double_riichi : bool;
    is_menzen : bool;
  }
end

module WinInfo = struct
  type t = {
    state : WinStateInfo.t;
    hand : WinHandInfo.t;
  }
end
