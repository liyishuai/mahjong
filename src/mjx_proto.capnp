@0xd84d720491040000;

interface Agent {
  takeAction @0 (observation :Observation) -> (action :Action);
}

enum ActionType {
  # After draw
  actionTypeDiscard @0;
  actionTypeTsumogiri @1;
  actionTypeRiichi @2;
  actionTypeClosedKan @3;
  actionTypeAddedKan @4;
  actionTypeTsumo @5;
  actionTypeAbortiveDrawNineTerminals @6;
  
  # After other's discard
  actionTypeChi @7;
  actionTypePon @8;
  actionTypeOpenKan @9;
  actionTypeRon @10;
  actionTypeNo @11;
  
  # Dummy
  actionTypeDummy @12; # 99 in proto, sequential in capnp usually better or just list them. Capnp enums are dense 0-based.
}

enum EventType {
  eventTypeDiscard @0;
  eventTypeTsumogiri @1;
  eventTypeRiichi @2;
  eventTypeClosedKan @3;
  eventTypeAddedKan @4;
  eventTypeTsumo @5;
  eventTypeAbortiveDrawNineTerminals @6;
  eventTypeChi @7;
  eventTypePon @8;
  eventTypeOpenKan @9;
  eventTypeRon @10;
  
  # 11 skipped in proto
  eventTypeDraw @11; # Proto 12
  eventTypeRiichiScoreChange @12; # Proto 13
  eventTypeNewDora @13; # Proto 14
  eventTypeAbortiveDrawFourRiichis @14; # Proto 15
  eventTypeAbortiveDrawThreeRons @15; # Proto 16
  eventTypeAbortiveDrawFourKans @16; # Proto 17
  eventTypeAbortiveDrawFourWinds @17; # Proto 18
  eventTypeExhaustiveDrawNormal @18; # Proto 19
  eventTypeExhaustiveDrawNagashiMangan @19; # Proto 20
}

struct Score {
  round @0 :UInt32;
  honba @1 :UInt32;
  riichi @2 :UInt32;
  tens @3 :List(Int32);
}

struct Event {
  type @0 :EventType;
  who @1 :Int32;
  tile @2 :UInt32;
  open @3 :UInt32;
}

struct PublicObservation {
  gameId @0 :Text;
  playerIds @1 :List(Text);
  initScore @2 :Score;
  doraIndicators @3 :List(UInt32);
  events @4 :List(Event);
}

struct Hand {
  closedTiles @0 :List(UInt32);
  opens @1 :List(UInt32);
}

struct PrivateObservation {
  who @0 :Int32;
  initHand @1 :Hand;
  drawHistory @2 :List(UInt32);
  currHand @3 :Hand;
}

struct Observation {
  who @0 :Int32;
  publicObservation @1 :PublicObservation;
  privateObservation @2 :PrivateObservation;
  roundTerminal @3 :RoundTerminal;
  legalActions @4 :List(Action);
}

struct Win {
  who @0 :Int32;
  fromWho @1 :Int32;
  hand @2 :Hand;
  winTile @3 :UInt32;
  fu @4 :UInt32;
  ten @5 :UInt32;
  tenChanges @6 :List(Int32);
  yakus @7 :List(UInt32);
  fans @8 :List(UInt32);
  yakumans @9 :List(UInt32);
  uraDoraIndicators @10 :List(UInt32);
}

struct NoWinner {
  tenpais @0 :List(TenpaiHand);
  tenChanges @1 :List(Int32);
}

struct TenpaiHand {
  who @0 :Int32;
  hand @1 :Hand;
}

struct RoundTerminal {
  finalScore @0 :Score;
  wins @1 :List(Win);
  noWinner @2 :NoWinner;
  isGameOver @3 :Bool;
}

struct State {
  hiddenState @0 :HiddenState;
  publicObservation @1 :PublicObservation;
  privateObservations @2 :List(PrivateObservation);
  roundTerminal @3 :RoundTerminal;
}

struct HiddenState {
  gameSeed @0 :UInt64;
  wall @1 :List(UInt32);
  uraDoraIndicators @2 :List(UInt32);
}

struct Action {
  type @0 :ActionType;
  who @1 :Int32;
  tile @2 :UInt32;
  open @3 :UInt32;
}

struct GameResult {
  gameSeed @0 :UInt64;
  playerIds @1 :List(Text);
  
  struct StringIntPair {
    key @0 :Text;
    value @1 :Int32;
  }
  tens @2 :List(StringIntPair);
  rankings @3 :List(StringIntPair);
}
