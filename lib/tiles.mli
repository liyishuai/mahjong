type wind = North | East | South | West

type dragon = White | Green | Red

type number =
  | One
  | Two
  | Three
  | Four
  | Five
  | Aka
  | Six
  | Seven
  | Eight
  | Nine

type tile =
  | Man of number
  | So of number
  | Pin of number
  | Wind of wind
  | Dragon of dragon

val random_set : int array -> tile array

val string_of_number : number -> string

val string_of_wind : wind -> string

val string_of_dragon : dragon -> string

val string_of_tile : tile -> string

val string_of_tiles : tile array -> string

val string_of_seeds : int array -> string
