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

let random_set (seed : int array) : tile array =
  let four_set : tile array =
    let one_set (numbers : number array) : tile array =
      Array.concat
        [ Array.map (fun n -> Man n) numbers
        ; Array.map (fun n -> So n) numbers
        ; Array.map (fun n -> Pin n) numbers
        ; Array.map (fun w -> Wind w) [|North; East; South; West|]
        ; Array.map (fun d -> Dragon d) [|White; Green; Red|] ]
    in
    let normal_set : tile array =
      one_set [|One; Two; Three; Four; Five; Six; Seven; Eight; Nine|]
    in
    let aka_set : tile array =
      one_set [|One; Two; Three; Four; Aka; Six; Seven; Eight; Nine|]
    in
    Array.concat [normal_set; normal_set; normal_set; aka_set]
  in
  Random.full_init seed ;
  Array.shuffle ~rand:Random.int four_set ;
  four_set

let string_of_number (n : number) : string =
  match n with
  | One ->
      "1"
  | Two ->
      "2"
  | Three ->
      "3"
  | Four ->
      "4"
  | Five ->
      "5"
  | Aka ->
      "0"
  | Six ->
      "6"
  | Seven ->
      "7"
  | Eight ->
      "8"
  | Nine ->
      "9"

let string_of_wind (w : wind) : string =
  match w with North -> "1" | East -> "2" | South -> "3" | West -> "4"

let string_of_dragon (d : dragon) : string =
  match d with White -> "5" | Green -> "6" | Red -> "7"

let string_of_tile (t : tile) : string =
  match t with
  | Man n ->
      string_of_number n ^ "m"
  | So n ->
      string_of_number n ^ "s"
  | Pin n ->
      string_of_number n ^ "p"
  | Wind w ->
      string_of_wind w ^ "z"
  | Dragon d ->
      string_of_dragon d ^ "z"

let string_of_tiles (tiles : tile array) : string =
  String.concat "" (List.map string_of_tile (Array.to_list tiles))

let string_of_seeds (seeds : int array) : string =
  String.concat " " (Array.to_list (Array.map string_of_int seeds))
