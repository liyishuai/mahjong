type honor = East | South | West | North | White | Green | Red

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

type tile = Man of number | So of number | Pin of number | Honor of honor

let random_set (seed : int array) : tile array =
  let four_set : tile array =
    let one_set (numbers : number array) : tile array =
      Array.concat
        [ Array.map (fun n -> Man n) numbers
        ; Array.map (fun n -> So n) numbers
        ; Array.map (fun n -> Pin n) numbers
        ; Array.map
            (fun z -> Honor z)
            [|East; South; West; North; White; Green; Red|] ]
    in
    let aka_set : tile array =
      one_set [|One; Two; Three; Four; Aka; Six; Seven; Eight; Nine|]
    in
    let normal_set : tile array =
      one_set [|One; Two; Three; Four; Five; Six; Seven; Eight; Nine|]
    in
    Array.concat [aka_set; normal_set; normal_set; normal_set]
  in
  Random.full_init seed ;
  Array.shuffle ~rand:Random.int four_set ;
  four_set

let int_of_number (n : number) : int =
  match n with
  | One ->
      1
  | Two ->
      2
  | Three ->
      3
  | Four ->
      4
  | Five | Aka ->
      5
  | Six ->
      6
  | Seven ->
      7
  | Eight ->
      8
  | Nine ->
      9

let string_of_number (n : number) : string =
  match n with Aka -> "0" | _ -> string_of_int (int_of_number n)

let int_of_honor (z : honor) : int =
  match z with
  | East ->
      1
  | South ->
      2
  | West ->
      3
  | North ->
      4
  | White ->
      5
  | Green ->
      6
  | Red ->
      7

let string_of_tile (t : tile) : string =
  match t with
  | Man n ->
      string_of_number n ^ "m"
  | So n ->
      string_of_number n ^ "s"
  | Pin n ->
      string_of_number n ^ "p"
  | Honor z ->
      string_of_int (int_of_honor z) ^ "z"

let string_of_tiles (tiles : tile array) : string =
  String.concat "" (List.map string_of_tile (Array.to_list tiles))

let string_of_seeds (seeds : int array) : string =
  String.concat " " (Array.to_list (Array.map string_of_int seeds))

let hexstring_of_seeds (seeds : int array) : string =
  String.concat "" (Array.to_list (Array.map (Printf.sprintf "%08x") seeds))

let salt_of_hexstring (str : string) : int array =
  let len = String.length str / 8 in
  Array.init len (fun i -> int_of_string ("0x" ^ String.sub str (i * 8) 8))
