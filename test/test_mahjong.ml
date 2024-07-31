open Mahjong

let hash (s : string) : string = Digest.BLAKE512.(to_hex (string s)) ;;

Random.self_init ()

let salt : int array = Array.init 18 (fun _ -> Random.bits ()) ;;

print_endline ("Salt     # :\t" ^ hash (Tiles.string_of_seeds salt))

let inputs : int array = Array.init 4 (fun _ -> Random.bits ()) ;;

print_endline ("Inputs     :\t" ^ Tiles.string_of_seeds inputs)

let seeds : int array = Array.append salt inputs

let mountain : string = Tiles.string_of_tiles (Tiles.random_set seeds) ;;

print_endline ("Mountain # :\t" ^ hash mountain) ;
print_endline ("Mountain   :\t" ^ mountain) ;
print_endline ("Salt       :\t" ^ Tiles.string_of_seeds salt)
