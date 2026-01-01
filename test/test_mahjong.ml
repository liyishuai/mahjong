open Mahjong

(* BLAKE512 requires OCaml 5.2+. For older versions, use MD5: Digest.(to_hex (string s)) *)
let hash (s : string) : string = Digest.(to_hex (string s));;

Random.self_init ()

let salt : int array = Array.init 17 (fun _ -> Random.bits ())
let hex_salt : string = Tiles.hexstring_of_seeds salt;;

print_endline "---------------------------";
print_endline ("Salt     # :\t" ^ hash hex_salt)

let inputs : int array = Array.init 4 (fun _ -> Random.bits ());;

print_endline ("Inputs dec :\t" ^ Tiles.string_of_seeds inputs);
print_endline "---------------------------"

let seeds : int array = Array.append salt inputs
let mountain : string = Tiles.string_of_tiles (Tiles.random_set seeds);;

print_endline ("Mountain # :\t" ^ hash mountain);
print_endline "---------------------------";
print_endline ("Mountain   :\t" ^ mountain);
print_endline ("Salt   hex :\t" ^ hex_salt);
print_endline "---------------------------";
print_endline ("Salt   dec :\t" ^ Tiles.string_of_seeds salt)
