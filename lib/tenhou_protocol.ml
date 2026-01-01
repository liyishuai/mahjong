(** Tenhou protocol encoding/decoding *)

open Tiles

(** Tenhou tile encoding (0-135) *)
let tenhou_tile_of_tile (t : tile) (instance : int) : int =
  let base = match t with
    | Man n -> (int_of_number n - 1) * 4
    | Pin n -> 36 + (int_of_number n - 1) * 4
    | So n -> 72 + (int_of_number n - 1) * 4
    | Honor h -> 108 + (int_of_honor h - 1) * 4
  in
  (* Aka (red 5) is encoded as tile 16, 52, 88 *)
  match t with
  | Man Aka -> 16
  | Pin Aka -> 52
  | So Aka -> 88
  | _ -> base + (instance mod 4)

(** Convert Tenhou tile code to tile *)
let tile_of_tenhou_tile (code : int) : tile =
  if code < 0 || code > 135 then
    failwith (Printf.sprintf "Invalid Tenhou tile code: %d" code)
  else if code < 36 then
    (* Man tiles *)
    let num = code / 4 in
    let n = match num with
      | 0 -> One | 1 -> Two | 2 -> Three | 3 -> Four
      | 4 -> if code = 16 then Aka else Five
      | 5 -> Six | 6 -> Seven | 7 -> Eight | _ -> Nine
    in
    Man n
  else if code < 72 then
    (* Pin tiles *)
    let num = (code - 36) / 4 in
    let n = match num with
      | 0 -> One | 1 -> Two | 2 -> Three | 3 -> Four
      | 4 -> if code = 52 then Aka else Five
      | 5 -> Six | 6 -> Seven | 7 -> Eight | _ -> Nine
    in
    Pin n
  else if code < 108 then
    (* So tiles *)
    let num = (code - 72) / 4 in
    let n = match num with
      | 0 -> One | 1 -> Two | 2 -> Three | 3 -> Four
      | 4 -> if code = 88 then Aka else Five
      | 5 -> Six | 6 -> Seven | 7 -> Eight | _ -> Nine
    in
    So n
  else
    (* Honor tiles *)
    let num = (code - 108) / 4 in
    let h = match num with
      | 0 -> East | 1 -> South | 2 -> West | 3 -> North
      | 4 -> White | 5 -> Green | _ -> Red
    in
    Honor h

(** Tenhou message types *)
type tenhou_msg =
  | Helo of string  (** Login response *)
  | Rejoin of string * string * string  (** Rejoin info *)
  | Go of int * int  (** Game start: lobby, game type *)
  | Un of string array  (** Player names *)
  | Taikyoku of int * int  (** Game ID, seat *)
  | Init of init_info  (** Round start *)
  | Draw of int  (** Tsumo: tile code *)
  | Discard of int * int  (** Discard: player, tile *)
  | Call of call_info  (** Naki (call) *)
  | Reach of int * int  (** Riichi: player, step *)
  | Agari of agari_info  (** Win *)
  | Ryuukyoku of ryuukyoku_info  (** Draw game *)
  | Dora of int  (** New dora indicator *)
  | Prof  (** Profile request *)
  | Bye  (** Logout *)
  | Unknown of string  (** Unknown message *)

and init_info =
  { seed : int array  (** Round seed: round, honba, riichi_sticks, dice1, dice2, dora_indicator *)
  ; ten : int array  (** Starting points / 100 *)
  ; oya : int  (** Dealer seat *)
  ; hai : int array  (** Initial hand tiles *)
  }

and call_info =
  { caller : int  (** Who called *)
  ; call_type : call_type  (** Type of call *)
  ; tiles : int array  (** Tiles involved *)
  ; from_who : int  (** Who discarded (for chi/pon/kan) *)
  }

and call_type =
  | Chi
  | Pon
  | Daiminkan  (** Called kan *)
  | Kakan  (** Added kan *)
  | Ankan  (** Closed kan *)

and agari_info =
  { winner : int
  ; from_who : int  (** -1 for tsumo *)
  ; score_changes : int array
  ; yaku : (int * int) list  (** Yaku ID, han *)
  ; dora_count : int
  ; ura_dora_count : int
  }

and ryuukyoku_info =
  { reason : string
  ; score_changes : int array
  ; tenpai : bool array
  }

(** Parse Tenhou XML message *)
let parse_message (xml : string) : tenhou_msg =
  (* Simple XML tag extraction *)
  let get_tag s =
    let len = String.length s in
    if len < 2 || s.[0] <> '<' then ""
    else
      (* Find end of tag name (space, /, >, or digit for single-letter tags like T45) *)
      let rec find_end i =
        if i >= len then i
        else match s.[i] with
          | ' ' | '/' | '>' -> i
          | '0'..'9' -> i  (* Stop at digit for tags like T45 *)
          | _ -> find_end (i + 1)
      in
      let end_idx = find_end 1 in
      String.sub s 1 (end_idx - 1)
  in
  
  let get_attr name s =
    let pattern = name ^ "=\"" in
    try
      let start = String.index_from s 0 (String.get pattern 0) in
      let rec find_pattern pos =
        if pos + String.length pattern > String.length s then raise Not_found
        else if String.sub s pos (String.length pattern) = pattern then pos
        else find_pattern (pos + 1)
      in
      let attr_start = find_pattern start + String.length pattern in
      let attr_end = String.index_from s attr_start '"' in
      Some (String.sub s attr_start (attr_end - attr_start))
    with Not_found -> None
  in
  
  let parse_int_list s =
    String.split_on_char ',' s
    |> List.filter (fun x -> String.length x > 0)
    |> List.map int_of_string
    |> Array.of_list
  in
  
  let tag = get_tag xml in
  match tag with
  | "HELO" ->
      let uname = Option.value ~default:"" (get_attr "uname" xml) in
      Helo uname
  
  | "GO" ->
      let t = Option.value ~default:"0" (get_attr "type" xml) |> int_of_string in
      let lobby = Option.value ~default:"0" (get_attr "lobby" xml) |> int_of_string in
      Go (lobby, t)
  
  | "UN" ->
      let names = Array.init 4 (fun i ->
        let attr = Printf.sprintf "n%d" i in
        Option.value ~default:"" (get_attr attr xml)
      ) in
      Un names
  
  | "TAIKYOKU" ->
      let oya = Option.value ~default:"0" (get_attr "oya" xml) |> int_of_string in
      let log = Option.value ~default:"" (get_attr "log" xml) in
      Taikyoku (int_of_string log, oya)
  
  | "INIT" ->
      let seed = Option.value ~default:"0,0,0,0,0,0" (get_attr "seed" xml) |> parse_int_list in
      let ten = Option.value ~default:"250,250,250,250" (get_attr "ten" xml) |> parse_int_list in
      let oya = Option.value ~default:"0" (get_attr "oya" xml) |> int_of_string in
      let hai = Option.value ~default:"" (get_attr "hai" xml) |> parse_int_list in
      Init { seed; ten; oya; hai }
  
  | "DORA" ->
      let hai = Option.value ~default:"0" (get_attr "hai" xml) |> int_of_string in
      Dora hai
  
  | "REACH" ->
      let who = Option.value ~default:"0" (get_attr "who" xml) |> int_of_string in
      let step = Option.value ~default:"1" (get_attr "step" xml) |> int_of_string in
      Reach (who, step)
  
  | "BYE" -> Bye
  | "PROF" -> Prof
  
  | _ ->
      (* Check for draw/discard tags (T, U, V, W for draws; D, E, F, G for discards) *)
      if String.length tag = 1 then
        let c = tag.[0] in
        let parse_tile_from_tag () =
          try
            let tile_str = String.sub xml 2 (String.length xml - 4) in
            Some (int_of_string tile_str)
          with _ -> None
        in
        if c >= 'T' && c <= 'W' then
          (* Draw *)
          match parse_tile_from_tag () with
          | Some tile -> Draw tile
          | None -> Unknown xml
        else if c >= 'D' && c <= 'G' then
          (* Discard *)
          let player = Char.code c - Char.code 'D' in
          match parse_tile_from_tag () with
          | Some tile -> Discard (player, tile)
          | None -> Unknown xml
        else
          Unknown xml
      else
        Unknown xml

(** Encode action to Tenhou XML *)
let encode_discard (tile : int) : string =
  Printf.sprintf "<D p=\"%d\"/>" tile

let encode_reach (tile : int) : string =
  Printf.sprintf "<REACH hai=\"%d\"/>" tile

let encode_noop () : string =
  "<N />"

let encode_pon (tiles : int array) (from : int) : string =
  Printf.sprintf "<N type=\"1\" hai0=\"%d\" hai1=\"%d\" who=\"%d\"/>" 
    tiles.(0) tiles.(1) from

let encode_chi (tiles : int array) : string =
  Printf.sprintf "<N type=\"0\" hai0=\"%d\" hai1=\"%d\"/>" 
    tiles.(0) tiles.(1)

let encode_kan (tiles : int array) (kan_type : int) : string =
  Printf.sprintf "<N type=\"%d\" hai=\"%d,%d,%d,%d\"/>"
    kan_type tiles.(0) tiles.(1) tiles.(2) tiles.(3)

let encode_tsumo () : string =
  "<AGARI />"

let encode_ron () : string =
  "<AGARI />"

let encode_ryuukyoku () : string =
  "<RYUUKYOKU />"

(** Heartbeat/keep-alive *)
let encode_keepalive () : string =
  "<Z />"

(** Login message *)
let encode_helo (username : string) (auth_token : string) : string =
  Printf.sprintf "<HELO name=\"%s\" tid=\"%s\" sx=\"M\" />" username auth_token
