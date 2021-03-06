let valid_base64_chars =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" 

let char_of_6bit_int i = 
  match i with 
  | number when number < 0 -> `Invalid_argument "Number appears to be negative"
  | number when number < 64 -> `Ok (String.get valid_base64_chars i)
  | _ -> `Invalid_argument "Number out of 6-bit range"

let six_bit_int_of_char c =
  try `Ok (String.index valid_base64_chars c)
  with Not_found -> `Invalid_argument "Non-base64 character in input"

let map_char_to_int c =
  let normalized = Char.lowercase c in
  match normalized with
  | '0' -> `Ok 0
  | '1' -> `Ok 1
  | '2' -> `Ok 2
  | '3' -> `Ok 3
  | '4' -> `Ok 4
  | '5' -> `Ok 5
  | '6' -> `Ok 6
  | '7' -> `Ok 7
  | '8' -> `Ok 8
  | '9' -> `Ok 9
  | 'a' -> `Ok 10
  | 'b' -> `Ok 11
  | 'c' -> `Ok 12
  | 'd' -> `Ok 13
  | 'e' -> `Ok 14
  | 'f' -> `Ok 15
  | _ -> `Invalid_argument "Unreadable hex character"

let pair_to_byte left right =
  let msb = (map_char_to_int left) in
  let lsb = (map_char_to_int right) in
  match msb, lsb with
  | `Invalid_argument s, `Invalid_argument _ -> `Invalid_argument s
  | `Invalid_argument s, `Ok _ -> `Invalid_argument s
  | `Ok _, `Invalid_argument s -> `Invalid_argument s
  | `Ok m, `Ok l -> `Ok ((m * 16) + l)
  | _, _ -> `Invalid_argument "Couldn't translate hex pair to byte"

let char_0 left =
  char_of_6bit_int (left lsr 2)

let char_1 left middle =
  char_of_6bit_int (((left land 3) lsl 4) + (middle lsr 4))

let char_2 middle right =
  char_of_6bit_int (((middle land 15) lsl 2) + (right lsr 6))

let char_3 right =
  char_of_6bit_int (right land 63)

let three_bytes_to_chars left middle right =
  match (char_0 left, char_1 left middle, char_2 middle right, char_3 right) with 
  | `Ok zero, `Ok one, `Ok two, `Ok three ->
    `Ok (Printf.sprintf "%c%c%c%c" zero one two three)
  | _ -> 
    let err_string = (Printf.sprintf "Couldn't generate base64 from bytes %d
        %d %d" left middle right) in
    `Invalid_argument err_string

module Decode = struct
  let tuple s =
    let char_or_padding c =
      match c with
      | '=' -> `Padding
      | i -> six_bit_int_of_char i
    in
    (char_or_padding (String.get s 0), 
     char_or_padding (String.get s 1),
     char_or_padding (String.get s 2),
     char_or_padding (String.get s 3))

  let first leftmost leftmid = 
    ((leftmost land 63) lsl 2) + ((leftmid land 48) lsr 4)

  let midchar l r = 
    ((l land 15) lsl 4) + ((r lsr 2) land 15)

  let last mr_int rr_int =
    ((mr_int land 3) lsl 6) + (rr_int land 63)

end


(** hex -> base64 *)
(* take 3 chars at a time and slice them into 4 chars. *)
(* input is a string in hex format, so each "char" is two chars *)
let rec base64_of_hex hex =

  (* TODO: functorize over string and list -- both libraries provide
   * the necessary functions in Core.Std *)
  match (String.length hex) with
  | l when l mod 2 <> 0 -> `Invalid_argument "Uneven number of hex digits"
  | 0 -> `Ok ""
  | 6 -> (* direct conversion *) (
      let byte_0 = pair_to_byte (String.get hex 0) (String.get hex 1) in
      let byte_1 = pair_to_byte (String.get hex 2) (String.get hex 3) in
      let byte_2 = pair_to_byte (String.get hex 4) (String.get hex 5) in
      match byte_0, byte_1, byte_2 with
      | `Ok left, `Ok middle, `Ok right ->
        three_bytes_to_chars left middle right
      | _ -> `Invalid_argument "Couldn't translate hex pair to byte"
    )
  | 2 -> (
      let byte_0 = pair_to_byte (String.get hex 0) (String.get hex 1) in
      match byte_0 with
      | `Ok left -> (
          match (char_0 left, char_1 left 0 ) with
          | `Ok p, `Ok q -> `Ok (Printf.sprintf "%c%c==" p q)
          | _ -> `Invalid_argument "Couldn't translate hex to base64"
        )
      | _ -> `Invalid_argument "Couldn't translate hex pair to byte"
    )
  | 4 -> (
      let byte_0 = pair_to_byte (String.get hex 0) (String.get hex 1) in
      let byte_1 = pair_to_byte (String.get hex 2) (String.get hex 3) in
      match byte_0, byte_1 with
      | `Ok left, `Ok middle -> (
          match (char_0 left, char_1 left middle, char_2 middle 0) with
          | `Ok p, `Ok q, `Ok r -> `Ok (Printf.sprintf "%c%c%c=" p q r)
          | _ -> `Invalid_argument "Couldn't translate hex to base64"
        )
      | _ -> `Invalid_argument "Couldn't translate hex pair to byte"
    )
  (* TODO: this isn't tail-call recursive and it could be. *)
  | p -> 
    let prefix = String.sub hex 0 6 in
    let this = (base64_of_hex prefix) in
    let next = (base64_of_hex (String.sub hex 6 (p-6))) in
    match (this, next) with
    | `Ok p, `Ok q -> `Ok (Printf.sprintf "%s%s" p q)
    | `Invalid_argument s, _ | _, `Invalid_argument s ->
      `Invalid_argument s

(** int list -> base64 *)
let base64_of_int_list l =
  (* TODO: this is a stupid way to do this; refactor and encode directly *)
  base64_of_hex (String.concat "" (List.map (Printf.sprintf "%02x") l))

let rec hex_of_base64 base64 =
  (* take 4 chars, slice them into 3 chars, do reverse lookup byte-to-pair *)
  match (String.length base64) with
  | 0 -> `Ok ""
  | 4 -> (
      match Decode.tuple (String.sub base64 0 4) with
      | `Ok leftmost, `Ok leftmid, `Padding, `Padding -> 
        `Ok (Printf.sprintf "%02x" (Decode.first leftmost leftmid))
      | `Ok leftmost, `Ok leftmid, `Ok mr_int, `Padding ->
        `Ok (Printf.sprintf "%02x%02x" (Decode.first leftmost leftmid) 
               (Decode.midchar leftmid mr_int))
      | `Ok leftmost, `Ok leftmid, `Ok mr_int, `Ok rr_int ->
        `Ok ((Printf.sprintf "%02x%02x%02x" 
                (Decode.first leftmost leftmid) 
                (Decode.midchar leftmid mr_int) 
                (Decode.last mr_int rr_int)))
      | _, _, _, _ -> `Invalid_argument "Unparseable chunk in base64"
    )
  | l when l mod 4 <> 0 -> `Invalid_argument "Strange number of characters"
  | p -> 
    let prefix = String.sub base64 0 4 in
    let this = (hex_of_base64 prefix) in
    let next = (hex_of_base64 (String.sub base64 4 (p-4))) in
    match (this, next) with
    | `Ok p, `Ok q -> `Ok (Printf.sprintf "%s%s" p q)
    | `Invalid_argument s, _ | _, `Invalid_argument s ->
      `Invalid_argument s

let rec int_list_of_base64 base64 =
  match (String.length base64) with
  | 0 -> `Ok []
  | 4 -> (
      match Decode.tuple (String.sub base64 0 4) with
      | `Ok leftmost, `Ok leftmid, `Padding, `Padding -> 
        `Ok [(Decode.first leftmost leftmid)]
      | `Ok leftmost, `Ok leftmid, `Ok mr_int, `Padding ->
        `Ok [( Decode.first leftmost leftmid) ; (Decode.midchar leftmid mr_int)]
      | `Ok leftmost, `Ok leftmid, `Ok mr_int, `Ok rr_int ->
        `Ok [ (Decode.first leftmost leftmid) 
            ; (Decode.midchar leftmid mr_int) 
            ; (Decode.last mr_int rr_int) ]
      | _, _, _, _ -> `Invalid_argument "Unparseable chunk in base64"
    )
  | l when l mod 4 <> 0 -> `Invalid_argument "Strange number of characters"
  | p -> 
    let prefix = String.sub base64 0 4 in
    let this = (int_list_of_base64 prefix) in
    let next = (int_list_of_base64 (String.sub base64 4 (p-4))) in
    match (this, next) with
    | `Ok p, `Ok q -> `Ok (List.append p q)
    | `Invalid_argument s, _ | _, `Invalid_argument s ->
      `Invalid_argument s

