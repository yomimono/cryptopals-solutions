module Base64 : sig

  val base64_of_hex : string -> string

end = struct
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

  (** hex -> base64 *)
  (* take 3 chars at a time and slice them into 4 chars. *)
  (* input is a string in hex format, so each "char" is two chars *)
  let rec base64_of_hex hex =
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

end
