module Bytestring = struct

  let map_char_to_int c =
    let normalized = Char.lowercase c in
    match normalized with
      | '0' -> 0
      | '1' -> 1
      | '2' -> 2
      | '3' -> 3
      | '4' -> 4
      | '5' -> 5
      | '6' -> 6
      | '7' -> 7
      | '8' -> 8
      | '9' -> 9
      | 'a' -> 10
      | 'b' -> 11
      | 'c' -> 12
      | 'd' -> 13
      | 'e' -> 14
      | 'f' -> 15
      | _ -> raise (Invalid_argument "nonparseable hex digit")

  let pair_to_byte str =
    let msb = (map_char_to_int (String.get str 0)) in
    let lsb = (map_char_to_int (String.get str 1)) in
    (msb * 16) + lsb

  let rec pairwise str = 
    match (String.length str) with
    | 0 -> []
    | 2 -> [ str ]
    | n when n mod 2 <> 0 -> raise (Invalid_argument "odd number of hex digits")
    | l -> String.sub str 0 2 :: pairwise (String.sub str 2 (l - 2))

  let rec int_list_to_hexstring_list l =
    match l with 
    | [] -> []
    | hd :: tl -> (Printf.sprintf "%02x" hd) :: int_list_to_hexstring_list tl

  let int_list_to_hexstring l =
    String.concat "" (int_list_to_hexstring_list l)

  let hexstring_to_int_list hex =
    List.map pair_to_byte (pairwise hex)

  let int_list_to_ascii l = 
    let char_list = List.map char_of_int l in
    let string_list = List.map (Printf.sprintf "%c") char_list in
    String.concat "" string_list

  let hexstring_to_ascii s =
    int_list_to_ascii (hexstring_to_int_list s)

  let rec ascii_to_int_list (s: string) =
    let l = String.length s in
    match l with
    | 0 -> []
    | _ -> (int_of_char (String.get s 0)) :: 
           ascii_to_int_list (String.sub s 1 (l - 1))

  let ascii_to_hexstring s =
    int_list_to_hexstring (ascii_to_int_list s)

end
