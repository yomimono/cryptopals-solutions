module Fixed_xor = struct

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

  let hexstring_to_int_list hex =
    List.map pair_to_byte (pairwise hex)

  let rec int_list_to_hexstring l =
    match l with 
    | [] -> []
    | hd :: tl -> (Printf.sprintf "%x" hd) :: int_list_to_hexstring tl

  let xor str1 str2 =
    let str1_pw = hexstring_to_int_list str1 in
    let str2_pw = hexstring_to_int_list str2 in
    let int_list_xord = List.map2 (lxor) str1_pw str2_pw in
    String.concat "" (int_list_to_hexstring int_list_xord);

end
