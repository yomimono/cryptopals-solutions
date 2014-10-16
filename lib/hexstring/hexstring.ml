type t = string

let int_list_of_hexstring hex =
  let pair_to_byte str =
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
    in

    let msb = (map_char_to_int (String.get str 0)) in
    let lsb = (map_char_to_int (String.get str 1)) in
    (msb * 16) + lsb
  in
  let rec pairwise str = 
    match (String.length str) with
    | 0 -> []
    | 2 -> [ str ]
    | n when n mod 2 <> 0 -> raise (Invalid_argument "odd number of hex digits")
    | l -> String.sub str 0 2 :: pairwise (String.sub str 2 (l - 2))
  in
  List.map pair_to_byte (pairwise hex)

let hexstring_of_int_list l =
  let rec int_list_to_hexstring_list l =
    match l with 
    | [] -> []
    | hd :: tl -> (Printf.sprintf "%02x" hd) :: int_list_to_hexstring_list tl
  in
  String.concat "" (int_list_to_hexstring_list l)


let ascii_of_int_list l = 
  let char_list = List.map char_of_int l in
  let string_list = List.map (Printf.sprintf "%c") char_list in
  String.concat "" string_list

let ascii_of_t s =
  ascii_of_int_list (int_list_of_hexstring s)

let rec int_list_of_ascii (s: string) =
  let l = String.length s in
  match l with
  | 0 -> []
  | _ -> (int_of_char (String.get s 0)) :: 
         int_list_of_ascii (String.sub s 1 (l - 1))

let t_of_ascii s =
  hexstring_of_int_list (int_list_of_ascii s)

let int_list_of_t t =
  int_list_of_hexstring t

let t_of_int_list_exn l =
  hexstring_of_int_list l

let string_of_t (t : t) = (t : string)

let t_of_string_exn (s : string) = 
  let _l = int_list_of_hexstring s in
  (s : t)
