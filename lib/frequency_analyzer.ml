
let english_frequents = 
  List.map int_of_char [' '; 'E';'T';'A';'O';'I';'N';'S';'H';'R';'D';'L';'U' ]

let add_count count_so_far item =
  let already = Hashtbl.mem count_so_far item in
  match already with 
  | false -> Hashtbl.add count_so_far item 1
  | true -> 
    let count = Hashtbl.find count_so_far item in
    Hashtbl.replace count_so_far item (count + 1)

let rec count_occurrences count_so_far l =
  match l with
  | [] -> count_so_far
  | [ hd ] -> add_count count_so_far hd; count_so_far
  | hd :: tl -> 
    add_count count_so_far hd;
    count_occurrences count_so_far  tl

let fold_hashmap key value last_result =
  let (last_key, last_value) = last_result in
  match compare value last_value with
  | n when n >= 0 -> (key, value)
  | _ -> last_result

let get_count str =
  let l = Hexstring.int_list_of_t str in
  count_occurrences (Hashtbl.create 255) l

let most_common str =
  let freq_map = get_count str in
  let (most_common, _) = Hashtbl.fold fold_hashmap freq_map (0, 0) in
  if Hashtbl.length freq_map = 0 then None
  else Some most_common

let try_decode str =
  let l = Hexstring.int_list_of_t str in
  let freq_map = get_count str in
  let (most_common, _) = Hashtbl.fold fold_hashmap freq_map (0, 0) in
  let xor = (List.hd english_frequents) lxor most_common in
  (* let char_map_result = List.map char_of_int (List.map ((lxor) xor) l) *)
  let int_list_result = (List.map ((lxor) xor) l) in
  (Hexstring.t_of_int_list_exn int_list_result)
