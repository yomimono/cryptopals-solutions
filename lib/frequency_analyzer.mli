val try_decode : Hexstring.t -> Hexstring.t
val get_count : Hexstring.t -> (int, int) Hashtbl.t
val most_common : Hexstring.t -> int option
val best_guess_key : int list -> int
