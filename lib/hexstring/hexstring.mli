type t

(** Transform a list of 8-bit integers to a hexadecimal string *)
val t_of_int_list_exn: int list -> t
(** Transform a string with each pair of characters representing an 8-bit
      integer to a list of those integers *)
val int_list_of_t: t -> int list
(** Represent a list of 8-bit integers as an ASCII string *)
val ascii_of_int_list: int list -> string
(** Decode a hex string as ASCII characters and return the resulting string.
*)
val ascii_of_t: t -> string
(** Represent a string of ASCII characters as a hex string. *)
val t_of_ascii: string -> t

