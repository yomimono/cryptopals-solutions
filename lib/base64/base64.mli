(** Take a string containing pairs of characters representing hexadecimal
  numbers, and base64-encode it. *)
val base64_of_hex : string -> 
  [ `Invalid_argument of string | `Ok of string ]

(** Encode base64 from a list of 8-bit integers. *)
val base64_of_int_list : int list -> 
  [ `Invalid_argument of string | `Ok of string ]

(** Decode base64 into a string containing pairs of characters representing
  hexadecimal numbers. *)
val hex_of_base64 : string -> 
  [ `Invalid_argument of string | `Ok of string ]

(** Decode base64 into a string containing pairs of characters representing
  hexadecimal numbers. *)
val int_list_of_base64 : string -> 
  [ `Invalid_argument of string | `Ok of int list ]

