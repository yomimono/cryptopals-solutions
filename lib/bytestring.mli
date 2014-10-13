module Bytestring : sig

  (** Transform a list of 8-bit integers to a hexadecimal string *)
  val int_list_to_hexstring : int list -> string
  (** Transform a string with each pair of characters representing an 8-bit
      integer to a list of those integers *)
  val hexstring_to_int_list : string -> int list
  (** Represent a list of 8-bit integers as an ASCII string *)
  val int_list_to_ascii : int list -> string
  (** Decode a hex string as ASCII characters and return the resulting string.
  *)
  val hexstring_to_ascii : string -> string
  (** Represent a string of ASCII characters as a hex string. *)
  val ascii_to_hexstring : string -> string
end 
