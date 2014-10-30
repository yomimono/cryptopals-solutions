(** generate n random integers between 0 and 255 for use as an AES key *)
val random_key : int -> int list
(** take a given keysize and plaintext, and encrypt it with randomized
      parameters as per the problem 11 specification *)
val random_encrypt : int -> int list -> int list

val oracle : int list -> Cryptokit.Cipher.chaining_mode
