(** compute the Hamming distance between two lists of integers.
  * if the lists are of unequal length, all excess bits 
  * will be held to be unequal. *)
val distance : int list -> int list -> int
