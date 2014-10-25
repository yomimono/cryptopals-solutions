(* use key to encrypt l (both int lists) *)
let encrypt key l =
  let module C = Cryptokit.Cipher in
  let padded = Hexstring.pad (Some (List.length key)) l in
  []

let decrypt key l =
  []
