module Repeating_xor : sig
  val repeat_xor : string -> string -> string
end = struct
  let rec repeat_xor key plaintext =
    let open Fixed_xor in
    let (keyl, ptl) = (String.length key, String.length plaintext) in
    match compare keyl ptl with
    | 0 -> Fixed_xor.xor key plaintext
    | 1 -> (* key longer than plaintext *)
      Fixed_xor.xor (String.sub key 0 ptl) plaintext
    | _ -> (* plaintext longer than key *)
      let ciphertext = Fixed_xor.xor key (String.sub plaintext 0 keyl) in
      Printf.sprintf "%s%s" ciphertext (repeat_xor key (String.sub plaintext keyl
                                                        (ptl - keyl)))

end
