let distance a b =
  let bitcount n = 
    let isset n p = ((n land p) / p) in
    isset n 128 + isset n 64 + isset n 32 + isset n 16 + isset n 8 + isset n 4 
    + isset n 2 + isset n 1
  in
  let diffbits = List.map2 (lxor) a b in  (* TODO: handle unequal lengths *)
  let diffcount = List.map bitcount diffbits in
  List.fold_left (+) 0 diffcount

