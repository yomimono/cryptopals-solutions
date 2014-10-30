open Core.Std 

let encrypt key plaintext =
  let module C = Cryptokit.Cipher in
  let string_key = (Hexstring.ascii_of_int_list key) in
  let encryptor = C.aes ~mode:C.ECB string_key C.Encrypt in
  ignore (List.map ~f:(encryptor#put_byte) 
            (Hexstring.pad (Some (List.length key)) plaintext));
  encryptor#finish;
  let ciphertext = encryptor#get_string in
  (Hexstring.int_list_of_t (Hexstring.t_of_ascii ciphertext))

let decrypt key ciphertext =
  let module C = Cryptokit.Cipher in
  let string_key = (Hexstring.ascii_of_int_list key) in
  let decryptor = C.aes ~mode:C.ECB string_key C.Decrypt in
  ignore (List.map ~f:(decryptor#put_byte) ciphertext);
  decryptor#finish;
  let plaintext = decryptor#get_string in
  (Hexstring.int_list_of_t (Hexstring.t_of_ascii plaintext))
