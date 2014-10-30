open Core.Std

let random_key size = 
  Random.self_init ();
  if size < 0 then [] 
  else List.init size (fun _f -> Random.int 256)

let random_encrypt size plaintext =
  let garbage _ = (List.init ((Random.int 5) + 5) (fun _f -> Random.int 256)) in
  let key = random_key size in
  Random.self_init ();
  let padded = garbage () @ plaintext @ garbage () in
  let mode = 
    match (Random.bool ()) with
    | true -> Aes_ecb.encrypt
    | false -> Aes_cbc.encrypt
  in
  mode key padded

let oracle ciphertext = 
  let blocks = List.groupi ciphertext ~break:(fun i _ _ -> i mod 16 = 0) in
  (* super crude - if we find 1 duplicate block, claim ECB *)
  match List.find_a_dup blocks with
  | Some p -> Cryptokit.Cipher.ECB
  | None -> Cryptokit.Cipher.CBC
