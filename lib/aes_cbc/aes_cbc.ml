open Core.Std


let rec get_n_bytes n (e : Cryptokit.transform) acc =
  if n = 0 then List.rev acc else
    let next = try (Some e#get_byte) with End_of_file -> None in
    match next with
    | None -> List.rev acc
    | Some p -> get_n_bytes (n-1) e (p :: acc)

(* use key to encrypt l (both int lists) *)
let encrypt key l =
  let module C = Cryptokit.Cipher in
  let encrypt_blocks key blocksize text_so_far right_text =
    let string_key = (Hexstring.ascii_of_t (Hexstring.t_of_int_list_exn key)) in
    let cryptor = C.aes ~mode:C.ECB string_key C.Encrypt in
    (* use the last 16 bytes of text_so_far as our obscuring value *)
    let left_text = List.sub text_so_far 
        ~pos:((List.length text_so_far) - blocksize) ~len:blocksize in
    let block_to_crypt = List.map2_exn ~f:(lxor) left_text
        right_text in
    (* since we need to include the output of the encryption in the 
       plaintext for the next block, this unavoidably a unit-returning
       side-effecting operation *)
    ignore (List.map ~f:(cryptor#put_byte) block_to_crypt);
    cryptor#finish;
    let text = get_n_bytes blocksize cryptor [] in
    (* keep appending the new ciphertext to the ciphertext so far *)
    text_so_far @ text
  in
  let padded = Hexstring.pad (Some (List.length key)) l in
  let blocksize = (List.length key) in
  let iv = (List.init blocksize (fun _ -> 0)) in
  let pt = List.groupi ~break:(fun n _ _ -> n mod blocksize = 0) padded in
  let ciphertext = List.fold_left pt ~init:iv 
      ~f:(fun (a: int list) (b: int list) -> encrypt_blocks key blocksize a b) in
  (* trim 16 bytes of initial garbage, resulting from IV *)
  List.sub ~pos:blocksize ~len:((List.length ciphertext)-blocksize) ciphertext

let decrypt key l =
  let module C = Cryptokit.Cipher in
  let string_key = (Hexstring.ascii_of_t (Hexstring.t_of_int_list_exn key)) in
  (* left side is ciphertext which was XOR'd with right side's plaintext
   * during encryption *)
  let decrypt_blocks key blocksize (ct, pt_so_far) right =
    let cryptor = C.aes ~mode:C.ECB string_key C.Decrypt in
    ignore (List.map ~f:(cryptor#put_byte) right);
    ignore (cryptor#finish);
    let xor_obscured_block = get_n_bytes blocksize cryptor [] in
    let pt = List.map2_exn ct xor_obscured_block ~f:(lxor) in
    (right, pt_so_far @ pt)
  in
  let blocksize = (List.length key) in
  let iv = (List.init blocksize (fun _ -> 0), [])
  in
  let ct = List.groupi ~break:(fun n _ _ -> n mod blocksize = 0) l in
  let (_, plaintext) = List.fold_left ct ~init:iv 
      ~f:(fun a b -> decrypt_blocks key blocksize a b) in
  (* List.sub ~pos:blocksize ~len:((List.length plaintext)-blocksize) plaintext
  *)
  plaintext
