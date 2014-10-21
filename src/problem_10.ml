open Core.Std
open Cmdliner

(* use an ECB encryptor to get CBC behavior by recombinating the 
          ciphertext with the next block's plaintext ourselves. *)
let xor_blocks ( key : string ) 
    ( blocksize : int )
    ( ciphertext_so_far : int list ) 
    ( right_plaintext : int list ) : ( int list ) =
  let module C = Cryptokit.Cipher in
  let encryptor = C.aes ~mode:C.ECB key C.Encrypt in
  (* use the last 16 bytes of ciphertext_so_far as our obscuring value *)
  let left_ciphertext = List.sub ciphertext_so_far 
      ~pos:((List.length ciphertext_so_far) - 16) ~len:16 in
  let block_to_encrypt = List.map2_exn ~f:(lxor) left_ciphertext
      right_plaintext in
  let rec get_n_bytes n (e : Cryptokit.transform) acc =
    if n = 0 then List.rev acc else
      let next = try (Some e#get_byte) with End_of_file -> None in
      match next with
      | None -> List.rev acc
      | Some p -> get_n_bytes (n-1) e (p :: acc)
  in
  (* since we need to include the output of the encryption in the 
     plaintext for the next block, this is unavoidably a unit-returning
     side-effecting operation (unless there's something about cryptokit I
     don't know, which seems relatively likely) *)
  ignore (List.map ~f:(encryptor#put_byte) block_to_encrypt);
  encryptor#flush;
  encryptor#finish;
  let ciphertext = get_n_bytes blocksize encryptor [] in
  (* keep appending the new ciphertext to the ciphertext so far *)
  ciphertext_so_far @ ciphertext

let aes_encrypt key file =
  Printf.printf "trying to encrypt %s with %s\n" file key;
  let blocksize = (String.length key) in
  In_channel.with_file file ~f:
    ( fun f ->
       let module C = Cryptokit.Cipher in
       let pt = String.concat ~sep:"" (In_channel.input_lines f) |>
                Hexstring.t_of_ascii |>
                Hexstring.int_list_of_t ~padding:blocksize |>
                List.groupi ~break:(fun n _ _ -> n mod blocksize = 0) in

       let iv = (List.init blocksize (fun _ -> 0)) in
       let ciphertext = List.fold_left pt ~init:iv 
           ~f:(fun a b -> xor_blocks key blocksize a b) in
       (* solution needs the initial 16-byte IV removed *)
       let ciphertext = (List.sub ~pos:blocksize 
                           ~len:((List.length ciphertext)-blocksize) 
                           ciphertext) |>
                        Base64.base64_of_int_list in
       match ciphertext with
       | `Ok ciphertext -> 
         Printf.printf "%s\n" ciphertext;
         `Ok ciphertext 
       | `Invalid_argument s -> `Error (false, s)
    )

let key =
  let doc = "16-byte key to use for encryption" in
  Arg.(required & pos ~rev:false 0 
         (some string) None & info [] ~docv:"FILE" ~doc)

let file =
  let doc = "File to read for ciphertext to encrypt" in
  Arg.(required & pos ~rev:false 1
         (some string) None & info [] ~docv:"FILE" ~doc)

let cmd =
  let doc = "AES-encrypt in CBC mode, then base64-encode, the message in file
  using the given key." in
  Term.(ret (pure aes_encrypt $ key $ file)),
  Term.info "encrypt-aes-cbc" ~version:"0.0.1" ~doc

let () = match Term.eval cmd with 
  | `Error _ -> exit 1
  | `Ok _ | `Version | `Help -> exit 0
