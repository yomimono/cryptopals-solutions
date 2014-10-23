open Core.Std
open Cmdliner

(* use an ECB encryptor to get CBC behavior by recombinating the 
          ciphertext with the next block's plaintext ourselves. *)
let xor_blocks 
    direction
    ( key : string ) 
    ( blocksize : int )
    ( text_so_far : int list ) 
    ( right_text : int list ) : ( int list ) =
  let module C = Cryptokit.Cipher in
  let cryptor = C.aes ~mode:C.ECB key direction in
  (* use the last 16 bytes of text_so_far as our obscuring value *)
  let left_text = List.sub text_so_far 
      ~pos:((List.length text_so_far) - 16) ~len:16 in
  let block_to_crypt = List.map2_exn ~f:(lxor) left_text
      right_text in
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
  ignore (List.map ~f:(cryptor#put_byte) block_to_crypt);
  cryptor#flush;
  cryptor#finish;
  let text = get_n_bytes blocksize cryptor [] in
  (* keep appending the new ciphertext to the ciphertext so far *)
  text_so_far @ text

let aes_crypt cryptor key file =
  let module C = Cryptokit.Cipher in
  let chooser cryptor blocksize s =
    match cryptor with
    | C.Encrypt ->
      `Ok (Hexstring.int_list_of_t ~padding:blocksize (Hexstring.t_of_ascii s))
    | C.Decrypt ->
      match (Base64.int_list_of_base64 s) with
      | `Ok s -> `Ok (Hexstring.pad (Some blocksize) s)
      | `Invalid_argument s -> `Invalid_argument s
  in
  let blocksize = (String.length key) in
  In_channel.with_file file ~f:
    ( fun f ->
       let pt = String.concat ~sep:"" (In_channel.input_lines f) |>
                chooser cryptor blocksize 
       in 
       match pt with 
       | `Invalid_argument s -> `Error (false, s)
       | `Ok pt ->
         let pt = List.groupi ~break:(fun n _ _ -> n mod blocksize = 0) pt in
         let iv = (List.init blocksize (fun _ -> 0)) in 
         let ciphertext = List.fold_left pt ~init:iv 
             ~f:(fun a b -> xor_blocks cryptor key blocksize a b) in
         (* solution needs the initial 16-byte IV removed *)
         let ciphertext = (List.sub ~pos:blocksize 
                             ~len:((List.length ciphertext)-blocksize) 
                             ciphertext) (* |>
                                            Base64.base64_of_int_list  in
                                            match ciphertext with
                                            | `Ok ciphertext -> 
                                            Printf.printf "%s\n" ciphertext;
                                            `Ok ciphertext 
                                            | `Invalid_argument s -> `Error (false, s) *)
         in
         Printf.printf "%s\n" (Hexstring.ascii_of_int_list ciphertext);
         `Ok ciphertext

    )

let mode = 
  let open Cryptokit.Cipher in
  let doc = "Mode to use - one of --encrypt or --decrypt" in
  let cryptitude = Arg.enum ["encrypt", Encrypt; "decrypt", Decrypt] in
  Arg.(required & pos ~rev:false 0 (some cryptitude) None & 
       info [] ~docv:"CRYPTOR" ~doc)

let key =
  let doc = "16-byte key to use for encryption" in
  Arg.(required & pos ~rev:false 1
         (some string) None & info [] ~docv:"KEY" ~doc)

let file =
  let doc = "File to read for ciphertext to encrypt" in
  Arg.(required & pos ~rev:false 2
         (some string) None & info [] ~docv:"FILE" ~doc)

let cmd =
  let doc = "AES-encrypt in CBC mode, then base64-encode, the message in file
  using the given key." in
  Term.(ret (pure aes_crypt $ mode $ key $ file)),
  Term.info "encrypt-aes-cbc" ~version:"0.0.1" ~doc

let () = match Term.eval cmd with 
  | `Error _ -> exit 1
  | `Ok _ | `Version | `Help -> exit 0
