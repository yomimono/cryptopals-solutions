open Core.Std
open Cmdliner

let aes_crypt cryptor key file =
  let module C = Cryptokit.Cipher in
  In_channel.with_file file ~f:
    ( fun f ->
       let text = String.concat ~sep:"" (In_channel.input_lines f) 
       in 
       let int_list_ify t = Hexstring.int_list_of_t (Hexstring.t_of_ascii t) in
       let result = 
         match cryptor with
       | C.Encrypt ->
         `Ok (Aes_cbc.encrypt (int_list_ify key) (int_list_ify text))
       | C.Decrypt -> 
         match (Base64.int_list_of_base64 text) with
         | `Ok ciphertext -> `Ok (Aes_cbc.decrypt (int_list_ify key) ciphertext)
         | `Invalid_argument s -> `Error (false, s)
       in
       match result with
       | `Ok s -> Printf.printf "%s\n" (Hexstring.ascii_of_int_list s);
         `Ok s
       | `Error s -> `Error s

    )

let mode = 
  let open Cryptokit.Cipher in
  let doc = "Mode to use - one of encrypt or decrypt" in
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
