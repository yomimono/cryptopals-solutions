open Cmdliner

let aes_decrypt key file =
  let open Core.Std in
  Printf.printf "trying to decrypt %s with %s\n" file key;
  In_channel.with_file file ~f:
    ( fun f ->
       let base64_enc = (String.concat ~sep:"" (In_channel.input_lines f)) in
       match Base64.int_list_of_base64 base64_enc with
       | `Invalid_argument s -> `Error (false, s)
       | `Ok d -> (* base64 was valid, yay! *)
         let plaintext = Aes_ecb.decrypt (Hexstring.int_list_of_t
                                            (Hexstring.t_of_ascii key)) d in
         Printf.printf "%s\n" (Hexstring.ascii_of_t (Hexstring.t_of_int_list_exn
                                                       plaintext));
         `Ok plaintext
    )

let key =
  let doc = "16-byte key to use for decryption" in
  Arg.(required & pos ~rev:false 0 
         (some string) None & info [] ~docv:"FILE" ~doc)

let file =
  let doc = "File to read for ciphertext to decrypt" in
  Arg.(required & pos ~rev:false 1
         (some string) None & info [] ~docv:"FILE" ~doc)

let cmd =
  let doc = "Base64-decode, then AES-decrypt in EBC mode, the message in file
  using the given key." in
  Term.(ret (pure aes_decrypt $ key $ file)),
  Term.info "break_xor" ~version:"0.0.1" ~doc

let () = match Term.eval cmd with 
  | `Error _ -> exit 1
  | `Ok _ | `Version | `Help -> exit 0
