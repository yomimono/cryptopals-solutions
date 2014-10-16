open Cmdliner

let xor_stream key filename = 
  Repeating_xor.repeat_xor_stream key 
    (Stream.of_channel (open_in filename)) stdout;
  `Ok ""

let key =
  let doc = "ASCII-encoded key for repeating XOR" in
  Arg.(required & pos ~rev:false 0 (some string) None & 
       info [] ~docv:"KEY" ~doc)

let file =
  let doc = "File to read for plaintext to encrypt" in
  Arg.(required & pos ~rev:false 1
         (some string) None & info [] ~docv:"FILE" ~doc)

let cmd =
  let doc = "repeating-XOR encrypt a file with a key" in
  Term.(ret (pure xor_stream $ key $ file)),
  Term.info "xror" ~version:"0.0.1" ~doc

let () = match Term.eval cmd with 
  | `Error _ -> exit 1 
  | _ -> exit 0
