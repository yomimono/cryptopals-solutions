open Core.Std
open Cmdliner

let average_hamming_by_keysize ciphertext size =
  

let break_xor file =
  In_channel.with_file file ~f:
    (fun f ->
       let str = (String.concat ~sep:"" (In_channel.input_lines f)) in
       let decoded = Base64.hex_of_base64 str in
       match decoded with
       | `Ok d -> Printf.printf "%s" d; `Ok 0
       | `Invalid_argument s -> `Error (false, s)
    )

let file =
  let doc = "File to read for ciphertext to decrypt" in
  Arg.(required & pos ~rev:false 0
         (some string) None & info [] ~docv:"FILE" ~doc)

let cmd =
  let doc = "Attempt to extract a repeating-XOR key by edit distance and
  frequency analysis" in
  Term.(ret (pure break_xor $ file)),
  Term.info "break_xor" ~version:"0.0.1" ~doc

let () = match Term.eval cmd with 
  | `Error _ -> exit 1 
  | _ -> exit 0
