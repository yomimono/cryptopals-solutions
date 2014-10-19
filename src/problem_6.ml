open Core.Std
open Cmdliner

let break_xor file =
  let open Break_rxor in
  In_channel.with_file file ~f:
    (fun f ->
       let str = (String.concat ~sep:"" (In_channel.input_lines f)) in
       match Base64.int_list_of_base64 str with
       | `Invalid_argument s -> `Error (false, s)
       | `Ok d -> Printf.printf "Decoded base64\n";
         let distances = try_keysizes d 2 40 in
         (* comparator for sorting (hamming distance, keysize) tuples by 
            distance, then keysize,
            shuttling invalid_argument results to the end of the list *)
         let sd = List.sort distances ~cmp:comparator in
         (* TODO: remove debug output *)
         ignore (List.map ~f:(Printf.printf "%s") (List.map ~f:printer sd));
         match (List.hd sd) with 
         | Some `Ok best -> 
           let tx_blocks = (transpose_blocks (blocks (snd best) d [])) in
           let guesses = (guess_keys tx_blocks) in
           Printf.printf "Recovered key: %s\n" (List.to_string ~f:(Printf.sprintf
                                                                     "%02x") guesses);
           Printf.printf "(ASCII: %s)\n" (List.to_string ~f:(Printf.sprintf
                                   "%c") (List.map ~f:char_of_int guesses));
           let attempted_decrypt = Repeating_xor.repeat_xor_int_list guesses d
               in
           let readable_attempted_decrypt = Hexstring.ascii_of_int_list
               attempted_decrypt in
               Printf.printf "%s\n" readable_attempted_decrypt;
           `Ok attempted_decrypt
         | None -> `Error (false, "No best hamming distance candidate somehow")
         | Some `Invalid_argument s -> 
           `Error (false, "No keysize yielded meaningful results -
           pathologically small ciphertext size?")
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
