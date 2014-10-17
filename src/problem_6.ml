open Core.Std
open Cmdliner

let average_hamming_by_keysize ciphertext (size : int) =
  if (List.length ciphertext) < (size * 2) then 
    `Invalid_argument "key size exceeds available text"
  else
    let (one, two) = List.split_n ciphertext size in
    let distance = (Hamming.distance one (fst (List.split_n two size))) in
    let normalized = (Int.to_float distance) /. (Int.to_float size) in
    `Ok (normalized, size)

let try_keysizes ciphertext =
  let sizes = List.range ~stride:1 ~start:`inclusive ~stop:`inclusive 2 40 in
  List.map ~f:(average_hamming_by_keysize ciphertext) sizes

let break_xor file =
  In_channel.with_file file ~f:
    (fun f ->
       let str = (String.concat ~sep:"" (In_channel.input_lines f)) in
       match Base64.int_list_of_base64 str with
       | `Invalid_argument s -> `Error (false, s)
       | `Ok d -> Printf.printf "Decoded base64\n";
         let distances = try_keysizes d in
         (* comparator for sorting (hamming distance, keysize) tuples by 
            distance, then keysize,
            shuttling invalid_argument results to the end of the list *)
         let comparator one two =
           match (one, two) with 
           (* Invalid_argument is larger than any `Ok value*)
           | `Ok (p, a), `Invalid_argument _ -> -1
           | `Invalid_argument _ , `Ok (p, a) -> 1
           | `Invalid_argument _, `Invalid_argument _ -> 1
           | `Ok (p, a), `Ok (q, b) -> 
             match Float.compare p q with
             | 0 -> Int.compare a b (* prefer smaller key sizes *)
             | n -> n (* sort by hamming distance *)
         in
         let sd = List.sort distances ~cmp:comparator in
         match (List.hd sd) with 
         | Some `Ok best ->
           Printf.printf "Best keysize: %d (%f)\n" (snd best) (fst best);
           `Ok 0
         | None -> 
           `Error (false, "No best hamming distance candidate somehow")
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
