open Core.Std 
open Cmdliner

let find_aes_ebc file =
  let rec substrings str n acc =
    match (String.length str) with
    | 0 -> List.rev acc
    | i when i < n -> List.rev (str :: acc)
    | i -> substrings (String.slice str n (String.length str)) n 
             ((String.slice str 0 n) :: acc)
  in
  let count_occurrences (l: string list) : ((int * string) list )= 
    let grouped = List.group l ~break:(fun a b -> (String.compare a b) <> 0) in
    let counts = List.map ~f:List.length grouped in
    let heads = List.map ~f:List.hd_exn grouped in
    List.zip_exn counts heads
  in
  let print_counts (l : (int * string) list) = 
    String.concat ~sep:" "
      (List.map ~f:(fun (a, b) -> Printf.sprintf "(%d, %s) " a b) l)
  in
  let repeats (l: (int * string) list) =
    List.filter ~f:(fun (a, b) -> match a with | 1 -> false | _ -> true) l
  in
  In_channel.with_file file ~f:
    ( fun f ->
       let strs = List.map ~f:(fun f -> substrings f 16 []) 
           (In_channel.input_lines f) in
       let sorted = List.map ~f:(List.sort ~cmp:String.compare) strs in
       let counts = List.map ~f:count_occurrences sorted in
       let interesting = List.map ~f:repeats counts in
       (* Printf.printf "%s\n" (List.to_string ~f:print_counts counts); *)
       (* Printf.printf "%s\n" (List.to_string ~f:print_counts interesting) *)
       `Ok interesting
    )

let file =
  let doc = "File to read for ciphertext to scan for AES-EBC block" in
  Arg.(required & pos ~rev:false 0
         (some string) None & info [] ~docv:"FILE" ~doc)

let cmd =
  let doc = "Attempt to figure out which string in a file of hexstrings is
  likely to be an AES-EBC-encrypted ciphertext" in
  Term.(ret (pure find_aes_ebc $ file)),
  Term.info "detect-aes-ebc" ~version:"0.0.1" ~doc

let () = match Term.eval cmd with 
  | `Error _ -> exit 1 
  | `Ok interesting -> 
    let rec printer l = 
      match l with
      | [] -> ""
      | (n, m) :: moar_list -> 
        match n with
        | 1 -> printer moar_list
        | q -> (Printf.sprintf "%d copies of %s\n" n m) ^ printer moar_list
    in
    let print_wrapper index item =
      match (printer item) with
      | "" -> ""
      | s -> Printf.sprintf "Interesting items in string number %d: %s\n" index
               s;
    in
    Printf.printf "%s" (String.concat ~sep:"" (List.mapi interesting
                                                 ~f:print_wrapper))
  ; exit 0
  | _ -> exit 0
