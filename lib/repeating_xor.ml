let rec repeat_xor key plaintext =
  let (keyl, ptl) = (String.length key, String.length plaintext) in
  match compare keyl ptl with
  | 0 -> Fixed_xor.xor key plaintext
  | 1 -> (* key longer than plaintext *)
    Fixed_xor.xor (String.sub key 0 ptl) plaintext
  | _ -> (* plaintext longer than key *)
    let ciphertext = Fixed_xor.xor key (String.sub plaintext 0 keyl) in
    Printf.sprintf "%s%s" ciphertext (repeat_xor key (String.sub plaintext keyl
                                                        (ptl - keyl)))

let rec repeat_xor_stream key in_stream out_channel =
  let rec next_n s l n =
    if n = 0 then List.rev l
    else
      try (next_n s (Stream.next s :: l) (n - 1)) with
      | Stream.Failure -> List.rev l
  in
  let batchlength = (String.length key) * 5 in
  let next_slice = next_n in_stream [] batchlength in
  let slicelength = List.length next_slice in
  match slicelength with
  | 0 -> ()
  | _ -> (* make a string (TODO: currently inefficiently) *)
    let batch = String.init slicelength (fun i -> List.nth next_slice i) in 
    let cryptotext = repeat_xor key batch in
    Printf.fprintf out_channel "%s" cryptotext ;
    repeat_xor_stream key in_stream out_channel

let rxor_exn key file =
  repeat_xor_stream key (Stream.of_channel (open_in file)) stdout

let rec repeat_xor_int_list key plaintext =
  let open Core.Std in
  let (keyl, ptl) = (List.length key, List.length plaintext) in
  match compare keyl ptl with
  | 0 -> Fixed_xor.xor_int_list key plaintext
  | 1 -> (* key longer than plaintext *)
    Fixed_xor.xor_int_list (List.slice key 0 ptl) plaintext
  | _ -> (* plaintext longer than key *)
    let this, next = List.split_n plaintext keyl in
    (Fixed_xor.xor_int_list key this) @ 
    (repeat_xor_int_list key next)

