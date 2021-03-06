open Core.Std

let find_strings file =
  let open Frequency_analyzer in
  In_channel.with_file file ~f:(fun f ->
      let encoded_strings = List.map ~f:Hexstring.t_of_ascii 
                                        (In_channel.input_lines f ) in
      let decoded_strings = List.map ~f:Frequency_analyzer.try_decode encoded_strings in 
      let ascii_strings = List.map ~f:Hexstring.ascii_of_t decoded_strings in
      ignore (List.map ~f:(Printf.printf "%s\n") ascii_strings);
      ()
  )

let spec = 
  let open Command.Spec in
  empty
  +> anon ("filename" %: file)

let command =
  Command.basic
    ~summary:"Scan a list of newline-separated strings for XOR'd English test"
    spec
    (fun filename () -> find_strings filename)

let () =
  Command.run command
