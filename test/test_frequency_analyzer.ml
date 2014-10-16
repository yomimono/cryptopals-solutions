open OUnit2
open Frequency_analyzer

let counts_accurate _ = 
  let simple_str = "AAAAB" in
  let simple_map = Frequency_analyzer.get_count (Hexstring.t_of_ascii
                                                   simple_str) in
  assert_equal (Hashtbl.find simple_map (int_of_char 'A')) 4;
  assert_equal (Hashtbl.find simple_map (int_of_char 'B')) 1;
  assert_equal (Hashtbl.mem simple_map 0) false

let most_common_accurate _ =
  let freq_str = "ABCDEAAAAAAAA" in
  let winner = Frequency_analyzer.most_common (Hexstring.t_of_ascii freq_str) in
  assert_equal winner (Some (int_of_char 'A'))

let empty_strings _ =
  let empty_hex = Hexstring.t_of_ascii "" in
  assert_equal (Hashtbl.length (Frequency_analyzer.get_count empty_hex)) 0;
  assert_equal (Frequency_analyzer.most_common empty_hex) None;
  assert_equal (Frequency_analyzer.try_decode empty_hex) empty_hex
(*
let decodes_string _ =
  let open Hexstring in
  (* key is 0x28 *)
  let test_string = 
    "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
  in
  let decoded = Frequency_analyzer.try_decode test_string in
  assert_equal (Hexstring.ascii_of_hexstring decoded) "Cooking MC's like a pound of bacon"
*)
let suite =
  "frequency-analyzer-suite" >:::
  [
    "simple-counts-accurate" >:: counts_accurate ;
    "most-common-accurate" >:: most_common_accurate ;
    "empty-strings" >:: empty_strings (*;
    "decodes_string" >:: decodes_string *)
  ]

let _ =
  run_test_tt_main suite
