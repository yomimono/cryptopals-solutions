open OUnit2
open Frequency_analyzer

let counts_accurate _ = 
  let simple_str = "1a1a1a1a1b" in
  let simple_map = Frequency_analyzer.get_count simple_str in
  assert_equal (Hashtbl.find simple_map 26) 4;
  assert_equal (Hashtbl.find simple_map 27) 1;
  assert_equal (Hashtbl.mem simple_map 0) false

let most_common_accurate _ =
  let freq_str = "01020304050101010102" in
  let winner = Frequency_analyzer.most_common freq_str in
  assert_equal winner (Some 1)

let empty_strings _ =
  assert_equal (Hashtbl.length (Frequency_analyzer.get_count "")) 0;
  assert_equal (Frequency_analyzer.most_common "") None;
  assert_equal (Frequency_analyzer.try_decode "") ""

let suite =
  "suite" >:::
  [
    "simple-counts-accurate" >:: counts_accurate ;
    "most-common-accurate" >:: most_common_accurate ;
    "empty-strings" >:: empty_strings
  ]

let _ =
  run_test_tt_main suite
