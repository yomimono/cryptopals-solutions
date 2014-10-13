open OUnit2
open Bytestring

let test_hexstring_to_int context =
  let test_input = "00010aff" in
  let actual_output = Bytestring.hexstring_to_int_list test_input in
  assert_equal [0;1;10;255] actual_output

let test_int_to_hexstring context =
  let test_input = [ 1; 10; 255; 16 ] in
  let actual_output = Bytestring.int_list_to_hexstring test_input in
  assert_equal "010aff10" actual_output

let test_bad_input test_input context =
  try  
    ignore (Bytestring.hexstring_to_int_list test_input); 
    assert_failure "Didn't raise exception for bad input"
  with
    Invalid_argument s -> assert_equal 1 1

let test_ascii_to_hexstring _ =
  let test_input = 
  " A\nAA" in
  let actual_output = Bytestring.ascii_to_hexstring test_input in
  assert_equal "20410a4141" actual_output

let suite = 
  "suite" >:::
    [ 
      "hexstring-to-intlist" >:: test_hexstring_to_int;
      "intlist-to-hexstring" >:: test_int_to_hexstring;
      "ascii-to-hexstring" >:: test_ascii_to_hexstring;
      "bad-hexstring-input" >:: test_bad_input "hi mom!";
      "bad-hexstring-input" >:: test_bad_input "532"
    ]

let _ =
  run_test_tt_main suite
