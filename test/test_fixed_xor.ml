open OUnit2

let test_hexstring_to_int context =
  let test_input = "00010aff" in
  let actual_output = Fixed_xor.hexstring_to_int_list test_input in
  assert_equal [0;1;10;255] actual_output

let test_int_to_hexstring context =
  let test_input = [ 1; 10; 255; 16 ] in
  let actual_output = Fixed_xor.int_list_to_hexstring test_input in
  assert_equal "010aff10" (String.concat "" actual_output)

let test_bad_input test_input context =
  try  
    ignore (Fixed_xor.hexstring_to_int_list test_input); 
    assert_failure "Didn't raise exception for bad input"
  with
  Invalid_argument s -> assert_equal 1 1

let xors_strings =
  let str1 = "1c0111001f010100061a024b53535009181c" in
  let str2 = "686974207468652062756c6c277320657965" in
  let expected_output = "746865206b696420646f6e277420706c6179" in
  assert_equal expected_output (Fixed_xor.xor str1 str2)

let suite = 
  "suite" >:::
    [ 
      "hexstring-to-intlist" >:: test_hexstring_to_int;
      "intlist-to-hexstring" >:: test_int_to_hexstring;
      "bad-hexstring-input" >:: test_bad_input "hi mom!";
      "bad-hexstring-input" >:: test_bad_input "532"
    ]

let _ = 
  run_test_tt_main suite
