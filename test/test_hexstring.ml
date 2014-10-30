open OUnit2
open Hexstring

let test_hexstring_to_int context =
  let test_input = "AB CD" in
  let actual_output = Hexstring.int_list_of_t (Hexstring.t_of_ascii test_input) in
  assert_equal ~printer:(fun l -> (String.concat "" (List.map (Printf.sprintf
                                                                "%d ") l) ))
    [65;66;32;67;68] actual_output

let test_int_to_hexstring context =
  let test_input = [99; 48; 102; 102; 101; 101] in
  let actual_output = Hexstring.t_of_int_list_exn test_input in
  assert_equal "c0ffee" (Hexstring.ascii_of_t actual_output)

let test_hexstring_of_ascii context =
  let test_input = 
  " A\nAA" in
  let actual_output = Hexstring.t_of_ascii test_input in
  assert_equal ~printer:(fun p -> p) " A\nAA" (Hexstring.ascii_of_t actual_output)

let test_pkcs7_padding context =
  let test_input = (Hexstring.t_of_ascii "c0ffee") in
  let expected_output = [99;48;102;102;101;101;2;2] in
  assert_equal expected_output (Hexstring.int_list_of_t ~padding:4 test_input);
  assert_equal 
    ~printer:(fun l -> String.concat "" (List.map (Printf.sprintf "%02x") l))
    [99;48;102;102;101;101] (Hexstring.int_list_of_t ~padding:6 test_input)

let suite = 
  "hexstring-suite" >:::
    [ 
      "hexstring-to-intlist" >:: test_hexstring_to_int;
      "intlist-to-hexstring" >:: test_int_to_hexstring;
      "ascii-to-hexstring" >:: test_hexstring_of_ascii;
      "pkcs-padding" >:: test_pkcs7_padding
    ]

let _ =
  run_test_tt_main suite
