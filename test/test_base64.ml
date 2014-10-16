open OUnit2
open Base64

let test_encoding context = 
  let test_input =
    "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
  in
  let test_output = 
    "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
  in
  let actual_output = (Base64.base64_of_hex test_input) in
  match actual_output with
  | `Ok s -> assert_equal test_output s
  | `Invalid_argument s -> assert_failure s

let test_pad_encoding context = 
  let test_input = 
    "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d00"
  in
  let test_output =
    "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29tAA=="
  in
  let actual_output = (Base64.base64_of_hex test_input) in
  match actual_output with
  | `Ok s -> assert_equal ~printer:(fun p -> Printf.sprintf "%s" p) test_output s
  | `Invalid_argument s -> assert_failure s

let test_uneven_bytes context =
  let test_input = "49276" in
  let actual_output = (Base64.base64_of_hex test_input) in
  match actual_output with
  | `Invalid_argument _ -> ()
  | _ -> assert_failure "Failure to fail on uneven number of bytes in hex string input"

let test_empty_string_encode context =
  let test_input = "" in
  let actual_output = (Base64.base64_of_hex test_input) in
  match actual_output with
  | `Ok s -> assert_equal "" s
  | _ -> assert_failure "Incorrect answer for empty string encode"

let test_small_string_encode context =
  let test_input = "000000" in
  let actual_output = (Base64.base64_of_hex test_input) in
  match actual_output with
  | `Ok s -> assert_equal "AAAA" s
  | _ -> assert_failure "Parse failure for simple string"

let test_recursive_encode context =
  let test_input = "000000010101010101" in
  let actual_output = (Base64.base64_of_hex test_input) in
  match actual_output with
  | `Ok s -> assert_equal "AAAAAQEBAQEB" s
  | _ -> assert_failure "Parse failure for simple string"

let test_decoding context =
  let a_output = (Base64.hex_of_base64 "AAAA") in
  let slash_output = (Base64.hex_of_base64 "////") in
  match a_output, slash_output with
  | `Ok p, `Ok q -> assert_equal ~printer:(fun p-> p) "000000" p;
    assert_equal ~printer:(fun p->p) "ffffff" q
  | _ , _ -> assert_failure "Parse failure for simple string"

let test_decode_padding context =
  let test_input = 
    "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29tAA=="
  in
  let expected_output =
    "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d00"
  in
  let actual_output = (Base64.hex_of_base64 test_input) in
  match actual_output with
  | `Ok s -> assert_equal ~printer:(fun p-> p) expected_output s
  | _ -> assert_failure "Parse failure for simple string"

let test_decode_invalid_char context = 
  let test_input = "\r\n\r\n\r\n" in
  match (Base64.hex_of_base64 test_input) with
  | `Ok _ -> assert_failure "Didn't fail on bogus string"
  | `Invalid_argument _ -> assert_equal 1 1

let suite = 
  "suite">:::
  [
   "small-string-no-pad-encode" >:: test_small_string_encode;
   "longer-string-no-pad-encode" >:: test_recursive_encode;
   "nopad-encoding" >:: test_encoding;
   "pad-encoding" >:: test_pad_encoding;
   "uneven-bytes" >:: test_uneven_bytes;
   "empty-string" >:: test_empty_string_encode;
   "test-decoding" >:: test_decoding;
   "test-decode-padding" >:: test_decode_padding;
   "test-decode-bad-input" >:: test_decode_invalid_char
  ]

let _ = 
    run_test_tt_main suite

