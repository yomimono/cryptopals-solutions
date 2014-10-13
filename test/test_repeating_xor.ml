open OUnit

let given_answer _ = 
  let open Repeating_xor in
  let open Bytestring in
  let open Fixed_xor in
  let expected = 
    "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
  in
  let input_string = Bytestring.ascii_to_hexstring 
      "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
  in
  let key = Bytestring.ascii_to_hexstring "ICE" in
  let output_string = Repeating_xor.repeat_xor key input_string in
  assert_equal ~printer:(Fixed_xor.xor expected) output_string expected


let simple_test _ =
  let open Repeating_xor in
  assert_equal (Repeating_xor.repeat_xor "0100" "ffffffffff") "fefffefffe"

let suite = 
  "suite" >:::
  [ "simple repeating test" >:: simple_test ;
    "ice ice baby" >:: given_answer ]

let _ =
  run_test_tt_main suite
