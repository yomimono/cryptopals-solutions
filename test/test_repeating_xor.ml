open OUnit

  (*
let given_answer _ = 
  let open Repeating_xor in
  let open Hexstring in
  let open Fixed_xor in
  let expected = 
    "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
  in
  let input_string = 
      "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
  in
  let key = "ICE" in 
  let output_string = Repeating_xor.repeat_xor key input_string in
  assert_equal ~printer:(Fixed_xor.xor expected) output_string expected
*)

let simple_test _ =
  let input_string = "AAAAAAA" in
  let key = "123" in
  let expected = "psrpsrp" in
  assert_equal ~printer:(fun p->p) (Repeating_xor.repeat_xor key input_string)
    expected

let int_list_test () =
  let open Core.Std in
  let input_list = [ 1; 2; 3; 4; 5] in
  let key = [ 32; 16 ] in
  let expected = [ 33 ; 18 ; 35 ; 20 ; 37 ] in
  let printer p =
    List.to_string ~f:(Printf.sprintf "%003d") p 
  in
  assert_equal ~printer:printer 
    (Repeating_xor.repeat_xor_int_list key input_list) expected

let suite = 
  "repeating-xor-suite" >:::
  [ "simple repeating test" >:: simple_test ;
    "repeating xor on int lists" >:: int_list_test
  (*;  "ice ice baby" >:: given_answer *)
  ]


let _ =
  run_test_tt_main suite
