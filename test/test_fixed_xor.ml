open OUnit2
open Fixed_xor

let xors_strings c =
(*  let str1 = "1c0111001f010100061a024b53535009181c" in
  let str2 = "686974207468652062756c6c277320657965" in 
    let expected_output = "746865206b696420646f6e277420706c6179" in *)
  let str1 = "aaaaaa" in (* decimal 97, hex 61 *)
  let str2 = "AAAAAA" in (* decimal 66, hex 41 *)
  let expected_output = "      " in  (* hex 32 *)
  assert_equal ~printer:(fun p -> p) expected_output (Fixed_xor.xor str1 str2)

let empty_strings c =
  assert_equal "" (Fixed_xor.xor "" "")

let suite = 
  "suite" >:::
    [ 
      "xors_strings" >:: xors_strings
    ]

let _ = 
  run_test_tt_main suite
