open OUnit2

let test_blocks c =
  let test_input = [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16] in
  assert_equal (Break_rxor.blocks 5 test_input []) 
    [ [1;2;3;4;5] ; [6;7;8;9;10] ; [11;12;13;14;15] ; [16]];
  assert_equal (Break_rxor.blocks 4 test_input [])
    [ [1;2;3;4 ] ; [5;6;7;8] ; [9;10;11;12] ; [13;14;15;16 ]];
  assert_equal (Break_rxor.blocks 20 test_input []) [ test_input ]

let test_transpose_blocks c =
  let test_input = 
    [ [1;2;3;4;5] ; [6;7;8;9;10] ; [11;12;13;14;15] ; [16]];
  in
  let expected_output = 
    [ [ 1; 6; 11; 16] ; [2; 7; 12] ; [ 3; 8; 13 ]; [ 4; 9; 14 ] ; [ 5; 10; 15]]
  in
  assert_equal (Break_rxor.transpose_blocks test_input) expected_output

let test_guess_keys c =
  let test_input = [ [ 32; 32 ; 32 ; 32 ; 32 ]; [ 35; 35 ; 35 ; 35 ; 35 ] ] in
  assert_equal (Break_rxor.guess_keys test_input) [ 0; 3 ]

let suite = 
  "break-rxor-lib" >::: [
    "test-block-breaks" >:: test_blocks;
    "test-transpose-blocks" >:: test_transpose_blocks ;
    "test-guess-keys" >:: test_guess_keys
  ]

let () =
  run_test_tt_main suite
