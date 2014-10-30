open OUnit2
open Core.Std

let key = Hexstring.int_list_of_t (Hexstring.t_of_ascii "YELLOW SUBMARINE")
let printer l = (String.concat ~sep:"" (List.map ~f:(Printf.sprintf "0x%02x ;") l))

let simple_encrypt c = 
  let module C = Cryptokit.Cipher in
  let block_0 = List.init 16 (fun _f -> 0x81) in
  let aes_block_0 = [ 0xf1 ; 0x26 ; 0xc2 ; 0x36 ; 0x22 ; 0x57 ; 0xab ; 0x38 ;
                       0x50 ; 0x78 ; 0x43 ; 0x65 ; 0x71 ; 0x85 ; 0x25 ; 0xaf ]
  in
  let block_1 = List.init 16 (fun _f -> 0x80) in
  (* the output from AES-encrypting (_aes_block_0 lxor block_1) with the key 
     "YELLOW SUBMARINE" *)
  let second_encrypted_block = [ 0xca ; 0x9f ; 0xde ; 0xfe ; 0x11 ; 0xb3 ; 0x08 
                               ; 0x17 ; 0x35 ; 0xe8 ; 0x62 ; 0xe5 ; 0x66 ; 0x28 
                               ; 0x4f ; 0xf2 ] in
  let expected_output = List.append aes_block_0 second_encrypted_block in
  assert_equal ~printer:printer expected_output
    (Aes_cbc.encrypt key (List.append block_0 block_1)) 

let simple_decrypt c =
  (* test against the expected output from simple_encrypt *)
  let input = [ 0xf1 ; 0x26 ; 0xc2 ; 0x36 ; 0x22 ; 0x57 ; 0xab ; 0x38 ;
                0x50 ; 0x78 ; 0x43 ; 0x65 ; 0x71 ; 0x85 ; 0x25 ; 0xaf ;
                0xca ; 0x9f ; 0xde ; 0xfe ; 0x11 ; 0xb3 ; 0x08 ; 0x17 ; 
                0x35 ; 0xe8 ; 0x62 ; 0xe5 ; 0x66 ; 0x28 ; 0x4f ; 0xf2 ] in
  let expected_output = List.append (List.init 16 (fun _f -> 0x81)) 
      (List.init 16 (fun _f -> 0x80)) in
  assert_equal ~printer:printer expected_output (Aes_cbc.decrypt key input)

let empty_calls c = 
  assert_equal ~printer:printer [] (Aes_cbc.encrypt key []);
  assert_equal ~printer:printer [] (Aes_cbc.decrypt key [])

let bad_input c = 
  (* invalid ints in key, data *)
  (* key is not a valid keylength *)
  ()

let suite = 
  "aes-cbc-implementation" >::: [
    "test-two-block-encrypt" >:: simple_encrypt ;
    "test-two-block-decrypt" >:: simple_decrypt ;
    "test-empty-calls" >:: empty_calls ;
    "test-bad-input" >:: bad_input
  ]

let () =
  run_test_tt_main suite
