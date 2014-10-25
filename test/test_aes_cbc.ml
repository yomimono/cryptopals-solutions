open OUnit2
open Core.Std

let key = Hexstring.int_list_of_t (Hexstring.t_of_ascii "YELLOW SUBMARINE")

let simple_encrypt c = 
  let module C = Cryptokit.Cipher in
  let block_0 = List.init 16 (fun _f -> 81) in
  let _aes_block_0 = [ 67;179;154; 70; 44; 117;46 ;176
                     ;238;233;207; 84; 242;224;209;27 ] in
  let block_1 = List.init 16 (fun _f -> 128) in
  (* the output from AES-encrypting (_aes_block_0 lxor block_1) with the key 
     "YELLOW SUBMARINE" *)
  let second_encrypted_block = [ 038;  047;  006;  078;  105;  022;  055;  128;
                                 066;  164;  017;  211;  034;  168;  180;  013]
  in
  let expected_output = List.append block_1 second_encrypted_block in
  assert_equal expected_output (Aes_cbc.encrypt key (List.append block_0 block_1))


let simple_decrypt c =
  (* test against the expected output from simple_encrypt *)
  let input = [ 67;179;154; 70; 44; 117;46 ;176                          
              ;238;233;207; 84; 242;224;209;27 ; 038;  047;  006;
                078;  105;  022; 055;  128; 066; 164;  017;  211;
                034;  168;  180; 013] in
  let expected_output = List.append (List.init 16 (fun _f -> 81)) 
      (List.init 16 (fun _f -> 128)) in
  assert_equal expected_output (Aes_cbc.decrypt key input)

let empty_calls c = 
  assert_equal [] (Aes_cbc.encrypt key []);
  assert_equal [] (Aes_cbc.decrypt key [])

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
