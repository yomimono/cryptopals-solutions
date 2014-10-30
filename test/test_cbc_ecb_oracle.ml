open OUnit2

let generate_key context =
  let module O = Cbc_ecb_oracle in
  assert_equal 16 (List.length (O.random_key 16));
  assert_equal 0 (List.length (O.random_key 0));
  assert_equal 0 (List.length (O.random_key ((-1) * 20)));
  assert_equal false ((=) (O.random_key 16) (O.random_key 16))

let make_rando context =
  let module O = Cbc_ecb_oracle in
  assert_equal true ((List.length (O.random_encrypt 16
                       [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16])) > 26)

let try_oracle context =
  let open Core.Std in
  let module O = Cbc_ecb_oracle in
  let cbc_ciphertext = (Aes_cbc.encrypt (O.random_key 16) 
                          (List.init 256 (fun _f -> 0xff))) in
  let ecb_ciphertext = (Aes_ecb.encrypt (O.random_key 16) 
                          (List.init 256 (fun _f -> 0xff))) in
  assert_equal Cryptokit.Cipher.ECB (O.oracle ecb_ciphertext);
  assert_equal Cryptokit.Cipher.CBC (O.oracle cbc_ciphertext)

let suite = 
  "cbc-ecb-oracle" >::: [
    "random-key" >:: generate_key;
    "random-encrypt" >:: make_rando;
    "try_oracle" >:: try_oracle
  ]

let _ = 
  run_test_tt_main suite
