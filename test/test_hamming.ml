open OUnit2

let test_hamming c =
  let module H = Hexstring in
  assert_equal ~printer:(fun p -> Printf.sprintf "%d" p) 
    (Hamming.distance (H.int_list_of_t (H.t_of_ascii "this is a test"))
       (Hexstring.int_list_of_t (H.t_of_ascii "wokka wokka!!!"))) 37

let suite = 
  "hamming-distance" >::: [
    "test_hamming" >:: test_hamming
  ]

let () = 
  run_test_tt_main suite
