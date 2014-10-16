let xor str1 str2 =
  let str1_pw = Hexstring.int_list_of_t (Hexstring.t_of_ascii str1) in
  let str2_pw = Hexstring.int_list_of_t (Hexstring.t_of_ascii str2) in
  let int_list_xord = List.map2 (lxor) str1_pw str2_pw in
  Hexstring.ascii_of_t (Hexstring.t_of_int_list_exn int_list_xord)
