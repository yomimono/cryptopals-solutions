module Fixed_xor = struct

  let xor str1 str2 =
    let str1_pw = Bytestring.hexstring_to_int_list str1 in
    let str2_pw = Bytestring.hexstring_to_int_list str2 in
    let int_list_xord = List.map2 (lxor) str1_pw str2_pw in
    Bytestring.int_list_to_hexstring int_list_xord

end
