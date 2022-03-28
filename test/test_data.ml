open! Base
open Stdio

module D = Data.Lookup

let%expect_test "trivial" =
  print_endline "Hello World!";
  [%expect {| Hello World! |}]

let%expect_test "empty" =
  let pre = D.empty_int_keys in
  printf "%d" (List.length(Map.to_alist(pre)));
  [%expect {| 0 |}]

let%expect_test "pretty_print_int_keys" =
  let pre = D.empty_int_keys in 
  let post = D.insert pre 42 "foo" in
  let post' = D.insert post 42 "bar" in
  let post'' = D.insert post' 24 "baz" in 
  print_endline(D.pretty_print post'');
  [%expect {|(24, [baz]), (42, [foo, bar])|}]
