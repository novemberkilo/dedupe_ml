open! Base
open Stdio

module D = Data.Lookup

let%expect_test "size" =
  let directory = "fixtures/mangled" in 
  let result = Dedupe.lookup_by_size directory in 
  let ok_result = 
    match result with
    | Error e -> "Boom!: "^e
    | Ok ok_result -> D.pretty_print(ok_result) in
  print_endline ok_result;
  [%expect {||}]
