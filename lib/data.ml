module Lookup = struct
  open Base

  let empty_int_keys = Map.empty (module Int)
  let empty_string_keys = Map.empty (module String)

  (* update map with a pair (size, file_path) *)
  (* let insert : t -> a' -> string -> t *)
  let insert pre key path =
    match Map.find pre key with
    | None -> Map.set pre ~key:key ~data:[path]
    | Some paths -> Map.set pre ~key:key ~data:(path :: paths)

  let list_of_strings_to_string : string list -> string = fun ls ->
    let f : string -> string -> string = fun a b ->
      if String.length(a) = 0 then b else a^", "^b in
    let ls' = List.rev ls in
    List.fold ls' ~init:"" ~f:f

  (* for expect tests *)
  let pretty_print pre =
    let f acc (i, ls) = 
      let s = "("^(Int.to_string i)^", ["^(list_of_strings_to_string ls)^"])" in
      if String.length(acc) = 0 then s else acc^", "^s in 
    List.fold (Map.to_alist pre) ~init:"" ~f:f
end
