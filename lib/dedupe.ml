module D = Data.Lookup
open Base
open Stdio

let rec lookup_by_size' directory size_map =
  let contents = Array.to_list (Caml.Sys.readdir directory) in
  let go size_map_accumulator file =
    if Caml.Sys.is_directory (directory ^ "/" ^ file) then
      let size_map' =
        lookup_by_size' (directory ^ "/" ^ file) D.empty_int_keys
      in
      let merge_fn ~key:_ opts =
        match opts with
        | `Left v -> Some v
        | `Right v -> Some v
        | `Both (v, v') -> Some (List.append v v')
      in
      Map.merge size_map_accumulator size_map' ~f:merge_fn
    else
      let stats = Unix.stat (directory ^ "/" ^ file) in
      let size = stats.st_size in
      D.insert size_map_accumulator size (directory ^ "/" ^ file)
  in
  List.fold contents ~init:size_map ~f:go

let lookup_by_size directory =
  let size_map = D.empty_int_keys in
  if not (Caml.Sys.is_directory directory) then
    Caml.Result.error ("Directory " ^ directory ^ " does not exist.")
  else
    let m = lookup_by_size' directory size_map in
    let filtered = Base.Map.filter m ~f:(fun v -> List.length v > 1) in
    Caml.Result.ok filtered

let lookup_by_hash duplicates_by_size =
  let files_grouped_by_size =
    List.map ~f:snd (Map.to_alist duplicates_by_size)
  in
  let go accumulator files =
    let empty_md5_map = D.empty_string_keys in
    let md5_map =
      List.fold files ~init:empty_md5_map ~f:(fun md5_map_accum file ->
          D.insert md5_map_accum (Caml.Digest.file file) file)
    in
    let filtered_map = Map.filter md5_map ~f:(fun v -> List.length v > 1) in
    let grouped_by_md5 = List.map ~f:snd (Map.to_alist filtered_map) in
    List.append accumulator grouped_by_md5
  in
  Base.List.fold files_grouped_by_size ~init:[] ~f:go

let run directory =
  let duplicates_by_size =
    Caml.Result.map_error print_endline (lookup_by_size directory)
  in
  let duplicates_by_hash =
    lookup_by_hash (Caml.Result.get_ok duplicates_by_size)
  in
  let () = print_endline "" in
  Base.List.iter duplicates_by_hash ~f:(fun l ->
      let () = Base.List.iter l ~f:print_endline in
      print_endline "")
