module D = Data.Lookup

(* collect files by size : dir -> Lookup(int) *)
(* detect dupes : string list list -> string list list *)

(* let lookup_by_size : string -> (string list list, e Error) Result = *)
let rec lookup_by_size' directory size_map =
  let contents = Array.to_list(Sys.readdir directory) in 
  let go size_map_accumulator file =
    if Sys.is_directory (directory^"/"^file)
    then 
      let size_map' = lookup_by_size' (directory^"/"^file) D.empty_int_keys in 
      let merge_fn ~key:_ opts =
        match opts with
        | `Left v -> Some v 
        | `Right v -> Some v 
        | `Both (v, v') -> Some(List.append v v') 
      in 
      Base.Map.merge size_map_accumulator size_map' ~f:merge_fn
    else
      let stats = Unix.stat (directory^"/"^file) in 
      let size = stats.st_size in 
      D.insert size_map_accumulator size (directory^"/"^file) 
  in
  Base.List.fold contents ~init:size_map ~f:go  

let lookup_by_size directory =
  let size_map = D.empty_int_keys in
  if not (Sys.is_directory directory)
    then
      Result.error("Directory "^directory^" does not exist.")
    else
      let m = lookup_by_size' directory size_map in
      let filtered = Base.Map.filter m ~f:(fun v -> List.length(v) > 1) in 
      Result.ok(filtered)

let lookup_by_hash duplicates_by_size = 
  let files_grouped_by_size = List.map snd (Base.Map.to_alist duplicates_by_size) in 
  let go accumulator files =
    let empty_md5_map = D.empty_string_keys in
    let md5_map = 
      Base.List.fold 
        files 
        ~init:empty_md5_map 
        ~f:(fun md5_map_accum file -> 
          D.insert md5_map_accum (Digest.file file) file)
    in
    let filtered_map = Base.Map.filter md5_map ~f:(fun v -> List.length(v) > 1) 
    in 
    let grouped_by_md5 = List.map snd (Base.Map.to_alist filtered_map) in
    List.append accumulator grouped_by_md5
  in
  Base.List.fold files_grouped_by_size ~init:[] ~f:go

let run directory =
  let duplicates_by_size = 
    Result.map_error print_endline (lookup_by_size directory) in 
  let duplicates_by_hash = 
    lookup_by_hash (Result.get_ok duplicates_by_size) 
  in 
  let () = print_endline "" in
  Base.List.iter duplicates_by_hash ~f:(fun l -> 
    let () = Base.List.iter l ~f:print_endline in
    print_endline "") 
