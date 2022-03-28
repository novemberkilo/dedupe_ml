module D = Data.Lookup

(* collect files by size : dir -> Lookup(int) *)
(* detect dupes : string list list -> string list list *)

(* let lookup_by_size : string -> (string list list, e Error) Result = *)
let rec lookup_by_size' directory size_map =
  let contents = Array.to_list(Sys.readdir directory) in 
  let go size_map_accumulator file =
    if Sys.is_directory file
    then 
      let size_map' = lookup_by_size' file D.empty_int_keys in 
      let merge_fn ~key:_ opts =
        match opts with
        | `Left v -> Some v 
        | `Right v -> Some v 
        | `Both (v, v') -> Some(List.append v v') 
      in 
      Base.Map.merge size_map_accumulator size_map' ~f:merge_fn
    else
      let stats = Unix.stat file in 
      let size = stats.st_size in 
      D.insert size_map_accumulator size file 
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



