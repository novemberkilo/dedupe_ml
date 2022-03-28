open Cmdliner

(* dedupe DIR *)

let dedupe dir = print_endline(String.concat " " ["Dedupe"; dir])

let dir =
  let env =
    let doc = "Overrides the default directory to search in." in 
    Cmd.Env.info "DIR" ~doc
  in 
  let doc = "The directory to search in." in 
  Arg.(value & pos 0 string "." & info [] ~env ~docv:"DIR" ~doc)

let dedupe_t = Term.(const dedupe $ dir)

let cmd =
  let doc = "find duplicate files in a directory and its subdirectories" in 
  let info = Cmd.info "dedupe" ~version:"%%VERSION%%" ~doc in 
  Cmd.v info dedupe_t

let main () = exit (Cmd.eval cmd)
let () = main ()
