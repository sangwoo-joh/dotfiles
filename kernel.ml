module F = Format

exception Fatal_error of string

let path =
  let path =
    lazy
      (let raw_path =
         try Unix.getenv "PATH"
         with Not_found -> raise (Fatal_error "Impossible: PATH does not set")
       in
       String.split_on_char ':' raw_path )
  in
  fun () -> Lazy.force path


let run ?(pipe = false) cmd args =
  let path = path () in
  let cmd = List.find Sys.file_exists (List.map (fun dir -> Filename.concat dir cmd) path) in
  let cmd = Filename.quote_command cmd args in
  if not pipe then
    Unix.(
      match system cmd with
      | WEXITED 0 ->
          None
      | _ ->
          raise (Fatal_error (F.sprintf "Fatal: failed to run command '%s'" cmd)))
  else
    (* return Piped result*)
    let ic = Unix.open_process_in cmd in
    let buf = Buffer.create 80 in
    let rec reader () =
      Buffer.add_channel buf ic 80 ;
      reader ()
    in
    (try reader () with End_of_file -> ()) ;
    Some (String.trim (Buffer.contents buf))


let run_single ?(pipe = false) cmd = run ~pipe cmd []

type os = Linux | Darwin | Other of string
