module F = Format

exception Fatal_error of string

module List = struct
  include List

  let is_empty = function
    | [] -> true
    | _ -> false
end

(** Make path canonical.
    Stole this implementation from Python's posixpath.normpath
*)
let canonicalize path =
  let empty = "" in
  let dir_cur = "." in
  let dir_parent = ".." in
  let dir_sep = '/' in
  let is_relative = String.get path 0 <> dir_sep in
  let components = String.split_on_char dir_sep (String.trim path) in
  let new_components = ref [] in
  let rec loop = function
    | [] -> ()
    | comp :: tl ->
        if comp = dir_cur || comp = empty then ()
        else if
          comp <> dir_parent
          || (is_relative && List.is_empty !new_components)
          || (not (List.is_empty !new_components))
             && List.hd !new_components = dir_parent
        then new_components := comp :: !new_components
        else if not (List.is_empty !new_components) then
          new_components := List.tl !new_components ;
        loop tl
  in
  loop components ;
  let canonical_form =
    String.concat Filename.dir_sep (List.rev !new_components)
  in
  if not is_relative then "/" ^ canonical_form
  else if canonical_form = empty then dir_cur
  else canonical_form


let lazise f =
  let raw = lazy (f ()) in
  fun () -> Lazy.force raw


let path =
  lazise (fun () ->
      let raw_path =
        try Unix.getenv "PATH"
        with Not_found -> raise (Fatal_error "Impossible: PATH does not set")
      in
      String.split_on_char ':' raw_path )


let check_executable cmd =
  try
    let path = path () in
    List.find Sys.file_exists
      (List.map (fun dir -> canonicalize (Filename.concat dir cmd)) path)
  with Not_found ->
    raise (Fatal_error (F.sprintf "Fatal: executable '%s' is not in PATH." cmd))


let run ?(pipe = false) cmd args =
  let cmd = check_executable cmd in
  let cmd = Filename.quote_command cmd args in
  if not pipe then
    Unix.(
      match system cmd with
      | WEXITED 0 -> None
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

let os =
  lazise (fun () ->
      match Sys.os_type with
      | "Unix" -> (
        match run ~pipe:true "uname" ["-s"] with
        | Some "Linux" -> Linux
        | Some "Darwin" -> Darwin
        | Some other -> Other other
        | None -> Other "unknown" )
      | other -> Other other )
