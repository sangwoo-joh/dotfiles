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

(** only support two os..
    also, no support for package managers other than apt (e.g. pacman, yum, dnf, zypper, ...)
*)
type os = Linux | Darwin

let os =
  lazise (fun () ->
      match Sys.os_type with
      | "Unix" -> (
        match run ~pipe:true "uname" ["-s"] with
        | Some "Linux" -> Linux
        | Some "Darwin" -> Darwin
        | Some other ->
            raise (Fatal_error (F.sprintf "Not supported os: %s" other))
        | None -> raise (Fatal_error "Unknown operating system") )
      | other -> raise (Fatal_error (F.sprintf "Not supported os: %s" other)) )


module type Package_intf = sig
  val manager : string
  (** main package manager command for the package manager *)

  val install : string
  (** package install command e.g. install or get *)

  val options : string list
  (** additional options e.g. --yes or --verbose *)

  val packages : string list
  (** a list of packages to install *)
end

module Make_installer (Package : Package_intf) = struct
  let install () =
    ignore (check_executable Package.manager) ;
    let args =
      (* the order of options does not matter *)
      Package.install :: List.rev_append Package.options Package.packages
    in
    ignore (run Package.manager args)
end

module Brew = Make_installer (struct
  let manager = "brew"

  let install = "install"

  let options = []

  let packages =
    [ "ruby"
    ; "tmux"
    ; "m4"
    ; "bzip2"
    ; "gzip"
    ; "the_silver_searcher"
    ; "htop"
    ; "tree"
    ; "emacs-plus@28"
    ; "--with-native-comp" ]
end)

module Apt = Make_installer (struct
  let manager = "apt"

  let install = "install"

  let options = ["--yes"]

  let packages =
    [ "m4"
    ; "silversearcher-ag"
    ; "tmux"
    ; "tree"
    ; "ruby"
    ; "htop"
    ; "openssh-server"
    ; "graphviz"
    ; "rsync"
    ; "tar"
    ; "bzip2"
    ; "gzip"
    ; "zip"
    ; "autoconf"
    ; "make"
    ; "gcc"
    ; "cmake"
    ; "etckeeper"
    ; "moreutils"
    ; "build-essential"
    ; "libmysqlclient-dev" ]
end)

module Opam = Make_installer (struct
  let manager = "opam"

  let install = "install"

  let options = ["-y"]

  let packages =
    [ "merlin"
    ; "tuareg"
    ; "ocp-indent"
    ; "ocamlformat"
    ; "dune"
    ; "base"
    ; "core"
    ; "utop" ]
end)

module Cargo = Make_installer (struct
  let manager = "cargo"

  let install = "install"

  let options = []

  let packages =
    match os () with
    | Linux -> ["dutree"; "loc"; "bat"; "exa"; "eva"; "hyperfine"; "bb"]
    | Darwin ->
        ["dutree"; "loc"; "bat"; "exa"; "eva"; "hyperfine"]
        (* Can't compile bb in Darwin *)
    | _ -> assert false
end)

module LinkMap = Map.Make (String)

let links =
  let here = Unix.getcwd () in
  let join target = canonicalize (Filename.concat here target) in
  LinkMap.(
    empty
    |> add (join "emacs") "~/.emacs.d"
    |> add (join "nvim") "~/.config/nvim"
    |> add (join "tmux/.tmux.conf") "~/.tmux.conf"
    |> add (join "tmux/.tmux.color.conf") "~/.tmux.color.conf"
    |> add (join "python/flake8") "~/.config/flake8"
    |> add (join "python/pylintrc") "~/.config/pylintrc")


let link () =
  LinkMap.iter
    (fun source target ->
      if not (Sys.file_exists target) then Unix.symlink source target )
    links


let setup () =
  (* install packages *)
  ( match os () with
  | Linux -> Apt.install ()
  | Darwin -> Brew.install () ) ;
  Opam.install () ;
  Cargo.install () ;
  (* link configs *)
  link ()
