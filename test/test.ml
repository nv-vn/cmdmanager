open Cmdtypes

let hello = function
  | _ -> `String "hello"
[@@cmd "hello"]
[@@cmd "helloworld"]

let add =
  let rec  add acc = function
    | [] -> `String (string_of_float acc)
    | (`Int i)::rest -> add (acc +. float i) rest
    | (`Float f)::rest -> add (acc +. f) rest
    | _ -> `Nothing in
  add 0.0 [@@cmd "add"]

let show = function
  | `Nothing -> "(Nothing)"
  | `String s -> "\"" ^ s ^ "\""

let run_command cmd =
  Printf.printf "%s: %s\n" cmd (show @@ Cmd.read cmd)

let () =
  let cmds = ["hello"; "helloworld"; "hello 1"; "add"; "add 0 1"; "add true"; "add 3.2 -5.6 1"] in
  List.iter run_command cmds
