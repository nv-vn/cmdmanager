open Cmdtypes

type command = input list -> output

let commands : (string, command) Hashtbl.t = Hashtbl.create 100

let register ?(commands=commands) name f = Hashtbl.replace commands name f

let read ?(commands=commands) cmd =
  let lexbuf = Lexing.from_string cmd in
  match Parser.main Lexer.token lexbuf with
  | [] -> `Nothing
  | cmd'::args -> begin
      match Hashtbl.find_all commands (string_of_input cmd') with
      | [] -> `Nothing
      | f::_ -> f args
    end
