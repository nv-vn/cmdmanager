type input = [`String of string | `Int of int | `Float of float | `Bool of bool]
type output = [`Nothing | `String of string]

let commands : (string, input list -> output) Hashtbl.t = Hashtbl.create 100

let register name f = Hashtbl.replace commands name f
