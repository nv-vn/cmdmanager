type input =
  [ `String of string
  | `Int of int
  | `Float of float
  | `Bool of bool
  ]

type output =
  [ `Nothing
  | `String of string
  ]

let string_of_input = function
  | `String s -> s
  | `Int i -> string_of_int i
  | `Float f -> string_of_float f
  | `Bool b -> string_of_bool b

let rec string_of_inputs = function
  | [] -> ""
  | x::xs -> string_of_input x ^ string_of_inputs xs
