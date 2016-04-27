open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

(* No need to use batteries for just this *)
let (>>=) xs f = List.flatten @@ List.map f xs

let _ = default_mapper.structure_item

let cmd_mapper argv =
  {default_mapper with
   structure_item = fun mapper -> function
     | {pstr_desc = Pstr_value (rec_flag, values); pstr_loc} ->
       let values' =
         values >>= begin function
           | {pvb_attributes} as e when List.exists (fun ({txt}, _) -> txt = "cmd") pvb_attributes -> begin
               let fn = match e.pvb_pat.ppat_desc with
                 | Ppat_var {txt} -> Exp.ident {txt = Lident txt; loc = pstr_loc}
                 | _ -> failwith "Must provide a name for a command's function" in
               let args = pvb_attributes >>= begin function
                   | ({txt = "cmd"}, PStr [{pstr_desc = Pstr_eval (name, _)}]) ->
                     [Vb.mk (Pat.any ()) [%expr Cmd.register Cmd.commands [%e name] [%e fn]]]
                   | _ -> []
                 end in
               e::args
             end
           | x -> [x]
         end in
       {pstr_desc = Pstr_value (rec_flag, values'); pstr_loc}
     | stri -> stri
  }

let _ = register "ppx_cmd" cmd_mapper
