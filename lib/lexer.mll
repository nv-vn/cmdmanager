{
  open Parser
}


let digit = ['0'-'9']

rule token = parse
  | [' ' '\t' '\n' '\r']
    { token lexbuf }
  | "true"
    { TRUE }
  | "false"
    { FALSE }
  | digit+ as num
    { INT (int_of_string num) }
  | '-' digit+ as num
    { INT ~-(int_of_string num) }       
  | digit+ as numa '.' digit* as numb
    { FLOAT (float_of_string @@ numa ^ "." ^ numb) }
  | '.' digit+ as num
    { FLOAT (float_of_string @@ "0." ^ num) }
  | '-' digit+ as numa '.' digit* as numb
    { FLOAT ~-.(float_of_string @@ numa ^ "." ^ numb) }
  | '-' '.' digit+ as num
    { FLOAT ~-.(float_of_string @@ "0." ^ num) }
  | '"'
    { STRING (string "" false lexbuf) }
  | _ as ch
    { STRING (ident (String.make 1 ch) lexbuf) }

and string acc escaped = parse
  | '\\'
    { string acc true lexbuf }
  | '"'
    { if not escaped then acc
      else string (acc ^ "\"") false lexbuf }
  | _ as ch
    { if not escaped then string (acc ^ (String.make 1 ch)) false lexbuf
      else string (acc ^ "\\" ^ (String.make 1 ch)) false lexbuf }

and ident acc = parse
  | eof | [' ' '\t' '\n' '\r']
    { acc }
  | _ as ch
    { ident (acc ^ (String.make 1 ch)) lexbuf }
