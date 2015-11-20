{

  type token = Int of int | Float of float | String of string | Eof

  open Lexing

}

let space = [ ' ' '\t' ]
let eol = ('\r'? '\n' | '\n'? '\r')
let sign = [ '-' '+' ]
let digit = [ '0'-'9' ]
let nonspace = [ ^' ' '\t' '\n' '\r' ]

rule token = parse

| space+
    { token lexbuf }

| eol 
    { new_line lexbuf ; token lexbuf }

| sign? digit+ as w
    { Int (int_of_string w) }

| sign? digit* '.' digit* (['e' 'E'] sign? digit+)? as w
    { Float (float_of_string w) }

| nonspace+ as s
    { String s }

| eof
    { raise End_of_file } 

| _ { (* empty token *) raise End_of_file }

{

  let str_of_tok = function
    | Float f -> Printf.sprintf "float '%f'" f
    | Int i -> Printf.sprintf "int '%d'" i
    | String s -> Printf.sprintf "string '%s'" s
    | Eof -> "End_of_file"

  let error lex got expected =
    let pos = lex.lex_curr_p in
    let msg = 
      Printf.sprintf "BasicLexer: \
          got %s instead of %s%s at line %d, before character %d"
        (str_of_tok got) expected
        (if pos.pos_fname = "" then "" else " in " ^ pos.pos_fname)  
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol) 
    in 
    invalid_arg msg
  
  let token ?(no_eof=false) expected lex =
    try token lex 
    with End_of_file -> 
      if no_eof then error lex Eof expected else raise End_of_file

  let float ?(no_eof=false) lex =
    match token ~no_eof "float" lex with
    | Float f -> f
    | Int i -> float_of_int i
    | tok -> error lex tok "float"

  let int ?(no_eof=false) lex = 
    match token ~no_eof:no_eof "int" lex with
    | Int i -> i
    | tok -> error lex tok "int"

  let string ?(no_eof=false) lex = 
    match token ~no_eof "string" lex with
    | Float f -> string_of_float f
    | Int i -> string_of_int i
    | String s -> s
    | _ -> assert false

  let token ?(no_eof=false) lex = token ~no_eof "token" lex


  let iter f lex =
    try while true do 
      f (token lex) 
    done with End_of_file ->
    ()

  let iter_int f lex =
    try while true do 
      f (int lex) 
    done with End_of_file ->
    ()

  let unit () =
    (* let lex = Lexing.from_channel stdin in *)
    let lex = Lexing.from_string "lqkdj 123 i? 12ZER -.2e+127 -lsdkjf.sldkf" in
    iter (fun t -> Printf.printf "%s\n" (str_of_tok t) ; flush stdout) lex ;
    let lex = Lexing.from_string "123 4\n 5\n 00123 bug 34 56\n" in
    try
      iter_int (fun i -> Printf.printf "%d\n" i ; flush stdout) lex ;
      assert false ;
    with Invalid_argument _ ->
    ()

}
