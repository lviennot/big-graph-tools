{
type token = EOL | WORD of string

}

rule token = parse
| [' ' '\t']
    { token lexbuf }
| '\n'
    { EOL }
| [^ ' ' '\t' '\n']+ as s
    { WORD s }
| _
    { failwith "unexpected character" }

{

  let iter f fname =
    let cin = open_in fname in
    let lex = Lexing.from_channel cin in
    while true do 
      f (token lex) 
    done ;
    ()
      
let unit () =
  iter (fun t ->
    match t with
      | EOL -> Printf.printf "\n"
      | WORD s -> Printf.printf "%s\n" s
  ) Sys.argv.(1)

}
