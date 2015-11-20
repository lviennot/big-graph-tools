(** Trivial argument parsing : com_name1 v11 v22 com_name2 v2 ... *)

let i = ref 1
let n = Array.length Sys.argv

exception Wrong_argument of string

let next () =
  if !i >= n then raise (Wrong_argument "argument missing") ;
  let arg = Sys.argv.(!i) in
  incr i ;
  arg

let has_more () = !i < n

let string help = 
  try 
    next ()
  with _ -> 
    raise (Wrong_argument (Printf.sprintf "Argument %s missing" help))

let int help =
  try
    int_of_string (next ())
  with _ -> 
    raise (Wrong_argument (Printf.sprintf "Argument %s: int expected" help))

let float help =
  try
    float_of_string (next ())
  with _ -> 
    raise (Wrong_argument (Printf.sprintf "Argument %s: float expected" help))

let usage help coms more =
  flush stdout ;
  Printf.eprintf "Usage of %s :\n%s\n" Sys.argv.(0) help ;
  List.iter (fun (name, (help, f)) ->
    Printf.eprintf "  %s %s\n" name help ;
  ) coms ;
  Printf.eprintf "%s\n" more ;
  flush stderr ;
  exit 2

let parse usage_help coms =
  let coms = List.map (fun (name, help, f) -> name, (help, f)) coms in
  while has_more () do
    let com = next () in
    let help, f = 
      try 
        List.assoc com coms
      with Not_found -> 
        usage usage_help coms 
          (Printf.sprintf "Unrecognized command : %s" com) 
    in
    try
      f ()
    with Wrong_argument msg ->
      usage usage_help coms 
        (Printf.sprintf "Wrong argument for %s : %s" com msg) 
  done



