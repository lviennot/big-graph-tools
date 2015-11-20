(** Trivial argument parsing : com_name1 v11 v22 com_name2 v2 ... *)

let cur_pos = ref 1
let n = Array.length Sys.argv
let seen = Array.make n false

let rec unseen_pos i =
  if i >= n || not seen.(i) then i else unseen_pos (i+1)


exception Wrong_argument of string

let wrong_arg fmt =
  let after msg = raise (Wrong_argument msg) in
  Printf.ksprintf after fmt


(** Read next argument. *)

let has_more () = unseen_pos !cur_pos < n

let next () =
  let i = unseen_pos !cur_pos in
  if i >= n then wrong_arg "argument missing" ;
  let arg = Sys.argv.(i) in
  seen.(i) <- true ;
  cur_pos := i + 1 ;
  arg

let string help = 
  try 
    next ()
  with _ -> wrong_arg "Argument %s missing" help

let int help =
  try
    int_of_string (next ())
  with _ -> wrong_arg "Argument %s: int expected" help

let float help =
  try
    float_of_string (next ())
  with _ -> wrong_arg "Argument %s: float expected" help


(** Read distant argument (found by key). *)

let search key =
  let rec iter i =
    if i >= n then n
    else if (not seen.(i)) && Sys.argv.(i) = key then begin
      seen.(i) <- true ; 
      i 
    end else iter (i+1)
  in iter !cur_pos

let boolOption key =
  let i = search key in
  i < n

let option_arg key i =
  let i = i + 1 in
  if i >= n || seen.(i) then wrong_arg "Option %s: argument missing" key
  else begin
    seen.(i) <- true ;
    Sys.argv.(i) 
  end

let stringOption key default =
  let i = search key in
  if i >= n then default 
  else option_arg key i

let intOption key default =
  let i = search key in
  if i >= n then default else
    try
      int_of_string (option_arg key i) 
    with _ -> wrong_arg "Option %s: int expected" key

let floatOption key default =
  let i = search key in
  if i >= n then default else
    try
      float_of_string (option_arg key i) 
    with _ -> wrong_arg "Option %s: float expected" key


(** Parse helpers. *)

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



