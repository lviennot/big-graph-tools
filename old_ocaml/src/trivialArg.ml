(** Trivial argument parsing : com_name1 v11 v22 com_name2 v2 ... *)

let cur_pos = ref 1
let n = Array.length Sys.argv
let seen = Array.make n false

let rec unseen_pos i =
  if i >= n || not seen.(i) then i else unseen_pos (i+1)


exception Invalid_argument of string

let invalid_arg fmt =
  let after msg = raise (Invalid_argument msg) in
  Printf.ksprintf after fmt


(** Read next argument. *)

let has_more () = unseen_pos !cur_pos < n

let next () =
  let i = unseen_pos !cur_pos in
  if i >= n then invalid_arg "argument missing" ;
  let arg = Sys.argv.(i) in
  seen.(i) <- true ;
  cur_pos := i + 1 ;
  arg

let string help = 
  try 
    next ()
  with _ -> invalid_arg "argument [%s] missing" help

let int help =
  try
    int_of_string (next ())
  with _ -> invalid_arg "argument [%s]: int expected" help

let float help =
  try
    float_of_string (next ())
  with _ -> invalid_arg "argument [%s]: float expected" help


(** Read distant argument (found by key). *)

let options = ref []

let addOption key default help =
  if not (List.mem_assoc key !options) then
    options := (key, (default, help)) :: !options

let search key =
  let rec iter i =
    if i >= n then n
    else if (not seen.(i)) && Sys.argv.(i) = key then begin
      seen.(i) <- true ; 
      i 
    end else iter (i+1)
  in iter !cur_pos

let boolOption key help =
  addOption key "false" help ;
  let i = search key in
  i < n

let option_arg key i =
  let i = i + 1 in
  if i >= n || seen.(i) then invalid_arg "Option '%s': argument missing" key
  else begin
    seen.(i) <- true ;
    Sys.argv.(i) 
  end

let stringOption key default help =
  addOption key default help ;
  let i = search key in
  if i >= n then default 
  else option_arg key i

let intOption key default help =
  addOption key (string_of_int default) help ;
  let i = search key in
  if i >= n then default else
    try
      int_of_string (option_arg key i) 
    with _ -> invalid_arg "Option %s: int expected" key

let floatOption key default help =
  addOption key (string_of_float default) help ;
  let i = search key in
  if i >= n then default else
    try
      float_of_string (option_arg key i) 
    with _ -> invalid_arg "Option %s: float expected" key


(** Parse helpers. *)

let commands = ref []

let addCommand name help (f : unit -> unit) =
  commands := (name, (help, f)) :: !commands


let usage help more =
  flush stdout ;
  Printf.eprintf "Usage of %s:\n\n%s [options] [commands]\n\n%s\n\n" 
    Sys.argv.(0) Sys.argv.(0) help ;

  if !options <> [] then begin
    Printf.eprintf "Available options :\n" ;
    List.iter (fun (key, (default, help)) ->
      Printf.eprintf "  %s %s (default is %s)" key help default ;
    ) !options ;
  end ;

  if !commands <> [] then begin
    Printf.eprintf "\n\nAvailable commands :\n" ;
    let coms = List.rev !commands in
    List.iter (fun (name, (help, f)) ->
      Printf.eprintf "  %s %s\n" name help ;
    ) coms ;
  end ;

  if more <> "" then begin
    Printf.eprintf "\n%s\n" more ;
  end ;
  flush stderr ;
  exit 2

let parse ?commands:(more_cmds=[]) ?(help_option=true) usage_help =
  List.iter (fun (name, help, f) -> addCommand name help f) more_cmds ;
  if help_option then 
    addCommand "-help" " : Display this usage mesage with list of commands."
      (fun () -> usage usage_help "") ;
  let coms = List.rev !commands in
  while has_more () do
    let com = next () in
    let help, f = 
      try 
        List.assoc com coms
      with Not_found -> 
        usage usage_help (Printf.sprintf "Unrecognized command: '%s'" com) 
    in
    try
      Debug.info "--- Run command %s :" com ;
      f ()
    with Invalid_argument msg ->
      Printf.eprintf "Wrong argument for '%s' command: '%s'\n  usage: \
                         %s %s\n" com msg com help ;
      flush stderr ;
      exit 2 ;
  done




