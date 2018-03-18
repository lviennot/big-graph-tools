(* Laurent Viennot
 * Copyright Inria 2015 *)

(* Directed graphs with numbered nodes and edges labeled with ints. *)

module Int = struct 
  type t = int 
  let compare = compare 
  let hash i = i land max_int
  let equal = (=)
  let default = 0
end

module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled(Int)(Int)

let the_graph = ref (G.create ())

let graph_of_edge_file fname =
  let f = if fname = "-" then stdin else open_in fname in
  let input b n =
    let nread = input f b 0 n in
    if nread = 0 then raise End_of_file
    else nread
  in
  let lexbuf = Lexing.from_function input in
  let nline = ref 0 and ntotal = Debug.file_nlines fname in
  let int () =
    match Wordline.token lexbuf with
      | Wordline.WORD u ->
        (try 
           int_of_string u 
         with _ -> 
           failwith (Printf.sprintf "int expected line %d, got : %s" !nline u))
      | Wordline.EOL -> 
        failwith (Printf.sprintf "int expected at end of line %d" !nline)
  in
  let g = G.create ~size:97 () in
  (try while true do
      match Wordline.token lexbuf with
        | Wordline.EOL -> 
          incr nline ;
          Debug.progress !nline ntotal "Reading %s" fname
        | Wordline.WORD u ->
          G.add_edge g (int_of_string u) (int ())
    done with End_of_file -> ()) ;
  if fname <> "-" then close_in f ;
  g

module Bfs = Treeverse.Bfs(G)

let coms = [

  "read", "[format] [file] : read a graph from a file \
           (supported formats : edges (two ints per edge), file=- for stdin)",
  (fun () ->
    match TrivialArg.string "format" with
      | "edges" ->
        let fname = TrivialArg.string "file" in
        the_graph := graph_of_edge_file fname ;
      | _ -> invalid_arg "Only format 'edges' is supported."
  );

  "write", "[format] : write the graph to stdout \
            (supported format : csv_edges (tab separated ints : src dst lab))",
  (fun () ->
    let g = !the_graph in
    match TrivialArg.string "format" with
      | "csv_edges" ->
        G.iter_vertex (fun u ->
         G.iter_succ_e (fun e ->
           Printf.printf "%d\t%d\t%d\n" (G.E.src e) (G.E.dst e) (G.E.label e) ;
         ) g u ;
        ) g
      | _ -> invalid_arg "Only format 'csv_edges' is supported."
  );

  "skeleton", "[thick_fac] [thick_add] [node] : compute a skeleton \
      from a BFS tree rooted at [node] \
      (cuting branches at nodes v s.t. dist([node], furthest descendant of v) 
             < [thick_fac] * dist([node], v) + [thick_add]).",
  (fun () ->
   let g = !the_graph in
   let thick_fac = TrivialArg.float "thick_fac" in
   let thick_add = TrivialArg.int "thick_add" in
   let u = TrivialArg.int "node" in
   let tree = Bfs.tree g u in 
   for i=0 to Array.length tree - 1 do
     let u, p, d = tree.(i) in
     Printf.printf "%d %d %d\n" d p u ;
   done ;
   ()
   (* the_graph := G.skeleton ~thick_fac:thick_fac ~thick_add:thick_add g u *)
  );

]

let () =
  Printexc.record_backtrace true ;
  try
    Debug.set_verbosity "info"  ;

    let usage = Printf.sprintf "[com list] : apply some operations on graphs\
      \nExample : %s read edges edge_file.txt skeleton 1.2 4 write edges_label\
      \n\nAvailable commands :" 
      Sys.argv.(0) in 
    TrivialArg.parse usage coms ;

    Debug.info "end" ;
    Debug.the_end () ;
    ()
  with e ->
    flush stdout ;
    Printf.eprintf "Error: %s\n%s\n\n" 
      (Printexc.to_string e) (Printexc.get_backtrace ()) ;
    flush stderr ;
    exit 2
