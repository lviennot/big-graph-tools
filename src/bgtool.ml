(* Laurent Viennot, Inria 2015 *)

(** Big graph manipulation tool. *)


module G = 
  IntDigraph.Compact 
    (IntDigraph.IntVec0) 
    (IntDigraph.OfInt32 (struct let default = Int32.zero end))

type res = Graph of G.t
    
let stack : res list ref = ref []    
      
let push_graph g = 
  stack := (Graph g) :: !stack
    
let pop_graph () = 
  match !stack with
  | Graph g :: st -> stack := st ; g
  | _ -> invalid_arg "Bgtool: no graph to process"
    
let pop () = 
  match !stack with 
  | _ :: st -> stack := st 
  | _ -> () 

module UInt32 = struct
  let byte_len = 5
  let max_int = -1 lsr (Sys.word_size - 1 - 8 * byte_len)
end

module Bin = GraphIO.Bin (G) (UInt32)
module Txt = GraphIO.Txt (G)

let () =

  TrivialArg.addCommand "-read_edges"
    "  [fname] : read pairs of ints [u1 v1 u2 v2 ...] from file [fname]\n\t\
     ([-] for stdin, [.gz] extension for gzipped file) as the edges of a graph."
    (fun () ->
      let fname = TrivialArg.string "fname" in
      let cin = GraphIO.cin_of_filename fname in
      push_graph (Txt.read_edges cin) ;
      if fname <> "-" then close_in cin ;
    ) ;

  TrivialArg.addCommand "-write_edges"
    "  [fname] : write the graph obtained so far with format compatible \n\t\
       to 'read_edges' command and classical [.csv] format with two \n\t\
       columns. (See 'read_edges' about meaning of [-] and [.gz] in [fname].)"
    (fun () ->
      let g = pop_graph () in
      let fname = TrivialArg.string "fname" in
      let cout = GraphIO.cout_of_filename fname in
      Txt.write_edges cout g ;
      if fname <> "-" then close_out cout ;
    ) ;

  TrivialArg.addCommand  "-read_adj"
    "  [fname] : read a graph [g] with adjacencies given by a sequence of \n\t\
       integers [n m u1 d1 v11 v12 .. v1d1  u2 d2 v21 v22 ...] from file \n\t\
       [fname] where [n] (resp. [m]) is the number of nodes (resp. edges), \n\t\
       [u1, u2, ...] are the nodes of [g], [d1, d2, ...] are their \n\t\
       respective degress, [u1->v11 u1->v12 ... u2->v21 u2->v22 ...] are \n\t\
       the edges of [g]. (See 'read_edges' about meaning of [-] and [.gz] \n\t\
       in [fname].)"
    (fun () ->
      let fname = TrivialArg.string "fname" in
      let cin = GraphIO.cin_of_filename fname in
      push_graph (Txt.read_adjacencies cin) ;
      if fname <> "-" then close_in cin ;
    ) ;

  TrivialArg.addCommand  "-read_adj_bin"
    "  [fname] : read a graph given by adjacencies similarly as \n\t\
       'read_adj' from a binary file [fname] (each integer is coded in 8 
       little-endian bytes)."
    (fun () ->
      let fname = TrivialArg.string "fname" in
      let cin = GraphIO.cin_of_filename fname in
      push_graph (Bin.read_adjacencies cin) ;
      if fname <> "-" then close_in cin ;
    ) ;

  TrivialArg.addCommand  "-adj_txt_to_bin"
    "  [fsrc] [fdst] : read from [fsrc] a graph given by adjacencies as with
       'read_adj' and write a binary version of it in [fdst] (each integer 
       is coded in 8 little-endian bytes). [fdst] must be a regular file name."
    (fun () ->
      let fsrc = TrivialArg.string "fsrc" in
      let cin = GraphIO.cin_of_filename fsrc in
      let fdst = TrivialArg.string "fdst" in
      Bin.adjacencies_txt_to_bin cin fdst ;
      if fsrc <> "-" then close_in cin ;
    ) ;

  ()



let () =
  Debug.run ~verbose:(TrivialArg.boolOption "-verbose" "verbose mode") 
    (fun _ ->
      Debug.info "--- Start" ;
      TrivialArg.parse "Apply some operations on a graph." ;
      Debug.info "--- End" ;    
    )

