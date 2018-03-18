(* Laurent Viennot, Inria 2015 *)

(** Big graph manipulation tool. *)

(* Work with bigarrays. *)

module Int32Array = struct 
  module B = Bigarray
  module BA1 = B.Array1
  type t = (int32, B.int32_elt, B.c_layout) BA1.t
  type key = int
  type value = int
  let string_of_value = string_of_int
  let value_size = 32
  let create n = 
    Debug.mem_add (n * 4) ;
    BA1.create B.int32 B.c_layout n
  let make n e =
    Debug.mem_add (n * 4) ;
    let e = Int32.of_int e in
    let t = BA1.create B.int32 B.c_layout n in
    for i=0 to n-1 do t.{i} <- e done ;
    t
  let release t = Debug.mem_sub (4 * BA1.dim t)
  let get t i = Int32.to_int (BA1.get t i)
  let set t i e = BA1.set t i (Int32.of_int e)
  let length = BA1.dim
  let blit s os t ot len =
    let s = BA1.sub s os len and t = BA1.sub t ot len in
    BA1.blit s t
  let iter f t = 
    for i=0 to length t - 1 do f i (Int32.to_int (BA1.get t i)) done 
end

module IntArray = struct 
  module B = Bigarray
  module BA1 = B.Array1
  type t = (int, B.int_elt, B.c_layout) BA1.t
  type key = int
  type value = int
  let string_of_value = string_of_int
  let value_size = Sys.word_size - 1
  let create n = 
    Debug.mem_add (n * Sys.word_size / 8) ;
    BA1.create B.int B.c_layout n
  let make n e =
    Debug.mem_add (n * Sys.word_size / 8) ;
    let t = BA1.create B.int B.c_layout n in
    for i=0 to n-1 do t.{i} <- e done ;
    t
  let release t = Debug.mem_sub (Sys.word_size / 8 * BA1.dim t)
  let get = BA1.get
  let set = BA1.set
  let length = BA1.dim
  let blit s os t ot len =
    let s = BA1.sub s os len and t = BA1.sub t ot len in
    BA1.blit s t
  let iter f t = 
    for i=0 to length t - 1 do f i (BA1.get t i) done 
end

module BG = IndexGraph.Make (Int32Array) (IntArray)

module WG = struct
  module G = IndexGraph.Weighted  (Int32Array) (Int32Array)
    (IndexGraph.IntIndex) (IndexGraph.IntNativeArray)
    (Int32Array) (IndexGraph.IntWeight)
  include G
  module WT = IntArray (* Avoiding weight overflow when summing. *)
end


(* ocamlgraph *)
module OG = struct
  include Graph.Imperative.Digraph.Concrete(struct 
    type t = int
    let equal u v = u = v
    let compare u v = u - v
    let hash u = u land max_int
  end)
  let create ?(n=4) ?(m=8) () = create ~size:n ()
  let n = nb_vertex
  let m = nb_edges
  let sort _ = ()
end

module G = BG


type res = 
  | Graph of G.t
  | WeightedGraph of WG.t    

let stack : res list ref = ref []    
      
let push_graph g = 
  stack := (Graph g) :: !stack
    
let pop_graph () = 
  match !stack with
  | Graph g :: st -> stack := st ; g
  | WeightedGraph _ :: _ -> 
    invalid_arg "Bgtool: weighted graph when expecting unweighted"
  | _ -> invalid_arg "Bgtool: no graph to process"
    
let push_weighted_graph g = 
  stack := (WeightedGraph g) :: !stack
    
let pop_weighted_graph () = 
  match !stack with
  | WeightedGraph g :: st -> stack := st ; g
  | _ -> invalid_arg "Bgtool: no weighted graph to process"
    
let pop () = 
  match !stack with 
  | op :: st -> stack := st ; op 
  | _ -> invalid_arg "Bgtool: operand empty stack"



(* ------------------------- Input / Output graph ---------------------- *)

module IO = struct

  let cin_of_filename fname =
    if fname = "-" then stdin 
    else if Ext.String.has_suffix fname ".gz" then
      Unix.open_process_in (Printf.sprintf "gunzip -c %s" fname)
    else open_in fname

  let cout_of_filename fname =
    if fname = "-" then stdout
    else if Ext.String.has_suffix fname ".gz" then
      Unix.open_process_out (Printf.sprintf "gzip -c > %s" fname)
    else open_out fname

  module UInt32 = struct
    let byte_len = 5
    let max_int = -1 lsr (Sys.word_size - 1 - 8 * byte_len)
  end
    
  module Bin = GraphIO.Bin (G) (UInt32)
  module Txt = GraphIO.Txt (G)
  module WTxt = GraphIO.WeightedTxt (WG)
end


let () = 

  TrivialArg.addCommand "read-edges"
    "[fname] : read pairs of ints [u1 v1 u2 v2 ...] from file [fname]\n\t\
     ([-] for stdin, [.gz] extension for gzipped file) as the edges of a graph."
    (fun () ->
      let fname = TrivialArg.string "fname" in
      let cin = IO.cin_of_filename fname in
      push_graph (IO.Txt.read_edges cin) ;
      if fname <> "-" then close_in cin ;
    ) ;

  TrivialArg.addCommand "read-edges-index"
    "[fname] : read pairs of strings [u1 v1 u2 v2 ...] from file [fname]\n\t\
     ([-] for stdin, [.gz] extension for gzipped file) as the edges of a graph."
    (fun () ->
      let fname = TrivialArg.string "fname" in
      let cin = IO.cin_of_filename fname in
      let g, _ = IO.Txt.read_edges_index cin in
      push_graph g ;
      if fname <> "-" then close_in cin ;
    ) ;

  TrivialArg.addCommand "read-src-dst-wgt"
    "[fname] : read triples of ints [u1 v1 w1 u2 v2 w2 ...] from file\n\t\
     [fname] ([-] for stdin, [.gz] extension for gzipped file) as the\n\t\
     weighted edges of a graph."
    (fun () ->
      let fname = TrivialArg.string "fname" in
      let cin = IO.cin_of_filename fname in
      push_weighted_graph (IO.WTxt.read_src_dst_wgt cin) ;
      if fname <> "-" then close_in cin ;
    ) ;

  TrivialArg.addCommand "write-edges"
    "[fname] : write the graph obtained so far with format compatible \n\t\
       to 'read-edges' command in classical [.csv] format with two \n\t\
       columns. (See 'read_edges' about meaning of [-] and [.gz] in [fname].)"
    (fun () ->
      let g = pop_graph () in
      let fname = TrivialArg.string "fname" in
      let cout = IO.cout_of_filename fname in
      IO.Txt.write_edges cout g ;
      if fname <> "-" then close_out cout ;
    ) ;

  TrivialArg.addCommand "write-src-dst-wgt"
    "[fname] : write the graph obtained so far with format compatible \n\t\
       to 'read-src-dst-wgt' command in classical [.csv] format with three \n\t\
       columns. (See 'read_edges' about meaning of [-] and [.gz] in [fname].)"
    (fun () ->
      let g = pop_weighted_graph () in
      let fname = TrivialArg.string "fname" in
      let cout = IO.cout_of_filename fname in
      IO.WTxt.write_src_dst_wgt cout g ;
      if fname <> "-" then close_out cout ;
    ) ;

  TrivialArg.addCommand  "read-adj"
    "[fname] : read a graph [g] with adjacencies given by a sequence of \n\t\
       integers [n m u1 d1 v11 v12 .. v1d1  u2 d2 v21 v22 ...] from file \n\t\
       [fname] where [n] (resp. [m]) is the number of nodes (resp. edges), \n\t\
       [u1, u2, ...] are the nodes of [g], [d1, d2, ...] are their \n\t\
       respective degress, [u1->v11 u1->v12 ... u2->v21 u2->v22 ...] are \n\t\
       the edges of [g]. (See 'read-edges' about meaning of [-] and [.gz] \n\t\
       in [fname].) Specify '-n [n] -m [m]' to indicate the size of the graph."
    (fun () ->
      let n = TrivialArg.intOption "-n" 0 "number of vertices" in
      let m = TrivialArg.intOption "-m" 0 "number of edges" in
      let fname = TrivialArg.string "fname" in
      let cin = IO.cin_of_filename fname in
      push_graph (IO.Txt.read_adjacencies ~n ~m cin) ;
      if fname <> "-" then close_in cin ;
    ) ;

  TrivialArg.addCommand  "read-adj-test"
    "[fname] : read a graph [g] with adjacencies given by a sequence of \n\t\
       integers [n m u1 d1 v11 v12 .. v1d1  u2 d2 v21 v22 ...] from file \n\t\
       [fname] where [n] (resp. [m]) is the number of nodes (resp. edges), \n\t\
       [u1, u2, ...] are the nodes of [g], [d1, d2, ...] are their \n\t\
       respective degress, [u1->v11 u1->v12 ... u2->v21 u2->v22 ...] are \n\t\
       the edges of [g]. (See 'read-edges' about meaning of [-] and [.gz] \n\t\
       in [fname].) Specify '-n [n] -m [m]' to indicate the size of the graph."
    (fun () ->
      let n = TrivialArg.intOption "-n" 0 "number of vertices" in
      let m = TrivialArg.intOption "-m" 0 "number of edges" in
      let fname = TrivialArg.string "fname" in
      let cin = IO.cin_of_filename fname in
      push_graph (IO.Txt.read_adj_test ~n ~m cin) ;
      if fname <> "-" then close_in cin ;
    ) ;

  TrivialArg.addCommand "write-adj"
    "[fname] : write the graph obtained so far with format compatible \n\t\
       to 'read-adj'. (See 'read-edges' about meaning of [-] and [.gz] \n\t\
       in [fname].)"
    (fun () ->
      let g = pop_graph () in
      let fname = TrivialArg.string "fname" in
      let cout = IO.cout_of_filename fname in
      IO.Txt.write_adjacencies cout g ;
      if fname <> "-" then close_out cout ;
    ) ;

  TrivialArg.addCommand  "read-adj-bin"
    "[fname] : read a graph given by adjacencies similarly as \n\t\
       'read-adj' from a binary file [fname] (each integer is coded in 8 
       little-endian bytes)."
    (fun () ->
      let fname = TrivialArg.string "fname" in
      let cin = IO.cin_of_filename fname in
      push_graph (IO.Bin.read_adjacencies cin) ;
      if fname <> "-" then close_in cin ;
    ) ;

  TrivialArg.addCommand  "adj-txt-to-bin"
    "[fsrc] [fdst] : read from [fsrc] a graph given by adjacencies as with
       'read_adj' and write a binary version of it in [fdst] (each integer 
       is coded in 8 little-endian bytes). [fdst] must be a regular file name."
    (fun () ->
      let fsrc = TrivialArg.string "fsrc" in
      let cin = IO.cin_of_filename fsrc in
      let fdst = TrivialArg.string "fdst" in
      IO.Bin.adjacencies_txt_to_bin cin fdst ;
      if fsrc <> "-" then close_in cin ;
    ) ;

  ()


let () =  (* ---------------- Random graphs ---------------------- *)

  TrivialArg.addCommand "rnd-d-regular"
    "[n] [d] : generate a random [d] regular graph with [n] nodes."
    (fun () ->
      let n = TrivialArg.int "n" in
      let d = TrivialArg.int "d" in
      let g = G.create ~n ~m:(d*n) () in
      for u=0 to n-1 do
        G.unsafe_adj_begin g u ;
        for i=1 to d do
          let v = Random.int n in
          G.unsafe_adj_add g u v ;
        done ;
        G.unsafe_adj_end g u ;
        if u mod 10 = 0 || u = n-1 then 
          Debug.progress u n "rnd-d-regular d=%d" d ;
      done ;
      push_graph  g ;
    ) ;

  TrivialArg.addCommand "rnd-gnm"
    "[n] [d] : generate a random graph with [n] nodes and [m] edges."
    (fun () ->
      let n = TrivialArg.int "n" in
      let m = TrivialArg.int "m" in
      let g = G.create ~n ~m () in
      for i=1 to m do
        let u = Random.int n in
        let v = Random.int n in
        G.add_edge g u v ;
        if i mod 100 = 0 || i = m then 
          Debug.progress i m "rnd-gnm n=%d" n ;
      done ;
      push_graph  g ;
    ) ;

  ()


module Scc = Component.StronglyConnected (Component.Array) (G)
module Diam = Diameter.Unweighted (G)

module WScc = Component.StronglyConnected (Component.Array) (WG)
module WDiam = Diameter.Make (WG)

let () =  (* ---------------- Operate on graph ---------------------- *)

  TrivialArg.addCommand "reverse"
    ": reverse edges of the current graph."
    (fun () ->
      match pop () with
        | Graph g -> push_graph (G.reverse g)
        | WeightedGraph g -> push_weighted_graph (WG.reverse g)
        (*| _ -> invalid_arg "no graph to reverse"*)
    ) ;

  TrivialArg.addCommand "sort"
    ": sort edges of the current graph and store reverse graph."
    (fun () ->
      let g = pop_graph () in
      G.sort g ;
      push_graph g ;
    ) ;

  TrivialArg.addCommand "largest-scc"
    ": reduce graph to largest strongly connected component."
    (fun () ->
      let g = pop_graph () in
      let scc = Scc.components g in
      let largest, u_max, _ = Scc.largest scc in
      let in_largest u = Scc.number scc u = largest in
      let h = G.induced_subgraph ~n:(u_max+1) in_largest g in
      push_graph h ;
    ) ;

  TrivialArg.addCommand "diameter"
    ": compute diameter of the largest strongly connected component of the\n\t\
     current graph."
    (fun () ->
      match pop () with
        | Graph g ->
          let scc = Scc.components g in
          let _, u_max, s_max = Scc.largest scc in
          let diam = Diam.diameter_radius_scc g u_max in
          if diam.Diam.diam_lb = diam.Diam.diam_ub then
            Debug.info "Diam = %d" diam.Diam.diam_lb
          else 
            Debug.info "%d <= Diam <= %d" diam.Diam.diam_lb diam.Diam.diam_ub
        | WeightedGraph g ->
          let scc = WScc.components g in
          let _, u_max, s_max = WScc.largest scc in
          let diam = WDiam.diameter_radius_scc g u_max in
          if diam.WDiam.diam_lb = diam.WDiam.diam_ub then
            Debug.info "Diam = %d" diam.WDiam.diam_lb
          else 
            Debug.info "%d <= Diam <= %d" diam.WDiam.diam_lb diam.WDiam.diam_ub
    ) ;

  TrivialArg.addCommand "unweighted-diameter"
    ": compute diameter of the largest strongly connected component of the\n\t\
     current graph."
    (fun () ->
      let g = pop_graph () in
      let scc = Scc.components g in
      let _, u_max, s_max = Scc.largest scc in
      let diam = Diam.diameter_radius_scc g u_max in
      if diam.Diam.diam_lb = diam.Diam.diam_ub then
        Debug.info "Diam = %d" diam.Diam.diam_lb
      else 
        Debug.info "%d <= Diam <= %d" diam.Diam.diam_lb diam.Diam.diam_ub ;
    ) ;

  TrivialArg.addCommand "weighted-diameter"
    ": compute diameter of the largest strongly connected component of the\n\t\
     current weighted graph."
    (fun () ->
      let g = pop_weighted_graph () in
      let scc = WScc.components g in
      let _, u_max, s_max = WScc.largest scc in
      let diam = WDiam.diameter_radius_scc g u_max in
      if diam.WDiam.diam_lb = diam.WDiam.diam_ub then
        Debug.info "Diam = %d" diam.WDiam.diam_lb
      else 
        Debug.info "%d <= Diam <= %d" diam.WDiam.diam_lb diam.WDiam.diam_ub ;
    ) ;

  TrivialArg.addCommand "diameter-scc"
    "[node] : compute diameter of strongly connected component of [node]."
    (fun () ->
      let g = pop_graph () in
      let s = TrivialArg.int "node" in
      let diam = Diam.diameter_radius_scc g s in
      if diam.Diam.diam_lb = diam.Diam.diam_ub then
        Debug.info "Diam = %d" diam.Diam.diam_lb
      else 
        Debug.info "%d <= Diam <= %d" diam.Diam.diam_lb diam.Diam.diam_ub ;
    ) ;

  TrivialArg.addCommand "test-time"
    ": test Sys.time."
    (fun () ->
      let module A = Int32Array in
      let n = 100*1000*1000 in
      Debug.info "go for %d" n ;
      (* let t = A.BA1.create A.B.int32 A.B.c_layout n in *)
      let t = A.create n in
      Debug.info "array created" ;
      let j = ref 0 in
      let f i =
        A.set t i !j ;
      in
      for i=0 to n-1 do
        j := !j + 1 ;
        f i ;
        if i mod 1000 = 0 || i = n-1 then Debug.progress i n "test" ;
      done ;
    ) ;

  ()

let () =
  Debug.run ~verbose:(TrivialArg.boolOption "-verbose" "verbose mode") 
    (fun _ -> TrivialArg.parse "Apply some operations on a graph.")

