(* Laurent Viennot, Inria 2015 *)

(** Write and read graphs. *)

module type IntGraph = sig
  include Sig.Graph with type node = int
  val out_degree : t -> vertex -> int
  val unsafe_adj_begin : t -> node -> unit
  val unsafe_adj_add : t -> node -> node -> unit
  val unsafe_adj_end : t -> node -> unit
end

module type IntWeightedGraph = sig
  include Sig.LabeledGraph with type node = int and type label = int
  val out_degree : t -> vertex -> int
  val unsafe_adj_begin : t -> node -> unit
  val unsafe_adj_add : t -> node -> label -> node -> unit
  val unsafe_adj_end : t -> node -> unit
end

module type UIntCoding = sig
  val byte_len : int
  val max_int : int
end

(** Binary format : [n m u_0 deg_0 v_01 ... v_0{deg_0} u_1 deg_1 v_11 ... ]. *)
module Bin (G : IntGraph) (UInt : UIntCoding) = struct

  (* Little Endian *)

  let write_i cout i =
    if i < 0 || i > UInt.max_int then invalid_arg "Bin.write_i" ;
    let low_byte = 0xff in
    let rec iter i b =
      if b < UInt.byte_len then begin
        let c = Char.chr (i land low_byte) in
        output_char cout c ;
        iter (i lsr 8) (b+1)
      end
    in iter i 0

  let read_i cin =
    let rec iter i dec b =
      if b >= UInt.byte_len then i
      else let c = Char.code (input_char cin) in 
           iter (i + c lsl dec) (dec + 8) (b+1)
    in iter 0 0 0


  let write_adjacencies cout g =
    let n = G.n g and m = G.m g and n' = ref 0 and m' = ref 0 in
    write_i cout UInt.byte_len ;
    write_i cout n ;
    write_i cout m ;
    G.iter_vertex (fun u ->
      incr n' ;
      write_i cout (G.node g u) ;  (* write here *)
      let du = G.out_degree g u and du' = ref 0 in
      write_i cout du ;  (* write here *)
      G.iter_succ (fun v -> 
        incr du' ; 
        incr m' ;
        if !m' < 100 || !m' mod 100 = 0 || !m' = m then
          Debug.progress !m' m "Bin.write_adjacencies n=%d m=%d" n m ;
        write_i cout (G.node g v)  ;  (* write here *)
      ) g u ;
      assert (!du' = du) ;
    ) g ;
    assert (n = !n' && m = !m') ;
    Debug.info "Wrote graph with n=%d m=%d" n m ;
    ()

  let read_adjacencies cin =
    let b_len = read_i cin in
    if b_len <> UInt.byte_len then 
      invalid_arg "Bin: wrong byte length in binary file" ;
    let n = read_i cin and m = read_i cin in 
    Debug.info ~lap:false "n %d m %d" n m ;
    let g = G.create ~n:n ~m:m () in
    let m' = ref 0 in
    for i=1 to n do
      let u = read_i cin in
      let du = read_i cin in
      if du = 0 then G.add_node g u ;
      for j=1 to du do
        let v = read_i cin in
        incr m' ; 
        if !m' < 100 || !m' mod 100 = 0 || !m' = m then
          Debug.progress !m' m "Bin.read_adjacencies n=%d m=%d" n m ;
        G.add_edge g u v ;
      done ;
    done ;
    Debug.info "Read graph with n=%d m=%d" (G.n g) (G.m g) ;
    g


  let adjacencies_txt_to_bin cin fdst =
    let lex = Lexing.from_channel cin in
    let read ?(no_eof=true) () = BasicLexer.int ~no_eof lex in 
    let cout = open_out fdst in
    let n = ref 0 and m = ref 0 in
    write_i cout UInt.byte_len ;
    write_i cout 0 ; write_i cout 0 ; (* write here, reserve space for n, m *)
    let max_u = ref min_int and min_u = ref max_int and max_deg = ref 0 in
    try
      while true do
        let u = read ~no_eof:false () in
        write_i cout u ; (* write here *)
        incr n ;
        if u < !min_u then min_u := u ;
        if u > !max_u then max_u := u ;
        let du = read () in
        write_i cout du ; (* write here *)
        if du > !max_deg then max_deg := du ;
        for j=1 to du do
          let v = read () in
          write_i cout v ; (* write here *)
          incr m ;
          if !m < 100 || !m mod 100 = 0 then
            Debug.progress !m (-1) "Bin.adjacencies_to_bin" ;
        done ;
      done ;
    with End_of_file ->
      close_out cout ;
      let fd = Unix.openfile fdst [Unix.O_WRONLY] 0o644 in
      assert (0 = Unix.lseek fd 0 Unix.SEEK_SET) ;
      let write n =
        let b = Bytes.make UInt.byte_len (Char.chr 0) in
        let low_byte = 0xff in
        let n = ref n in
        for i=0 to UInt.byte_len - 1 do
          Bytes.set b i (Char.chr (!n land low_byte)) ;
          n := !n lsr 8 ;
        done ;
        let ofs = ref 0 and len = ref UInt.byte_len in
        while !len > 0 do
          let nw = Unix.write fd b !ofs !len in
          ofs := !ofs + nw ;
          len := !len - nw ;
        done ;
      in
      write (min !n UInt.max_int) ; (* write here *)
      write  (min !m UInt.max_int) ; (* write here *)
      Unix.close fd ;
      Debug.info "Converted graph with n=%d m=%d min_u=%d max_u=%d max_deg=%d"
        !n !m !min_u !max_u !max_deg ;
      ()

end


module SizeOfComment (G : sig
  type t
  val create : ?n:int -> ?m:int -> unit -> t
end) = struct
(* Helper function for trying to get n and m from comment line. *)

  let n_eq = Str.regexp "n *=? *\\([0-9]+\\)"
  let m_eq = Str.regexp "m *=? *\\([0-9]+\\)"

  let search_int rex s default =
    try
      ignore (Str.search_forward rex s 0) ;
      int_of_string (Str.matched_group 1 s)
    with _ -> default

  type graph_info = { 
    mutable n : int ;
    mutable m : int ;
    mutable g : G.t option ;
  }

  let graph_info ?(n=4) ?(m=8) () = { n = n ; m = m ; g = None ; }

  let graph_get gi = 
    match gi.g with 
      | Some g' -> g' 
      | None ->  
        let g' = G.create ~n:gi.n ~m:gi.m () in
        gi.g <- Some g' ;
        Debug.info "Created graph with n=%d m=%d" gi.n gi.m ;
        g'

  let graph_guess_size gi s =
    match gi.g with 
      | Some _ -> () (* too late *)
      | None ->
        let n' = search_int n_eq s 4 in
        if n' > gi.n then gi.n <- n' ;
        let m' = search_int m_eq s 8 in
        if m' > gi.m then gi.m <- m' ;
        ()
end


module Txt (G : IntGraph) = struct

  let write_edges cout g =
    let n = G.n g and m = G.m g and m' = ref 0 in
    Printf.fprintf cout "# n=%d m=%d\n" n m ;
    G.iter_edges (fun u v ->
      Printf.fprintf cout "%d\t%d\n" u v ;
      incr m' ; 
      if !m' < 100 || !m' mod 100 = 0 || !m' = m then
        Debug.progress !m' m "Txt.write_edges n=%d m=%d" n m ;
    ) g ;
    assert (m = !m') ;
    Debug.info "Wrote graph with m=%d" m ;
    ()

  module Lex = BasicLexer

  module SoC = SizeOfComment (G)
  open SoC

  let read_edges cin =
    let gi = graph_info () in
    try
      let lex = Lexing.from_channel cin in
      let parse_comment s = graph_guess_size gi s in
      let m' = ref 0 in
      while true do
        let u = BasicLexer.int ~parse_comment lex in
        let v = BasicLexer.int ~parse_comment ~no_eof:true lex in
        incr m' ;
        if !m' < 100 || !m' mod 100 = 0 || !m' = gi.m then
          Debug.progress !m' gi.m "Txt.read_edges%s%s"
            (if gi.n >= 0 then Printf.sprintf " n=%d" gi.n else "") 
            (if gi.m >= 0 then Printf.sprintf " m=%d" gi.m else "") ;
        G.add_edge (graph_get gi) u v
      done ;
      graph_get gi
    with End_of_file ->
      let g = graph_get gi in
      Debug.info "Read graph with n=%d m=%d" (G.n g) (G.m g) ;
      g

  let read_edges_index cin =
    let h = Hashtbl.create 4 in
    let u_nb = ref 0 in
    let index u_name =
      try Hashtbl.find h u_name
      with Not_found ->
        let u = !u_nb in
        (* if u_name = "404377052" then Debug.info "%s is %d" u_name u ; *)
        incr u_nb ;
        Hashtbl.add h u_name u ;
        u
    in
    let gi = graph_info () in
    try
      let lex = Lexing.from_channel cin in
      let parse_comment s = graph_guess_size gi s in
      let m' = ref 0 in
      while true do
        let u = BasicLexer.string ~parse_comment lex in
        let v = BasicLexer.string ~parse_comment ~no_eof:true lex in
        incr m' ;
        if !m' < 100 || !m' mod 100 = 0 || !m' = gi.m then
          Debug.progress !m' gi.m "Txt.read_edges%s%s"
            (if gi.n >= 0 then Printf.sprintf " n=%d" gi.n else "") 
            (if gi.m >= 0 then Printf.sprintf " m=%d" gi.m else "") ;
        G.add_edge (graph_get gi) (index u) (index v)
      done ;
      graph_get gi, h
    with End_of_file ->
      let g = graph_get gi in
      Debug.info "Read graph with n=%d m=%d" (G.n g) (G.m g) ;
      g, h

  let read_adjacencies ?(n=0) ?(m=0) cin =
    let gi = graph_info ~n ~m () in
    let lex = Lexing.from_channel cin in
    let parse_comment s = graph_guess_size gi s in
    let read () = BasicLexer.int ~parse_comment ~no_eof:true lex in
    let u_max = ref (-1) and v_max = ref (-1) and deg_max = ref 0 in
    try
      let m' = ref 0 in
      while true do
        let u = BasicLexer.int ~parse_comment lex in
        if u > !u_max then u_max := u ;
        let du = read () in
        if du > !deg_max then deg_max := du ;
        G.unsafe_adj_begin (graph_get gi) u ;
        for j=1 to du do
          let v = read () in
          if v > !v_max then v_max := v ;
          incr m' ;
          if !m' < 100 || !m' mod 100 = 0 || !m' = gi.m then
            Debug.progress !m' (if gi.m > 0 then gi.m else -1)
              "Txt.read_adjacencies%s%s"
              (if gi.n > 0 then Printf.sprintf " n=%d" gi.n else "") 
              (if gi.m > 0 then Printf.sprintf " m=%d" gi.m else "") ;
          (* G.add_edge (graph_get gi) u v ; *)
          G.unsafe_adj_add (graph_get gi) u v ;
        done ;
        G.unsafe_adj_end (graph_get gi) u ;
        if m <> 0 && !m' >= m then raise End_of_file ;
      done ;
      graph_get gi
    with End_of_file ->
      let g = graph_get gi in
      Debug.info "Read graph with n=%d m=%d u_max=%d v_max=%d deg_max=%d" 
        (G.n g) (G.m g) !u_max !v_max !deg_max ;
      g

  module B = Bigarray
  module BA1 = B.Array1

  let read_adj_test ?(n=0) ?(m=0) cin =
    let gi = graph_info ~n ~m () in
    let t = Array.make (2*m) 0 in
    (* let t = BA1.create B.int32 B.c_layout (2*m) in *)
    Debug.info "array created" ;
    let lex = Lexing.from_channel cin in
    let read () = BasicLexer.int ~no_eof:true lex in
    let u_max = ref (-1) and v_max = ref (-1) and deg_max = ref 0 in
    try
      let m' = ref 0 in
      while true do
        let u = BasicLexer.int lex in
        if u > !u_max then u_max := u ;
        let du = read () in
        if du > !deg_max then deg_max := du ;
        (* if du = 0 then G.add_node (graph_get gi) u *)
        for j=1 to du do
          let v = read () in
          if v > !v_max then v_max := v ;
          incr m' ;
          if !m' < 100 || !m' mod 100 = 0 || !m' = m then
            Debug.progress !m' (if gi.m > 0 then gi.m else -1)
              "Txt.read_adjacencies%s%s"
              (if gi.n > 0 then Printf.sprintf " n=%d" gi.n else "") 
              (if gi.m > 0 then Printf.sprintf " m=%d" gi.m else "") ;
          (* G.add_edge (graph_get gi) v u ; *)
          let i = ((!m'-1) lsl 1) in
          t.(i) <- u ; t.(i+1) <- v ;
          (* t.{i} <- Int32.of_int u ; t.{i+1} <- Int32.of_int v ; *)
          if m <> 0 && !m' >= m then raise End_of_file ;
        done ;
      done ;
      graph_get gi
    with End_of_file ->
      let g = graph_get gi in
      Debug.info "Read graph with n=%d m=%d u_max=%d v_max=%d deg_max=%d" 
        (G.n g) (G.m g) !u_max !v_max !deg_max ;
      g

  let write_adjacencies cout g =
    let n = G.n g and m = G.m g and n' = ref 0 and m' = ref 0 in
    Printf.fprintf cout "# n=%d m=%d\n" n m ;
    G.iter_vertex (fun u ->
      incr n' ;
      let du = G.out_degree g u in
      if du > 0 then
        Printf.fprintf cout "%d %d" (G.node g u) du ;
      let du' = ref 0 in
      G.iter_succ (fun v ->
        incr m' ; incr du' ;
        if !m' < 100 || !m' mod 100 = 0 || !m' = m then
          Debug.progress !m' m "Txt.write_edges n=%d m=%d" n m ;
        Printf.fprintf cout " %d" (G.node g v) ;
      ) g u ;
      if du > 0 then
        Printf.fprintf cout "\n" ;
      assert (du = !du') ;
    ) g ;
    assert (m = !m') ;
    ()

end


module WeightedTxt (G : IntWeightedGraph) = struct

  let write_src_dst_wgt cout g =
    let n = G.n g and m = G.m g and m' = ref 0 in
    Printf.fprintf cout "# n=%d m=%d\n" n m ;
    G.iter_edges (fun u w v ->
      Printf.fprintf cout "%d\t%d\t%d\n" u v w ;
      incr m' ; 
      if !m' < 100 || !m' mod 100 = 0 || !m' = m then
        Debug.progress !m' m "Txt.write_edges n=%d m=%d" n m ;
    ) g ;
    assert (m = !m') ;
    Debug.info "Wrote graph with m=%d" m ;
    ()

  module Lex = BasicLexer

  module SoC = SizeOfComment (G)
  open SoC

  let read_src_dst_wgt cin =
    let gi = graph_info () in
    let u_min = ref max_int and u_max = ref min_int in
    let w_min = ref max_int and w_max = ref min_int in
    try
      let lex = Lexing.from_channel cin in
      let parse_comment s = graph_guess_size gi s in
      let m' = ref 0 in
      while true do
        let u = BasicLexer.int ~parse_comment lex in
        let v = BasicLexer.int ~parse_comment ~no_eof:true lex in
        let w = BasicLexer.int ~parse_comment ~no_eof:true lex in
        if max u v > !u_max then u_max := max u v ;
        if min u v < !u_min then u_min := min u v ;
        if w > !w_max then w_max := w ;
        if w < !w_min then w_min := w ;
        incr m' ;
        if !m' < 100 || !m' mod 100 = 0 || !m' = gi.m then
          Debug.progress !m' gi.m "Txt.read_edges%s%s"
            (if gi.n >= 0 then Printf.sprintf " n=%d" gi.n else "") 
            (if gi.m >= 0 then Printf.sprintf " m=%d" gi.m else "") ;
        G.add_edge (graph_get gi) u w v
      done ;
      graph_get gi
    with End_of_file ->
      let g = graph_get gi in
      Debug.info "Read graph with n=%d m=%d u_min=%d u_max=%d w_min=%d w_max=%d"
        (G.n g) (G.m g) !u_min !u_max !w_min !w_max ;
      g

end


let unit () =
  ()

