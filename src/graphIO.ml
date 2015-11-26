(* Laurent Viennot, Inria 2015 *)

(** Write and read graphs. *)

module Gen = struct

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

end

include Gen

module type UIntCoding = sig
  val byte_len : int
  val max_int : int
end

(** Binary format : [n m u_0 deg_0 v_01 ... v_0{deg_0} u_1 deg_1 v_11 ... ]. *)
module Bin (G : IntDigraph.S) (UInt : UIntCoding) = struct

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
      write_i cout u ;  (* write here *)
      let du = G.out_degree g u and du' = ref 0 in
      write_i cout du ;  (* write here *)
      G.iter_succ (fun v -> 
        incr du' ; 
        incr m' ;
        Debug.progress !m' m "Bin.write_adjacencies n=%d m=%d" n m ;
        write_i cout v  ;  (* write here *)
      ) g u ;
      assert (!du' = du) ;
    ) g ;
    assert (n = !n' && m = !m') ;
    Debug.info ~lap:true "Wrote graph with n=%d m=%d" n m ;
    ()

  let read_adjacencies cin =
    let b_len = read_i cin in
    if b_len <> UInt.byte_len then 
      invalid_arg "Bin: wrong byte length in binary file" ;
    let n = read_i cin and m = read_i cin in Debug.info "n %d m %d" n m ;
    let g = G.create (* ~n:n ~m:m *) () in
    let m' = ref 0 in
    for i=1 to n do
      let u = read_i cin in
      let du = read_i cin in
      for j=1 to du do
        let v = read_i cin in
        incr m' ; 
        Debug.progress !m' m "Bin.read_adjacencies n=%d m=%d" n m ;
        G.add_edge g u v ;
      done ;
    done ;
    Debug.info ~lap:true "Read graph with n=%d m=%d" (G.n g) (G.m g) ;
    g


  let adjacencies_txt_to_bin cin fdst =
    let lex = Lexing.from_channel cin in
    let read ?(no_eof=true) () = BasicLexer.int ~no_eof lex in 
    let cout = cout_of_filename fdst in
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
      Debug.info ~lap:true 
        "Converted graph with n=%d m=%d min_u=%d max_u=%d max_deg=%d"
        !n !m !min_u !max_u !max_deg ;
      ()

end


module Txt (G : IntDigraph.S) = struct

  let write_edges cout g =
    let n = G.n g and m = G.m g and m' = ref 0 in
    Printf.fprintf cout "# n=%d m=%d\n" n m ;
    G.iter_edges (fun u v ->
      Printf.fprintf cout "%d\t%d\n" u v ;
      incr m' ; 
      Debug.progress !m' m "Txt.write_edges n=%d m=%d" n m ;
    ) g ;
    assert (m = !m') ;
    Debug.info ~lap:true "Wrote graph with m=%d" m ;
    ()

  module Lex = BasicLexer


(* Helper function for trying to get n and m from comment line. *)

  let n_eq = Str.regexp "m *=? *\\([0-9]+\\)"
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

  let graph_info () = { n = -1 ; m = -1 ; g = None ; }

  let graph_get gi = 
    match gi.g with 
      | Some g' -> g' 
      | None ->  
        let g' = G.create ~n:(max gi.n 4) ~m:(max gi.m 8) () in
        gi.g <- Some g' ;
        g'

  let graph_guess_size gi s =
Debug.info "com : %s" s ;
    match gi.g with 
      | Some _ -> () (* too late *)
      | None ->
        let n' = search_int n_eq s 4 in
        if n' >= 0 then gi.n <- n' ;
        let m' = search_int m_eq s 8 in
        if m' >= 0 then gi.m <- m' ;
        ()

(* Ouf ! Now we go : *)

  let read_edges cin =
    let gi = graph_info () in
    try
      let lex = Lexing.from_channel cin in
      let parse_comment s = graph_guess_size gi s in
      let m' = ref 0 in
      while true do
        let u = BasicLexer.int ~parse_comment lex 
        and v = BasicLexer.int ~parse_comment ~no_eof:true lex in
        incr m' ;
        Debug.progress !m' gi.m "Txt.read_edges%s%s"
          (if gi.n >= 0 then Printf.sprintf " n=%d" gi.n else "") 
          (if gi.m >= 0 then Printf.sprintf " m=%d" gi.m else "") ;
        G.add_edge (graph_get gi) u v
      done ;
      graph_get gi
    with End_of_file ->
      let g = graph_get gi in
      Debug.info ~lap:true "Read graph with n=%d m=%d" (G.n g) (G.m g) ;
      g


  let read_adjacencies cin =
    let gi = graph_info () in
    let lex = Lexing.from_channel cin in
    let parse_comment s = graph_guess_size gi s in
    let read () = BasicLexer.int ~parse_comment ~no_eof:true lex in 
    try
      let m' = ref 0 in
      while true do
        let u = BasicLexer.int ~parse_comment lex in
        let du = read () in
        for j=1 to du do
          let v = read () in
          incr m' ;
          Debug.progress !m' gi.m "Txt.read_adjacencies%s%s"
            (if gi.n >= 0 then Printf.sprintf " n=%d" gi.n else "") 
            (if gi.m >= 0 then Printf.sprintf " m=%d" gi.m else "") ;
          G.add_edge (graph_get gi) u v ;
        done ;
      done ;
      graph_get gi
    with End_of_file ->
      let g = graph_get gi in
      Debug.info ~lap:true "Read graph with n=%d m=%d" (G.n g) (G.m g) ;
      g

  let write_adjacencies cout g =
    let n = G.n g and m = G.m g and n' = ref 0 and m' = ref 0 in
    Printf.fprintf cout "# n=%d m=%d\n" n m ;
    G.iter_vertex (fun u ->
      incr n' ;
      let du = G.out_degree g u in
      Printf.fprintf cout "%d %d" u du ;
      let du' = ref 0 in
      G.iter_succ (fun v ->
        incr m' ; incr du' ;
        Debug.progress !m' m "Txt.write_edges n=%d m=%d" n m ;
        Printf.fprintf cout " %d" v ;
      ) g u ;
      Printf.fprintf cout "\n" ;
      assert (du = !du') ;
    ) g ;
    assert (m = !m') ;
    ()

end


let unit () =
  ()

