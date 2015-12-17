(* Laurent Viennot, Inria 2015 *)

(** Indexed graphs : nodes are numbered from [0] to [n-1].  Compact
    implementation consisting in two arrays of ints.  Graphs with [n] nodes and
    [m] edges are represented withing [2n+m] words.  Using bigarrays, graphs
    with [n<2^31] nodes and [m<2^63] edges can be represented within [12n+4m]
    bytes. *)

module type ArrayTable = sig
  type t
  type key = int
  type value
  val value_size : int (* in bits *)
  val create : int -> t
  val make : int -> value -> t
  val get : t -> int -> value
  val set : t -> int -> value -> unit
  val length : t -> int
  val blit : t -> int -> t -> int -> int -> unit
  (* val iter : (int -> int -> unit) -> t -> unit *)
  val release : t -> unit (* monitor mem usage with bigarrays *)
  val string_of_value : value -> string (* monitor vertices *)
end

module type S = sig
  type t
  type vertex = int
  val iter_succ : (vertex -> unit) -> t -> vertex -> unit
  val iter_vertex : (vertex -> unit) -> t -> unit
  val symmetric : bool

  val add_edge : t -> vertex -> vertex -> unit
  val create : ?n:int -> ?m:int -> unit -> t
  val n : t -> int
  val m : t -> int
  val iter_edges : (vertex -> vertex -> unit) -> t -> unit

  module V : ArrayTable with type value = vertex
  module I : ArrayTable with type value = int

  val out_degree : t -> vertex -> int
  val string_of_vertex : t -> vertex -> string

  val sort : t -> unit
  val sort_test_quick : t -> unit
  val reverse : t -> t
  val induced_subgraph : ?n:int -> ?m:int -> (vertex -> bool) -> t -> t

  type edge = int (* edges have indexes also *)
  module E : ArrayTable with type value = edge
  val next_succ : t -> vertex -> edge -> vertex * edge

  val unsafe_adj_begin : t -> vertex -> unit
  val unsafe_adj_add : t -> vertex -> vertex -> unit
  val unsafe_adj_end : t -> vertex -> unit
end

(** General graphs with indexed nodes. *)
module type G = sig
  include S
  type node
  val vertex : t -> node -> vertex
  val node : t -> vertex -> node
  val add_node : t -> node -> unit
  val add_edge : t -> node -> node -> unit
  val iter_edges : (node -> node -> unit) -> t -> unit

  val unsafe_adj_begin : t -> node -> unit
  val unsafe_adj_add : t -> node -> node -> unit
  val unsafe_adj_end : t -> node -> unit

  module Reverse : sig
    type g = t
    include Sig.GraphAlgo
  (** Computing [reverse g] involves copying the index of [g]. For more
      efficiency, and lower memory usage, use the following. *)
    val reverse : g -> t
  end
end

(** General graphs with indexed nodes and labeled edges. *)
module type LG = sig
  include G
  type label
  val add_edge : t -> node -> label -> node -> unit
  val unsafe_adj_add : t -> node -> label -> node -> unit
  val iter_labeled_succ : (label -> vertex -> unit) -> t -> vertex -> unit
  val iter_edges : (node -> label -> node -> unit) -> t -> unit
  module L : ArrayTable with type value = label
end

(** General graphs with indexed nodes and weighted edges. *)
module type WG = sig
  include LG
  module W : Sig.Weight with type t = label
  module WReverse : sig
    type g = t
    include Sig.GraphAlgo
  (** Computing [reverse g] involves copying the index of [g]. For more
      efficiency, and lower memory usage, use the following. *)
    val reverse : g -> t
  end
end


module Make
  (V : ArrayTable with type value = int) 
  (E : ArrayTable with type value = int) 
  : sig
    include S
    (* Index graphs are graphs. *)
    type node = int
    val vertex : t -> node -> vertex
    val node : t -> vertex -> node
    val add_node : t -> node -> unit
  end
= struct

  (* V and E provide arrays of ints for storing indexes of nodes and edges
     respectively. *)

  (* Cell in vector of edges is either :
     - a dest (if >= 0)
     - if < 0 : 
         x a short link to previous dest (index difference) 
         x or a long link to previous dest (index coded in cell & previous cell)
         x or a block of sorted dests (cell codes block size). *)

  type vertex = int
  type edge = int

  module V = V
  module E = E
  module I = V

  let () = assert (V.value_size < Sys.word_size)
  let max_node = -1 lsr (Sys.word_size - 1 - V.value_size + 1)
  let sign_bit = 1 lsl (V.value_size - 1)
  let short_lnk_bit = 1 lsl (V.value_size - 2)
  let sorted_blk_bit = 1 lsl (V.value_size - 3)
  let long_lnk_bits = -1 lsl (V.value_size - 1)
  let min_link = - short_lnk_bit
  let mask = -1 lsr (Sys.word_size - 1 - V.value_size + 3)

  let no_node = min_link
  let no_edge = -1

  let e_next_link t i v =
    if v > min_link then i+v
    else if v = no_node then no_edge 
    else if v land sorted_blk_bit <> 0 then i-1
    else (v land mask) lsl (V.value_size - 1) + V.get t (i-1)

  let e_push_link t i j =
    assert (j < i) ;
    let di = j - i in
    if di > min_link then (V.set t i di ; i+1)
    else begin
      V.set t i (j land max_node) ;
      let high = j lsr (V.value_size - 1) in
      assert (high < sorted_blk_bit) ;
      V.set t (i+1) (high lxor long_lnk_bits) ; 
      i+2
    end

  let e_sorted_block t i n =
    assert (n < sorted_blk_bit) ;
    V.set t i (n lxor long_lnk_bits lxor sorted_blk_bit)

  let e_sorted_block_size t i v =
    (*V.get t i*) v land mask

  type t = {
    mutable n : int ; (* maximum index + 1 *)
    mutable m : int ; (* nb of edges *)
    (* mutable m_sorted : int ; nb of edges in sorted blocks *)
    mutable first_succ : E.t ;
    mutable edges : V.t ;
    mutable edges_len : int ;
    mutable reverse : t option ;
  }

  let create ?(n=0) ?(m=0) () = {
    n = n ;
    m = 0 ;
    first_succ = E.make n no_edge ;
    edges = V.create (m+n) ;
    edges_len = 0 ;
    reverse = None ;
  }

  let resize g um =
    (* shrink by 3/2 -> mem waist <= n/2 ; copy time <= 3n *)
    let len = E.length g.first_succ in
    if um >= len then begin
      let len' = um + 2 + um/2 in
      let vtx = E.make len' no_edge in
      E.blit g.first_succ 0 vtx 0 g.n ;
      E.release g.first_succ ;
      g.first_succ <- vtx ;
      Debug.info ~lap:false "Resized first_succ %d to %d" len len' ;
    end ;
    let len = V.length g.edges in
    if g.edges_len + 2 >= len then begin
      let len' = len + 4 + len/2 in
      let edg = V.make len' no_node in
      V.blit g.edges 0 edg 0 g.edges_len ;
      V.release g.edges ;
      g.edges <- edg ;
      Debug.info ~lap:false "Resized edges %d to %d" len len' ;
    end

  type node = int
  let add_node g u = 
    if u >= E.length g.first_succ then resize g u ;
    if u >= g.n then g.n <- u+1
  let node g u = u
  let vertex g u = u

  let add_edge g u v =
    if 0 > u || u > max_node || 0 > v || v > max_node then 
      invalid_arg "node out of possible range" ;
    let um = max u v in
    if um >= E.length g.first_succ || g.edges_len + 1 >= V.length g.edges then 
      resize g um ;
    g.n <- max (um+1) g.n ;
    g.reverse <- None ;
    g.m <- g.m + 1 ;
    let e = g.edges_len in (* new index for edge *)
    let e_u = E.get g.first_succ u in
    let e_u, e = 
      if e_u = no_edge then (V.set g.edges e no_node ; e, e+1)
      else e_u, e  
    in
    let e =
      if e_u = e-1 (* consecutive edges from [u] *) 
      then e
      else e_push_link g.edges e e_u (* Hack linked list in [g.edges] *)
    in
    V.set g.edges e v ;
    E.set g.first_succ u e ;
    g.edges_len <- e+1 ;
    ()


  let unsafe_adj_begin g u =
    if u >= E.length g.first_succ then resize g u ;
    if u >= g.n then g.n <- u+1 ;
    V.set g.edges g.edges_len no_node ;
    g.edges_len <- g.edges_len + 1

  let unsafe_adj_add g _ v =
    if v >= E.length g.first_succ || g.edges_len >= V.length g.edges then 
      resize g v ;
    if v >= g.n then g.n <- v+1 ;
    V.set g.edges g.edges_len v ;
    g.edges_len <- g.edges_len + 1 ;
    g.m <- g.m + 1

  let unsafe_adj_end g u =
    E.set g.first_succ u (g.edges_len - 1)


  let iter_succ f g u =
    if 0 > u || u >= g.n then invalid_arg "node out of range" ;
    let rec iter e =
      if e <> no_edge then begin
        let v = V.get g.edges e in
        let e' = if v >= 0 then (f v ; e-1) else e_next_link g.edges e v in
        iter e'
      end
    in
    iter (E.get g.first_succ u)

  let next_succ g u e_last =
    if 0 > u || u >= g.n then invalid_arg "node out of range" ;
    let rec iter e =
      if e <> no_edge then begin
        let v = V.get g.edges e in
        if v >= 0 then v, e
        else iter (e_next_link g.edges e v)
      end else raise Not_found
    in
    iter (if e_last <> no_edge then e_last - 1 else E.get g.first_succ u)

  let first_succ g u = next_succ g u no_edge

  let fold_succ f g u a =
    if 0 > u || u >= g.n then invalid_arg "node out of range" ;
    let rec iter a e =
      if e = no_edge then a else begin
        let v = V.get g.edges e in
        let a, e' = 
          if v >= 0 then (f v a, e-1) 
          else a, e_next_link g.edges e v 
        in
        iter a e'
      end
    in
    iter a (E.get g.first_succ u)



  let min_dicho = 7

  let reverse g =
    match g.reverse with
      | Some h -> h
      | _ ->
        let h = create ~n:g.n ~m:0 () in
        (* Degrees : *)
        let edges_len = ref 0 in
        for u=0 to g.n-1 do
          iter_succ (fun v -> 
            let d = E.get h.first_succ v in
            incr edges_len ;
            if d = no_edge then incr edges_len ;
            let d = if d = no_edge then 1 else d+1 in
            E.set h.first_succ v d ;
            if d mod mask = min_dicho then incr edges_len ;
          ) g u ;
        done ;
        h.edges_len <- !edges_len ;
        h.edges <- V.make !edges_len no_node ;
        Debug.info "Reverse computed degrees n=%d m=%d edges_len=%d mask=%d" 
          g.n g.m !edges_len mask ;
        (* Prefix sums : *)
        let sum = ref 0 in
        for v=0 to h.n-1 do
          let d = E.get h.first_succ v in
          if d <> no_edge then begin
            (* Dft val : V.set h.edges !sum no_node ;*) (* End block word *)
            E.set h.first_succ v !sum ;
            let hdr = ref 0 in
            let rec iter pos deg = (* Sorted block headers *)
              if deg < min_dicho then () 
              else if deg <= mask then begin
                e_sorted_block h.edges (pos+deg) deg ;
                incr hdr ;
              end else begin
                e_sorted_block h.edges (pos+mask) mask ;
                incr hdr ;
                iter (pos+mask+1) (deg-mask)
              end
            in iter (!sum+1) d ;
            sum := !sum + (d+1+ !hdr) ; (* end block word & srt. blck headers *)
          end
        done ;
        assert (!sum = !edges_len) ;
        Debug.info "Reverse computed degree sums %d for m=%d" !sum g.m ;
        (* Edges : *)
        let nedg = ref 0 in
        for u=g.n-1 downto 0 do
          iter_succ (fun v ->
            let e = 1 + E.get h.first_succ v in
            V.set h.edges e u ;
            (* Point to sorted header if any: *)
            let e = 
              if e+1 < !edges_len && V.get h.edges (e+1) <> no_node 
              then e+1 else e 
            in
            E.set h.first_succ v e ;
            incr nedg ;
            if !nedg < 100 || !nedg mod 100 = 0 || !nedg = g.m then
              Debug.progress !nedg g.m "Reverse edges" ; 
          ) g u ;
        done ;
        h.m <- g.m ;
        Debug.info "Reverse computed edges" ;
        (* Check : *)
        let pos = ref (h.edges_len - 1) in
        let check_dest v prev =
          assert (v >= 0) ;
          (match !prev with Some v' -> assert (v' <= v) |_-> ()) ;
          prev := Some v ;
        in
        for u=h.n-1 downto 0 do
          let e = E.get h.first_succ u in
          if e <> no_edge then begin
            assert (e = !pos) ;
            let rec iter e prev =
              let v = V.get h.edges e in
              if v = no_node then e - 1 else begin
                let e' = if v >= 0 then begin
                  check_dest v prev ;
                  e - 1
                end else begin
                  assert (e_next_link h.edges e v = e-1) ;
                  let d = e_sorted_block_size h.edges e v in
                  let e' = e - d in
                  for e=e-1 downto e' do
                    check_dest (V.get h.edges e) prev ;
                  done ;
                  e' - 1
                end in
                iter e' prev
              end
            in
            pos := iter e (ref None) ;
          end ;
        done ;
        Debug.info "Reverse checked" ;
        g.reverse <- Some h ;
        h


  let n g = g.n

  let m g = g.m

  let out_degree g u = fold_succ (fun _ d -> d+1) g u 0

  let string_of_vertex _ u = string_of_int u

  let iter_vertex f g =
    for u=0 to g.n - 1 do f u done

  let iter_edges f g = iter_vertex (fun u -> iter_succ (f u) g u) g

  let symmetric = false

  let sort g =
    let h = reverse g in
    E.release g.first_succ ;
    V.release g.edges ;
    g.first_succ <- E.create 0 ; 
    g.edges <- V.create 0 ;
    let g' = reverse h in
    g.first_succ <- g'.first_succ ;
    g.edges <- g'.edges ;
    g.edges_len <- g'.edges_len ;
    ()


  let induced_subgraph ?(n=0) ?(m=0) pred g =
    let h = create ~n ~m:(g.m * n / (1+g.n)) () in
    let nedg = ref 0 in
    iter_edges (fun u v ->
      incr nedg ;
      if !nedg < 100 || !nedg mod 100 = 0 || !nedg = g.m then
        Debug.progress !nedg g.m "Induced_subgraph %d / %d" n g.n ;
      if pred u && pred v then add_edge h u v
    ) g ;
    h


  let sort_test_quick g =
    let t = V.make (2*g.m) no_node in
    let i = ref 0 in
    iter_edges (fun u v -> V.set t !i u ; incr i ; V.set t !i v ; incr i) g ;
    Debug.info "Edge array m=%d" g.m ;
    let compare i j =
      let i = i lsl 1 and j = j lsl 1 in
      let c = V.get t (i+1) - V.get t (j+1) in
      if c <> 0 then c 
      else V.get t i - V.get t j
    in
    let swap i j =
      let i = i lsl 1 and j = j lsl 1 in
      let tmp = V.get t i and tmp' = V.get t (i+1) in
      V.set t i (V.get t j) ; V.set t (i+1) (V.get t (j+1)) ;
      V.set t j tmp ; V.set t (j+1) tmp' ;
    in
  let partition l r =
    let l0 = l and r0 = r in
    let ipiv = (l + r) / 2 in
    swap ipiv r0 ;
    let ipiv = r0 in
    let l = ref l and r = ref (pred r) in
    while !l < !r do
      while !l < r0 && compare !l ipiv < 0 do incr l done ;
      while !r > l0 && compare !r ipiv > 0 do decr r done ;
      if !l < !r then begin
        swap !l !r ;
        decr r ;
      end
    done ;
    if compare !l ipiv < 0 then incr l ;
    swap !l ipiv ;
    !l
  in
  let l0 = 0 and r0 = g.m-1 in
  let rec sort l r =
    if l < r then begin
      Debug.progress (l-l0) (r0-l0) "edge_sorting" ;
      let j = partition l r in
      sort l (pred j) ;
      sort (succ j) r ;
    end
  in
  sort l0 r0 ;
  Debug.info "sorted" ;
  ()

end




module Weighted
  (V : ArrayTable with type value = int) 
  (E : ArrayTable with type value = int) 
  (NodeIndex : Sig.Table with type value = int)
  (NodeArray : ArrayTable with type value = NodeIndex.key)
  (LabelArray : ArrayTable) 
  (W : Sig.Weight with type t = LabelArray.value)
  : WG with type node = NodeIndex.key and type label = LabelArray.value
= struct

  type vertex = int
  type edge = int
  type node = NodeIndex.key
  type label = LabelArray.value

  module V = V
  module E = E
  module I = V
  module L = LabelArray
  module W = W

  let () = assert (V.value_size < Sys.word_size)
  let max_node = -1 lsr (Sys.word_size - 1 - V.value_size + 1)
  let sign_bit = 1 lsl (V.value_size - 1)
  let short_lnk_bit = 1 lsl (V.value_size - 2)
  let sorted_blk_bit = 1 lsl (V.value_size - 3)
  let long_lnk_bits = -1 lsl (V.value_size - 1)
  let min_link = - short_lnk_bit
  let mask = -1 lsr (Sys.word_size - 1 - V.value_size + 3)

  let no_node = min_link
  let no_edge = -1

  let e_next_link t i v =
    if v > min_link then i+v
    else if v = no_node then no_edge 
    else if v land sorted_blk_bit <> 0 then i-1
    else (v land mask) lsl (V.value_size - 1) + V.get t (i-1)

  let e_push_link t i j =
    assert (j < i) ;
    let di = j - i in
    if di > min_link then (V.set t i di ; i+1)
    else begin
      V.set t i (j land max_node) ;
      let high = j lsr (V.value_size - 1) in
      assert (high < sorted_blk_bit) ;
      V.set t (i+1) (high lxor long_lnk_bits) ; 
      i+2
    end

  let e_sorted_block t i n =
    assert (n < sorted_blk_bit) ;
    V.set t i (n lxor long_lnk_bits lxor sorted_blk_bit)

  let e_sorted_block_size t i v =
    (*V.get t i*) v land mask

  type t = {
    mutable n : int ; (* maximum index + 1 *)
    mutable m : int ; (* nb of edges *)
    (* mutable m_sorted : int ; nb of edges in sorted blocks *)
    mutable first_succ : E.t ;
    mutable last_succ : E.t ;
    mutable edges : V.t ;
    mutable edges_len : int ;
    mutable current_source : int ;
    mutable reverse : t option ;
    mutable index : NodeIndex.t ;
    mutable nodes : NodeArray.t ;
    mutable labels : L.t
  }

  let create ?(n=0) ?(m=0) () = {
    n = 0 ;
    m = 0 ;
    first_succ = E.make n no_edge ;
    last_succ = E.create n ;
    edges = V.create m ;
    edges_len = 0 ;
    current_source = no_node ;
    reverse = None ;
    index = NodeIndex.create n ;
    nodes = NodeArray.create n ;
    labels = L.create m ;
  }

  let resize_node g um =
    (* shrink by 3/2 -> mem waist <= n/2 ; copy time <= 3n *)
    let len = E.length g.first_succ in
    let len' = um + 2 + um/2 in
    let fsucc' = E.make len' no_edge in
    E.blit g.first_succ 0 fsucc' 0 g.n ;
    E.release g.first_succ ;
    g.first_succ <- fsucc' ;
    let lsucc' = E.create len' in
    E.blit g.last_succ 0 lsucc' 0 g.n ;
    E.release g.last_succ ;
    g.last_succ <- lsucc' ;
    let nds' = NodeArray.create len' in
    NodeArray.blit g.nodes 0 nds' 0 g.n ;
    NodeArray.release g.nodes ;
    g.nodes <- nds' ;
    Debug.info ~lap:false "Resized nodes %d to %d for n=%d" len len' (um+1) ;
    ()

  let resize_edge g =
    let len = V.length g.edges in
    let len' = len + 2 + len/2 in
    let edg = V.make len' no_node in
    V.blit g.edges 0 edg 0 g.edges_len ;
    V.release g.edges ;
    g.edges <- edg ;
    Debug.info ~lap:false "Resized edges %d to %d" len len' ;
    let lbl' = L.create len' in
    L.blit g.labels 0 lbl' 0 g.edges_len ;
    L.release g.labels ;
    g.labels <- lbl' ;
    Debug.info ~lap:false "Resized labels %d to %d" len len' ;
    ()

  let vertex g x = NodeIndex.get g.index x

  let node g u = NodeArray.get g.nodes u

  let add_node g x =
    try 
      NodeIndex.get g.index x
    with Not_found ->
      let u = g.n in (* new node *)
      if u >= E.length g.first_succ then resize_node g u ;
      NodeIndex.set g.index x u ;
      NodeArray.set g.nodes u x ;
      g.n <- u + 1 ;
      u

  let add_edge g x lbl y =
    let u = add_node g x in
    let v = add_node g y in
    if 0 > u || u > max_node || 0 > v || v > max_node then 
      invalid_arg "node out of possible range" ;
    if g.edges_len + 2 >= V.length g.edges then resize_edge g ;
    let e = g.edges_len in (* new index for edge *)
    let e_u = E.get g.first_succ u in
    let e_u = if e_u = no_edge then (E.set g.last_succ u e ; e-1) else e_u in
    let e =
      if e_u = e-1 (* consecutive edges from [u] *) 
      then e
      else e_push_link g.edges e e_u (* Hack linked list in [g.edges] *)
    in
    V.set g.edges e v ;
    L.set g.labels e lbl ;
    E.set g.first_succ u e ;
    g.m <- g.m + 1 ;
    g.edges_len <- e+1 ;
    g.reverse <- None


  let unsafe_adj_begin g x =
    let u = add_node g x in
    E.set g.last_succ u g.edges_len ;
    g.current_source <- u

  let unsafe_adj_add g x lbl y =
    assert (g.current_source = NodeIndex.get g.index x) ;
    let v = add_node g y in
    if g.edges_len + 2 >= V.length g.edges then resize_edge g ;
    if v >= g.n then g.n <- v+1 ;
    let e = g.edges_len in
    V.set g.edges e v ;
    L.set g.labels e lbl ;
    g.edges_len <- e + 1 ;
    g.m <- g.m + 1

  let unsafe_adj_end g x =
    let u = NodeIndex.get g.index x in
    assert (g.current_source = u) ;
    E.set g.first_succ u (g.edges_len - 1) ;
    g.current_source <- no_node


  let iter_succ f g u =
    if 0 > u || u >= g.n then invalid_arg "node out of range" ;
    let first = E.get g.first_succ u in
    if first <> no_edge then begin
      let last = E.get g.last_succ u in
      let rec iter e =
        let v = V.get g.edges e in
        let e' = if v >= 0 then (f v ; e-1) else e_next_link g.edges e v in
        if e <> last then iter e'
      in
      iter first
    end

  let iter_labeled_succ f g u =
    if 0 > u || u >= g.n then invalid_arg "node out of range" ;
    let first = E.get g.first_succ u in
    if first <> no_edge then begin
      let last = E.get g.last_succ u in
      let rec iter e =
        let v = V.get g.edges e in
        let e' = 
          if v >= 0 then (f (L.get g.labels e) v ; e-1) 
          else e_next_link g.edges e v in
        if e <> last then iter e'
      in
      iter first
    end



  let next_succ g u e_last =
    if 0 > u || u >= g.n then invalid_arg "node out of range" ;
    if e_last = E.get g.last_succ u then raise Not_found ;
    let rec iter e =
      if e <> no_edge then begin
        let v = V.get g.edges e in
        if v >= 0 then v, e
        else iter (e_next_link g.edges e v)
      end else raise Not_found
    in
    iter (if e_last <> no_edge then e_last - 1 else E.get g.first_succ u)

  let first_succ g u = next_succ g u no_edge

  let fold_succ f g u a =
    if 0 > u || u >= g.n then invalid_arg "node out of range" ;
    let first = E.get g.first_succ u in
    if first = no_edge then a else begin
      let last = E.get g.last_succ u in
      let rec iter a e =
        let v = V.get g.edges e in
        let a, e' = 
          if v >= 0 then (f v a, e-1) 
          else a, e_next_link g.edges e v 
        in
        if e = last then a else iter a e'
      in
      iter a first
    end


  let reverse ?(copy_labels=true) g =
    match g.reverse with
      | Some h -> h
      | _ ->
        let h = create ~n:0 ~m:0 () in
        h.index <- g.index ;
        h.nodes <- g.nodes ;
        Debug.info "Reverse: creating graph n=%d m=%d" g.n g.m ;
          (* Degrees : *)
        h.n <- g.n ;
        h.first_succ <- E.make g.n 0 ;
        h.last_succ <- E.create g.n ;
        let edges_len = ref 0 in
        for u=0 to g.n-1 do
          iter_succ (fun v -> 
            incr edges_len ;
            let d = E.get h.first_succ v in
            E.set h.first_succ v (d+1) ;
          ) g u ;
        done ;
        assert (!edges_len = g.m) ;
        h.m <- !edges_len ;
        h.edges_len <- !edges_len ;
        h.edges <- V.create !edges_len ;
        h.labels <- L.create (if copy_labels then !edges_len else 0) ;
        Debug.info "Reverse: computed degrees" ;
          (* Prefix sums : *)
        let sum = ref 0 in
        for v=0 to h.n-1 do
          let d = E.get h.first_succ v in
          if d <> 0 then begin
            E.set h.last_succ v !sum ;
            E.set h.first_succ v (!sum-1) ; (* will increase as we add edges *)
            sum := !sum + d ; (* end block word & srt. blck headers *)
          end else
            E.set h.first_succ v no_edge ;
        done ;
        assert (!sum = !edges_len) ;
        Debug.info "Reverse: computed degree sums" ;
          (* Edges : *)
        let nedg = ref 0 in
        for u=g.n-1 downto 0 do
          iter_labeled_succ (fun lbl v ->
            let e = 1 + E.get h.first_succ v in
            V.set h.edges e u ;
            E.set h.first_succ v e ;
            if copy_labels then L.set h.labels e lbl ;
            incr nedg ;
            if !nedg mod 100 = 0 || !nedg = g.m then
              Debug.progress !nedg g.m "Reverse edges" ; 
          ) g u ;
        done ;
        Debug.info "Reverse: computed edges" ;
          (* Check : *)
        let pos = ref (h.edges_len - 1) in
        let check_dest v prev =
          assert (v >= 0) ;
          match prev with Some v' -> assert (v' <= v) | _ -> ()
        in
        for u=h.n-1 downto 0 do
          let e = E.get h.first_succ u in
          if e <> no_edge then begin
            assert (e = !pos) ;
            let last = E.get h.last_succ u in
            let rec iter e prev =
              let v = V.get h.edges e in
              assert (v >= 0) ;
              check_dest v prev ;
              if e = last then e-1 else iter (e-1) (Some v)  
            in
            pos := iter e None ;
          end ;
        done ;
        Debug.info "Reverse: checked" ;
        g.reverse <- Some h ;
        h


  let n g = g.n

  let m g = g.m

  let iter_vertex f g =
    for u=0 to g.n - 1 do f u done

  let iter_edges f g = 
    iter_vertex (fun u -> 
      iter_labeled_succ (fun lbl v -> f (node g u) lbl (node g v)) g u
    ) g

  let symmetric = false

  let out_degree g u = fold_succ (fun _ d -> d+1) g u 0

  let string_of_vertex g u =
    NodeArray.string_of_value (node g u)

  module Reverse = struct
    type g = t
    type t = g
    type vertex = int

    let n = n
    let m = m
    let iter_vertex = iter_vertex
    let iter_succ = iter_succ
    let symmetric = false
    let string_of_vertex = string_of_vertex

    module V = V
    module I = I

    let reverse g = reverse ~copy_labels:false g

  end

  module WReverse = struct
    type g = t
    type t = g
    type vertex = int

    let n = n
    let m = m
    let iter_vertex = iter_vertex
    let iter_succ = iter_succ
    let symmetric = false
    let string_of_vertex = string_of_vertex

    module V = V
    module I = I

    type label = L.value
    let iter_labeled_succ = iter_labeled_succ
    module L = L
    module W = W

    let reverse g = reverse ~copy_labels:true g
  end

  let sort g =
    let h = reverse g in
    E.release g.first_succ ;
    g.first_succ <- E.create 0 ;
    E.release g.last_succ ;
    g.last_succ <- E.create 0 ;
    V.release g.edges ;
    g.edges <- V.create 0 ;
    L.release g.labels ;
    g.labels <- L.create 0 ;
    let g' = reverse h in
    g.first_succ <- g'.first_succ ;
    g.last_succ <- g'.last_succ ;
    g.edges <- g'.edges ;
    g.edges_len <- g'.edges_len ;
    ()

  let sort_test_quick g = failwith "not here"


  let reverse_copy_index g =
    let h = reverse ~copy_labels:true g in
    h.index <- NodeIndex.create h.n ;
    h.nodes <- NodeArray.create h.n ;
    iter_vertex (fun u ->
      let x = node g u in
      NodeIndex.set h.index x u ;
      NodeArray.set h.nodes u x ;
    ) g ;
    h

  let reverse g = reverse g


  let add_node g x = ignore (add_node g x)

  let induced_subgraph ?(n=0) ?(m=0) pred g =
    let h = create ~n ~m () in
    let nedg = ref 0 in
    iter_vertex (fun u ->
      add_node h (node g u) ;
      iter_labeled_succ (fun lbl v ->
        if pred u && pred v then add_edge h (node g u) lbl (node g v) ;
        incr nedg ;
        if !nedg mod 100 = 0 || !nedg = g.m then
          Debug.progress !nedg g.m "Induced_subgraph %d / %d" n g.n ;
      ) g u ;
    ) g ;
    h

end



module IntNativeArray = struct
  type t = int array
  type key = int
  type value = int
  let value_size = Sys.word_size - 1
  include Array
  let create n = make n 0
  let release _ = () (* Gc stats get it. *)
  let iter = iteri
  let string_of_value = string_of_int
end


include Make (IntNativeArray) (IntNativeArray)

module IntIndex = struct
  type t = (int, int) Hashtbl.t
  type key = int
  type value = int
  let create n = Hashtbl.create n
  let set = Hashtbl.replace
  let get = Hashtbl.find
end

module UnitArray = struct
  type t = unit
  type key = int
  type value = unit
  let value_size = 0
  let create _ = ()
  let make _ () = ()
  let get () _ = ()
  let set () _ () = ()
  let length () = max_int
  let blit () _ () _ _ = ()
  let release () = ()
  let string_of_value () = "_"
end

module Unweighted = struct
  type t = unit
  let zero = ()
  let infinity = ()
  let compare () () = 0
  let add () () = ()
  let to_string () = "_"
end

module IntGraph = struct

  include Weighted (IntNativeArray) (IntNativeArray)
    (IntIndex) (IntNativeArray)
    (UnitArray) (Unweighted)

  let add_edge g x y = add_edge g x () y
  let unsafe_adj_add g x y = unsafe_adj_add g x () y
  let iter_edges f g = iter_edges (fun x _ y -> f x y) g

end

module IntWeight = struct
  type t = int
  let zero = 0
  let infinity = max_int
  let compare w w' = w - w'
  let add w w' =
    if w = infinity || w' = infinity then infinity else
      let s = w + w' in
      if s < 0 || s = infinity then failwith "weight_overflow" else
        s
  let to_string = string_of_int
end

module IntWeightedGraph = struct
  include Weighted (IntNativeArray) (IntNativeArray)
    (IntIndex) (IntNativeArray) (IntNativeArray) (IntWeight)
  module WT = L
end
