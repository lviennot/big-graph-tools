(* Laurent Viennot, Inria 2015 *)

(** [n]-bounded Digraphs with multiple edges.
    
    Vertices are integers in the range [0..n-1]. Edges are ordered pairs of
    vertices.  The representation is suited for dense set of vertices ([n]
    should not be too big compared to the actual number of vertices).

    The intended usage is construct, sort, and then compute :
    add edges to a graph, sort its edges, then make efficient computations on
    the graph (without modifying it).

    The representation uses basically two int arrays and has pretty compact
    memory usage: [2n + m] words if [m] edges with same source are added
    consecutively, and [2(n + m)] words at most otherwise.  (Assumes [n <=
    max_int], and [m <= max_int].)
    
    It amounts to an array of adjency lists when edges are added in arbitrary
    order, and to an array of adjency arrays when edges are sorted.
    
    A call to [sort g] sorts lexicographically the edges in [O(n+m)] time.  (If
    edges were already sorted then nothing is done.)  This reduces out-degree
    computation to [O(1)] time and edge detection to [O(log degree)] time
    (instead of [O(degree)] in both cases).  Additionnally, it reduces the size
    to [2n + m] words and the reverse graph can be obtained as a byproduct of
    the computation.
    
    A guess [n'] of [n] must be provided at graph creation (a table of [2n']
    words is then provisioned). If edges are inserted for higher indices the
    table grows if it is dense engough. Otherwise (sparse indices), a hashtable
    is used (with additional cost of roughly 5n words).
    
    Module [IntLabeled] provides graphs with int labels (both on edges and
    vertices).  Module [Labeled] provides graphs with labels of any type.
    
    When [n < 2^31] and [m < 2^48], functor [Compact] can provide graphs within
    [16n + 4m] bytes using bigarrays.
    
    When predecessors are accessed, the reverse graph is computed in [O(n+m)]
    time and the graph size doubles.

    Example :

    {[
    let module G = IntDigraph in
    let g = G.create () in
    List.iter (fun (u,v) -> G.add_edge g u v) 
    [2,4; 1,8; 1,2; 2,4; 2,3; 3,4; 2,5; 4,1; 2,9; 1,9; 2,8; 1,7; 2,4; 2,7;] ;
    G.add_vertex g 17 ;
    G.sort g ;
    Printf.printf "-- Diraph with %d nodes and %d edges :\n%s" (G.n_mem g) (G.m g) (G.to_string g) ;
    let degrees = Array.make (G.n g) 0 in
    G.iter_vertex (fun u -> degrees.(u) <- G.in_degree g u + G.out_degree g u) g ;
    ]}

*)

(** Signature for digraphs. *)
module type S = sig
  type t
(*
  type index = int
  type vertex (* usually [index] or [index * v_label] *)
  type edge (* usually [index * index] or [vertex * e_label * vertex ] *)
*)

  val create : ?n:int -> ?m:int -> unit -> t

  val n : t -> int (* bound n on indices in graph : u < n for every vertex u *)
  val n_mem : t -> int (* number of vertices actually added, dsty = n_mem/n *)
  val m : t -> int (* number of edges *)

  val add_vertex : t -> int -> unit
  val del_vertex : t -> t -> int -> unit (* need reverse graph to get preds *)
  val mem_vertex : t -> int -> bool

  val add_edge : t -> int -> int -> unit
  val del_edge : ?only_first:bool -> t -> int -> int -> unit
  val mem_edge : t -> int -> int -> bool
  val multiplicity_edge : t -> int -> int -> int
  val out_degree : t -> int -> int

  val reverse : ?store_reverse:bool -> t -> t
  val symmetrize : t -> unit
  val sort : ?store_reverse:bool -> t -> unit

  val iter_succ : (int -> unit) -> t -> int -> unit
  val fold_succ : (int -> 'a -> 'a) -> t -> int -> 'a -> 'a
  val iter_vertex : (int -> unit) -> t -> unit
  val fold_vertex : (int -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_edges : (int -> int -> unit) -> t -> unit
  val fold_edges : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a

  (** Access predecessor information. 

      The first call to one of the following functions costs the computation of
      the reverse graph (O(n+m) time).  This occurs after any edge insertion or
      deletion.  *)

  val in_degree : t -> int -> int
  val iter_pred : (int -> unit) -> t -> int -> unit
  val fold_pred : (int -> 'a -> 'a) -> t -> int -> 'a -> 'a

(* debug : *)
  val dump : out_channel -> t -> unit
  val to_string : t -> string

end

(** Signature for unbounded indexed digraphs. *)
module type US = sig
  include S
  type vertex
  val index : t -> vertex -> int
  val vertex : t -> int -> vertex
  val add_vertex : t -> vertex -> int (* return the index of the vertex *)
end

(** Signature for labeled digraphs. *)
module type LS = sig
  include S
  type v_label
  type e_label
  val add_vertex_l : t -> int -> v_label -> unit
  val vertex_l : t -> int -> v_label
  val add_edge_l : t -> int -> e_label -> int -> unit
  val find_edge_l : t -> int -> int -> e_label
  val find_all_edges_l : t -> int -> int -> e_label list
  val fold_multi_edge_l : (e_label -> 'a -> 'a) -> t -> int -> int -> 'a -> 'a
  val iter_succ_l : (e_label -> int -> unit) -> t -> int -> unit
  val fold_succ_l : (e_label -> int -> 'a -> 'a) -> t -> int -> 'a -> 'a
  val iter_vertex_l : (int -> v_label -> unit) -> t -> unit
  val fold_vertex_l : (int -> v_label -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_edges_l : (int -> e_label -> int -> unit) -> t -> unit
  val fold_edges_l : (int -> e_label -> int -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_pred_l : (e_label -> int -> unit) -> t -> int -> unit
  val fold_pred_l : (e_label -> int -> 'a -> 'a) -> t -> int -> 'a -> 'a
  (* Indexed digraphs : labels are also identifiers. *)
  val label_find_vertex : t -> v_label -> int
  val label_add_vertex : t -> v_label -> int (* Create vrtx if label unknonw. *)
  val label_add_edge : t -> v_label -> v_label -> unit 
  val label_add_edge_l : t -> v_label -> e_label -> v_label -> unit 
  val to_string_l : 
    (v_label -> string) -> ?elab_to_str:(e_label -> string) ->t -> string

end


(* Vector where we store edges of a graph : *)
module type EdgeVec = sig
  type t
  type label
  val default_dest : int (* non-vertex *)
  val default_label : label
  val label : t -> int -> label
  val dest : t -> int -> int
  val label_dest : t -> int -> label * int
  val set_label : t -> int -> label -> unit
  val set_dest : t -> int -> int -> unit
  val set_label_dest : t -> int -> label * int -> unit
  val make : int -> t
  val length : t -> int (* 1 + highest index set *)
end

(* Vector with information associated to vertices : *)
module type VertexVec = sig
  type t
  type label
  val default_edge : int (* default index for non-edge *)
  val default_label : label
  val edge_begin : t -> int -> int (* index of first out-edge *)
  val edge_end : t -> int -> int (* index of last out-edge *)
  val edge_bounds : t -> int -> int * int (* first, last *)
  val label : t -> int -> label
  val index : t -> label -> int (* implemented in indexed digraphs *)
  val set_edge_begin : t -> int -> int -> unit
  val set_edge_end : t -> int -> int -> unit
  val set_edge_bounds : t -> int -> int * int -> unit
  val set_label : t -> int -> label -> unit
  val make : int -> t
  val length : t -> int (* 1 + highest index set *)
end

module Make (V : VertexVec) (E : EdgeVec) 
  : LS with type v_label = V.label and type e_label = E.label
= struct

  (* Invariants :
   *
   * For edge index [i] with [e.(i) = v, l] : 
   * - [v >= 0] and [e(i)] is an edge with destination [v] and label [l]
   *     or [v < 0] and [l = E.default_label] indicating that next edge in
   *     current adjacency list is at index [i-v].
   *
   * For node index [u] with [v.(u) = u_beg, u_end, l], we have : 
   * - [u_beg = u_end] (empty successor list) or edge [e.(u_end) = v, l] with 
   *   [v >= 0] (last successor) or [v = -1] (last successor  has been deleted);
   * - [u_beg = u_end = V.default_edge] and [l = V.default_label] when [u] has 
   *     not been inserted in the graph, or [u_beg <> u_end 
   *     || u_beg,u_end <> V.default_edge,V.default_edge] otherwise.
   * 
   * We obtain unlabeled graphs by using modules [V] and [E] with unit
   * label type (and adequate storage of elements). *)

  type v_label = V.label
  type e_label = E.label

  type t = {
    mutable n : int ;
    mutable m : int ;
    mutable v : V.t ;
    mutable e : E.t ;
    mutable packed : bool ; (* When all successors of every vertex are stored
                               consecutively in [e]. *)
    mutable sorted : bool ; (* When packed and successors are in non-decreasing
                               order. *)
    mutable last_src : int ; (* Source of last edge inserted in [e]. *)
    mutable reverse : t option ; (* Reverse graph. *)
  }

  let create ?(n=8) ?(m=32) () = { 
    n = 0 ; 
    m = 0 ;
    v = V.make n ;
    e = E.make m ;
    packed = true ;
    sorted = true ;
    last_src = -1 ;
    reverse = None ;
  }

  let dump out g =
    Printf.fprintf out "---- graph with %d vertices :\n" g.n ;
    for u=0 to V.length g.v - 1 do
      let b, e = V.edge_bounds g.v u in
      Printf.fprintf out "%d : %d, %d\n" u b e ;
    done ;
    Printf.fprintf out "---- and %d edges :\n" g.m ;
    for i=0 to E.length g.e - 1 do
      let v = E.dest g.e i in
      Printf.fprintf out "%d : %d\n" i v ;
    done ;
    Printf.fprintf out "---- \n" ;
    ()

  let dump_adj out g u =
    let b, e = V.edge_bounds g.v u in
    Printf.fprintf out "---- edges  of %d : range [%d, %d[ = \n" u b e ;
    for i=b to e - 1 do
      let v = E.dest g.e i in
      Printf.fprintf out "%d : %d\n" i v ;
    done ;
    Printf.fprintf out "---- \n" ;
    ()

  let n g = V.length g.v
  let n_mem g = g.n
  let m g = g.m

  let mem_vertex g u =
    let b, e = V.edge_bounds g.v u in
    b <> e || b <> V.default_edge || e <> V.default_edge


  let index_max g = V.length g.v - 1

  let add_vertex_be g u =
    let b, e = V.edge_bounds g.v u in
    if b = V.default_edge && b = e then begin 
      let not_dft = succ V.default_edge in
      let be = not_dft, not_dft in
      V.set_edge_bounds g.v u be ; (* marks that vertex is present *)
      g.n <- g.n + 1 ;
      be
    end else 
      b, e

  let invalidate_reverse g =
    match g.reverse with 
      | None -> () 
      | Some h ->
        g.reverse <- None ;
        match h.reverse with
          | Some g' when g' == g -> h.reverse <- None
          | _ -> ()

  let add_edge_l g u l_uv v =
    invalidate_reverse g ;
    g.m <- g.m + 1 ;
    ignore (add_vertex_be g v) ;
    let b, e = add_vertex_be g u in
    let e_len = E.length g.e in
    if b = e then begin (* [uv] is first out-edge for [u] *)
      E.set_label_dest g.e e_len (l_uv, v) ;
      V.set_edge_bounds g.v u (e_len, e_len+1) ;
    end else begin
      let l_u_last, u_last = E.label_dest g.e (e-1) in
      assert (u_last >= -1) ; (* check invariant *)
      if g.last_src = u then begin (* consecutive *)
        assert (e = e_len) ;
        E.set_label_dest g.e e_len (l_uv, v) ;
        V.set_edge_end g.v u (e_len+1) ;
        if u_last < 0 || u_last > v then g.sorted <- false ;
      end else begin (* if not : hack linked list in array *)
        E.set_label_dest g.e (e-1) (E.default_label, e-1-e_len) ;
        E.set_label_dest g.e e_len (l_u_last, u_last) ;
        E.set_label_dest g.e (e_len+1) (l_uv, v) ;
        V.set_edge_end g.v u (e_len+2) ;
        g.packed <- false ;
        g.sorted <- false ;
      end ;
    end ;
    g.last_src <- u

  let add_edge g u v = add_edge_l g u E.default_label v

  let add_vertex g u = ignore (add_vertex_be g u)

  (* If vertex is already present, we just change its label. *)
  let add_vertex_l g u l = 
    add_vertex g u ; V.set_label g.v u l

  let vertex_l g u =
    if not (mem_vertex g u) then invalid_arg "no such vertex" ;
    V.label g.v u


  let del_edge ?(only_first=false) g u v =
    let b, e = V.edge_bounds g.v u in
    let rec iter i =
      if i < e then begin
        let v' = E.dest g.e i in
        if v' = v then begin 
          E.set_label_dest g.e i (E.default_label, -1) ;
          g.m <- g.m -1 ;
        end ;
        if v <> v' || not only_first then
          let i = if v' >= 0 then i+1 else i-v' in 
          iter i
      end
    in
    iter b ;
    g.sorted <- false ; g.packed <- false ; 
    invalidate_reverse g ;
    ()


  let iter_succ_l f g u =
    let b, e = V.edge_bounds g.v u in
    let rec iter i =
      if i < e then begin
        let l_uv, v = E.label_dest g.e i in
        let i = if v >= 0 then (f l_uv v ; i+1) else i-v in
        iter i
      end
    in
    iter b

  let fold_succ_l f g u a =
    let b, e = V.edge_bounds g.v u in
    let rec iter i a =
      if i >= e then a else begin
        let l_uv, v = E.label_dest g.e i in
        let i, a = if v >= 0 then i+1, f l_uv v a else i-v, a in
        iter i a
      end
    in
    iter b a


  let del_vertex g rev_g u =
    if mem_vertex g u then begin
      iter_succ_l (fun _ v ->
        del_edge g v u
      ) rev_g u ;
      let b = V.default_edge in
      let deg = fold_succ_l (fun _ _ d -> d+1) g u 0 in
      g.m <- g.m - deg ;
      V.set_edge_bounds g.v u (b,b) ; (* marks that vertex is not member *)
      V.set_label g.v u V.default_label ;
      if u = g.n - 1 then 
        while g.n > 0 && not (mem_vertex g (g.n - 1)) do g.n <- g.n -1 done ;
      g.sorted <- false ; g.packed <- false ; 
      invalidate_reverse g ;
    end



  let find_edge_sorted_interv g u v =
    assert (g.sorted) ;
    let b, e = V.edge_bounds g.v u in
    (* Dichotomie on interval [l,r] (inclusive bounds here). *)
    let rec dicho l r =
      if l > r then l, l (* empty interval *)
      else if l = r then begin
        if E.dest g.e l = v then l, l+1 else l, l
      end else begin
        let m = (l + r) / 2 in
        let v' = E.dest g.e m in
        assert (v' >= 0) ; (* sorted implies packed *)
        if v' = v then begin (* found ! *)
          (* TODO : dicho here too. *)
          let l = ref m in
          while !l > b && E.dest g.e (!l - 1) = v do decr l done ;
          let r = ref (m+1) in
          while !r < e && E.dest g.e !r = v do incr r done ;
          !l, !r
        end else begin
          let l, r = 
            if v' < v then (* min r *) (m + 1), r 
            else (* v' > v *) l, max l (m-1) (* in case r = l+1 *)
          in
          dicho l r
        end
      end
    in
    dicho b (e-1)

  let mem_edge_sorted g u v =
    let b, e = find_edge_sorted_interv g u v in b < e

  let find_edge_sorted_l g u v =
    let b, e = find_edge_sorted_interv g u v in
    if b >= e then raise Not_found ;
    let l, v' = E.label_dest g.e b in
    assert (v = v') ;
    l

  let fold_edge_sorted_l f g u v a =
    let b, e = find_edge_sorted_interv g u v in
    let rec iter i a =
      if i >= e then a else begin
        let l_uv, v = E.label_dest g.e i in
        assert (v >= 0) ; (* sorted implies packed *)
        iter (i+1) (f l_uv a)
      end
    in
    iter b a

  let iter_vertex f g =
    for u=0 to index_max g do 
      if mem_vertex g u then f u
    done

  let fold_vertex f g a =
    let a = ref a in
    for u=0 to index_max g do 
      if mem_vertex g u then a:= f u !a
    done ;
    !a

  let iter_edges_l f g =
    iter_vertex (fun u -> iter_succ_l (fun l v -> f u l v) g u) g



  (* Optimisations when packed and/or sorted. *)

  let out_degree g u =
    if g.packed then 
      let b, e = V.edge_bounds g.v u in e - b
    else
      fold_succ_l (fun _ _ deg -> deg+1) g u 0

  (* Always return a sorted graph. *)
  let reverse g =
    match g.reverse with
      | Some h when h.sorted -> h
      | _ ->
        let h = create ~n:(n g) ~m:(m g) () in
    (* Set labels and compute in-degrees in edge_begin component of h.v : *)
        iter_vertex (fun u ->
          V.set_label h.v u (V.label g.v u) ;
          V.set_edge_begin h.v u 0 ;
        ) g ;
        iter_edges_l (fun _ _ v ->
          let d = V.edge_begin h.v v in V.set_edge_begin h.v v (d+1)
        ) g ;
    (* Prefix sums : *)
        let sum = ref 0 in
        iter_vertex (fun u ->
          let d = V.edge_begin h.v u in
          if d = 0 then begin (* to detect zero degree node we need : *)
            let not_dft = succ V.default_edge in
            V.set_edge_bounds h.v u (not_dft, not_dft) ;
          end else begin
            V.set_edge_bounds h.v u (!sum, !sum) ;
          end ;
          sum := !sum + d ;
        ) g ;
        assert (!sum = g.m) ;
    (* Insert edges : *)
        let v_max = ref (-1) in
        iter_edges_l (fun u l v ->
          if v > !v_max then v_max := v ;
          let i = V.edge_end h.v v in
          E.set_label_dest h.e i (l, u) ;
          V.set_edge_end h.v v (i+1) ;
        ) g ;
        h.last_src <- !v_max ;
        h.n <- g.n ; h.m <- g.m ;
        assert (h.sorted && h.packed);
    (* check : *)
        let prev = ref 0 in
        iter_vertex (fun u ->
          let b, e = V.edge_bounds h.v u in
          if b <> e then begin
            assert (!prev = b) ;
            prev := e ;
          end ;
        ) h ;
        h

  let sort ?(store_reverse=true) g =
    if not g.sorted then begin
      let h = reverse g in
      g.v <- V.make 0 ; g.e <- E.make 0 ; (* free mem *)
      invalidate_reverse g ; (* don't get g as the reverse of its reverse *)
      let g' = reverse h in
      g.v <- g'.v ; g.e <- g'.e ; 
      g.packed <- true ; g.sorted <- true ;
      g.last_src <- g'.last_src ; 
      if store_reverse then (g.reverse <- Some h (*; h.reverse <- Some g *) )
      else g.reverse <- None ;
    end

  let reverse ?(store_reverse=true) g =
    let h = reverse g in
    if store_reverse then begin
      (* TRICKY: if g.sorted then h.reverse <- Some g ; *)
      g.reverse <- Some h ;
    end ;
    h

  let symmetrize g =
    let h = reverse ~store_reverse:false g in
    assert (h.sorted) ;
    let g' = reverse ~store_reverse:false h in
    assert (g'.sorted && not (g' == g)) ;
    iter_edges_l (fun u l v ->
      if not (mem_edge_sorted h v u) then add_edge_l g u l v ;
    ) h ;
  (* Multiple edges problem : if u->v has multiplicity 2 and v->1 has mult > 0,
     no edge v->u is added. *)
    ()

  (* The rest is now simple. *)

  let iter_succ f g u = iter_succ_l (fun _ v -> f v) g u
  let fold_succ f g u a = fold_succ_l (fun _ v a -> f v a) g u a

  let in_degree g v = out_degree (reverse g) v
  let iter_pred_l f g v = iter_succ_l f (reverse g) v
  let fold_pred_l f g v a = fold_succ_l f (reverse g) v a
  let iter_pred f g v = iter_succ f (reverse g) v
  let fold_pred f g v a = fold_succ f (reverse g) v a

  exception Found
  let mem_edge g u v =
    if g.sorted then mem_edge_sorted g u v
    else
      try 
        iter_succ_l (fun _ v' -> if v = v' then raise Found) g u ;
        false
      with Found -> true

  exception Found_l of E.label
  let find_edge_l g u v =
    if g.sorted then find_edge_sorted_l g u v
    else
      try 
        iter_succ_l (fun l' v' -> if v = v' then raise (Found_l l')) g u ;
        raise Not_found
      with Found_l l -> l

  let fold_multi_edge_l f g u v a =
    fold_edge_sorted_l f g u v a

  let multiplicity_edge g u v =
    fold_multi_edge_l (fun _ m -> m+1) g u v 0

  let find_all_edges_l g u v =
    fold_multi_edge_l (fun l labs -> l :: labs) g u v []


  let iter_edges f g = 
    iter_edges_l (fun u _ v -> f u v) g

  let fold_edges_l f g a = 
    fold_vertex (fun u a -> fold_succ_l (fun l v a -> f u l v a) g u a) g a

  let fold_edges f g a = 
    fold_vertex (fun u a -> fold_succ (f u) g u a) g a

  let iter_vertex_l f g = 
    iter_vertex (fun u -> let l = vertex_l g u in f u l) g

  let fold_vertex_l f g a = 
    fold_vertex (fun u a -> let l = vertex_l g u in f u l a) g a

  (* Indexed digraphs : labels are also identifiers. *)
  let label_find_vertex g l = V.index g.v l

  let label_add_vertex g l =
    try V.index g.v l with Not_found -> 
      let u = n g in add_vertex_l g u l ; u

  let label_add_edge g lu lv =
    let u = label_add_vertex g lu and v = label_add_vertex g lv in 
    add_edge_l g u E.default_label v

  let label_add_edge_l g lu luv lv = 
    let u = label_add_vertex g lu and v = label_add_vertex g lv in 
    add_edge_l g u luv v


  let to_string g = 
    let b = Buffer.create (7*(n g) + 3*(m g)) in (* 2 digits *)
    iter_vertex (fun u ->
      Printf.bprintf b "%d -> " u ;
      iter_succ (fun v ->
        Printf.bprintf b " %d" v
      ) g u ;
      Printf.bprintf b "\n" ;
    ) g ;
    Buffer.contents b

  let to_string_l vlab_to_str ?(elab_to_str=fun _ -> "") g = 
    let b = Buffer.create (9*(n g) + 4*(m g)) in (* 4 letters *)
    iter_vertex_l (fun u lu ->
      Printf.bprintf b "%d:%s -> " u (vlab_to_str lu)  ;
      iter_succ_l (fun luv v ->
        Printf.bprintf b " %s:%s" 
          (elab_to_str luv) (vlab_to_str (vertex_l g v))
      ) g u ;
      Printf.bprintf b "\n" ;
    ) g ;
    Buffer.contents b

end



module OcamlGraph = struct

  module Concrete (G : LS with type v_label = unit) = struct

    include G

    module V = struct 
      type label = int
      type t = int
      let create u = u
      let label u = u
    end

    module E = struct
      type label = G.e_label
      type vertex = int
      type t = int * label * int
      let create u l v = u, l, v
      let src (u, _, _) = u
      let dst (_, _, v) = v
      let label (_, l, _) = l
    end

    let nb_vertex = n_mem
    let nb_edges = m
    let is_empty g = n_mem g = 0

    let add_edge_e g e = add_edge_l (E.src e) (E.label e) (E.dst e)
    let mem_edge_e g e = mem_edge g (E.src e) (E.dst e)
    let find_edge g u v = E.create u (find_edge_l u v) v
    let find_all_edges g u v =
      fold_multi_edge_l (fun l edgs -> (E.create u l v) :: edgs) g u v []

    let succ g u = fold_succ (fun v adj -> v :: adj) g u []
    let succ_e g u = fold_succ_l (fun l v adj -> (E.create u l v) :: adj) g u []
    let iter_succ_e f g u = iter_succ_l (fun l v -> f (E.create u l v)) g u
    let fold_succ_e f g u a = 
      fold_succ_l (fun l v a -> f (E.create u l v) a) g a

    let iter_edges_e f g = iter_vertex (fun u -> iter_succ_e (f u) g u) g
    let fold_edges_e f g a = 
      fold_edges_l (fun u l v a -> f (E.create u l v) a) g a 

    let pred g v = succ (reverse g) v
    let pred_e g v = succ_e (reverse g) v
    let iter_pred_e f g v = iter_succ_e f (reverse g) v
    let fold_pred_e f g v a = fold_succ_e f (reverse g) v a

  end

end

(* We basically use two vectors of ints for a graph. *)

module Int0 = struct 
  type t = int 
  let default = 0 
  let hash i = i land max_int
  let equal i j = i = j
end
module IntVec0 = Vector.MakeGap (Int0)

(** Index digraphs with int labels on nodes and edges. *)
module IntLabeled = struct

  module E : EdgeVec = struct
    include IntVec0
    type label = int
    let default_label = default ()
    let default_dest = default ()
    let label t i = get t (2*i)
    let set_label t i = set t (2*i)
    let dest t i = get t (2*i+1)
    let set_dest t i = set t (2*i+1)
    let label_dest t i = let i = 2*i in get t i, get t (i+1) 
    let set_label_dest t i (l,v) = let i = 2*i in set t i l ; set t (i+1) v
    let make n = make ~size:(2*n) ()
    let length t = (length t) / 2
  end 

  module V : VertexVec = struct
    include IntVec0
    type label = int
    let default_label = default ()
    let default_edge = default ()
    let edge_begin t i = get t (3*i)
    let set_edge_begin t i = set t (3*i)
    let edge_end t i = get t (3*i+1)
    let set_edge_end t i = set t (3*i+1)
    let label t i = get t (3*i+2)
    let set_label t i = set t (3*i+2)
    let edge_bounds t i = let i = 3*i in get t i, get t (i+1) 
    let set_edge_bounds t i (b,e) = let i = 3*i in set t i b ; set t (i+1) e
    let make n = make ~size:(3*n) ()
    let length t = (length t) / 3
    let index t _ = failwith "cannot map label to index"
  end

  include Make (V) (E)

end



(** Digraphs without any label. *)
module UnLabeled
  (IntVecV : Vector.S with type elt = int) 
  (IntVecE : Vector.S with type elt = int) 
  : S 
= struct

  module E : EdgeVec = struct
    include IntVecV
    type label = unit
    let default_label = ()
    let default_dest = default ()
    let label t i = ()
    let set_label t i () = ()
    let dest = get
    let set_dest = set
    let label_dest t i = let v = get t i in ((), v)
    let set_label_dest t i ((), v) = set t i v
    let make n = make ~size:n ()
  end 

  module V : VertexVec = struct
    include IntVecE
    type label = unit
    let default_label = ()
    let default_edge = default ()
    let edge_begin t i = get t (2*i)
    let set_edge_begin t i = set t (2*i)
    let edge_end t i = get t (2*i+1)
    let set_edge_end t i = set t (2*i+1)
    let label t i = ()
    let set_label t i () = ()
    let index t () = invalid_arg "IntDigraph: cannot map label to index"
    let edge_bounds t i = let i = 2*i in get t i, get t (i+1) 
    let set_edge_bounds t i (b,e) = let i = 2*i in set t i b ; set t (i+1) e
    let make n = make ~size:(2*n) ()
    let length t = (length t) / 2
  end

  include Make (V) (E)

end


module OfInt32 (D : sig val default : int32 end) = Vector.OfArrayGap (struct
  module B = Bigarray
  module BA1 = B.Array1
  type t = (int32, B.int32_elt, B.c_layout) BA1.t
  type elt = int32
  let make n e = 
    let t = BA1.create B.int32 B.c_layout n in
    for i=0 to n-1 do t.{i} <- e done ;
    t
  let empty = BA1.create B.int32 B.c_layout 0
  let get = BA1.get
  let set = BA1.set
  let length = BA1.dim
  let blit s os t ot len =
    let s = BA1.sub s os len and t = BA1.sub t ot len in
    BA1.blit s t
end) (struct
  type t = int32
  include D
end)


(** [2^31]-bounded digraphs. *)
module Compact 
  (IntVecV : Vector.S with type elt = int) 
  (Int32VecE : Vector.S with type elt = int32) 
  : S 
= struct

  module E : EdgeVec = struct
    include Int32VecE
    type label = unit
    let default_label = ()
    let default_dest = Int32.to_int (default ())
    let label t i = ()
    let set_label t i () = ()
    let dest t i = Int32.to_int (get t i)
    let max_int32 = Int32.to_int Int32.max_int
    let set_dest t i v =
      if v > max_int32 then 
        invalid_arg "IntDigraph.Compact: vertices must fit in int32" ;
      set t i (Int32.of_int v)
    let label_dest t i = let v = Int32.to_int (get t i) in ((), v)
    let set_label_dest t i ((), v) = set_dest t i v
    let make n = make ~size:n ()
  end 

  module V : VertexVec = struct
    include IntVecV
    type label = unit
    let default_label = ()
    let default_edge = default ()
    let edge_begin t i = get t (2*i)
    let set_edge_begin t i = set t (2*i)
    let edge_end t i = get t (2*i+1)
    let set_edge_end t i = set t (2*i+1)
    let label t i = ()
    let set_label t i () = ()
    let index t () = invalid_arg "IntDigraph: cannot map label to index"
    let edge_bounds t i = let i = 2*i in get t i, get t (i+1) 
    let set_edge_bounds t i (b,e) = let i = 2*i in set t i b ; set t (i+1) e
    let make n = make ~size:(2*n) ()
    let length t = (length t) / 2
  end

  include Make (V) (E)

end

module type DefaultValHashedType = sig
  type t 
  val hash : t -> int
  val equal : t -> t -> bool
  val default : t
end

(** Indexed diraphs with labels, vertex labels are mapped to indexes. *)
module Labeled (LV : DefaultValHashedType) (LE : Vector.DefaultValType)
  : LS with type v_label = LV.t and type e_label = LE.t
= struct

  module E : EdgeVec with type label = LE.t = struct
    type label = LE.t
    let default_label = LE.default
    let default_dest = -1
    include Vector.MakeGap (struct 
      type t = label * int
      let default = default_label, default_dest
    end)
    let label t i = let l,_ = get t i in l
    let set_label t i l = let _,v = get t i in set t i (l,v)
    let dest t i = let _,v = get t i in v
    let set_dest t i v = let l,_ = get t i in set t i (l,v)
    let label_dest = get 
    let set_label_dest = set
    let make n = make ~size:n ()
  end 

  module V : VertexVec with type label = LV.t = struct
    type label = LV.t
    let default_edge = -1
    let default_label = LV.default
    module V = Vector.MakeGap (struct 
      type t = int * int * label
      let default = default_edge, default_edge, default_label
    end)
    module L = Hashtbl.Make (LV) 
    type t = V.t * int L.t
    let make n = V.make ~size:n (), L.create n
    let length (t,_) = V.length t
    let edge_begin (t,_) i = let b,_,_ = V.get t i in b
    let set_edge_begin (t,_) i b = let _,e,l = V.get t i in V.set t i (b,e,l)
    let edge_end (t,_) i = let _,e,_ = V.get t i in e
    let set_edge_end (t,_) i e = let b,_,l = V.get t i in V.set t i (b,e,l)
    let label (t,_) i = let _,_,l = V.get t i in l
    let set_label (t,h) i l = 
      let b,e,_ = V.get t i in 
      V.set t i (b,e,l) ; L.replace h l i
    let index (_,h) l = L.find h l 
    let edge_bounds (t,_) i = let b,e,_ = V.get t i in b,e
    let set_edge_bounds (t,_) i (b,e) = 
      let _,_,l = V.get t i in V.set t i (b,e,l)
  end

  include Make (V) (E)

end


module IndexInt = Labeled (Int0) (Int0)

module G = UnLabeled (IntVec0) (IntVec0)
include G

let unit () =
  let g = G.create () in
  List.iter (fun (u,v) -> G.add_edge g u v) 
    [2,4; 1,8; 1,2; 2,4; 2,3; 3,4; 2,5; 4,1; 2,9; 1,9; 2,8; 1,7; 2,4; 2,7;] ;
  G.add_vertex g 17 ;
  Printf.printf "-- Graph with %d nodes and %d edges :\n%s" 
    (G.n_mem g) (G.m g) (G.to_string g) ;
  let h = G.reverse g in
  Printf.printf "-- Reverse : \n%s" (G.to_string h) ;
  G.sort g ;
  Printf.printf "-- Sorted : \n%s" (G.to_string g) ;
  let u = 2 and v = 6 in
  Printf.printf "-- mem edge %d %d : %b, mult %d\n"
    u v (G.mem_edge g u v) (G.multiplicity_edge g u v) ;
  let u = 2 and v = 4 in
  Printf.printf "-- mem edge %d %d : %b, mult %d\n"
    u v (G.mem_edge g u v) (G.multiplicity_edge g u v) ;
  let u = 2 and v = 7 in
  Printf.printf "-- mem edge %d %d : %b, mult %d\n"
    u v (G.mem_edge g u v) (G.multiplicity_edge g u v) ;
  let u = 1 and v = 17 in
  Printf.printf "-- mem edge %d %d : %b, mult %d\n"
    u v (G.mem_edge g u v) (G.multiplicity_edge g u v) ;  
  flush stdout ;

  let u = 4 and v = 17 in
  G.add_edge g u v ;
  Printf.printf "-- Added %d -> %d : \n%s" u v (G.to_string g) ;
  G.symmetrize g ;
  Printf.printf "-- Symmetrized : \n%s" (G.to_string g) ;
(*
  let module S = struct type t = string end in
  let module G = Labeled (S) (S) in
  let g = G.create "no_one" "none" in
  G.add_vertex_l g 1 "a" ; G.add_vertex_l g 2 "b" ; G.add_vertex_l g 3 "c" ;
  G.add_vertex_l g 4 "d" ; G.add_vertex_l g 5 "e" ;
  G.add_edge_l g 1 "1->2" 2 ;
  G.add_edge_l g 2 "2->3" 3 ;
  G.add_edge_l g 2 "2->4" 4 ;
  G.add_edge_l g 3 "3->4" 4 ;
  G.add_edge_l g 2 "2->5" 5 ;
  G.add_edge_l g 4 "4->1" 1 ;
  G.iter_vertex_l (fun u u_l ->
    Printf.printf "%d %s :" u u_l ;
    G.iter_succ_l (Printf.printf " %s %d") g u ;
    Printf.printf "\n" ;
  ) g ;
*)
  ()
    

