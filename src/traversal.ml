(* Laurent Viennot, Inria 2015 *)

(** Traversal algorithms with tree construction. *)

(** Minimal signature needed for graphs. *)
module type G = sig
  type t
  val n : t -> int
  val iter_vertex : (int -> unit) -> t -> unit
  val fold_vertex : (int -> 'a -> 'a) -> t  -> 'a -> 'a
  val iter_succ : (int -> unit) -> t -> int -> unit
  val fold_succ : (int -> 'a -> 'a) -> t -> int -> 'a -> 'a
  val iter_pred : (int -> unit) -> t -> int -> unit
  val fold_pred : (int -> 'a -> 'a) -> t -> int -> 'a -> 'a
end

(** Minimal signature for arrays/hashtable depending on graph implementation. *)
module type A = sig
  type 'a t
  val make : int -> 'a -> 'a t
  val length : 'a t -> int (* TODO : not available with hashtbl ! *)
  val set : 'a t -> int -> 'a -> unit
  val get : 'a t -> int -> 'a
(* TODO: add iter and fold instead *)
end 

(** In a graph traversal, each node is numbered (first node visited: 0,
    second node: 1, ...), has eventually a parent (the ndoe from which it
    was discovered, the starting node(s) of the traversal are their own
    parent, the parent information thus induces a forest), and a distance
    information (the length of parent chain up to a source node). *)
module Traversal (A : A) = struct
  type visit_info = { nb : int ; dist : int ; parent : int ; }
      
(* Invariant : [parent.nb < nb]. *)
  type t = { 
    visit : visit_info A.t ;
    mutable n : int ; (* number of nodes visited during graph traversal *)
  }

  let non_vertex = -1

  let visit_nb t u = (A.get t.visit u).nb

(** Has vertex been visited. *)
  let visited t u = visit_nb t u <> non_vertex

  let parent t u = (A.get t.visit u).parent

(** Distance from root. *)
  let dist t u = (A.get t.visit u).dist

  let nb_visited t = t.n

(** Total number of nodes (including non-visited nodes). *)
  let nb_nodes t = A.length t.visit 

  exception Found of int

  let last_visited t =
    try
      for u=0 to A.length t.visit - 1 do
        let nb = (A.get t.visit u).nb in
        if nb = t.n - 1 then raise (Found u)
      done ;
      invalid_arg "Traversal"
    with Found u -> u

(** Visited nodes in traversal order. *)
  let order t =
    let perm = A.make t.n non_vertex in
    let n = ref 0 in
    for u=0 to A.length t.visit - 1 do
      let nb = (A.get t.visit u).nb in
      if nb <> non_vertex then begin
        assert (A.get perm nb = non_vertex) ;
        A.set perm nb u ;
        incr n ;
      end ;
    done ;
    assert (!n = t.n) ;
    perm

(** Iterate on [u], [parent(u)], ..., [v] if [v] is ancestor of [u], and
    up to the root of the tree containing [u] otherwise. *)
  let rec iter_path f t u v =
    f u ;
    let p = parent t u in
    if u <> v && p <> u then 
      iter_path f t p v

  let rec fold_path f t u v a =
    let a = f u a in
    let p = parent t u in
    if u = v || p = u then a else 
      fold_path f t p v a

  let path_rev t u w = fold_path (fun v pth -> v :: pth) t u w []

  let path t u w = List.rev (path_rev t u w)


  let make n =
    { visit =
        A.make n { nb = non_vertex ; dist = max_int ; parent = non_vertex ; };
      n = 0 ;
    }

  let set t u nb dist par = 
    assert (visit_nb t u = non_vertex && nb >= 0 && nb < A.length t.visit) ;
    A.set t.visit u { nb = nb ; dist = dist ; parent = par ; } ;
    t.n <- t.n + 1 ;
    ()
end


module Bfs (A : A) (G : G) = struct

  module T = Traversal (A)
  include T

  module Queue = Vector.Queue (struct type t = int end)

  exception Found of T.t

  let forest ?(find=None) ?(follow_succ=true) ?(follow_pred=false) g srcs =
    try
      let n = G.n g in
      let f = T.make n in
      let unvisited u = T.visit_nb f u = T.non_vertex in
      let nb = ref 0 in
      let q = Queue.create ~size:(max 8 (n/8)) () in
      let push u dist parent =
        if unvisited u then begin
          T.set f u !nb dist parent ;
          incr nb ;
          match find with
            | Some v when u = v -> raise (Found f)
            | _ -> Queue.add q u
        end
      in
      List.iter (fun s -> push s 0 s) srcs ;
      while not (Queue.is_empty q) do
        let u = Queue.pop q in
        let d = T.dist f u + 1 in
        if follow_succ then G.iter_succ (fun v -> push v d u) g u ;
        if follow_pred then G.iter_pred (fun v -> push v d u) g u ;
      done ;
      f
    with Found f -> f

  let tree ?(follow_succ=true) ?(follow_pred=false) g u = 
    forest ~follow_succ ~follow_pred g [u]

  (** @raise Not_found if [v] is not reachable from [u]. *)
  let find_dist ?(follow_succ=true) ?(follow_pred=false) g u v =
    let f = forest ~find:(Some v)  ~follow_succ ~follow_pred g [u] in
    if T.visit_nb f v = T.non_vertex then raise Not_found 
    else T.dist f v

end


module Dfs (A : A) (G : G) = struct

  module T = Traversal (A)
  include T

  module Stack = Vector.Stack (struct 
    type t = int let default = 0 
  end)

  exception Found of T.t

  let def g s =
    let n = G.n g in
    let f = T.make n in
    let unvisited u = T.visit_nb f u = T.non_vertex in
    let nb = ref 0 in
    let q = Stack.create ~size:(max 8 (n/8)) () in
    let push u parent =
      if unvisited u then begin
        T.set f u !nb 0 parent ;
        incr nb ;
      end
    in
    push s s ;
    while not (Stack.is_empty q) do
      let u = Stack.pop q in
      G.iter_succ (fun v -> push v u) g u ;
    done ;
    f

end


module Array : A = struct
  type 'a t = 'a array
  include Array
end

module Hashtbl : A = struct
  type 'a t = { tbl : (int, 'a) Hashtbl.t ; dft : 'a ; n : int ; }
  let make n dft = { tbl = Hashtbl.create n ; dft = dft ; n = n ; }
  let set t i a = Hashtbl.replace t.tbl i a
  let get t i = try Hashtbl.find t.tbl i with Not_found -> t.dft
  let length t = t.n
end

let unit () =
  let module G = IntDigraph in
  let module Bfs = Bfs(Array)(G) in
  Printf.printf "A simple Bfs :\n" ;
  let g = G.create () in
  let edg = G.add_edge g in
  let a, b, c, d, e = 1, 2, 3, 4, 5 in
  edg 1 2 ; edg 1 3 ;
  edg 2 4 ; edg 3 5 ;
  edg 4 5 ;
  let bfs = Bfs.tree g a in
  G.iter_vertex (fun u ->
    Printf.printf "%d : nb=%d dist=%d parent=%d\n" 
      u (Bfs.visit_nb bfs u) (Bfs.dist bfs u) (Bfs.parent bfs u) ;
  ) g ;
  assert (Bfs.dist bfs 2 = 1 && Bfs.dist bfs 4 = 2) ;
  assert (Bfs.visit_nb bfs 4 = Bfs.visit_nb bfs 2 + 2 
         && Bfs.visit_nb bfs 5 = Bfs.visit_nb bfs 3 + 2) ;
  assert (Bfs.parent bfs 5 = 3) ;
  ()
  
