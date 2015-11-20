(* Laurent Viennot
 * Copyright Inria 2015 *)

(* Traversal algorithms with tree construction. *)

module type G = sig
  val is_directed : bool
  type t
  module V : Graph.Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val fold_vertex : (V.t -> 'a -> 'a) -> t  -> 'a -> 'a
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val fold_succ : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
end

module Bfs (G : G) = struct

  module H = Hashtbl.Make(G.V)

  let tree g u =
    let h = H.create 97 in
    let q = Queue.create () in
    let nb = ref 0 in
    let push part dist v =
      if not (H.mem h v) then begin
        H.add h v (!nb, part, dist) ;
        Queue.add (v, dist) q ;
        incr nb ;
      end
    in
    push u 0 u ;
    while not (Queue.is_empty q) do
      let v, d = Queue.pop q in
      let d = d + 1 in
      G.iter_succ (push v d) g v
    done ;
    let a = Array.make !nb (u, u, 0) in
    H.iter (fun v (nb, part, dist) -> a.(nb) <- (v, part, dist)) h ;
    a

end

let unit () =
  let module G = Graph.Imperative.Graph.Abstract(struct type t = char end) in
  let module B = Bfs(G) in
  Printf.printf "A simple Bfs :\n" ;
  let g = G.create () in
  let vtx = G.V.create and edg = G.add_edge g in
  let a, b, c, d, e = vtx 'a', vtx 'b', vtx 'c', vtx 'd', vtx 'e' in
  edg a b ; edg a c ;
  edg b d ; edg c e ;
  edg d e ;
  let t = B.tree g a in
  let lab = G.V.label in
  for i = 0 to Array.length t - 1 do
    let v, p, d = t.(i) in
    Printf.printf "%d %c -> %c\n" d (lab v)  (lab p) ;
  done ;
  let u1, p1, d1 = t.(1) and u3, p3, d3 = t.(3) in
  assert (d1 = 1 && d3 = 2) ;
  assert ((lab u1 = 'b' && lab u3 = 'd') || (lab u1 = 'c' && lab u3 = 'e')) ;
  assert (p3 = u1) ;
  ()
  
