
let g_rnd add_e add_v n m =
  for i=1 to m do
    let u = Random.int n and v = Random.int n in
    add_e u v ;
    Debug.progress i m "rnd: adding edges" ;
  done

let g_src add_e add_v n m =
  let m0 = m and m = ref m in
  for u=0 to n-1 do
    let d = Random.int (max 1 (2 * !m / n)) in
    let d = d * d in (* skew distr *)
    for i=1 to d do
      let v = Random.int n in
      add_e u v ;
      decr m ;
      Debug.progress (m0 - !m) m0 "src: adding edges" ;
    done
  done

let g_empty add_e add_v n m =
  for u=0 to n-1 do
    add_v u
  done

let g_vertex_max_deg fold deg g =
  fold (fun u (um,dm) ->
    let d = deg g u in
    if d > dm then u,d else um,dm
  ) g (-1,0)


module IG = IntDigraph

let goIG constr n m =
  let modG = "IG" in
  let module G = IG in
  let module Bfs = Traversal.Bfs(G) in
  let g = G.create ~n:n ~m:m () in
  Debug.info "%s created" modG ;
  constr (G.add_edge g) (G.add_vertex g) n m ;
  Debug.info "%s filled" modG ;
  let um, dm = g_vertex_max_deg G.fold_vertex G.out_degree g in
  Debug.info "%s deg max : %d at %d" modG dm um ;
  G.sort g ;
  Debug.info "%s sorted" modG ;
  let t = Bfs.tree g um in
  Debug.info "%s Bfs %d visited" modG (Traversal.nb_visited t) ;
  ()


module HG = IntDigraph.UnBounded

let goHG constr n m =
  let modG = "HG" in
  let module G = HG in
  let module Bfs = Traversal.Bfs(G) in
  let g = G.create ~n:n ~m:m () in
  Debug.info "%s created" modG ;
  constr (G.add_edge g) (G.add_vertex g) n m ;
  Debug.info "%s filled" modG ;
  let um, dm = g_vertex_max_deg G.fold_vertex G.out_degree g in
  Debug.info "%s deg max : %d at %d" modG dm um ;
  G.sort g ;
  Debug.info "%s sorted" modG ;
  let t = Bfs.tree g um in
  Debug.info "%s Bfs %d visited" modG (Traversal.nb_visited t) ;
  ()


module BG = 
  IntDigraph.Compact 
    (IntDigraph.IntVec0) 
    (IntDigraph.OfInt32 (struct let default = Int32.zero end))

let goBG constr n m =
  let modG = "BG" in
  let module G = BG in
  let module Bfs = Traversal.Bfs(G) in
  let g = G.create ~n:n ~m:m () in
  Debug.info "%s created" modG ;
  constr (G.add_edge g) (G.add_vertex g) n m ;
  Debug.info "%s filled" modG ;
  let um, dm = g_vertex_max_deg G.fold_vertex G.out_degree g in
  Debug.info "%s deg max : %d at %d" modG dm um ;
  G.sort g ;
  Debug.info "%s sorted" modG ;
  let t = Bfs.tree g um in
  Debug.info "%s Bfs %d visited" modG (Traversal.nb_visited t) ;
  ()


module OG =
  Graph.Imperative.Digraph.ConcreteBidirectional(struct 
    type t = int
    let equal u v = u = v
    let compare u v = u - v
    let hash u = u land max_int
  end)
module BfsOG = Traversal.Bfs(struct 
  include OG
  let n g = 1 + fold_vertex max g 0
end)


let goOG constr n m =
  let modG = "OG" in
  let module G = OG in
  let module Bfs = BfsOG in
  let g = G.create ~size:n () in
  Debug.info "%s created" modG ;
  constr (G.add_edge g) (G.add_vertex g) n m ;
  Debug.info "%s filled" modG ;
  let um, dm = g_vertex_max_deg G.fold_vertex G.out_degree g in
  Debug.info "%s deg max : %d at %d" modG dm um ;
  let t = Bfs.tree g um in
  Debug.info "%s Bfs %d visited" modG (Traversal.nb_visited t) ;
  ()


let main () =
  Debug.set_verbosity "info"  ;
  Random.init 123 ;
  let n = TrivialArg.int "n" in
  let m = TrivialArg.int "m" in
  let go = match TrivialArg.string "graph module (OG/IG/HG/BG)" with
    | "OG" ->goOG | "IG" -> goIG | "HG" -> goHG  | "BG" -> goBG 
    | _-> invalid_arg "graph module" 
  in
  let constr = match TrivialArg.string "construction method (rnd/src)" with
    | "rnd" -> g_rnd | "src" -> g_src | "empty" -> g_empty
    | _-> invalid_arg "graph module" 
  in
  go constr n m ;
  Debug.the_end () 

let () =
  Debug.run main




              
