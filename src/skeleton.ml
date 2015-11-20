(* Laurent Viennot, Inria 2015 *)

(** Skeleton construction of a graph. *)

module type G = IntDigraph.S

module Skeleton (A : Traversal.A) (G : G) = struct

  module T = Traversal.Traversal (A)

  (** The (tfac,tadd)-skeleton of a forest is defined as follows :
      - a node [u] is ``long'' if [dist(u,e) >= tfac * dist(r,u) + tadd]
        where [e] is the descendant of [u] that is furthest from [u]
        and [r] is the root of the tree containing [u],
      - the ``branch'' of a node [u] is the path from [u] to its furthest
        descendant [e],
      - the skeleton is the union of the branches of long nodes.
  *)
  let of_forest ?(tfac=1.) ?(tadd=2) (t : T.t) =
    let visit = T.order t in
    let n_tot = T.nb_nodes t and n = T.nb_visited t in
      (* Count sons in tree. *)
    let nsons = A.make n_tot 0 in
    for i = 1 to n - 1 do
      let v = A.get visit i in
      let p = T.parent t v in
      A.set nsons p (A.get nsons p + 1)
    done ;

  (* Find furthest descendant. *)
    let desc_node = A.make n_tot 0 in
    for v = 0 to n_tot-1 do A.set desc_node v v done ;
    let desc v = T.dist t (A.get desc_node v) in
    for i = n - 1 downto 1 do
      let v = A.get visit i in
      (* if nsons.(v) = 0 then desc_node.(v) is already v *)
      let p = T.parent t v in
      if desc v  >= desc p then A.set desc_node p (A.get desc_node v) ;
    done ;

  (* A node is "long" if its furthest descendant is far enough compared
     to the distance from the root to the node. *)
    let long_node v =
      let dist_v = T.dist t v in
      desc v >= int_of_float (tfac *. float_of_int dist_v) + tadd
    in
    let nsons_long = A.make n_tot 0 in
    for i = 1 to n - 1 do
      let v = A.get visit i in
      if long_node v then
        let p = T.parent t v in
        A.set nsons_long p (A.get nsons_long p + 1)
    done ;

  (* Mark long branches, i.e. long nodes and edges between them and path
     to furthest descendants of long nodes with no long-node son.
     (scan vertices by increasing distance).
     Other nodes are considered to belong to the branch of their lowest
     ascendant belonging to a branch.
     A long node is the root of a branch if its furthest descendant is
     different from its father furthest descendant.
     The root of a node is the root of its branch.
  *)
    let mark = A.make n_tot false in
    let root = A.make n_tot T.non_vertex in
    for i = 0 to n - 1 do
      let v = A.get visit i in
      let p = T.parent t v in
      if long_node v then begin
        A.set mark v true ;
        if i > 0 then begin
          if A.get desc_node p <> A.get desc_node v then A.set root v v
          else A.set root v (A.get root p) ;
        end else begin
          A.set root v v ;
        end ;
        if A.get nsons_long v = 0 then begin
          T.iter_path (fun v' -> 
            A.set mark v' true ;
            A.set root v' (A.get root v) ;
          ) t (A.get desc_node v) v ;
        end 
      end 
      else A.set root v (A.get root p)
    done ;
    mark, root, desc_node


  module Bfs = Traversal.Bfs (A) (G)

  (** [of_graph_comp g u] computes a skeleton of the component of [u] in
      graph [g] as follows:
      - An ``extremal'' node [r] is chosen (we use the last node in a BFS 
        from [u]).
      - A skeleton tree of the single source shortest path tree rooted at [r]
        is computed.
      - We remove branches that are covered by their parent branche where
        a branch [b] is covered when all nodes of [b] are at distance at 
        most [tadd] from the branch containing the parent of the root of [b],
        and a branch is a path from a root node to its furthest descendant 
        in the tree (we consider only branches that are maximal for inclusion).
      - If a path of length [tadd = 4] is found between two branches, it is
        added in the skeleton if this does create a cycle with length less
        than [girth = 3*tadd].
      Note that the resulting graph has girth (i.e. minimum cycle length)
      at least [girth].
      (Paremeters can be optionally set: [~tfac:1.5 ~tadd:6] control the
      computation of the skeleton tree, [~girth:10] would set the minimum cycle
      length to [10].).
  *)
  let of_graph_comp ?(tfac=1.2) ?(tadd=4) ?(girth=3*tadd) 
      g u (* component of u *) =
    let bfs = Bfs.tree ~follow_pred:true g u in
    (* Try to start from an extremity : *)
    let tree_root = Bfs.last_visited bfs in
    let s_neighb fold = fold (fun v s -> Printf.sprintf "%s %d" s v) g in
    Debug.info "Root is %d -> %s" tree_root 
      (s_neighb G.fold_pred tree_root (s_neighb G.fold_succ tree_root "")) ;
    let t = Bfs.tree ~follow_pred:true g tree_root in
    let n = Bfs.nb_visited t in
    Debug.info "Skeleton tree (tfac=%.2f tadd=%d) has %d nodes." tfac tadd n ;
    let mark, root_node, desc_node = of_forest ~tfac ~tadd t in
    let desc v = Bfs.dist t (A.get desc_node v) in
    let root v = A.get root_node v in
    let visit = Bfs.order t in

  (* A branch is covered and ignored if at distance [<= tadd] from parent branch *)
    let ncov = ref 0 and covedges = ref 0 in
    for i = 0 to n - 1 do
      let v = A.get visit i in
      let p = Bfs.parent t v in
      if i > 0 && root v = v then begin
        if not (A.get mark p) then 
          failwith "Not implemented : parent branch is already covered." ;
        let pbranch = Bfs.path_rev t (A.get desc_node (root p)) (root p) in
        let branch = Bfs.path_rev t (A.get desc_node v) v in
        let branch_forest = Bfs.forest ~follow_pred:true g pbranch in
        let d = List.fold_left (fun d u -> 
          max d (Bfs.dist branch_forest u)
        ) 1 branch 
        in
        if d <= tadd then begin
          A.set mark v false ;
          A.set root_node v (root p) ;
          List.iter (fun v' ->
            A.set mark v' false ;
            incr covedges ;
            A.set root_node v' (root p) ;
          ) branch ;
          decr covedges ; (* count edges removed *)
          incr ncov ;
        end
      end
    done ;
    for i = 0 to n - 1 do
      let v = A.get visit i in
      if not (A.get mark (root v)) then begin
        A.set root_node v (root (root v)) ;
        assert (A.get mark (root v)) ;
      end ;
    done ;
    Debug.info "Covered branches %d (%d edges)" !ncov !covedges ;

  (* Start the skeleton with non-covered branches. *)
    let skel = G.create ~n:n ~m:n () in
    for i = 0 to n - 1 do
      let v = A.get visit i in
      let p = Bfs.parent t v in
      if p <> v && A.get mark v then begin
        assert (A.get mark p) ;
        G.add_edge skel p v ;
      end ;
    done ;
    G.sort skel ;

  (* Connections between branches (branches are identified by root) :
     simply scan edges between two nodes in different branches,
     add paths of length <= tadd between two branches such that paths are
     tadd nodes away. *)
    let skel_inter = G.create ~n:n () in (* only inter-branch links *)
    let branch_links = Hashtbl.create 10 in
    let nlinks = ref 0 in
    let compare_branch u v = (* u v are roots of two branches *)
      let c = compare (desc u) (desc v) in
      if c <> 0 then - c (* longer before *) 
      else (* visited first before *)
        compare (Bfs.visit_nb t u) (Bfs.visit_nb t v)
    in
    let inter_branch_links = ref 0 in
    let add_link u v = (* u v are roots of two branches *)
      (* TODO : what about non-symetric graph ? *)
      incr inter_branch_links ;
      let u, v = if compare_branch u v < 0 then u, v else v, u in
      Hashtbl.replace branch_links (u, v) ()
    in
    for i = 0 to n - 1 do
      let v = A.get visit i in
      G.iter_succ (fun w ->
        if root w <> root v then add_link (root v) (root w)
      ) g v ;
    done ;
    Debug.info "Inter-branch links in graph : %d, yielding %d branch link(s)." 
      !inter_branch_links (Hashtbl.length branch_links) ;
    let skel_cp = G.create ~n:(G.n skel) ~m:(G.m skel) () in
    G.iter_edges (G.add_edge skel_cp) skel ;
    Hashtbl.iter (fun (u, v) () -> (* If one is parent, it is [u] *)
      let ubranch = Bfs.path_rev t (A.get desc_node u) u in
      let vbranch = Bfs.path_rev t (A.get desc_node v) v in
      let branch_forest = Bfs.forest ~follow_pred:true g ubranch in
      let i = ref 0 in  (* index on vbranch *)
      List.iter (fun v' ->
        let d' = Bfs.dist branch_forest v' in
        Debug.distr (Printf.sprintf "branch_%d_%d_dist" u v) d' ;
        if d' <= tadd then begin
          let path = Bfs.path_rev branch_forest v' Bfs.non_vertex in
          let w' = List.hd path in
          (* Add bridge if it does not create a short cycle. *)
          if d' + Bfs.find_dist ~follow_pred:true skel_cp v' w' >= girth 
          then begin
            Debug.distr (Printf.sprintf "branch_%d_%d_close_at" u v) !i ;
            (*  Debug.info "branch_%d_%d : link at %d : %d -- %d (dist %d)"
              u v !i v' w' (List.length path - 1) ;
            Printf.eprintf "path :" ;
            List.iter (Printf.eprintf " %d") path ;
            Printf.eprintf "\n" ; *)
            let rec iter = function
              | [] | [_] -> ()
              | u' :: v' :: path ->
                if not (G.mem_edge skel u' v' || G.mem_edge skel v' u') 
                then begin
                  G.add_edge skel_inter u' v' ;
                  G.add_edge skel_cp u' v' ;
                end ;
                A.set mark v' true ;
                iter (v' :: path)
            in iter path ;
            incr nlinks ;
          end ;
        end ;
        incr i ;
      ) vbranch ;
    ) branch_links ;
    G.sort skel_inter ; (* may have multiple edges *)
    let prev_edge = ref None in
    G.iter_edges (fun u v ->
      match !prev_edge with
        | Some (u',v') when u=u' && v=v' -> ()
        | _ ->
          prev_edge := Some (u,v) ;
          G.add_edge skel u v ;
    ) skel_inter ;
    G.sort skel ;
    Debug.info "Branch bridges added: %d (%d graph links)" 
      !nlinks (G.m skel_inter) ;

  (* Remove pending pieces of branches with length <= tadd *)
    let skel_pending = G.create ~n:n () in
    G.sort skel ;
    let deg u = G.out_degree skel u + G.in_degree skel u in
    let son u prev = (* when [u] has degree 1 or 2 *)
      let not_u_prev v u' = if v <> u && v <> prev then v else u' in
      G.fold_pred not_u_prev skel u (G.fold_succ not_u_prev skel u u) 
    in
    let skel_root =
      G.fold_vertex (fun leaf skel_root ->
        if deg leaf > 1 then skel_root else begin
          let u = ref leaf and prev = ref leaf and length = ref 0 in
          while deg !u <= (if !u = leaf then 1 else 2) && !length <= tadd do
            let son = son !u !prev in
            assert (son <> !u) ;
            prev := !u ; u := son ;
            incr length ;
          done ;
          if 0 = !length || !length > tadd then skel_root else begin
            Debug.info "Removing pending extremity %d of branch %d (%d links)."
              leaf (root leaf) !length ;
            let new_extremity = !u in
            let rec iter u s =
              G.add_edge skel_pending u s ;
              if s <> new_extremity then
                iter s (son s u)
            in iter leaf (son leaf leaf) ;
            if leaf = skel_root 
            then (assert (skel_root = tree_root) ; new_extremity) 
            else skel_root
          end
        end
      ) skel tree_root 
    in
    let rev = G.reverse skel in
    G.iter_edges (fun u _ ->
      G.del_vertex skel rev u ;
    ) skel_pending ;
    Debug.info "Removed %d pending links." (G.m skel_pending) ;
    Debug.info "Root is %d" skel_root ;
      
  (* Return skeleton graph. *)
    skel, skel_inter, skel_pending, skel_root

end


let main () =
  let module G = IntDigraph.IndexInt in
  let tfac = TrivialArg.float "tfac" in
  let tadd = TrivialArg.int "tadd" in
  let girth = TrivialArg.int "girth" in
  let fname = TrivialArg.string "edge_file" in
  let format = 
    try TrivialArg.string "format (csv/dot/gdf)" with _-> "csv"
  in
  Debug.info "File %s will output in format %s" fname format ;
  let cin = if fname = "-" then stdin else open_in fname in
  let lex = Lexing.from_channel cin in
  let g = G.create () in
  (try while true do
      let x = BasicLexer.int lex
      and y = BasicLexer.int ~no_eof:true lex in 
      G.label_add_edge g x y ;
    done with End_of_file -> ()) ;
  Debug.info "Read digraph with %d nodes and %d edges." (G.n_mem g) (G.m g) ;
  G.sort g ;
  Debug.info "Nb asym edges : %d"
    (G.fold_edges (fun u v nb ->
      nb + if not (G.mem_edge g v u) then 1 else 0
     ) g 0) ;
  G.sort g ;

  let module Skel = Skeleton (Traversal.Hashtbl) (G) in
  let um, dm = G.fold_vertex (fun u (um,dm) ->
    let d = G.out_degree g u in
    if d > dm then u,d else um,dm
  ) g (-1,0) in

  let skel, skel_inter, _, root = Skel.of_graph_comp ~tfac ~tadd ~girth g um in

  G.sort skel ;
  G.sort skel_inter ;
  let prt fmt = Printf.kfprintf (fun _ -> ()) stdout fmt 
  and vtx = G.vertex_l g in
  begin match format with

    | "csv" ->
      G.iter_edges (fun u v -> 
        prt "%d\t%d\n" (vtx u) (vtx v) ;
      ) skel ;

    | "dot" ->
      prt "digraph g_skel {\n" ;
      prt "  %d [ color=\"blue\" ];\n" (vtx root) ;

      G.iter_edges (fun u v -> 
        if G.mem_edge skel_inter u v || G.mem_edge skel_inter v u
        then prt "  %d -> %d [ color=\"red\" ];\n" (vtx u) (vtx v)
        else prt "  %d -> %d [ color=\"green\" ];\n" (vtx u) (vtx v) ;
      ) skel ;
      prt "}\n" ;

    | "gdf" ->
      (* Just color original graph : *)
      let green = "'200,20,20'" and red = "'20,200,20'" and dark = "'30,30,30'"
      and blue = "'20,20,200'" and light = "'100,100,100'" in
      prt "nodedef>name VARCHAR,label VARCHAR,color VARCHAR,index VARCHAR\n" ;
      G.iter_vertex (fun u -> 
        let label,color =
          if u = root then "root", blue
          else if G.mem_vertex skel u then "skel", green
          else "_", dark
        in
        prt "%d,%s,%s,%d\n" (vtx u) label color u ;
      ) g ;
      prt "edgedef>node1 VARCHAR,node2 VARCHAR,label VARCHAR,\
                   color VARCHAR,weight DOUBLE\n" ;
      G.iter_edges (fun u v -> 
        let label, color, weight =
          if G.mem_edge skel_inter u v || G.mem_edge skel_inter v u
          then "inter_branch", green, 1. 
          else if G.mem_edge skel u v || G.mem_edge skel v u
          then "tree", red, 1.
          else "", light, 0.2
        in
        prt "%d,%d,%s,%s,%.1f\n" (vtx u) (vtx v) label color weight ;
      ) g ;

    | _ -> assert false
  end ;

  Debug.info "Output skeleton with %d nodes and %d edges." 
    (G.n_mem skel) (G.m skel) ;
  () 

let () =
  Debug.run ~verbose:(TrivialArg.boolOption "-verbose") main
