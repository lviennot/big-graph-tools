(* Laurent Viennot, Inria 2015 *)

(** Traversal algorithms with tree construction. *)

(** Minimal signature needed for graphs. *)
module type G = sig
  include Sig.WeightedGraphAlgo
  val reverse : t -> t
end

module MakeGen (G : G) (Opt : sig
  val symmetric : bool
  val unweighted : bool
  (* If unweighted : *)
  val uniform_edge_weight : G.W.t option
end) = struct

  module W = struct
    include G.W
    let max w w' = if compare w w' > 0 then w else w' 
    let min w w' = if compare w w' < 0 then w else w' 
  end

  module D = G.WT (* Table of distances. *)

  let no_dist = W.infinity


  (* Use BFS on unweighted graphs. *)

  module Queue = Vector.Queue (struct type t = G.vertex end)

  let q = Queue.create ()

  let bfs_distances ?(dist_array=None) g s =
    let one = match Opt.uniform_edge_weight with Some w -> w |_->assert false in
    assert (W.compare one W.zero >= 0) ;
    let n = G.n g in
    let dist = match dist_array with
      | Some a -> a
      | None -> D.create n in
    G.iter_vertex (fun u -> D.set dist u no_dist) g ;
    Queue.clear q ;
    let push u d =
      if D.get dist u = no_dist then begin
        D.set dist u d ;
        Queue.add q u ;
      end 
    in
    let nedg = ref 0 and m = G.m g in
    push s W.zero ;
    while not (Queue.is_empty q) do
      let u = Queue.pop q in
      let d = W.add (D.get dist u) one in
      G.iter_succ (fun v -> 
        push v d ;
        incr nedg ;
        if !nedg < 100 || !nedg mod 100 = 0 || !nedg = m then
          Debug.progress !nedg m "Diameter.bfs_distances queue : %d" 
            (Queue.size q) ;
      ) g u ;
    done ;
    Debug.lap () ;
    dist


  (* Use Dijkstra on weighted graphs. *)

  module Heap = Vector.Heap (struct
    type t = G.vertex * W.t
    let compare (_,d) (_,d') = W.compare d d'
  end)

  let h = Heap.create ()

  let dijkstra_distances ?(dist_array=None) g s =
    let n = G.n g in
    let dist = match dist_array with
      | Some a -> a
      | None -> D.create n in
    G.iter_vertex (fun u -> D.set dist u no_dist) g ;
    Heap.clear h ;
    let push u d' =
      let d = D.get dist u in
      if d = no_dist || W.compare d' d < 0 then begin
        D.set dist u d' ;
        Heap.add h (u, d') ;
      end
    in
    let nedg = ref 0 and m = G.m g in
    push s W.zero ;
    while not (Heap.is_empty h) do
      let u, d = Heap.pop_min h in
      if W.compare d (D.get dist u) <= 0 then begin
        G.iter_labeled_succ (fun w v ->
          if W.compare w W.zero < 0 then failwith "dijkstra: negative weight" ;
          push v (W.add d w) ;
          incr nedg ;
          if !nedg < 100 || !nedg mod 100 = 0 || !nedg = m then
            Debug.progress !nedg m "Diameter.disktra_distances queue : %d"
              (Heap.size h) ;
        ) g u ;
      end ;
    done ;
    Debug.lap () ;
    dist

  type sweep_info = {
    source : G.vertex ;
    ecc : W.t ;
    last : G.vertex ;
    ecc' : W.t ;
    last' : G.vertex ;
  }
  let dum_sweep s e = 
    { source = s ; ecc = e ; last = s ; ecc' = e ; last' = s ; }

  type estimate = 
      Ecc | Ecc' | Ecc'Ecc | Dsum | Dsum' | Dmin | Dmin' | Dmax | Dmax'
  let str_of_estimate = function
    | Ecc -> "ecc" | Ecc' -> "ecc'"  | Ecc'Ecc -> "ecc'+ecc" 
    | Dsum -> "dsum" | Dmin -> "dmin" | Dmax -> "dmax"
    | Dsum' -> "dsum'" | Dmin' -> "dmin'" | Dmax' -> "dmax'"
  type extr = Min of estimate | Max of estimate
  let str_of_extr = function
    | Min e -> Printf.sprintf "min %s" (str_of_estimate e)
    | Max e -> Printf.sprintf "max %s" (str_of_estimate e)

  let periodic_heuristic period nsweep =
    period.((nsweep - 1) mod Array.length period)

  let basic_period =
    [| Max Ecc'; Min Ecc; Min Ecc'; Max Ecc; |]
  let big_period =
    let all = [| Ecc; Ecc'; Ecc'Ecc; Dsum; Dsum'; Dmin; Dmin'; |] in
    let all' = [| Ecc; Ecc'; Ecc'Ecc; Dsum; Dsum'; Dmax; Dmax'; |] in
    let min e = Min e and max e = Max e in
    Array.append (Array.map max all) (Array.map min all')
  let full_period =
    let all = [| Ecc; Ecc'; Ecc'Ecc; Dsum; Dsum'; Dmin; Dmin'; Dmax; Dmax' |] in
    let min e = Min e and max e = Max e in
    Array.append (Array.map max all) (Array.map min all)
  let pseudo_sum_sweep = 
    [| Max Ecc'; Min Ecc'Ecc; Max Ecc; Min Ecc'; Max Ecc'Ecc; Min Ecc; |]
  let maxmin_period_bizarre =
    [| Max Ecc'; Max Ecc'Ecc; Max Ecc; Min Ecc'; Max Dmin; Min Ecc; |]
  let maxmin_period =
    [| Max Ecc'; Min Ecc'Ecc; Max Ecc; Min Ecc'; Max Dmin; Min Ecc; |]

  let sum_sweep_period =
    [| Min Ecc; Min Ecc'Ecc; Max Ecc'; Max Ecc; Min Ecc'Ecc |]
  let sum_sweep_debug =
    [| Min Ecc; Min Ecc'Ecc; Max Ecc'; Max Ecc; Max Ecc'Ecc; Min Ecc' |]
  let sum_sweep_fun nsweep =
    let k = 6 in
    if nsweep <= k-1 then
      (if nsweep mod 2 = 1 then Max Dsum' else Max Dsum)
    else
      periodic_heuristic sum_sweep_debug (nsweep - k + 1)

  let period = big_period

  let periodic_heuristic_eriod nsweep =
    if nsweep <= 5 then
      (if nsweep mod 2 = 1 then Max Dsum' else Max Dsum)
    else periodic_heuristic big_period (nsweep - 5)

  exception Break

  type diam_info = {
    diam_lb : W.t ; diam_ub : W.t ; 
    diam_pair : sweep_info ; rev_diam_pair : sweep_info ;
    rad_lb : W.t ; rad_ub : W.t ; rad_center : sweep_info ;
    rev_rad_lb : W.t ; rev_rad_ub : W.t ; rev_rad_center : sweep_info ;
  }


  let diameter_radius_scc 
      ?(diam_only=false) ?(max_sweeps=max_int)
      ?(heuristic=periodic_heuristic_eriod)
      g u_scc =

    (* We need the reverse graph. *)
    let n = G.n g and g' = G.reverse g in

    (* And some arrays of bounds. *)
    let make i = 
      let a = D.create n in G.iter_vertex (fun u -> D.set a u i) g ; a in
    (* Eccentricities bounds : *)
    let ecc_lb = make W.zero and ecc_ub = make W.infinity in (* in [g] *)
    (* in [g'] : *)
    let ecc'_lb = if Opt.symmetric then ecc_lb else make W.zero 
    and ecc'_ub = if Opt.symmetric then ecc_ub else make W.infinity in 
    (* Sum of distances with sweep sources : *)
    let dsum = make W.zero in (* from sources to node *)
    let dsum' = if Opt.symmetric then dsum else make W.zero in (* from node *)
    (* Min of distances with sweep sources : *)
    let dmin = make W.zero  in (* from sources to node *)
    let dmin' = if Opt.symmetric then dmin else make W.zero in (* from node *)
    (* Max of distances with sweep sources : *)
    let dmax = make W.zero in (* from sources to node *)
    let dmax' = if Opt.symmetric then dmax else make W.zero in (* from node *)

    (* Main bounds. *)
    let diam_lb = ref W.zero and diam_ub = ref W.infinity in
    let rad_lb = ref W.zero and rad_ub = ref W.infinity in
    let rad'_lb = ref W.zero and rad'_ub = ref W.infinity in

    (* Best sweeps. *)
    let sw_diam = ref (dum_sweep u_scc W.zero) in
    let sw_diam' = ref (dum_sweep u_scc W.zero) in
    let sw_rad = ref (dum_sweep u_scc W.infinity) in
    let sw_rad' = ref (dum_sweep u_scc W.infinity) in
    
    (* Work on strongly connected component. *)
    let first_sweep = ref true and scc_size = ref n in
    let in_scc u =
      assert (not !first_sweep) ;
      D.get ecc_ub u <> W.infinity && D.get ecc'_ub u <> W.infinity 
    in

    (* Progress information about nodes and their excentricities. *)
    let sv = G.string_of_vertex g in
    let sw = W.to_string in

    let update_bounds s dist dist' = (* Update bounds after a sweep from [s]. *)

      (* Excentricities of [s] : *)
      let s_ecc = ref W.zero and last = ref s in
      let s_ecc' = ref W.zero and last' = ref s in
      (* Scan dist and dist' : *)
      G.iter_vertex (fun u ->
        let s_u = D.get dist u in (* dist s --> u *)
        let u_s = D.get dist' u in (* dist u --> s *)
        if s_u <> no_dist && u_s <> no_dist then begin (* in scc *)
          if W.compare s_u !s_ecc > 0 then (s_ecc := s_u ; last := u) ;
          if W.compare u_s !s_ecc' > 0 then (s_ecc' := u_s ; last' := u) ;
        end else if !first_sweep then begin (* not in scc *)
          decr scc_size ;
        end 
      ) g ;
      first_sweep := false ;
      let s_ecc = !s_ecc and last = !last in
      let s_ecc' = !s_ecc' and last' = !last' in
      let sw_s = { 
        source = s ; 
        ecc = s_ecc ; last = last ;
        ecc' = s_ecc' ; last' = last' ; 
      } in
      D.set ecc_lb s s_ecc ; D.set ecc_ub s s_ecc ;
      D.set ecc'_lb s s_ecc' ; D.set ecc'_ub s s_ecc' ;

      (* Diameter / radii bounds : *)
      diam_lb := W.max !diam_lb (W.max s_ecc s_ecc') ;
      rad_ub := W.min !rad_ub s_ecc ;
      rad'_ub := W.min !rad'_ub s_ecc' ;
      if W.compare s_ecc !sw_diam.ecc > 0 then sw_diam := sw_s ;
      if W.compare s_ecc' !sw_diam'.ecc' > 0 then sw_diam' := sw_s ;
      if W.compare s_ecc !sw_rad.ecc < 0 then sw_rad := sw_s ;
      if W.compare s_ecc' !sw_rad'.ecc' < 0 then sw_rad' := sw_s ;

      (* Node excentricities (scan dist and dist') : *)
      G.iter_vertex (fun u ->
        let s_u = D.get dist u in (* dist s --> u *)
        let u_s = D.get dist' u in (* dist u --> s *)
        if s_u <> no_dist && u_s <> no_dist then begin (* in scc *)
          (* Bound outward eccentricity of u *)
          let u_ecc_lb = D.get ecc_lb u and u_ecc_ub = D.get ecc_ub u in
          let lb = u_s and ub = W.add u_s s_ecc in
          if W.compare lb u_ecc_lb > 0 then D.set ecc_lb u lb ;
          if W.compare ub u_ecc_ub < 0 then D.set ecc_ub u ub ;
          (* Bound inward eccentricity of u *)
          let u_ecc'_lb = D.get ecc'_lb u and u_ecc'_ub = D.get ecc'_ub u in
          let lb' = s_u and ub' = W.add s_ecc' s_u in
          if W.compare lb' u_ecc'_lb > 0 then D.set ecc'_lb u lb' ;
          if W.compare ub' u_ecc'_ub < 0 then D.set ecc'_ub u ub' ;
          (* Sum of distances with sweep sources *)
          let s = D.get dsum u and s' = D.get dsum' u in
          D.set dsum u (W.add s s_u) ;
          D.set dsum' u (W.add s' u_s) ;
          (* Min of distances with sweep sources *)
          let m = D.get dmin u and m' = D.get dmin' u in
          if W.compare s_u m < 0 then D.set dmin u s_u ;
          if W.compare u_s m' < 0 then D.set dmin' u u_s ;
          (* Max of distances with sweep sources *)
          let mx = D.get dmax u and mx' = D.get dmax' u in
          if W.compare s_u mx > 0 then D.set dmax u s_u ;
          if W.compare u_s mx' > 0 then D.set dmax' u u_s ;
        end 
      ) g ;

      (* longest u --> s --> v gives an upper bound of the diameter,
         consider only u s.t. eccUB > diamLB and v s.t. ecc'UB > diamLB *)
      (* min_u ecc_lb(u) for u s.t. ecc_lb(u) < ecc_ub(u) is a lower bound 
         of radius, idem for rad' *)
      let u_s_max = ref W.zero and s_v_max = ref W.zero in
      let ecc_max = ref W.zero and ecc'_max = ref W.zero in
      let ecc_min = ref W.infinity and ecc'_min = ref W.infinity in
      (* Count vertices that matter : *)
      let lrg_ecc = ref 0 and lrg_ecc' = ref 0 in
      let sml_ecc = ref 0 and sml_ecc' = ref 0 in
      let bnd_diff = ref 0 and bnd'_diff = ref 0 in
      (* Scan bounds : *)
      G.iter_vertex (fun u ->
        if in_scc u then begin
          let lb = D.get ecc_lb u and ub = D.get ecc_ub u in
          if W.compare lb ub < 0 then begin
            incr bnd_diff ;
            if W.compare ub !diam_lb > 0 then begin
              incr lrg_ecc ;
              u_s_max := W.max !u_s_max (D.get dist' u) ; (* dist u --> s *)
            end ;
            ecc_min := W.min !ecc_min lb ;
            ecc_max := W.max !ecc_max ub ;
            if W.compare lb !rad_ub < 0 then incr sml_ecc ;
          end ;
          let lb' = D.get ecc'_lb u and ub' = D.get ecc'_ub u in
          if W.compare lb' ub' < 0 then begin
            incr bnd'_diff ;
            if W.compare ub' !diam_lb > 0 then begin
              incr lrg_ecc' ;
              s_v_max := W.max !s_v_max (D.get dist u) ; (* dist s --> u *)
            end ;
            ecc'_min := W.min !ecc'_min lb' ;
            ecc'_max := W.max !ecc'_max ub' ;
            if W.compare lb' !rad'_ub < 0 then incr sml_ecc' ;
          end ;
        end ;
      ) g ;
      (* General bounds : *)
      let u_v_max = W.add !u_s_max !s_v_max in
      diam_ub := W.min !diam_ub u_v_max ;
      diam_ub := W.min !diam_ub (W.min !ecc_max !ecc'_max) ;
      rad_lb := W.max !rad_lb !ecc_min ;
      rad'_lb := W.max !rad'_lb !ecc'_min ;
      Debug.info ~lap:false "  s_ecc'=%s(from %s) s_ecc=%s(to %s) \
                             u_s_m=%s s_v_m=%s scc_size=%d eccmin=%s ecc'min=%s"
        (sw s_ecc') (sv last') (sw s_ecc) (sv last) 
        (sw !u_s_max) (sw !s_v_max) !scc_size 
        (sw !ecc_min) (sw !ecc'_min) ;
      Debug.info "  bnd_diff=%d bnd'_diff=%d lrg_ecc=%d lrg_ecc'=%d \
                  sml_ecc=%d sml_ecc'=%d"
        !bnd_diff !bnd'_diff !lrg_ecc !lrg_ecc' !sml_ecc !sml_ecc' ;

    in

    let extreme_node extr =
      let cmp, worse = match extr with
        | Max _ -> W.compare, W.zero
        | Min _ -> (fun w w' -> W.compare w' w), W.infinity
      in
      let ecc'ecc_lb u = W.add (D.get ecc'_lb u) (D.get ecc_lb u) in
      let ecc'ecc_ub u = W.add (D.get ecc'_ub u) (D.get ecc_ub u) in
      let ecc_est, ecc_tie_bnd = match extr with
        | Min Ecc -> D.get ecc_lb, D.get ecc_ub 
        | Max Ecc -> D.get ecc_ub, D.get ecc_lb
        | Min Ecc' -> D.get ecc'_lb, D.get ecc'_ub
        | Max Ecc' -> D.get ecc'_ub, D.get ecc'_lb
        | Min Ecc'Ecc -> ecc'ecc_lb, ecc'ecc_ub
        | Max Ecc'Ecc -> ecc'ecc_ub, ecc'ecc_lb
        | Min Dsum -> D.get dsum, D.get ecc'_ub (* low dist srcs->, low ecc' *)
        | Max Dsum -> D.get dsum, D.get ecc'_lb
        | Min Dsum' -> D.get dsum', D.get ecc_ub
        | Max Dsum' -> D.get dsum', D.get ecc_lb
        | Min Dmin -> D.get dmin, D.get ecc'_ub
        | Max Dmin -> D.get dmin, D.get ecc'_lb
        | Min Dmin' -> D.get dmin', D.get ecc_ub
        | Max Dmin' -> D.get dmin', D.get ecc_lb
        | Min Dmax -> D.get dmax, D.get ecc'_ub
        | Max Dmax -> D.get dmax, D.get ecc'_lb
        | Min Dmax' -> D.get dmax', D.get ecc_ub
        | Max Dmax' -> D.get dmax', D.get ecc_lb
      in
    (* Break ties using [dsum] and [dsum'] :
      let dsum'dsum u = W.add (D.get dsum' u) (D.get dsum u) in
      let ecc_est, ecc_tie_bnd = match extr with
        | Min Ecc -> D.get ecc_lb, D.get dsum' 
        | Max Ecc -> D.get ecc_ub, D.get dsum'
        | Min Ecc' -> D.get ecc'_lb, D.get dsum
        | Max Ecc' -> D.get ecc'_ub, D.get dsum
        | Min Ecc'Ecc -> ecc'ecc_lb, dsum'dsum
        | Max Ecc'Ecc -> ecc'ecc_ub, dsum'dsum
        | Min Dsum -> D.get dsum, D.get ecc'_lb
        | Max Dsum -> D.get dsum, D.get ecc'_ub
        | Min Dsum' -> D.get dsum', D.get ecc_lb
        | Max Dsum' -> D.get dsum', D.get ecc_ub
        | Min Dmin -> D.get dmin, D.get dsum
        | Max Dmin -> D.get dmin, D.get dsum
        | Min Dmin' -> D.get dmin', D.get dsum'
        | Max Dmin' -> D.get dmin', D.get dsum'
        | Min Dmax -> D.get dmax, D.get dsum
        | Max Dmax -> D.get dmax, D.get dsum
        | Min Dmax' -> D.get dmax', D.get dsum'
        | Max Dmax' -> D.get dmax', D.get dsum'
      in
      let cmp_tie = cmp
    *)
      let bnd_gap = match extr with
        | Min Ecc | Max Ecc | Min Dsum | Max Dsum 
        | Min Dmin | Max Dmin | Min Dmax | Max Dmax ->
          fun u -> W.compare (D.get ecc_lb u) (D.get ecc_ub u) < 0
        | Min Ecc' | Max Ecc' | Min Dsum' | Max Dsum' 
        | Min Dmin' | Max Dmin' | Min Dmax' | Max Dmax' ->
          fun u -> W.compare (D.get ecc'_lb u) (D.get ecc'_ub u) < 0
        | Min Ecc'Ecc | Max Ecc'Ecc ->
          fun u -> 
            W.compare (D.get ecc_lb u) (D.get ecc_ub u) < 0
            || W.compare (D.get ecc'_lb u) (D.get ecc'_ub u) < 0
      in
      let u_max = ref u_scc and wgt = ref worse and tie = ref worse in
      G.iter_vertex (fun u ->
        if in_scc u then begin
          if bnd_gap u then begin
            let w = ecc_est u and wt = ecc_tie_bnd u in
            let c = cmp w !wgt in
            if c > 0 || (c = 0 && cmp wt !tie < 0) (* pref. large lb/ub gap *)
            then (u_max := u ; wgt := w ; tie := wt) ;
          end
        end
      ) g ;
      !u_max
    in

    let nsweeps = ref 0 and diam_sweeps = ref (-1) and rad_sweeps = ref (-1) in
    let distances_from = 
      if Opt.symmetric then bfs_distances else dijkstra_distances in
    let dist = D.create n in
    let dist' = if Opt.symmetric then dist else D.create n in

    let sweep ?sweep_name:(sname="") s =
      Debug.info ~lap:false "\nsweep %d %s from %s" !nsweeps sname (sv s) ;
      incr nsweeps ;
      Debug.info ~lap:false 
        "  bounds for %s : %s <= ecc <= %s,  %s <= ecc' <= %s" (sv s)
        (sw (D.get ecc_lb s)) (sw (D.get ecc_ub s)) 
        (sw (D.get ecc'_lb s)) (sw (D.get ecc'_ub s)) ;
      let dist = distances_from ~dist_array:(Some dist) g s in
      let dist' = 
        if Opt.symmetric then dist
        else distances_from ~dist_array:(Some dist') g' s in
      update_bounds s dist dist' ;
      Debug.info ~lap:false 
        "  %s <= diam <= %s,  %s <= rad <= %s,   %s <= rad' <= %s"
        (sw !diam_lb) (sw !diam_ub) 
        (sw !rad_lb) (sw !rad_ub) (sw !rad'_lb) (sw !rad'_ub) ;
      Debug.info ~lap:false 
        "  extremal nodes diam %s, diam' %s, rad %s, rad' %s"
        (sv !sw_diam.source) (sv !sw_diam'.source) 
        (sv !sw_rad.source) (sv !sw_rad'.source) ;
      if W.compare !diam_lb !diam_ub >= 0 && !diam_sweeps = -1 then 
        diam_sweeps := !nsweeps ;
      if W.compare !rad_lb !rad_ub >= 0 && W.compare !rad'_lb !rad'_ub >= 0 
        && !rad_sweeps = -1 then rad_sweeps := !nsweeps ;
      if !nsweeps >= max_sweeps then raise Break ; 
      if W.compare !diam_lb !diam_ub >= 0 && diam_only then raise Break ;
      if W.compare !diam_lb !diam_ub >= 0 
        && W.compare !rad_lb !rad_ub >= 0 && W.compare !rad'_lb !rad'_ub >= 0 
      then raise Break ;
    in
    
    (* Main loop. *)
    let first = ref true in
    begin try while W.compare !diam_lb !diam_ub < 0
      || W.compare !rad_lb !rad_ub < 0 || W.compare !rad'_lb !rad'_ub < 0 do
        let name, s =
          if !first then (first := false ; "initial node", u_scc)
          else begin
            let extr = heuristic !nsweeps in
            str_of_extr extr, extreme_node extr
          end
        in
        sweep ~sweep_name:name s
      done with Break -> () end ;
    Debug.info "Diameter : %s (<=%s, in %d sweeps), ecc(%s)=%s,  ecc'(%s)=%s"
      (sw !diam_lb) (sw !diam_ub) !diam_sweeps
      (sv !sw_diam.source) (sw !sw_diam.ecc) 
      (sv !sw_diam'.source) (sw !sw_diam'.ecc') ;
    Debug.info ~lap:false "Radius : %s (>= %s, rev %s, >= %s, in %d sweeps) \
         ecc(%s)=%s,%s   ecc'(%s)=%s,%s"
      (sw !rad_ub) (sw !rad_lb) (sw !rad'_ub) (sw !rad'_lb) !rad_sweeps
      (sv !sw_rad.source) (sw !sw_rad.ecc) (sw (W.add !sw_rad.ecc' !sw_rad.ecc))
      (sv !sw_rad'.source) (sw !sw_rad'.ecc')
      (sw (W.add !sw_rad'.ecc' !sw_rad'.ecc)) ;
    
    {
      diam_lb = !diam_lb ; diam_ub = W.max !diam_lb !diam_ub ;
      diam_pair = !sw_diam ; rev_diam_pair = !sw_diam' ;
      rad_lb = W.min !rad_lb !rad_ub ; rad_ub = !rad_ub ;
      rad_center = !sw_rad ;
      rev_rad_lb = W.min !rad'_lb !rad'_ub ; rev_rad_ub = !rad_ub ;
      rev_rad_center = !sw_rad' ;
    }      

end

module NoOpt = struct
  let symmetric = false
  let unweighted = false
  let uniform_edge_weight = None
end


module Make (G : G) = MakeGen (G) (NoOpt)

module Symmetric (G : Sig.WeightedGraphAlgo) = MakeGen (struct
  include G
  let reverse g = g
end) (struct
  include NoOpt
  let symmetric = true
end)



module IntWeight = struct
  type t = int
  let zero = 0
  let infinity = max_int
  let compare w w' = w - w'
  let add w w' = w + w'
  let to_string = string_of_int
end


module Unweighted (G : sig
  include Sig.GraphAlgo
  val reverse : t -> t
end) = MakeGen (struct
  include G
  type label = int
  let iter_labeled_succ _ _ = assert false
  module WT = I
  module W = IntWeight
end) (struct
  let symmetric = false
  let unweighted = true
  let uniform_edge_weight = Some 1
end)

module UnweightedSymmetric (G : Sig.GraphAlgo) = MakeGen (struct
  include G
  let reverse g = g
  type label = int
  let iter_labeled_succ _ _ = assert false
  module WT = I
  module W = IntWeight
end) (struct
  let symmetric = false
  let unweighted = true
  let non_weight = Some (-1)
  let uniform_edge_weight = Some 1
end)
