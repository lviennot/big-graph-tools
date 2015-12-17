(* Laurent Viennot, Inria 2015 *)

(** Strongly connected components. *)

(** Minimal signature needed for graphs. *)
module type G = sig
  type t
  val n : t -> int
  val m : t -> int
  val iter_vertex : (int -> unit) -> t -> unit
  val iter_succ : (int -> unit) -> t -> int -> unit
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


module StronglyConnected  (A : A) (G : G) = struct

  module Stack = Vector.Stack (struct type t = int end)

  exception Break

  let components g =

    let n = G.n g and m = G.m g in
    let nb = ref 0 and no_nb = -1 in
    let number = A.make n no_nb in (* Number in dfs visit. *)
    let parent = A.make n no_nb in
    let unvisited u = A.get number u = no_nb in
    (* The low link of [u] is the minimal number of a node reachable from [u]
       through a sequence of forward edges plus one back ege. *)
    let low_link = A.make n no_nb in

    let comp_nb = ref 0 in
    let component = A.make n no_nb in
    let comp_stack = Stack.create ~size:(max 8 (n/8)) () in
    let in_stack u = A.get component u = no_nb in (* Assumes [u] visited. *)
    let rec pop_scc u = 
      let v = Stack.pop comp_stack in
      A.set component v !comp_nb ;
      if v = u then incr comp_nb (* done *)
      else pop_scc u
    in

    let nedges = ref 0 in
    let dfs_stack = Stack.create ~size:(max 8 (n/8)) () in

    Debug.info "Start strongly connected components." ;

    let end_visit u unb ulnk =
      if ulnk = unb then pop_scc u 
      else begin (* update parent only if ulnk can be smaller than parnt *) 
        let p = A.get parent u in
        (* p -> u is a forward edge, update parent low_link *)
        let plnk = A.get low_link p in
        if ulnk < plnk then A.set low_link p ulnk ;
      end ;
    in

    let dfs u =
      A.set parent u u ;
      Stack.push dfs_stack u ;
      while not (Stack.is_empty dfs_stack) do
        let u = Stack.pop dfs_stack in
        if u >= 0 && unvisited u then begin (* start visit of [u] *)
          let unb = !nb in
          incr nb ;
          A.set number u unb ;
          Stack.push comp_stack u ;
          Stack.push dfs_stack (-(u+1)) ; (* come back at end of son visits *)
          let nsons = ref 0 in
          let llnk = ref unb in
          G.iter_succ (fun v ->
            incr nedges ;
            if !nedges < 100 || !nedges mod 100 = 0 || !nedges = m then
              Debug.progress !nedges m "StronglyConnected.components" ;
            if unvisited v then begin (* forward edge *)
              A.set parent v u ;
              Stack.push dfs_stack v ;
              incr nsons ;
            end else (* back edge *)
              if in_stack v then 
                llnk := min !llnk (A.get number v) 
          ) g u ;
          A.set low_link u !llnk ;
          if !nsons = 0 then begin (* no need to come back, end visit now *)
            assert (-(u+1) = Stack.pop dfs_stack) ;
            end_visit u unb !llnk ;
          end ;
        end else if u < 0 then begin
          let u = -(u+1) in
          end_visit u (A.get number u) (A.get low_link u) ;
        end
      done
    in

    G.iter_vertex (fun u ->
      if unvisited u then dfs u
    ) g ;

    Debug.info "Found %d strongly connected components." !comp_nb ;
    component, !comp_nb, g


  let number (nb, _, _) u = A.get nb u

  let largest (nb, n, g) =
    let size = Array.make n 0 in
    let s_max = ref 0 and c_max = ref 0 and u_max = ref 0 in
    G.iter_vertex (fun u ->
      let c = A.get nb u in
      let s = size.(c) + 1 in
      size.(c) <- s ;
      if s > !s_max then (s_max := s ; c_max := c ; u_max := u) ;
      if c = !c_max && u > !u_max then u_max := u ;
    ) g ;
    for c=0 to n-1 do
      let s = size.(c) in
      let rec log2 n l = if n <= 1 then l else log2 (n lsr 1) (l+1) in
      Debug.distr "log2_scc" (log2 s 0) ;
    done ;
    Debug.info "Largest component %d (of node %d) : size=%d " 
      !c_max !u_max !s_max ;
    !c_max, !u_max, !s_max

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
  let module G = IndexGraph in
  let module Scc = StronglyConnected(Array)(G) in
  let module IO = GraphIO.Txt (G) in
  let g = G.create () in
  let adj u l = List.iter (fun v -> G.add_edge g u v) l in
  adj 1 [2] ;
  adj 2 [23] ;
  adj 3 [18] ;
  adj 4 [] ;
  adj 5 [1; 12] ;
  adj 6 [21] ;
  adj 7 [13] ;
  adj 8 [11] ;
  adj 9 [7] ;
  adj 10 [6] ;
  adj 11 [9; 13] ;
  adj 12 [5] ;
  adj 13 [15] ;
  adj 14 [2] ;
  adj 15 [1; 8] ;
  adj 16 [4; 14] ;
  adj 17 [7] ;
  adj 18 [19] ;
  adj 19 [3] ;
  adj 20 [9; 13] ;
  adj 21 [] ;
  adj 22 [20] ;
  adj 23 [7; 12; 17] ;
  adj 24 [] ;
  adj 25 [17; 20; 22; 23] ;
  IO.write_adjacencies stdout g ; flush stdout ;
  let scc = Scc.components g in
  assert (Scc.number scc 7 = Scc.number scc 8) ;
  assert (Scc.number scc 7 = Scc.number scc 12) ;
  assert (Scc.number scc 7 = Scc.number scc 9) ;
  assert (Scc.number scc 7 <> Scc.number scc 14) ;
  assert (Scc.number scc 7 <> Scc.number scc 22) ;
  assert (Scc.number scc 7 <> Scc.number scc 25) ;
  assert (Scc.number scc 12 = Scc.number scc 5) ;
  assert (Scc.number scc 3 = Scc.number scc 19) ;
  let largest, _, _ = Scc.largest scc in
  Printf.printf "Largest scc :" ;
  G.iter_vertex (fun u ->
    if Scc.number scc u = largest then
      Printf.printf " %d" u ;
  ) g ;
  Printf.printf "\n" ; flush stdout ;
  ()
  
