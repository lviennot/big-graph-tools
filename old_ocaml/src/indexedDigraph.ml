(* Laurent Viennot
 * Copyright Inria 2015 *)

(* Indexed digraph based on [IntDigraph] : each node has an index in [0..n-1]:
 * only one hashtable, all other graph manipulations can use arrays. This allows
 * to sort edges in linear time (compute reverse of reverse).  A graph can thus
 * be constructed in linear time so that edge detection then takes [O(log
 * degree)] time. There is a drawback compared to using a set structure for
 * storing successors: efficient edge detection cannot be mixed with edge
 * insertions.
 * 
 * Memory usage for one graph is roughly [(6+ v_t)n + (2+e_lab_t)m] words where
 * [v_t] and [e_lab_t] denote the number of words used to store a vertex (with
 * its label) and an edge label respectively.
 * 
 * [IndexedDigraph] base module is similar to [IntDigraph] except that sparse
 * index set is allowed. [IndexedDigraph.Make] is more general than
 * [IntDigraph.Make] as it allows labels of any type (not necessarily [int]).  *)

module type S = sig
  include IntDigraph.S
end

module type HashedType = Hashtbl.HashedType

module Make (G : IntDigraph.SL) (V : HashedType) (E : IntDigraph.AnyType) : S
= struct

  module H = Hashtbl.Make (V)
  module VL = Vector.Make (struct type t = V.label end)
  type label = Elab.t
  module EL = Vector.Make (struct type t = label end)

  type t = { g : G.t ; 
             mutable n : int ;
             h : H.t ; vlab : VLabVec.t ; elab : ELabVec.t ; }

  let create ?(n=16000) ?(m=128000) () =
    { g = IntDigraph.create ~n:n ~m:m () ;
      h = H.create n ;
      vlab = VLabVec.make_no_default ~size:n () ;
      elab = ELabVec.make_no_default ~size:m () ;
    }

  type vertex = V.t
  
  let add_vertex t u =  

end


module IntVertex : VertexType 
= struct
  type t = int
  let equal u v = u = v
  let hash u = u land max_int
  type label = t
  let label u = u
  let create u = u
end

module IntLabeled = Make (IntVertex) (struct type t = int end)


