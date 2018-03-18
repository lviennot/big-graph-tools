(* Laurent Viennot, Inria 2015 *)

(** Minimal signatures for graph algorithms provided by BigGraph. *)

(** Tables for associating information to vertices. (When using bigarrays, we
    need different modules for different value types.) *)
module type Table = sig
  type t
  type key
  type value
  val create : int -> t
  val set : t -> key -> value -> unit
  val get : t -> key -> value 
end

(** Minimal graph signature for classical algorithms. *)
module type GraphAlgo = sig
  type t
  type vertex
  val n : t -> int
  val iter_vertex : (vertex -> unit) -> t -> unit
  val iter_succ : (vertex -> unit) -> t -> vertex -> unit

  (** Tables of vertices (to store parent of each vertex for example). *)
  module V : Table with type key = vertex and type value = vertex

  (** Tables of ints (to store a numbering of vertices for example). *)
  module I : Table with type key = vertex and type value = int

  (** The two following functions allow to print progress information during
      algorithms. (Dummy functions can be provided without altering algorithm
      correctness.) *)
  val m : t -> int
  val string_of_vertex : t -> vertex -> string
end

(** Graphs with vertex identifiers of type [node]. *)
module type Graph = sig
  include GraphAlgo
  val create : ?n:int -> ?m:int -> unit -> t
  type node
  val vertex : t -> node -> vertex
  val node : t -> vertex -> node
  val add_node : t -> node -> unit
  val add_edge : t -> node -> node -> unit (** Nodes are added if not prsent. *)
  val iter_edges : (node -> node -> unit) -> t -> unit
end

(** Graphs with identifiers and labeled edges. *)
module type LabeledGraph = sig
  include Graph
  type label
  val add_edge : t -> node -> label -> node -> unit
  val iter_labeled_succ : (label -> vertex -> unit) -> t -> vertex -> unit
  val iter_edges : (node -> label -> node -> unit) -> t -> unit
end

(** Labels are often weights. *)
module type Weight = sig
  type t
  val zero : t
  val infinity : t
  val compare : t -> t -> int
  val add : t -> t -> t
  val to_string : t -> string (* Progress information. *)
end

(** Weighted graphs. *)
module type WeightedGraphAlgo = sig
  include GraphAlgo
  type label
  val iter_labeled_succ : (label -> vertex -> unit) -> t -> vertex -> unit

  (** Tables of weights (to store weighted distances for example). *)
  module WT : Table with type key = vertex and type value = label

  (** Manipulate labels as weights. *)
  module W : Weight with type t = label
end



