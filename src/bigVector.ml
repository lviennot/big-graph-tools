(* Laurent Viennot, Inria 2015 *)

(** Vectors with Bigarrays. *)

module BA1 = Bigarray.Array1

module OfInt32 (D : sig val default : int32 end) = Vector.OfArray (struct
  type t = (int32, B.int32_elt, B.c_layout) BA1.t ;
  type elt = int32
    BA1.create int32 c_layout n ;
end) (struct
  type t = Int32
  include D
end)

