(* Laurent Viennot, Inria 2015 *)

(** Dynamic arrays.

    [Make(E)] provides basic vectors from classical arrays. New elements can
    only be inserted at the end of the vector.

    [MakeGap(E)] provides vectors with gaps. Module [E] for elements must
    provide a default value. [get v i] is allowed for all [i >= 0], the
    default value is returned if the position [i] was not set before.
    [set v i e] is allowed for all [i >= 0], memory is allocated for all
    positions [0..i] at least.

    The length of a vector [v] is defined as the length of the smallest prefix
    containing all values explicitly set so far. (Equivalently, [length =
    index_max + 1] where [index_max] is the highest index that was set so far.)

    Functors [...OfArray(A)(E)] allow to build vectors from any module [A]
    providing arrays of [E.t] ([Bigarray] for example).
*)


(** Signature for vectors. *)
module type S = sig
  type t
  type elt
  val make : ?size:int -> unit -> t
  val set : t -> int -> elt -> unit
  val get : t -> int -> elt
  val clear : t -> unit

  (** Maximal index given to [set] so far, -1 if no value has been set yet. *)
  val index_max : t -> int

  (** Equivalent to [1 + index_max]. *)
  val length : t -> int

  (** Number of cells allocated in memory. *)
  val capacity : t -> int

  val blit : t -> int -> t -> int -> int -> unit

  (** Default value in vectors, @raise Invalid_argument if no default value is
      associated to vectors defined by the module. *)
  val default : unit -> elt
end

(** Signature for arrays. *)
module type ArrayType = sig
  type t
  type elt
  val make : int -> elt -> t
  val empty : t
  val get : t -> int -> elt
  val set : t -> int -> elt -> unit
  val length : t -> int
  val blit : t -> int -> t -> int -> int -> unit
end

(** Signature for elements. *)
module type DefaultValType = sig
  type t
  val default : t
end

(** Signature for elements when no default value is necessary. *)
module type AnyType = sig
  type t
end


(** Get a Vector from an Array. *)
module OfArrayGap (A : ArrayType) (E : DefaultValType with type t = A.elt) 
  : S with type elt = E.t 
= struct

  type elt = E.t
  let default = E.default

  type t = { 
    mutable v : A.t ; 
    mutable n : int ; (* First index from which no [set] occured. *)
  }

  let make ?(size=4) () =
    { v = A.make size default ; n = 0 ; }

  let resize t n =
    (* shrink by 5/4 -> mem waist <= n/4 ; copy time <= 4n *)
    let len' = n + 2 + n/4 in
    let v' = A.make len' default in
    A.blit t.v 0 v' 0 t.n ;
    t.v <- v'

  let set t i e = 
    if i >= t.n then begin
      let n' = i+1 in
      if i >= A.length t.v then resize t n' ;
      t.n <- n' ;
    end ;
    A.set t.v i e

  let get t i = 
    if i >= t.n then default
    else A.get t.v i

  let clear t =
    for i = 0 to t.n - 1 do A.set t.v i default done ;
    t.n <- 0

  let length t = t.n

  let capacity t = A.length t.v

  let index_max t = (length t) - 1

  let blit src offsrc dst offdst len =
    A.blit src.v offsrc dst.v offdst len

  let default () = E.default

end



module OfArray (A : ArrayType) : S with type elt = A.elt
= struct

  type elt = A.elt
  let default () = invalid_arg "Vector.Ofarray"

  type t = { 
    mutable v : A.t ; 
    mutable n : int ; (* First index from which no [set] occured. *)
  }

  let make ?(size=4) () =
    { v = A.empty ; n = -size ; } (* Store [size] in [n] until we have some
                                     element to make the array. *)

  let resize t n e =
    (* shrink by 5/4 -> mem waist <= n/4 ; copy time <= 4n *)
    let len' = n + 2 + n/4 in
    let v' = A.make len' e in
    A.blit t.v 0 v' 0 t.n ;
    t.v <- v'

  let set t i e = 
    if i >= t.n then begin
      let n' = i+1 in
      if t.n < 0 then begin
        let size = - t.n in
        t.n <- 0 ;
        t.v <- A.make (max (n' + 2 + n'/4) size) e ;
      end ;
      if i > t.n then invalid_arg "Vector.MakeNoDefault.set" ;
      if i >= A.length t.v then resize t n' e ;
      t.n <- n' ;
    end ;
    A.set t.v i e

  let get t i = 
    if t.n < 0 || i >= t.n then invalid_arg "Vector.MakeNoDefault.get"
    else A.get t.v i

  let clear t =
    t.n <- 0

  let length t = t.n

  let index_max t = (length t) - 1

  let capacity t = A.length t.v

  let blit src offsrc dst offdst len =
    A.blit src.v offsrc dst.v offdst len  
end


(** Queue implementation in a vector. It is FIFO : first in is first out. *)
module QueueOfArray (A : ArrayType) = struct

  module V = OfArray (A)

  type t = { mutable v : V.t ; mutable front : int ; mutable back : int ; }
  type elt = V.elt

  let create ?(size=4) () =
    { v = V.make ~size:size () ; front = 0 ; back = 0 ; }

  let is_empty t = t.front = t.back

  let add t e = 
    V.set t.v t.back e ;
    t.back <- t.back + 1

  let peek t =
    if is_empty t then raise Not_found ;
    V.get t.v t.front

  let compact t =
    let len = t.back - t.front in
    V.blit t.v t.front t.v 0 len ;
    t.front <- 0 ;
    t.back <- len

  let pop t =
    if is_empty t then raise Not_found ;
    let e = V.get t.v t.front in
    t.front <- t.front + 1 ;
    if t.front > t.back / 2 && t.back > (V.capacity t.v) / 2 then compact t ;
    e

end


(** Stack implementation in a vector. It is LIFO : last in is first out. *)
module StackOfArray (A : ArrayType) = struct

  module V = OfArray (A)

  type t = { mutable v : V.t ; mutable back : int ; }
  type elt = V.elt

  let create ?(size=4) () =
    { v = V.make ~size:size () ; back = -1 ; }

  let is_empty t = t.back < 0

  let add t e = 
    t.back <- t.back + 1 ;
    V.set t.v t.back e 

  let push = add

  let peek t = 
    if is_empty t then raise Not_found ;
    V.get t.v t.back

  let pop t = 
    if is_empty t then raise Not_found ;
    let e = V.get t.v t.back in
    t.back <- t.back - 1 ;
    e

end



module Array (E : AnyType) : ArrayType with type elt = E.t = struct
  type t = E.t array
  type elt = E.t
  include Array
  let empty = [||]
end

(** Vectors with gaps from usual arrays. [get v i] and [set v i e] are allowed
    for all [i >= 0]. *)
module MakeGap (E : DefaultValType) = OfArrayGap (Array(E)) (E)

(** Vectors from usual arrays. [get v i] is allowed
    for [i] in [0..length v -1]. [set v i e] is allowed for [i] in [0..length
    v]. *)
module Make (E : AnyType) = OfArray (Array(E))

module Queue (E : AnyType) = QueueOfArray (Array(E))

module Stack (E : AnyType) = StackOfArray (Array(E))

(** Examples of vector usage : 

module Int0 = struct type t = int let default = 0 end
module Int0Vec = Vector.MakeGap (Int0)
module IntQueue = Vector.Queue (Int0)
module StringVec = Vector.Make (struct type t = string end)

*)


(** Some utility functions. *)

module Util (A : ArrayType) = struct

  let rec for_iter b e f v =
    for i=b to e-1 do
      f (A.get v b) ;
    done

  let rec for_fold b e f v a =
    if b >= e then a else for_fold (b+1) e f v (f (A.get v b) a)

end

