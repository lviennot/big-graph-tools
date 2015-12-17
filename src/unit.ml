(* Laurent Viennot, Inria 2015 *)

let unit () =
  Vector.unit () ;
  IntDigraph.unit () ;
  Traversal.unit () ;
  Component.unit () ;
  ()

let () = Debug.run ~verbose:true unit
