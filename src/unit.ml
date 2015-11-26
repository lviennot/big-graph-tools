(* Laurent Viennot, Inria 2015 *)

let unit () =
  IntDigraph.unit () ;
  Traversal.unit () ;
  ()

let () = Debug.run ~verbose:true unit
