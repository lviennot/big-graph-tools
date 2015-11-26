(* Laurent Viennot, Inria 2015 *)

module type S = sig

end

let () =
  add_edge g (V.index x) (V.index y) ;
  iter_succ (fun v -> do_smthg (V.label v)) g (V.index x) ;
  ()


