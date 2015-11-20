(* Little usefull extensions... *)

module String = struct

  include String

  let of_iter ?separator:(sep=" ") iter elt_to_str set =
    let b = Buffer.create 20 in
    let first = ref true in
    iter (fun e -> 
      if !first then first := false else Buffer.add_string b sep ; 
      Buffer.add_string b (elt_to_str e) ;
    ) set ;
    Buffer.contents b

  let of_list ?separator:(sep=" ") = of_iter ~separator:sep List.iter

  let split ?separator:(sep=' ') ?separator':(sep'=sep) 
            ?iter_sep:(it_sep=false) s =
    let n = length s in
    let rec next_non_sep i =
      if i >= n then n else
      if not it_sep then i + 1 else
      if s.[i] <> sep && s.[i] <> sep' then i else
      next_non_sep (i + 1)
    in

    let i = try index_from s 0 sep with Not_found -> n in
    let i = 
      if sep = sep' then i 
      else min i (try index_from s 0 sep' with Not_found -> n) in
    if i = n then raise Not_found ;

    let bef = sub s 0 (i - 0) in

    let i = if not it_sep then i+1 else next_non_sep (i+1) in
    let aft = sub s i (n-i) in

    bef, aft


  let rec split_n ?separator:(sep=' ') ?separator':(sep'=sep)
                  ?iter_sep:(it_sep=false) n s =
    if n <=1 then [s] else
      try
        let e, s = split ~separator:sep ~separator':sep' ~iter_sep:it_sep s in
        e :: (split_n ~separator:sep ~separator':sep' ~iter_sep:it_sep (n-1) s)
      with Not_found -> (* less than n elements *)
        [s]

  let split_all ?separator:(sep=' ') ?separator':(sep'=sep)
                ?iter_sep:(it_sep=false) s =
    split_n ~separator:sep ~separator':sep' ~iter_sep:it_sep (length s) s

  let split_space_all = split_all ~separator:' ' ~separator':'\t' ~iter_sep:true

  let trim s =
    let space c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
    let i = ref 0 and n = length s in
    while !i < n && space s.[!i] do incr i done ;
    let j = ref (n - 1) in
    while !j > !i && space s.[!j] do decr j done ;
    if !i >= n then "" else sub s !i (!j + 1 - !i)

  let has_suffix s suf =
    let n = length s and nsuf = length suf in
    n >= nsuf && String.sub s (n - nsuf) nsuf = suf

  let del_suffix s suf =
    let n = length s and nsuf = length suf in
    String.sub s 0 (n - nsuf)

end


module Random = struct

  include Random

  let permute a =
    let n = Array.length a in
    for i = 0 to n - 1 do
      let j = i + Random.int (n - i) in
      let tmp = a.(i) in
      a.(i) <- a.(j) ;
      a.(j) <- tmp ;
    done

end


module List = struct

  include List

  let group cmp l =
    let l = sort cmp l in
    let rec iter acc equiv = function
      | [] -> if equiv = [] then acc else equiv :: acc
      | e :: l ->
        let acc, equiv = match equiv with
          | [] -> acc, [e]
          | e' :: _ when cmp e e' = 0 -> acc, e :: equiv
          | _ -> equiv :: acc, [e]
        in iter acc equiv l
    in List.rev (iter [] [] l)

end


let unit () =
  assert (String.split ~separator:'-' "12-34-56" = ("12", "34-56")) ;
  assert (String.split_space_all "   12   34\t56      78  "
            = [""; "12"; "34"; "56"; "78"; "";]) ;
  assert (String.split_n 3 "12 34 56" = ["12"; "34"; "56"]) ;
  assert (let by_x (i,_) (i',_) = compare i i' in
          let l = List.group by_x [1,2; 3,4; 1,1;] in
          let l = List.map (List.sort compare) l in
          l = [[1,1; 1,2]; [3,4]]) ;
  ()
