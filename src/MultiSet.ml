(* MultiSet implemented with a Set of Pairs *)
module OrderedPair (E : Set.OrderedType) =
  struct
   type t = E.t * int
   let compare (x:t) (y:t) = match x,y with (x0, nX), (y0, nY) -> E.compare x0 y0
  end
module Make (O : Set.OrderedType) =
  struct
    module SetPair = OrderedPair(O)
    module Mset = Set.Make(SetPair)
    include Mset
    let add (x : O.t) (s : Mset.t) = let found = Mset.find_opt (x,0) s in
                   match found with None -> Mset.add (x,1) s | Some (x,c) -> Mset.(remove (x,c) s |> add (x,c+1))
    let remove (x : O.t) (s : Mset.t) = let found = Mset.find_opt (x,0) s in
                 match found with None -> s | Some (x,c) -> if c > 1 then Mset.(remove (x,c) s |> add (x,c-1))
                                                            else Mset.remove (x,c) s
    let decrease ((x,k) : SetPair.t) (s : Mset.t) = let found = Mset.find_opt (x,k) s in
                 match found with None -> s | Some (y,c) -> if c > k then Mset.(remove (y,c) s |> add (x,c-k))
                                                            else Mset.remove (y,c) s
    let increase ((x,k) : SetPair.t) (s : Mset.t) = let found = Mset.find_opt (x,k) s in
                 match found with None -> Mset.add (x,k) s | Some (y,c) ->  Mset.(remove (y,c) s |> add (x,c+k))
    let diff (s0 : Mset.t) (s1 : Mset.t) = Seq.fold_left (fun s x -> decrease x s) s0 (Mset.to_seq s1)
    let union (s0 : Mset.t) (s1 : Mset.t) = Seq.fold_left (fun s x -> increase x s) s0 (Mset.to_seq s1)
  end


(* MultiSet implemented as a tuple (Set,Map)
 *module MSet (O : Set.OrderedType) =
 *   struct
 *     module Mmap = Map.Make(O)
 *     module MSet_ = Set.Make(O)
 *     open Mmap
 *     type map = int Mmap.t
 *     type t = MSet_.t * map
 *     let add (v : O.t) (elems,counters) : t = let new_elems = MSet_.add v elems
 *                                                           and new_counters = Mmap.update v (fun m -> match m with
 *                                                                                                         | Some y -> Some (y+1)
 *                                                                                                         | None -> Some 1) counters  in
 *                                    (new_elems,new_counters)
 *     let remove(v : O.t) (elems,counters) : t = let new_elems = MSet_.add v elems
 *                                                             and new_counters = Mmap.update v (fun m -> match m with
 *                                                                                                         | Some 1 -> None
 *                                                                                                         | Some y -> Some (y-1)
 *                                                                                                         | None -> Some 1) counters  in
 *                                    (new_elems,new_counters)
 *     let union ((vms1_elems,vms1_counters): t) ((vms2_elems,vms2_counters): t) = (MSet_.union vms1_elems vms2_elems, Mmap.union (fun k x y -> Some (x+y)) vms1_counters vms2_counters)
 *     let empty = (MSet_.empty,Mmap.empty)
 *   end *)
(* TESTS *)
module StrMSet = Make(String)
module Test =
struct
    let m = StrMSet.(empty |> add "a" |> add "b" |> add "a")
    let n = StrMSet.(empty |> add "a")
end
