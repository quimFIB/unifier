module OrderedPair :
  functor (E : Set.OrderedType) ->
    sig type t = E.t * int val compare : t -> t -> int end
module Make :
  functor (O : Set.OrderedType) ->
    sig
      module SetPair : sig type t = O.t * int val compare : t -> t -> int end
      module Mset :
        sig
          type elt = SetPair.t
          type t = Set.Make(SetPair).t
          val empty : t
          val is_empty : t -> bool
          val mem : elt -> t -> bool
          val add : elt -> t -> t
          val singleton : elt -> t
          val remove : elt -> t -> t
          val union : t -> t -> t
          val inter : t -> t -> t
          val disjoint : t -> t -> bool
          val diff : t -> t -> t
          val compare : t -> t -> int
          val equal : t -> t -> bool
          val subset : t -> t -> bool
          val iter : (elt -> unit) -> t -> unit
          val map : (elt -> elt) -> t -> t
          val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
          val for_all : (elt -> bool) -> t -> bool
          val exists : (elt -> bool) -> t -> bool
          val filter : (elt -> bool) -> t -> t
          val partition : (elt -> bool) -> t -> t * t
          val cardinal : t -> int
          val elements : t -> elt list
          val min_elt : t -> elt
          val min_elt_opt : t -> elt option
          val max_elt : t -> elt
          val max_elt_opt : t -> elt option
          val choose : t -> elt
          val choose_opt : t -> elt option
          val split : elt -> t -> t * bool * t
          val find : elt -> t -> elt
          val find_opt : elt -> t -> elt option
          val find_first : (elt -> bool) -> t -> elt
          val find_first_opt : (elt -> bool) -> t -> elt option
          val find_last : (elt -> bool) -> t -> elt
          val find_last_opt : (elt -> bool) -> t -> elt option
          val of_list : elt list -> t
          val to_seq_from : elt -> t -> elt Seq.t
          val to_seq : t -> elt Seq.t
          val add_seq : elt Seq.t -> t -> t
          val of_seq : elt Seq.t -> t
        end
      type elt = SetPair.t
      type t = Set.Make(SetPair).t
      val empty : t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val singleton : elt -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val compare : t -> t -> int
      val equal : t -> t -> bool
      val subset : t -> t -> bool
      val iter : (elt -> unit) -> t -> unit
      val map : (elt -> elt) -> t -> t
      val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val filter : (elt -> bool) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val split : elt -> t -> t * bool * t
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val add : O.t -> Mset.t -> Mset.t
      val remove : O.t -> Mset.t -> Mset.t
      val decrease : SetPair.t -> Mset.t -> Mset.t
      val increase : SetPair.t -> Mset.t -> Mset.t
      val diff : Mset.t -> Mset.t -> Mset.t
      val union : Mset.t -> Mset.t -> Mset.t
    end
module StrMSet :
  sig
    module SetPair :
      sig type t = String.t * int val compare : t -> t -> int end
    module Mset :
      sig
        type elt = SetPair.t
        type t = Set.Make(SetPair).t
        val empty : t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val compare : t -> t -> int
        val equal : t -> t -> bool
        val subset : t -> t -> bool
        val iter : (elt -> unit) -> t -> unit
        val map : (elt -> elt) -> t -> t
        val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val filter : (elt -> bool) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val split : elt -> t -> t * bool * t
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end
    type elt = SetPair.t
    type t = Set.Make(SetPair).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val singleton : elt -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
    val add : String.t -> Mset.t -> Mset.t
    val remove : String.t -> Mset.t -> Mset.t
    val decrease : SetPair.t -> Mset.t -> Mset.t
    val increase : SetPair.t -> Mset.t -> Mset.t
    val diff : Mset.t -> Mset.t -> Mset.t
    val union : Mset.t -> Mset.t -> Mset.t
  end
module Test : sig val m : StrMSet.Mset.t val n : StrMSet.Mset.t end
