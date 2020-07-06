module Var :
  sig
    type var = [ `Var of string ]
    type t = var
    val compare : var -> var -> int
  end
module Term :
  sig
    type term = [ `Term of string * term list | `Var of string ]
    type t = term
    val compare : term -> term -> int
    val str : term -> string
  end
module VarMSet :
  sig
    module SetPair : sig type t = Var.t * int val compare : t -> t -> int end
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
    val add : Var.t -> Mset.t -> Mset.t
    val remove : Var.t -> Mset.t -> Mset.t
    val decrease : SetPair.t -> Mset.t -> Mset.t
    val increase : SetPair.t -> Mset.t -> Mset.t
    val diff : Mset.t -> Mset.t -> Mset.t
    val union : Mset.t -> Mset.t -> Mset.t
  end
module Equality :
  sig
    type t = Equal of Term.term * Term.term
    val str : t -> string
    val compare : t -> t -> int
    val reflex : t -> t
    val sub : t -> Term.term -> Term.term
    val occurs : Var.var -> Term.term -> bool
    val conflict : Term.term -> Term.term -> bool
    val decomposable : t -> bool
    val decompose : t -> t list
    val getVarsTerm : Term.term -> VarMSet.Mset.t
    val getVarsEq : t -> VarMSet.Mset.t
  end
