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
module Unifier :
  sig
    type equality = Equal of Term.term * Term.term
    type t = equality
    val str : equality -> string
    val compare : equality -> equality -> int
    val reflex : equality -> equality
    val sub : equality -> Term.term -> Term.term
    val occurs : Var.var -> Term.term -> bool
    val conflict : Term.term -> Term.term -> bool
    val decomposable : equality -> bool
    val decompose : equality -> equality list
    val getVarsTerm : Term.term -> VarMSet.Mset.t
    val getVarsEq : equality -> VarMSet.Mset.t
  end
module EqSet :
  sig
    type elt = Unifier.t
    type t = Set.Make(Unifier).t
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
module UniSet :
  sig
    type t = { vSet : VarMSet.t; eqSet : EqSet.t; }
    val union : t -> t -> t
    val add : Unifier.equality -> t -> t
    val add_from_list : Unifier.equality list -> t -> t
    val add_from_seq : Unifier.equality Seq.t -> t -> t
    val remove : Unifier.equality -> t -> t
    val contains : Var.var -> t -> bool
    val empty : t
    val substitute : Unifier.equality -> t -> t
    val from_set : EqSet.t -> t
  end
module Unify :
  sig
    type t = UniSet.t = { vSet : VarMSet.t; eqSet : EqSet.t; }
    val union : t -> t -> t
    val add : Unifier.equality -> t -> t
    val add_from_list : Unifier.equality list -> t -> t
    val add_from_seq : Unifier.equality Seq.t -> t -> t
    val remove : Unifier.equality -> t -> t
    val contains : Var.var -> t -> bool
    val empty : t
    val substitute : Unifier.equality -> t -> t
    val from_set : EqSet.t -> t
    val eliminate : Unifier.equality -> EqSet.t -> EqSet.t
    val occurs_rec : Unifier.equality -> bool
    val conflict : Unifier.equality -> bool
    val unify_step : t * EqSet.t -> UniSet.t * EqSet.t
    val unify : t * EqSet.t -> t * EqSet.t
  end
val pretty_print : EqSet.t -> string
