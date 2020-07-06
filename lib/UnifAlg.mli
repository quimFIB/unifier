module EqSet :
  sig
    type elt = PropositionalSymbols.Equality.t
    type t = Set.Make(PropositionalSymbols.Equality).t
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
    val substitute : PropositionalSymbols.Equality.t -> t -> t
  end
module UniSet :
  sig
    type t = { vSet : PropositionalSymbols.VarMSet.t; eqSet : EqSet.t; }
    val union : t -> t -> t
    val add : PropositionalSymbols.Equality.t -> t -> t
    val add_from_list : PropositionalSymbols.Equality.t list -> t -> t
    val add_from_seq : PropositionalSymbols.Equality.t Seq.t -> t -> t
    val remove : PropositionalSymbols.Equality.t -> t -> t
    val contains : PropositionalSymbols.Var.var -> t -> bool
    val empty : t
    val substitute : PropositionalSymbols.Equality.t -> t -> t
    val from_set : EqSet.t -> t
  end
module Unify :
  sig
    type t =
      UniSet.t = {
      vSet : PropositionalSymbols.VarMSet.t;
      eqSet : EqSet.t;
    }
    val union : t -> t -> t
    val add : PropositionalSymbols.Equality.t -> t -> t
    val add_from_list : PropositionalSymbols.Equality.t list -> t -> t
    val add_from_seq : PropositionalSymbols.Equality.t Seq.t -> t -> t
    val remove : PropositionalSymbols.Equality.t -> t -> t
    val contains : PropositionalSymbols.Var.var -> t -> bool
    val empty : t
    val substitute : PropositionalSymbols.Equality.t -> t -> t
    val from_set : EqSet.t -> t
    type case = Var2Var | Term2Var | Var2Term | Term2Term
    val case_str : case -> string
    type unifyStep = Del | Flip | Fail | Substitute | Decompose | Eliminate
    type log = Log of unifyStep * PropositionalSymbols.Equality.t * case
    val log_str : log -> string
    val eliminate : PropositionalSymbols.Equality.t -> EqSet.t -> EqSet.t
    val occurs_rec : PropositionalSymbols.Equality.t -> bool
    val conflict : PropositionalSymbols.Equality.t -> bool
    val initSet : EqSet.t -> UniSet.t * EqSet.t
    val unify_step :
      UniSet.t * EqSet.t * log list -> UniSet.t * EqSet.t * log list
    val unify_loop :
      UniSet.t * EqSet.t * log list -> UniSet.t * EqSet.t * log list
    val unify : EqSet.t -> UniSet.t * EqSet.t * log list
    val unify_loop_logger :
      UniSet.t * EqSet.t * log list * UniSet.t list * EqSet.t list ->
      UniSet.t * EqSet.t * log list * UniSet.t list * EqSet.t list
    val unify_logger :
      EqSet.t -> UniSet.t * EqSet.t * log list * UniSet.t list * EqSet.t list
  end
