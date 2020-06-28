(* Robinson Unification Algorithm *)
module Var =
  struct
    type var = [`Var of string]
    type t = var
    let compare (v1 : var) (v2 : var) = compare v1 v2
  end
module Term =
  struct
    type term =
      [
        `Var of string
      | `Term of string * term list
      ]
    type t = term
    let compare (t1 : term) (t2 : term) = compare t1 t2
    let rec str (t : term) = match t with
      | `Var s -> s
      | `Term (s,l) -> s ^ "(" ^ (String.concat "," (List.map str l)) ^ ")"
    (* let rec sub (a : term) (sub : term) (t : term) = true *)
    (* let rec sub (`Var a) (sub : term) (t : term) = true *)
end

let t1 = Term.(`Term ("f",[`Var "a"]))
let t2 = Term.(`Term ("g",[`Var "b"]))
let t3 = Term.(`Term ("f",[`Var "c"]))
let t4 = Term.(`Term ("z",[`Var "a"]))
let t5 = Term.(`Term ("w",[`Var "c"]))

(* Need to comment this to load into utop and ...*)
open MultiSet
module VarMSet = MultiSet.Make(Var)

(* uncomment this *)
(* module VarMSet = Make(Var) *)

module Unifier =
  struct
    type equality = Equal of Term.term * Term.term
    type t = equality
    let str (eq : equality) = match eq with Equal (t1,t2) -> Term.str t1 ^ "=" ^ Term.str t2
    let compare (eq1 : equality) (eq2 : equality) = compare eq1  eq2
    let reflex (eq : equality) = match eq with Equal (t1,t2) -> Equal (t2,t1)
    let rec sub (equal : equality) (t : Term.term) : Term.term =
      match equal with
      | Equal (t_l,t_r) -> if t_l = t then t_r else match t with
                                                    | `Term (f,l) -> `Term (f, List.map (sub equal) l)
                                                    | _ -> t
    let rec occurs (v : Var.var) (t : Term.term) = match t with
      | `Term (f, l) -> List.exists (occurs v) l
      | _ -> t = (v :> Term.term)
    let conflict (t1: Term.term) (t2: Term.term) = match t1, t2 with
      | `Term (f,l1), `Term (g,l2) -> (f <> g) || (List.length l1 <> List.length l2)
      | _ -> false
    let decomposable (eq: equality) = match eq with Equal (t1,t2) ->
                                                     match t1,t2 with
                                                     | `Term (f, l_f), `Term (g, l_g) ->
                                                        if (f = g) && (List.length l_f = List.length l_g) then
                                                          true
                                                        else
                                                          false
                                                     | _,_ -> false

    let decompose (eq: equality) = match eq with Equal (t1,t2) ->
                                                  match t1,t2 with
                                                  | `Term (f, l_f), `Term (g, l_g) -> List.rev_map2 (fun x y -> Equal (x,y)) l_f l_g
                                                  | _,_ -> []
    let rec getVarsTerm (t : Term.term) = match t with
      | `Var x -> VarMSet.(empty |> add (`Var x))
      | `Term (f, l)-> List.fold_left VarMSet.union VarMSet.empty (List.map getVarsTerm l)

    let getVarsEq (eq : equality) = match eq with Equal (t1,t2) -> VarMSet.union (getVarsTerm t1) (getVarsTerm t2)
  end

module EqSet = Set.Make(Unifier)
module UniSet =
  struct
    open Unifier
    type t = {vSet : VarMSet.t; eqSet : EqSet.t;}
    let union (s0 : t) (s1 : t) = {vSet = VarMSet.union s0.vSet s1.vSet; eqSet = EqSet.union s0.eqSet s1.eqSet}
    let add (eq : Unifier.equality) (uniSet : t) = let found = EqSet.find_opt eq uniSet.eqSet in
                            match found with None -> {vSet = VarMSet.union uniSet.vSet (Unifier.getVarsEq eq); eqSet = EqSet.add eq uniSet.eqSet} | _ -> uniSet
    let add_from_list (eq_list : Unifier.equality list) (uniSet : t) = List.fold_left (fun s x -> add x s) uniSet eq_list
    let add_from_seq (eq_seq: Unifier.equality Seq.t) (uniSet : t) = Seq.fold_left (fun s x -> add x s) uniSet eq_seq
    let remove (eq : Unifier.equality) (uniSet : t) = let found = EqSet.find_opt eq uniSet.eqSet in
                               match found with None -> uniSet | Some eq0 -> let vars = Unifier.getVarsEq eq0 in
                                                      {vSet = VarMSet.diff uniSet.vSet vars ; eqSet = EqSet.remove eq0 uniSet.eqSet}
    let contains (x : Var.var) (uniSet: t) = VarMSet.mem (x,0) uniSet.vSet
    let empty = {vSet = VarMSet.empty; eqSet = EqSet.empty}
    let substitute (eq_sub : Unifier.equality) (uniSet: t) = match eq_sub with
      (* | Equal (`Var x, `Var y) -> uniSet *)
      | Equal (`Var x,t) -> let modifications = Seq.filter_map (fun eq -> match eq with
                                                                     | Equal (`Var x,`Var y) -> None
                                                                     | Equal (t1,t2) -> let new_eq = Equal (Unifier.sub eq_sub t1, Unifier.sub eq_sub t2) in
                                                                 if eq = new_eq then None else Some new_eq) (EqSet.to_seq uniSet.eqSet) in
     add_from_seq modifications empty

      | _ -> failwith "Invalid equation substitution, must be of the form Var = Term"
    let from_set (s : EqSet.t) = Seq.fold_left (fun s x -> add x s) empty (EqSet.to_seq s)
  end
module Unify =
  struct
  include UniSet
    let eliminate (eq : Unifier.equality) (eqSet : EqSet.t) = match eq with
      | Equal (t1, t2) -> EqSet.(map (fun x -> match x with Equal (t1,t2) -> Equal (Unifier.sub eq t1,Unifier.sub eq t2)) eqSet |> EqSet.add eq)
    (*occur check. If given equality of the form x = f(s0,...,sk) checking that for all si, x =/= si*)
    let occurs_rec (eq : Unifier.equality) = match eq with Equal (t1,t2) -> match t1, t2 with
                                                                          | `Var x, `Term (f, l) -> Unifier.occurs (`Var x) t2
                                                                          | _, _ -> false
    let conflict (eq : Unifier.equality) = match eq with Equal (t1,t2) -> Unifier.conflict t1 t2
    let unify_step (set, candidates) : UniSet.t * EqSet.t = let eq0 = EqSet.choose candidates in
                                  match eq0 with
                                    Equal (t1,t2) -> match t1, t2 with
                                                     | `Var x, `Var y -> let new_candidates = EqSet.remove eq0 candidates
                                                                         and delta = UniSet.substitute eq0 set in
                                                                        (UniSet.union set delta, EqSet.union new_candidates delta.eqSet)
                                                     | `Term (f, l), `Var x -> (UniSet.(remove eq0 set|> add (Unifier.reflex eq0)),EqSet.(remove eq0 set.eqSet |> add (Unifier.reflex eq0))) (*flip or swap*)
                                                     | `Var x, `Term (f, l)  -> let contains = UniSet.contains (`Var x) set
                                                                                and occurs = (Unifier.occurs (`Var x) (`Term (f, l))) in
                                                         if  contains && not occurs then (UniSet.union set (UniSet.substitute eq0 set), EqSet.remove eq0 candidates)
                                                         else if not contains then (set, EqSet.remove eq0 candidates)
                                                         else (UniSet.empty, EqSet.empty)
                                                     | _,_ -> if t1 = t2 then (UniSet.remove eq0 set, EqSet.remove eq0 candidates) (* Delete rule *)
                                                              (* Decompose rule *)
                                                              else if Unifier.decomposable eq0 then
                                                                let new_eqs_list = Unifier.decompose eq0 in (UniSet.add_from_list new_eqs_list (UniSet.remove eq0 set), EqSet.(remove eq0 candidates |> union (EqSet.of_list new_eqs_list)))
                                                              else (UniSet.empty, EqSet.empty)
    let rec unify(set, candidates) : UniSet.t * EqSet.t = if EqSet.is_empty candidates then (set, candidates)
                                                          else unify(unify_step (set,candidates))
  end
let pretty_print t = List.fold_left (fun x y -> x ^ y) "" (List.map (fun x -> (Unifier.str x) ^ ";") (EqSet.elements t))

let eq1 = Unifier.Equal (t1, t2)
let eq2 = Unifier.Equal (t2, t2)
let eq3 = Unifier.Equal (t3, t1)
let eq4 = Unifier.Equal (t3, t4)
let eq5 = Unifier.Equal (t1, t5)
let eqSub = Unifier.Equal (`Var "c", `Term ("y", [`Var "c0"; `Var "c1"]))

let failing = EqSet.(empty |> add eq1 |> add eq2 |> add eq3 |> add eq4 |> add eq5)
let failing_uni = UniSet.from_set failing

let foo = EqSet.(empty |> add eq2 |> add eq3)
let foo_uni = UniSet.from_set foo
let show_unif (unif : UniSet.t) = EqSet.elements unif.eqSet
let show_candidates (candidates : EqSet.t ) = EqSet.elements candidates

let (unif_0, candidates_0) = Unify.unify_step (foo_uni,foo)
let (unif_1, candidates_1) = Unify.unify_step (unif_0,candidates_0)
let (unif_result, candidates_result) = Unify.unify (foo_uni,foo)

(* "Pretty" print the EqSet elements *)
(* List.fold_left (fun x y -> x ^ y) "" (List.map Unifier.str (EqSet.elements foo)) *)

(* List.fold_left (fun x y -> x ^ y) "" (List.map (fun x -> (Unifier.str x) ^ ";") (EqSet.elements foo)) *)
