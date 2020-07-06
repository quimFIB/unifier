(* NOT Robinson's Unification Algorithm *)
open PropositionalSymbols

module EqSet =
  struct
    include Set.Make(Equality)
    let substitute (eq_sub : Equality.t) (eqSet: t) = match eq_sub with
      (* | Equality.Equal (`Var x, `Var y) -> eqSet *)
      | Equality.Equal (`Var x,t) -> let modSeq = Seq.map (fun eq -> match eq with
                                                                    | Equality.Equal (`Var x,`Var y) -> (eq, false)
                                                                    | Equality.Equal (t1,t2) -> let new_eq = Equality.Equal (Equality.sub eq_sub t1, Equality.sub eq_sub t2) in
                                                                        if eq = new_eq then (eq, false) else (new_eq,true)) (to_seq eqSet) in
            Seq.fold_left (fun s x -> add x s) empty (Seq.map (fun (x,b) -> x) modSeq)
      | _ -> failwith "Invalid equation substitution, must be of the form Var = Term"
  end

module UniSet =
  struct
    open Equality
    type t = {vSet : VarMSet.t; eqSet : EqSet.t;}
    let union (s0 : t) (s1 : t) = {vSet = VarMSet.union s0.vSet s1.vSet; eqSet = EqSet.union s0.eqSet s1.eqSet}
    let add (eq : Equality.t) (uniSet : t) = let found = EqSet.find_opt eq uniSet.eqSet in
                            match found with None -> {vSet = VarMSet.union uniSet.vSet (Equality.getVarsEq eq); eqSet = EqSet.add eq uniSet.eqSet} | _ -> uniSet
    let add_from_list (eq_list : Equality.t list) (uniSet : t) = List.fold_left (fun s x -> add x s) uniSet eq_list
    let add_from_seq (eq_seq: Equality.t Seq.t) (uniSet : t) = Seq.fold_left (fun s x -> add x s) uniSet eq_seq
    let remove (eq : Equality.t) (uniSet : t) = let found = EqSet.find_opt eq uniSet.eqSet in
                               match found with None -> uniSet | Some eq0 -> let vars = Equality.getVarsEq eq0 in
                                                      {vSet = VarMSet.diff uniSet.vSet vars ; eqSet = EqSet.remove eq0 uniSet.eqSet}
    let contains (x : Var.var) (uniSet: t) = VarMSet.mem (x,0) uniSet.vSet
    let empty = {vSet = VarMSet.empty; eqSet = EqSet.empty}
    let substitute (eq_sub : Equality.t) (uniSet: t) = match eq_sub with
      (* | Equal (`Var x, `Var y) -> uniSet *)
      | Equal (`Var x,t) -> let modifications = Seq.map (fun eq -> match eq with
                                                                     | Equal (`Var x,`Var y) -> (eq,false)
                                                                     | Equal (t1,t2) -> let new_eq = Equal (Equality.sub eq_sub t1, Equality.sub eq_sub t2) in
                                                                 if eq = new_eq then (eq,false) else (new_eq,true)) (EqSet.to_seq uniSet.eqSet) in
     (* let updated_eqs = Seq.filter (fun (x,modified) -> modified) modifications in *)
     {vSet = Seq.fold_left (fun s s' -> VarMSet.union s s') uniSet.vSet (Seq.filter_map (fun (x,modified) -> let new_vars = getVarsTerm t in if modified then Some new_vars else None) modifications);
     eqSet = EqSet.substitute eq_sub uniSet.eqSet}
      | _ -> failwith "Invalid equation substitution, must be of the form Var = Term"
    let from_set (s : EqSet.t) = Seq.fold_left (fun s x -> add x s) empty (EqSet.to_seq s)
  end

module Unify =
  struct
  include UniSet
    type case = Var2Var | Term2Var | Var2Term | Term2Term
    let case_str (c : case) = match c with
                              | Var2Var -> "Var2Var"
                              | Term2Var -> "Term2Var"
                              | Var2Term -> "Var2Term"
                              | Term2Term -> "Term2Term"
    type unifyStep = Del | Flip | Fail | Substitute | Decompose | Eliminate
    type log = Log of unifyStep * Equality.t * case
    let log_str (log : log) = match log with
                              | Log (Del, eq, c) -> "[Del;" ^ Equality.str eq ^ "; " ^ case_str c ^ "]"
                              | Log (Flip, eq, c) -> "[Flip;" ^ Equality.str eq ^ "; " ^ case_str c ^ "]"
                              | Log (Fail, eq, c) -> "[Fail;" ^ Equality.str eq ^ "; " ^ case_str c ^ "]"
                              | Log (Substitute, eq, c) -> "[Substitute;" ^ Equality.str eq ^ "; " ^ case_str c ^ "]"
                              | Log (Decompose, eq, c) -> "[Decompose;" ^ Equality.str eq ^ "; " ^ case_str c ^ "]"
                              | Log (Eliminate, eq, c) -> "[Eliminate;" ^ Equality.str eq ^ "; " ^ case_str c ^ "]"

    let eliminate (eq : Equality.t) (eqSet : EqSet.t) = match eq with
      | Equal (t1, t2) -> EqSet.(map (fun x -> match x with Equal (t1,t2) -> Equal (Equality.sub eq t1,Equality.sub eq t2)) eqSet |> EqSet.add eq)
    (*occur check. If given equality of the form x = f(s0,...,sk) checking that for all si, x =/= si*)
    let occurs_rec (eq : Equality.t) = match eq with Equal (t1,t2) -> match t1, t2 with
                                                                          | `Var x, `Term (f, l) -> Equality.occurs (`Var x) t2
                                                                          | _, _ -> false
    let conflict (eq : Equality.t) = match eq with Equal (t1,t2) -> Equality.conflict t1 t2
    let initSet (s : EqSet.t) = (UniSet.from_set s, s)
    let unify_step ((set, candidates, log) : UniSet.t * EqSet.t * log list) = let eq0 = EqSet.choose candidates in
                                                     match eq0 with
                                                       Equal (t1,t2) -> match t1, t2 with
                                                                        | `Var x, `Var y -> (UniSet.substitute eq0 set, EqSet.remove eq0 candidates, List.cons (Log (Substitute, eq0, Var2Var)) log)
                                                                        | `Term (f, l), `Var x -> (UniSet.(remove eq0 set|> add (Equality.reflex eq0)),EqSet.(remove eq0 set.eqSet |> add (Equality.reflex eq0)),
                                                                                                   List.cons (Log (Flip, eq0, Term2Var)) log) (*flip or swap*)
                                                                        | `Var x, `Term (f, l)  -> let contains = UniSet.contains (`Var x) set
                                                                                                   and occurs = (Equality.occurs (`Var x) (`Term (f, l))) in
                                                                            if  contains && not occurs then let subs_set = UniSet.substitute eq0 (UniSet.remove eq0 set) in
                                                                                                            (UniSet.add eq0 subs_set, EqSet.substitute eq0 (EqSet.remove eq0 candidates), List.cons (Log (Eliminate, eq0, Var2Term)) log)
                                                                            else if not contains then (set, EqSet.remove eq0 candidates, List.cons (Log (Del, eq0, Var2Term)) log)
                                                                            else (UniSet.empty, EqSet.empty, List.cons (Log (Fail, eq0, Var2Term)) log)
                                                                        | _,_ -> if t1 = t2 then (UniSet.remove eq0 set, EqSet.remove eq0 candidates, List.cons (Log (Del, eq0, Term2Term)) log) (* Delete rule *)
                                                                                  (* Decompose rule *)
                                                                                 else if Equality.decomposable eq0 then
                                                                                   let new_eqs_list = Equality.decompose eq0 in (UniSet.add_from_list new_eqs_list (UniSet.remove eq0 set),
                                                                                                                                EqSet.(remove eq0 candidates |> union (EqSet.of_list new_eqs_list)),
                                                                                                                                List.cons (Log (Decompose, eq0, Term2Term)) log)
                                                                                 else (UniSet.empty, EqSet.empty, List.cons (Log (Fail, eq0, Term2Term)) log)
    let rec unify_loop ((set, candidates, log) : UniSet.t * EqSet.t * log list)  = if EqSet.is_empty candidates then (set, candidates, log)
                                                                                  else unify_loop(unify_step (set,candidates,log))
    let unify (eqs_to_be_unified : EqSet.t) = let (set,candidates) = initSet eqs_to_be_unified in unify_loop (set, candidates, [])
    let rec unify_loop_logger ((set, candidates, log, set_list, candidates_list) : UniSet.t * EqSet.t * log list * UniSet.t list * EqSet.t list)
      = if EqSet.is_empty candidates then (set, candidates, log, set_list, candidates_list)
        else let (set_step, candidates_step, log_step) = unify_step (set,candidates,log) in unify_loop_logger(set_step, candidates_step, log_step, List.cons set_step set_list, List.cons candidates_step candidates_list)
    let unify_logger (eqs_to_be_unified : EqSet.t) = let (set,candidates) = initSet eqs_to_be_unified in unify_loop_logger(set,candidates,[],[],[])
  end

(* let pretty_print t = List.fold_left (fun x y -> x ^ y) "" (List.map (fun x -> (Equality.str x) ^ ";") (EqSet.elements t)) *)
