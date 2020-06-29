open UnifAlg

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