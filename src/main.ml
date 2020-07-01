open UnifAlg

let t1 = Term.(`Term ("f",[`Var "a"]))
let t2 = Term.(`Term ("g",[`Var "b"]))
let t3 = Term.(`Term ("f",[`Var "c"]))
let t4 = Term.(`Term ("z",[`Var "a"]))
let t5 = Term.(`Term ("w",[`Var "c"]))

(* Test #1 *)
let constC = Term.(`Term ("C",[]))
let constK = Term.(`Term ("K",[]))
let g_of_cx = Term.(`Term ("g", [constC; `Var "x"]))
let f_of_g_of_cx = Term.(`Term ("f", [g_of_cx]))

let h_of_yk = Term.(`Term ("h", [`Var "y"; constK]))
let f_of_h_of_yk = Term.(`Term ("f", [h_of_yk]))

let initSet (s : EqSet.t) = (UniSet.from_set s, s)

let eq1_test1 = Unifier.Equal (f_of_g_of_cx,f_of_h_of_yk)
let start_test_1 = EqSet.(empty |> add eq1_test1)
let (unif_test1, candidates_test1, log_test1) = Unify.unify (initSet start_test_1)

let eq_set_str (s : EqSet.t) = let representation = List.fold_left (fun x y -> x ^ y) "" (List.map (fun x -> (Unifier.str x) ^ ";") (EqSet.elements s)) in
                                if String.length representation > 0 then "{" ^ (String.sub representation 0 ((String.length representation)-1)) ^ "}"
                                else "{}"
let log_str (s : Unify.log list) = String.concat "\n" (List.rev (List.map (fun x -> (Unify.log_str x)) s))

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

let (unif_0, candidates_0, log_0) = Unify.unify_step (foo_uni,foo, [])
let (unif_1, candidates_1, log_1) = Unify.unify_step (unif_0,candidates_0, [])
let (unif_result, candidates_result, log_result) = Unify.unify (foo_uni,foo)

let main() = print_string (log_str log_test1);
        print_newline ();
        print_string (eq_set_str (unif_test1.eqSet));
        print_newline ();;
(* "Pretty" print the EqSet elements *)
(* List.fold_left (fun x y -> x ^ y) "" (List.map Unifier.str (EqSet.elements foo)) *)

let () = main ()
