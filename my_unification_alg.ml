(* A small OCaml program *)
print_string "Hello World!\n"

module Term =
  struct
    type term =
      | Var of string
      | Term of string * term list

    let rec str (t : term) = match t with
      | Var s -> s
      | Term (s,l) -> s ^ "(" ^ (String.concat "," (List.map str l)) ^ ")"
    (* let rec sub (a : term) (sub : term) (t : term) = true *)
    (* let rec sub (Var a) (sub : term) (t : term) = true *)
end

let t1 = Term.Term ("f",[Var "a"])
let t2 = Term.Term ("g",[Var "b"])
let t3 = Term.Term ("f",[Var "c"])
let t4 = Term.Term ("z",[Var "a"])
let t5 = Term.Term ("w",[Var "c"])


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
                                                    | Term (f,l) -> Term (f, List.map (sub equal) l)
                                                    | _ -> t
    let rec occurs (t : Term.term) (v : Term.term) = match v,t with
      | Var x, Term (f, l) -> List.exists (occurs v) l
      | _,_ -> false
    let conflict (t1: Term.term) (t2: Term.term) = match t1, t2 with
      | Term (f,l1), Term (g,l2) -> (f <> g) or (List.length l1 <> List.length l2)
      | _ -> false
    let decomposable (eq: equality) = match eq with Equal (t1,t2) ->
                                                     match t1,t2 with
                                                     | Term (f, l_f), Term (g, l_g) ->
                                                        if (f = g) & (List.length l_f = List.length l_g) then
                                                          true
                                                        else
                                                          false
                                                     | _,_ -> false

    let decompose (eq: equality) = match eq with Equal (t1,t2) ->
                                                  match t1,t2 with
                                                  | Term (f, l_f), Term (g, l_g) -> List.rev_map2 (fun x y -> Equal (x,y)) l_f l_g
                                                  | _,_ -> []
  end

module EqSet = Set.Make(Unifier)
type uniSet = {vars : Term.term list; eqSet : EqSet.t;}
module Unify =
  struct
    let eliminate (eq : Unifier.equality) (eqSet : EqSet.t) = match eq with
      | Equal (t1, t2) -> EqSet.(map (fun x -> match x with Equal (t1,t2) -> Equal (Unifier.sub eq t1,Unifier.sub eq t2)) eqSet |> EqSet.add eq)
    let unify (eqSet : EqSet.t) = let eq0 = EqSet.choose eqSet in
                                  match eq0 with
                                    Equal (t1,t2) -> match t1, t2 with
                                                     | Term (f, l), Var x -> EqSet.(remove eq0 eqSet |> add (Unifier.reflex eq0))
                                                     (* | Var x, Term (f, l) -> EqSet.(map (Unifier.sub t2) eqSet |> ) *)
                                                     | _,_ -> if t1 = t2 then EqSet.remove eq0 eqSet
                                                              else if Unifier.decomposable eq0 then EqSet.(remove eq0 eqSet |> union (of_list (Unifier.decompose eq0)))
                                                              else if Unifier.occurs t1 t2 then EqSet.empty (*we have to fail so return empty set for the time being*)
                                                              else eqSet
  end
let pretty_print t = List.fold_left (fun x y -> x ^ y) "" (List.map (fun x -> (Unifier.str x) ^ ";") (EqSet.elements t))

let eq1 = Unifier.Equal (t1, t2)
let eq2 = Unifier.Equal (t2, t2)
let eq3 = Unifier.Equal (t3, t1)
let eq4 = Unifier.Equal (t3, t4)
let eq5 = Unifier.Equal (t1, t5)
let eqSub = Unifier.Equal (Var "c", Term ("y", [Var "c0"; Var "c1"]))
let foo = EqSet.(empty |> add eq1 |> add eq2 |> add eq3 |> add eq4 |> add eq5)

(* "Pretty" print the EqSet elements *)
(* List.fold_left (fun x y -> x ^ y) "" (List.map Unifier.str (EqSet.elements foo)) *)

(* List.fold_left (fun x y -> x ^ y) "" (List.map (fun x -> (Unifier.str x) ^ ";") (EqSet.elements foo)) *)
