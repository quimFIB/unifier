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


module Unifier =
  struct
    type equality = Equal of Term.term * Term.term
    type t = equality
    let str (eq : equality) = match eq with Equal (t1,t2) -> Term.str t1 ^ "=" ^ Term.str t2
    let compare (eq1 : equality) (eq2 : equality) = compare eq1  eq2
    let reflex (eq : equality) = match eq with Equal (t1,t2) -> Equal (t2,t1)
    let sub (equal : equality) (t : Term.term) : Term.term =
      match equal with
      | Equal (t_l,t_r) -> if t_l = t then t_r else t
    let occurs (v : Term.term) (t : Term.term) = (v = t)
    let conflict (t1: Term.term) (t2: Term.term) = match t1, t2 with
      | Term (f,l1), Term (g,l2) -> (f <> g) or (List.length l1 <> List.length l2)
      | _ -> false
end

module EqSet = Set.Make(Unifier)
let pretty_print t = List.fold_left (fun x y -> x ^ y) "" (List.map (fun x -> (Unifier.str x) ^ ";") (EqSet.elements t))

let eq1 = Unifier.Equal (t1, t2)
let eq2 = Unifier.Equal (t2, t2)
let eq3 = Unifier.Equal (t3, t1)
let foo = EqSet.(empty |> add eq1 |> add eq2 |> add eq3)

(* "Pretty" print the EqSet elements *)
(* List.fold_left (fun x y -> x ^ y) "" (List.map Unifier.str (EqSet.elements foo)) *)

(* List.fold_left (fun x y -> x ^ y) "" (List.map (fun x -> (Unifier.str x) ^ ";") (EqSet.elements foo)) *)
