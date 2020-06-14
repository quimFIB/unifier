(* A small OCaml program *)
print_string "Hello World!\n"

(* type atom = Atom of String.t *)
module Term =
  struct
    type term =
      | Atom of string
      | Term of string * term list

    let rec str (t : term) = match t with
      | Atom s -> s
      | Term (s,l) -> s ^ "(" ^ (String.concat "," (List.map str l)) ^ ")"
    (* let rec sub (a : term) (sub : term) (t : term) = true *)
    (* let rec sub (Atom a) (sub : term) (t : term) = true *)
end

let t1 = Term.Term ("f",[Atom "a"])
let t2 = Term.Term ("f",[Atom "b"])
let t3 = Term.Term ("f",[Atom "c"])

module Unifier =
  struct
    type equality = Equal of Term.term * Term.term
    let reflex (eq : equality) = match eq with Equal (t1,t2) -> Equal (t2,t1)
    let sub (equal : equality) (t : Term.term) : Term.term =
      match equal with
      | Equal (t_l,t_r) -> if t_l = t then t_r else t
end
