(* A small OCaml program *)
print_string "Hello World!\n"

(* type atom = Atom of String.t *)
module Term =
  struct
    type term =
      | Bottom
      | Atom of string
      | Term of string * term list

    let rec str (t : term) = match t with
      | Bottom -> "_|_"
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
    let rec sub (a : Term.term) (sub_t : Term.term) (t : Term.term) : Term.term option = match a, t with
      | Atom x, Atom y -> if x = y then Some sub_t else Some (Atom x)
      (* | Atom x, Term (f,l) -> let result = List.map (fun x -> Option.to_list (sub x)) l in
       *    if List.exists (fun t -> match t with Some t -> false | None -> true) result = true then
       *      None
       *    else
       *      Some (Term (f, result)) *)
      | Atom x, Term (f,l) -> Some (Term (f, (List.flatten (List.map Option.to_list (List.map (sub a sub_t) l)))))
      | _ -> None
end
