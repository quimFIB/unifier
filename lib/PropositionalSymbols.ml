
module Var =
  struct
    type var = [`Var of string]
    type t = var
    let compare (v1 : var) (v2 : var) = compare v1 v2
    let str (v : var) = match v with `Var s -> s
  end
module Term =
  struct
    type term =
      [
        Var.var
      | `Term of string * term list
      ]
    type t = term
    let compare (t1 : term) (t2 : term) = compare t1 t2
    let rec str (t : term) = match t with
      | #Var.var as v -> Var.str v
      | `Term (s,l) -> s ^ "(" ^ (String.concat "," (List.map str l)) ^ ")"
    (* let rec sub (a : term) (sub : term) (t : term) = true *)
    (* let rec sub (`Var a) (sub : term) (t : term) = true *)
end

module Atomic =
  struct
    type atom = [`Atom of string * Term.term list]
    let rec str (a : atom) = match a with `Atom (s,l) ->
      s ^ "[" ^ (String.concat "," (List.map Term.str l)) ^ "]"
  end
module Literal =
struct
    type literal = [
      | Atomic.atom
      | `Not of Atomic.atom
    ]
    let rec str (l : literal) = match l with
      | #Atomic.atom as a -> Atomic.str a
      | `Not a -> let a_str = Atomic.str a in "¬" ^ a_str
end

module Proposition =
  struct
    type t =  [
      | `Top
      | `Bottom
      |  Atomic.atom
      | `Not of t
      | `Or of t * t
      | `And of t * t
      | `Exists of Var.var * t
      | `Forall of Var.var * t
    ]
    let rec str (p : t) = match p with
      | `Top -> "⊤"
      | `Bottom -> "⟂"
      |  #Atomic.atom as a -> Atomic.str a
      | `Not p -> let p_str = str p in "¬" ^ p_str
      | `Or (t0,t1) -> let t0_str = str t0 and t1_str = str t1 in t0_str ^ "∨" ^ t1_str
      | `And (t0,t1) -> let t0_str = str t0 and t1_str = str t1 in t0_str ^ "∧" ^ t1_str
      | `Exists (x,t) -> let x_str = Var.str x and t_str = str t in "∃" ^ x_str ^ "." ^ t_str
      | `Forall (x,t) -> let x_str = Var.str x and t_str = str t in "∀" ^ x_str ^ "." ^ t_str
  end

(* Need to comment this to load into utop and ...*)
open MultiSet
module VarMSet = MultiSet.Make(Var)

(* uncomment this *)
(* module VarMSet = Make(Var) *)

module Equality =
  struct
    type t = Equal of Term.term * Term.term
    let str (eq : t) = match eq with Equal (t1,t2) -> Term.str t1 ^ "=" ^ Term.str t2
    let compare (eq1 : t) (eq2 : t) = compare eq1  eq2
    let reflex (eq : t) = match eq with Equal (t1,t2) -> Equal (t2,t1)
    let rec sub (equal : t) (t : Term.term) : Term.term =
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
    let decomposable (eq: t) = match eq with Equal (t1,t2) ->
                                                     match t1,t2 with
                                                     | `Term (f, l_f), `Term (g, l_g) ->
                                                        if (f = g) && (List.length l_f = List.length l_g) then
                                                          true
                                                        else
                                                          false
                                                     | _,_ -> false

    let decompose (eq: t) = match eq with Equal (t1,t2) ->
                                                  match t1,t2 with
                                                  | `Term (f, l_f), `Term (g, l_g) -> List.rev_map2 (fun x y -> Equal (x,y)) l_f l_g
                                                  | _,_ -> []
    let rec getVarsTerm (t : Term.term) = match t with
      | `Var x -> VarMSet.(empty |> add (`Var x))
      | `Term (f, l)-> List.fold_left VarMSet.union VarMSet.empty (List.map getVarsTerm l)

    let getVarsEq (eq : t) = match eq with Equal (t1,t2) -> VarMSet.union (getVarsTerm t1) (getVarsTerm t2)
  end
