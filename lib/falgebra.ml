module Fix
type 'f fix = Fx ( 'f ('f fix) )
type 'f termF =
  | Var of string
  | TermF of string * 'f termF list
type term = termF fix
