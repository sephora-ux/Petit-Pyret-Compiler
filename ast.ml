(* Arbres de syntaxe abstraite de Pyret *)

type ident = string

type binop =
  | Badd | Bsub | Bmul | Bdiv
  | Beq | Bneq | Blt | Ble | Bgt | Bge
  | Band | Bor

type constant =
  | Cbool of bool
  | Cstring of string
  | Cint of int 

(* Types dans la syntaxe *)
type styp =
  | Tident of ident
  | Tpoly of ident * styp list
  | Tfun of styp list * styp

type expr =
  | Ecst of constant
  | Eident of ident
  | Ebinop of binop * expr * expr
  | Ecall of expr * expr list
  | Eblock of stmt list
  | Eif of expr * bool * stmt list * (expr * stmt list) list * stmt list option
  | Elam of (ident * styp) list * styp * bool * stmt list
  | Ecases of styp * expr * bool * (ident * ident option list * stmt list) list
  | Efor of ident * ((ident * styp) * expr) list * styp * bool * stmt list

and stmt =
  | Sfundef of ident * ident list * (ident * styp) list * styp * bool * stmt list
  | Sletdef of ident * styp option * expr
  | Svardef of ident * styp option * expr
  | Smut of ident * expr
  | Seval of expr
  | Sblock of stmt list

type file = ident list * stmt