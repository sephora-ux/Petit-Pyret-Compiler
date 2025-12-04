(* Typage statique pour Petit Pyret *)

open Ast

(* Types internes *)
type typ =
  | Tvar of tvar
  | TAny
  | TNothing
  | TBoolean
  | TNumber
  | TString
  | TList of typ
  | TFun of typ list * typ

and tvar = {
  id : int;
  mutable def : typ option;
}

module V = struct
  type t = tvar
  let compare v1 v2 = compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let create =
    let r = ref 0 in
    fun () -> incr r; { id = !r; def = None }
end

(* Réduction en tête *)
let rec head = function
  | Tvar { def = Some t } -> head t
  | t -> t

(* Forme canonique *)
let rec canon t = match head t with
  | Tvar _ | TAny | TNothing | TBoolean | TNumber | TString as t -> t
  | TList t1 -> TList (canon t1)
  | TFun (tl, t1) -> TFun (List.map canon tl, canon t1)

(* Sous-typage *)
let rec subtype t1 t2 = match head t1, head t2 with
  | _, TAny -> true
  | TNothing, _ -> true
  | t1, t2 when t1 = t2 -> true
  | TList t1, TList t2 -> subtype t1 t2
  | TFun (a1, r1), TFun (a2, r2) ->
      List.length a1 = List.length a2 &&
      List.for_all2 (fun x y -> subtype y x) a1 a2 &&
      subtype r1 r2
  | _ -> false

(* Occurrence *)
let rec occur v t = match head t with
  | Tvar w -> V.equal v w
  | TList t1 -> occur v t1
  | TFun (tl, t1) -> List.exists (occur v) tl || occur v t1
  | _ -> false

(* Unification *)
exception TypeError of string

let rec unify t1 t2 = match head t1, head t2 with
  | t1, t2 when t1 = t2 -> ()
  | Tvar v1, Tvar v2 when V.equal v1 v2 -> ()
  | Tvar v1, t2 ->
      if occur v1 t2 then raise (TypeError "occur check");
      v1.def <- Some t2
  | t1, Tvar v2 -> unify t2 t1
  | TList t1, TList t2 -> unify t1 t2
  | TFun (a1, r1), TFun (a2, r2) ->
      if List.length a1 <> List.length a2 then
        raise (TypeError "function arity mismatch");
      List.iter2 unify a1 a2;
      unify r1 r2
  | _ -> raise (TypeError "type mismatch")

(* Schémas de types *)
module Vset = Set.Make(V)

type schema = { vars : Vset.t; typ : typ }

(* Variables libres *)
let rec fvars t = match head t with
  | Tvar v -> Vset.singleton v
  | TList t1 -> fvars t1
  | TFun (tl, t1) ->
      List.fold_left (fun s t -> Vset.union s (fvars t)) (fvars t1) tl
  | _ -> Vset.empty

(* Environnement *)
module Smap = Map.Make(String)

type binding =
  | Immutable of schema
  | Mutable of typ

type env = {
  tyvars : Vset.t;
  bindings : binding Smap.t;
}

let empty_env = {
  tyvars = Vset.empty;
  bindings = Smap.empty;
}

(* Généralisation *)
let generalize env t =
  let env_fvars = Vset.fold (fun v s -> Vset.union s (fvars (Tvar v))) env.tyvars Vset.empty in
  let t_fvars = fvars t in
  { vars = Vset.diff t_fvars env_fvars; typ = t }

(* Ajout à l'environnement *)
let add_immut x t env =
  let schema = generalize env t in
  { env with bindings = Smap.add x (Immutable schema) env.bindings }

let add_mut x t env =
  let tv = fvars t in
  { tyvars = Vset.union env.tyvars tv;
    bindings = Smap.add x (Mutable t) env.bindings }

module Vmap = Map.Make(V)

let instantiate schema =
  let subst = Vset.fold (fun v m -> Vmap.add v (Tvar (V.create ())) m)
    schema.vars Vmap.empty in
  let rec inst t = match head t with
    | Tvar v as t -> (try Vmap.find v subst with Not_found -> t)
    | TList t1 -> TList (inst t1)
    | TFun (tl, t1) -> TFun (List.map inst tl, inst t1)
    | t -> t
  in
  inst schema.typ

(* Conversion syntax -> type *)
let rec styp_to_typ env = function
  | Tident "Any" -> TAny
  | Tident "Nothing" -> TNothing
  | Tident "Boolean" -> TBoolean
  | Tident "Number" -> TNumber
  | Tident "String" -> TString
  | Tident x -> Tvar (V.create ()) (* variable de type *)
  | Tpoly ("List", [t]) -> TList (styp_to_typ env t)
  | Tpoly _ -> raise (TypeError "unknown polymorphic type")
  | Tfun (tl, t) -> TFun (List.map (styp_to_typ env) tl, styp_to_typ env t)

(* Environnement initial *)
let initial_env =
  let env = empty_env in
  let add_prim x t env =
    { env with bindings = Smap.add x (Immutable { vars = Vset.empty; typ = t }) env.bindings }
  in
  let alpha = V.create () in
  let beta = V.create () in
  let ta = Tvar alpha in
  let tb = Tvar beta in
  env
  |> add_prim "nothing" TNothing
  |> add_prim "num-modulo" (TFun ([TNumber; TNumber], TNumber))
  |> add_prim "empty" (TList ta)
  |> add_prim "link" (TFun ([ta; TList ta], TList ta))
  |> add_prim "print" (TFun ([ta], ta))
  |> add_prim "raise" (TFun ([TString], ta))
  |> add_prim "each" (TFun ([TFun ([ta], tb); TList ta], TNothing))
  |> add_prim "fold" (TFun ([TFun ([ta; tb], ta); ta; TList tb], ta))

(* Typage des expressions *)
let rec type_expr env = function
  | Ecst (Cbool _) -> TBoolean
  | Ecst (Cint _) -> TNumber
  | Ecst (Cstring _) -> TString
  | Eident x ->
      (match Smap.find_opt x env.bindings with
       | Some (Immutable schema) -> instantiate schema
       | Some (Mutable t) -> t
       | None -> raise (TypeError ("unbound variable: " ^ x)))
  | Ebinop (op, e1, e2) ->
      let t1 = type_expr env e1 in
      let t2 = type_expr env e2 in
      (match op with
       | Badd | Bsub | Bmul | Bdiv ->
           unify t1 TNumber; unify t2 TNumber; TNumber
       | Blt | Ble | Bgt | Bge ->
           unify t1 TNumber; unify t2 TNumber; TBoolean
       | Beq | Bneq -> TBoolean
       | Band | Bor ->
           unify t1 TBoolean; unify t2 TBoolean; TBoolean)
  | Ecall (f, args) ->
      let tf = type_expr env f in
      let targs = List.map (type_expr env) args in
      let tret = Tvar (V.create ()) in
      unify tf (TFun (targs, tret));
      tret
  | Eblock stmts -> type_block env stmts
  | Eif (cond, _, then_b, elifs, else_opt) ->
      let tcond = type_expr env cond in
      unify tcond TBoolean;
      let tthen = type_block env then_b in
      let telifs = List.map (fun (c, b) ->
        let tc = type_expr env c in
        unify tc TBoolean;
        type_block env b) elifs in
      let telse = match else_opt with
        | Some b -> type_block env b
        | None -> TNothing in
      List.iter (unify tthen) telifs;
      unify tthen telse;
      tthen
  | Elam (params, rtyp, _, body) ->
      let param_types = List.map (fun (_, st) -> styp_to_typ env st) params in
      let ret_type = styp_to_typ env rtyp in
      let env' = List.fold_left2 (fun e (x, st) t ->
        add_immut x t e) env params param_types in
      let tbody = type_block env' body in
      if not (subtype tbody ret_type) then
        raise (TypeError "function body type mismatch");
      TFun (param_types, ret_type)
  | Ecases (styp, e, _, branches) ->
      let t = styp_to_typ env styp in
      let te = type_expr env e in
      if not (subtype te t) then
        raise (TypeError "cases type mismatch");
      let tbranches = List.map (fun (_, _, body) -> type_block env body) branches in
      (match tbranches with
       | [] -> raise (TypeError "empty cases")
       | t :: rest -> List.iter (unify t) rest; t)
  | Efor (f, froms, rtyp, _, body) ->
      (* for c(x1::t1 from e1, ...) -> t : b end
         devient: 
         c(lam(x1::t1, ...) -> t : b end, e1, ...) *)
      let params = List.map fst froms in
      let exprs = List.map snd froms in
      let lam = Elam (params, rtyp, false, body) in
      type_expr env (Ecall (Eident f, lam :: exprs))

and type_block env = function
  | [] -> raise (TypeError "empty block")
  | [Seval e] -> type_expr env e
  | [Smut (x, e)] ->
      let te = type_expr env e in
      (match Smap.find_opt x env.bindings with
       | Some (Mutable t) ->
           if not (subtype te t) then raise (TypeError "assignment type mismatch");
           TNothing
       | _ -> raise (TypeError ("not a mutable variable: " ^ x)))
  | stmt :: rest ->
      let env' = type_stmt env stmt in
      type_block env' rest

and type_stmt env = function
  | Sfundef (f, tparams, params, rtyp, _, body) ->
      let param_types = List.map (fun (_, st) -> styp_to_typ env st) params in
      let ret_type = styp_to_typ env rtyp in
      let fun_type = TFun (param_types, ret_type) in
      let env' = add_immut f fun_type env in
      let env'' = List.fold_left2 (fun e (x, st) t ->
        add_immut x t e) env' params param_types in
      let tbody = type_block env'' body in
      if not (subtype tbody ret_type) then
        raise (TypeError "function body type mismatch");
      env'
  | Sletdef (x, styp_opt, e) ->
      let te = type_expr env e in
      (match styp_opt with
       | Some st ->
           let t = styp_to_typ env st in
           if not (subtype te t) then raise (TypeError "let type mismatch");
           add_immut x t env
       | None -> add_immut x te env)
  | Svardef (x, styp_opt, e) ->
      let te = type_expr env e in
      (match styp_opt with
       | Some st ->
           let t = styp_to_typ env st in
           if not (subtype te t) then raise (TypeError "var type mismatch");
           add_mut x t env
       | None -> add_mut x te env)
  | Smut (x, e) ->
      let te = type_expr env e in
      (match Smap.find_opt x env.bindings with
       | Some (Mutable t) ->
           if not (subtype te t) then raise (TypeError "mutation type mismatch");
           env
       | _ -> raise (TypeError ("not a mutable variable: " ^ x)))
  | Seval e ->
      let _ = type_expr env e in
      env
  | Sblock stmts ->
      List.fold_left type_stmt env stmts

let type_file (_, main) =
  try
    let _ = type_stmt initial_env main in
    Ok ()
  with TypeError msg ->
    Error msg