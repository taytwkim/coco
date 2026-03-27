open Mlish_ast

exception TypeError

(* let type_error(s:string) = (print_endline s; raise TypeError) *)

let type_error(s:string) = raise TypeError;

(** tipe_scheme = Forall of (tvar list) * tipe

    e.g., Forall of ['a], Arrow_t('a, 'a)

    tipe_scheme is a wrapper for tipe with polymorphic tvars.

    tipe -> generalize -> tipe_scheme
    tipe_scheme -> instantiate -> tipe
  *)
type env = (var * tipe_scheme) list

let guess () : tipe = Guess_t (ref None)

(* makes fresh polymorphic variables like tvar0, tvar1, ... *)
let tvar_counter = ref 0

let fresh_tvar () : tvar =
  let n = !tvar_counter in
  tvar_counter := n + 1;
  "tvar" ^ string_of_int n

(** Why prune?

    During unification, a guess may be linked to another guess.
    For example:

    r1 -> r2
    r2 -> ?

    This means we do not yet know the final type, but we do know that
    [r1] and [r2] must stand for the same type.

    Later, [r2] may be resolved:

    r1 -> r2
    r2 -> int

    In that case, [r1] should also be treated as an [int].

    [prune] follows these links to recover the current final type before
    we compare or rewrite types.
 *)

let rec prune (t : tipe) : tipe =
  match t with
  | Guess_t ({ contents = Some t' } as r) ->
      let t'' = prune t' in
      r := Some t'';
      t''
  | _ -> t

(********** UNIFY **********)

(** (unify t1 t2) checks whether t1 and t2 can be made to mean the
    same type.

    While doing so, it may fill in unknown guesses or connect
    an unknown guess to another.
 *)

let rec occur (r : tipe option ref) (t : tipe) : bool =
  match t with
  | Int_t -> false
  | Bool_t -> false
  | Unit_t -> false
  | Tvar_t _ -> false
  | Fn_t (t1, t2) -> occur r t1 || occur r t2
  | Pair_t (t1, t2) -> occur r t1 || occur r t2
  | List_t t' -> occur r t'
  | Guess_t r' ->
      if r == r' then true
      else
        (match !r' with
         | None -> false
         | Some t' -> occur r t')

let rec unify (t1 : tipe) (t2 : tipe) : bool =
  let t1 = prune t1 in
  let t2 = prune t2 in
  match t1, t2 with
  | Int_t, Int_t -> true
  | Bool_t, Bool_t -> true
  | Unit_t, Unit_t -> true
  | Tvar_t x, Tvar_t y -> x = y

  (* A guess should always unify with itself. *)
  | Guess_t r1, Guess_t r2 when r1 == r2 -> true

  (* left hand side is a guess *)
  | Guess_t r, t ->
      (match !r with
       | None ->
           if occur r t then false
           else (r := Some t; true)
       | Some t1' -> unify t1' t)
  
  (* if guess is on right hand side, swap *)
  | t, Guess_t _ ->
      unify t2 t1

  | Fn_t (t11, t12), Fn_t (t21, t22) ->
      unify t11 t21 && unify t12 t22

  | Pair_t (t11, t12), Pair_t (t21, t22) ->
      unify t11 t21 && unify t12 t22

  | List_t t1', List_t t2' ->
      unify t1' t2'

  | _ -> false

(********** GENERALIZE **********)

(** We generalize at a [let] binding.

    1. First, infer the type of the right-hand side. This may create fresh
        guesses.

    2. Then, replace the guesses that are local to that inferred type with
        polymorphic type variables.

    Guesses coming from the outer environment should not be generalized.
 *)

(* walks tipe and collects unresolved guesses that appear inside it *)
let rec guesses_of_type (t : tipe) : tipe option ref list =
  match prune t with
  | Int_t -> []
  | Bool_t -> []
  | Unit_t -> []
  | Tvar_t _ -> []
  | Fn_t (t1, t2) -> guesses_of_type t1 @ guesses_of_type t2
  | Pair_t (t1, t2) -> guesses_of_type t1 @ guesses_of_type t2
  | List_t t' -> guesses_of_type t'
  | Guess_t r ->
      (match !r with
       | None -> [r]
       | Some t' -> guesses_of_type t')

(* takes a tipe scheme, and collects guesses inside the underlying type *)
let guesses_of_scheme (Forall (_, t) : tipe_scheme) : tipe option ref list =
  guesses_of_type t

(* walks the env and collect guesses from each scheme *)
let guesses_of_env (env : env) : tipe option ref list =
  List.concat (List.map (fun (_, scheme) -> guesses_of_scheme scheme) env)

(* helper used in diff *)
(* find ref in the list of refs - the exact ref, not just a ref with the same value *)
let mem_ref (r : tipe option ref) (rs : tipe option ref list) : bool =
  List.exists (fun r' -> r == r') rs

(* find guesses in the inferred type but not in env *)
let rec diff (xs : tipe option ref list) (ys : tipe option ref list) : tipe option ref list =
  match xs with
  | [] -> []
  | x :: xs' ->
      if mem_ref x ys then diff xs' ys
      else x :: diff xs' ys

(* replaces selected guesses with fresh tvars *)
let rec substitute_guesses (subs : (tipe option ref * tvar) list) (t : tipe) : tipe =
  match prune t with
  | Int_t -> Int_t
  | Bool_t -> Bool_t
  | Unit_t -> Unit_t
  | Tvar_t x -> Tvar_t x
  | Fn_t (t1, t2) ->
      Fn_t (substitute_guesses subs t1, substitute_guesses subs t2)
  | Pair_t (t1, t2) ->
      Pair_t (substitute_guesses subs t1, substitute_guesses subs t2)
  | List_t t' ->
      List_t (substitute_guesses subs t')
  | Guess_t r ->
      (match List.find_opt (fun (r', _) -> r == r') subs with
       | Some (_, tv) -> Tvar_t tv
       | None ->
           match !r with
           | None -> Guess_t r
           | Some t' -> substitute_guesses subs t')

let generalize (env : env) (t : tipe) : tipe_scheme =
  let t_gs = guesses_of_type t in
  let env_gs = guesses_of_env env in
  let gs = diff t_gs env_gs in
  let fresh_tvs = List.map (fun r -> (r, fresh_tvar ())) gs in
  let new_tipe = substitute_guesses fresh_tvs t in
  Forall (List.map snd fresh_tvs, new_tipe)

(********** INSTANTIATE **********)

(** We want to turn generic type variables like ['a] in a type such as
    'a -> 'a into fresh guesses.

    Those fresh guesses can later be resolved by unification.

    To do this, generate a fresh guess for each tvar,
    then call "substitute" to replace the type variables
    with those guesses.
 *)

let rec substitute (subs : (tvar * tipe) list) (t : tipe) : tipe =
  match t with
  | Int_t -> Int_t
  | Bool_t -> Bool_t
  | Unit_t -> Unit_t
  | Tvar_t x ->
      (match List.assoc_opt x subs with
       | Some t' -> t'
       | None -> Tvar_t x)
  | Fn_t (t1, t2) -> Fn_t (substitute subs t1, substitute subs t2)
  | Pair_t (t1, t2) -> Pair_t (substitute subs t1, substitute subs t2)
  | List_t t' -> List_t (substitute subs t')
  | Guess_t _ -> t

let instantiate (Forall (tvars, t) : tipe_scheme) : tipe =
  let fresh_guesses =
    List.map (fun tvar -> (tvar, guess ())) tvars
  in
  substitute fresh_guesses t

let lookup_env (x : var) (env : env) : tipe_scheme =
  match List.assoc_opt x env with
  | Some scheme -> scheme
  | None -> type_error ("unbound variable: " ^ x)

(* wrapper that throws an error if unify fails *)
let expect_unify (t1 : tipe) (t2 : tipe) (msg : string) : unit =
  if unify t1 t2 then ()
  else type_error msg

let rec tc (env : env) (e : Mlish_ast.exp) : tipe =
  match e with
  | (Var x, _) ->
    (** For example, we look up something like:
            x : Forall(['a], 'a -> 'a)
            We instantiate with guesses so that it can be constrained.
     *)
      instantiate (lookup_env x env)

  | (PrimApp (p, args), _) ->
      (match p, args with
       | Int _, [] -> Int_t
       | Bool _, [] -> Bool_t
       | Unit, [] -> Unit_t
       | Plus, [e1; e2]
       | Minus, [e1; e2]
       | Times, [e1; e2]
       | Div, [e1; e2] ->
           let t1 = tc env e1 in
           let t2 = tc env e2 in
           expect_unify t1 Int_t "expected int";
           expect_unify t2 Int_t "expected int";
           Int_t
       | Eq, [e1; e2]
       | Lt, [e1; e2] ->
           let t1 = tc env e1 in
           let t2 = tc env e2 in
           expect_unify t1 Int_t "expected int";
           expect_unify t2 Int_t "expected int";
           Bool_t
       | Pair, [e1; e2] ->
           let t1 = tc env e1 in
           let t2 = tc env e2 in
           Pair_t (t1, t2)
       | Fst, [e1] ->
           let t = tc env e1 in
           let t1 = guess () in
           let t2 = guess () in
           expect_unify t (Pair_t (t1, t2)) "expected pair";
           t1
       | Snd, [e1] ->
           let t = tc env e1 in
           let t1 = guess () in
           let t2 = guess () in
           expect_unify t (Pair_t (t1, t2)) "expected pair";
           t2
       | Nil, [] ->
           List_t (guess ())
       | Cons, [e1; e2] ->
           let t_hd = tc env e1 in
           let t_tl = tc env e2 in
           expect_unify t_tl (List_t t_hd) "expected list in cons";
           List_t t_hd
       | IsNil, [e1] ->
           let elem_t = guess () in
           let t = tc env e1 in
           expect_unify t (List_t elem_t) "expected list";
           Bool_t
       | Hd, [e1] ->
           let elem_t = guess () in
           let t = tc env e1 in
           expect_unify t (List_t elem_t) "expected list";
           elem_t
       | Tl, [e1] ->
           let elem_t = guess () in
           let t = tc env e1 in
           expect_unify t (List_t elem_t) "expected list";
           List_t elem_t
       | _ ->
           type_error "invalid primitive application")

  | (Fn (x, body), _) ->
    
    (** We are trying to type-check fun x -> body.
        We don't know x yet, create a fresh guess.

        Wrap the fresh guess as a tipe scheme with 
        an empty tvar list, extend the env, and 
        type-check the body.

        In the end, the function has the type
        arg_t -> body_t
      *)

      let arg_t = guess () in
      let body_t = tc ((x, Forall ([], arg_t)) :: env) body in
      Fn_t (arg_t, body_t)

  | (App (e1, e2), _) ->
      let t1 = tc env e1 in
      let t2 = tc env e2 in
      let tr = guess () in (* a fresh guess for return type *)
      expect_unify t1 (Fn_t (t2, tr)) "expected function";
      tr

  | (If (e1, e2, e3), _) ->
      let t1 = tc env e1 in
      let t2 = tc env e2 in
      let t3 = tc env e3 in
      expect_unify t1 Bool_t "expected bool";
      expect_unify t2 t3 "if branches must have same type";
      t2

  | (Let (x, e1, e2), _) ->
      let t1 = tc env e1 in
      let scheme = generalize env t1 in
      tc ((x, scheme) :: env) e2

let rec type_check_exp (e : Mlish_ast.exp) : tipe =
  tc [] e
