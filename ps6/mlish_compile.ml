module ML = Mlish_ast
module S = Scish_ast

exception ImplementMe

let bool_to_scish (b : bool) : S.exp =
  if b then S.Int 1 else S.Int 0

let rec compile_exp ((e, _) : ML.exp) : S.exp =
  match e with
  | ML.Var x ->
      S.Var x

  | ML.Fn (x, body) ->
      S.Lambda (x, compile_exp body)

  | ML.App (e1, e2) ->
      S.App (compile_exp e1, compile_exp e2)

  | ML.If (e1, e2, e3) ->
      S.If (compile_exp e1, compile_exp e2, compile_exp e3)

  | ML.Let (x, e1, e2) ->
      S.sLet x (compile_exp e1) (compile_exp e2)

  | ML.PrimApp (p, args) ->
      match p, args with
      | ML.Int n, [] ->
          S.Int n
      | ML.Bool b, [] ->
          bool_to_scish b
      | ML.Unit, [] ->
          S.Int 0
      | ML.Nil, [] ->
          S.Int 0
      | ML.Plus, [e1; e2] ->
          S.PrimApp (S.Plus, [compile_exp e1; compile_exp e2])
      | ML.Minus, [e1; e2] ->
          S.PrimApp (S.Minus, [compile_exp e1; compile_exp e2])
      | ML.Times, [e1; e2] ->
          S.PrimApp (S.Times, [compile_exp e1; compile_exp e2])
      | ML.Div, [e1; e2] ->
          S.PrimApp (S.Div, [compile_exp e1; compile_exp e2])
      | ML.Eq, [e1; e2] ->
          S.PrimApp (S.Eq, [compile_exp e1; compile_exp e2])
      | ML.Lt, [e1; e2] ->
          S.PrimApp (S.Lt, [compile_exp e1; compile_exp e2])
      | ML.Pair, [e1; e2] ->
          S.PrimApp (S.Cons, [compile_exp e1; compile_exp e2])
      | ML.Fst, [e1] ->
          S.PrimApp (S.Fst, [compile_exp e1])
      | ML.Snd, [e1] ->
          S.PrimApp (S.Snd, [compile_exp e1])
      | ML.Cons, [e1; e2] ->
          S.PrimApp (S.Cons, [compile_exp e1; compile_exp e2])
      | ML.IsNil, [e1] ->
          S.If (compile_exp e1, S.Int 0, S.Int 1)
      | ML.Hd, [e1] ->
          S.PrimApp (S.Fst, [compile_exp e1])
      | ML.Tl, [e1] ->
          S.PrimApp (S.Snd, [compile_exp e1])
      | _ ->
          raise ImplementMe
