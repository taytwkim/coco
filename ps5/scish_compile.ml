(* TODO:  your job is to map Scish Ast expressions to Cish Ast functions. 
   The file sample_input.scish shows a sample Scish expression and the
   file sample_output.cish shows the output I get from my compiler.
   You will want to do your own test cases...
 *)

exception Unimplemented

(* Cish AST wrappers *)
let cexp (e : Cish_ast.rexp) : Cish_ast.exp = (e, 0)
let cstmt (s : Cish_ast.rstmt) : Cish_ast.stmt = (s, 0)
let cint (n : int) : Cish_ast.exp = cexp (Cish_ast.Int n)
let cvar (x : string) : Cish_ast.exp = cexp (Cish_ast.Var x)

let rec find_index x lst = 
  match lst with
  | h::t -> if h = x then 0 else 1 + find_index x t
  | [] -> failwith ("Unbound variable " ^ x)

let lambda_counter = ref 0

let get_next_lambda_tag () =
  let tag = !lambda_counter in
  lambda_counter := tag + 1;
  tag

let if_counter = ref 0

let get_next_if_tag () =
  let tag = !if_counter in
  if_counter := tag + 1;
  tag

let rec compile_exp_helper (e : Scish_ast.exp) (env : string list) : (Cish_ast.exp * Cish_ast.func list) =
  match e with
  | Scish_ast.Int n -> (cint n, [])
  
  | Scish_ast.Var x -> 
      (* Variable lookup in env *)
      let idx = find_index x env in
      let offset_exp = cint (idx * 4) in
      let env_var_exp = cvar "env" in (* "env" is a function argument / local var *)
      let ptr_math_exp = cexp (Cish_ast.Binop (env_var_exp, Cish_ast.Plus, offset_exp)) in
      (cexp (Cish_ast.Load ptr_math_exp), [])
      
  | Scish_ast.PrimApp (op, exps) -> 
    (match op, exps with
      | Scish_ast.Plus, [e1; e2] -> compile_binop e1 e2 Cish_ast.Plus env
      | Scish_ast.Minus, [e1; e2] -> compile_binop e1 e2 Cish_ast.Minus env
      | Scish_ast.Times, [e1; e2] -> compile_binop e1 e2 Cish_ast.Times env
      | Scish_ast.Div, [e1; e2] -> compile_binop e1 e2 Cish_ast.Div env
      | Scish_ast.Eq, [e1; e2] -> compile_binop e1 e2 Cish_ast.Eq env
      | Scish_ast.Lt, [e1; e2] -> compile_binop e1 e2 Cish_ast.Lt env
      | Scish_ast.Cons, [e1; e2] -> 
          let (c1, f1) = compile_exp_helper e1 env in
          let (c2, f2) = compile_exp_helper e2 env in
          
          (* function that allocates 8 bytes, stores two words, and returns a pointer *)
          let cons_call = cexp (Cish_ast.Call (cvar "CONS", [c1; c2])) in
          let cons_func = Cish_ast.Fn {
            name = "CONS";
            args = ["e1"; "e2"];
            body =  cstmt (Cish_ast.Let ("p", cexp (Cish_ast.Malloc (cint 8)),
                    cstmt (Cish_ast.Seq (cstmt (Cish_ast.Exp (cexp (Cish_ast.Store (cvar "p", cvar "e1")))),
                    cstmt (Cish_ast.Seq (cstmt (Cish_ast.Exp (cexp (Cish_ast.Store (cexp (Cish_ast.Binop (cvar "p", Cish_ast.Plus, cint 4)), cvar "e2")))),
                    cstmt (Cish_ast.Return (cvar "p"))))))));
            pos = 0;
          } in
          (cons_call, f1 @ f2 @ [cons_func])

      | Scish_ast.Fst, [e1] -> 
          let (c1, f1) = compile_exp_helper e1 env in
          (cexp (Cish_ast.Load c1), f1)

      | Scish_ast.Snd, [e1] -> 
          let (c1, f1) = compile_exp_helper e1 env in
          (cexp (Cish_ast.Load (cexp (Cish_ast.Binop (c1, Cish_ast.Plus, cint 4)))), f1)

      | _ -> failwith "Invalid PrimApp"
    )
  
    (**
        lambda0(env) {
          return CLOSURE(0,env,1);
        }

        lambda1(env) {
          return *(env+1*4)+*(env+0*4);
        }
      *)
  | Scish_ast.Lambda (x, e_body) -> 
      let tag = get_next_lambda_tag () in
      let fn_name = "lambda" ^ (string_of_int tag) in
      let new_env = x :: env in
      let (body_cish, body_funcs) = compile_exp_helper e_body new_env in
      
      let cish_fn = Cish_ast.Fn {
          name = fn_name;
          args = ["env"];
          body = cstmt (Cish_ast.Return body_cish);
          pos = 0;
      } in
      
      let closure_call = cexp (Cish_ast.Call (cvar "CLOSURE", [cint tag; cvar "env"; cint (List.length env)])) in
      (closure_call, body_funcs @ [cish_fn])

  | Scish_ast.App (e1, e2) -> 
      let (c1, f1) = compile_exp_helper e1 env in
      let (c2, f2) = compile_exp_helper e2 env in
      let apply_call = cexp (Cish_ast.Call (cvar "APPLY", [c1; c2])) in
      (apply_call, f1 @ f2)

  | Scish_ast.If (e1, e2, e3) -> 
      let (c1, f1) = compile_exp_helper e1 env in
      let (c2, f2) = compile_exp_helper e2 env in
      let (c3, f3) = compile_exp_helper e3 env in
      
      let if_fn_name = "if_helper_" ^ (string_of_int (get_next_if_tag ())) in
      let if_stmt = cstmt (Cish_ast.If (c1, cstmt (Cish_ast.Return c2), cstmt (Cish_ast.Return c3))) in
      let if_fn = Cish_ast.Fn {
          name = if_fn_name;
          args = ["env"];
          body = if_stmt;
          pos = 0;
      } in
      
      (cexp (Cish_ast.Call (cvar if_fn_name, [cvar "env"])), f1 @ f2 @ f3 @ [if_fn])
      
and compile_binop e1 e2 op env =
    let (c1, f1) = compile_exp_helper e1 env in
    let (c2, f2) = compile_exp_helper e2 env in
    (cexp (Cish_ast.Binop (c1, op, c2)), f1 @ f2)

let cish_helpers num_lambdas =
    (**
        CLOSURE(tag,env,env_len) {
            let p = malloc(12);
            *p = tag;
            *(p+4) = env;
            *(p+8) = env_len;
            return p;
        }
      *)
    let closure_fn = Cish_ast.Fn {
      name = "CLOSURE";
      args = ["tag"; "env"; "env_len"];
      body =  cstmt (Cish_ast.Let ("p", cexp (Cish_ast.Malloc (cint 12)),
              cstmt (Cish_ast.Seq (cstmt (Cish_ast.Exp (cexp (Cish_ast.Store (cvar "p", cvar "tag")))),
              cstmt (Cish_ast.Seq (cstmt (Cish_ast.Exp (cexp (Cish_ast.Store (cexp (Cish_ast.Binop(cvar "p", Cish_ast.Plus, cint 4)), cvar "env")))),
              cstmt (Cish_ast.Seq (cstmt (Cish_ast.Exp (cexp (Cish_ast.Store (cexp (Cish_ast.Binop(cvar "p", Cish_ast.Plus, cint 8)), cvar "env_len")))),
              cstmt (Cish_ast.Return (cvar "p"))))))))));
      pos = 0;
    } in

    (**
        EXTEND_ENV(val,env,env_len) {
          let new_env = malloc((4+env_len*4)); {
            *new_env = val;
            let i = 0;
            for (i = 0;i<env_len;i = i+1) {
              *(new_env+i*4+4) = *(env+i*4);
            }
            return new_env;
          }
        }
      *)
    let extend_env_fn = Cish_ast.Fn {
      name = "EXTEND_ENV";
      args = ["val"; "env"; "env_len"];
      body =  cstmt (Cish_ast.Let ("new_env", cexp (Cish_ast.Malloc (cexp (Cish_ast.Binop (cint 4, Cish_ast.Plus, cexp (Cish_ast.Binop(cvar "env_len", Cish_ast.Times, cint 4)))))),
              cstmt (Cish_ast.Seq (cstmt (Cish_ast.Exp (cexp (Cish_ast.Store (cvar "new_env", cvar "val")))),
              cstmt (Cish_ast.Let ("i", cint 0,
                  cstmt (Cish_ast.Seq (
                    cstmt (Cish_ast.For (
                          cexp (Cish_ast.Assign ("i", cint 0)),
                          cexp (Cish_ast.Binop (cvar "i", Cish_ast.Lt, cvar "env_len")),
                          cexp (Cish_ast.Assign ("i", cexp (Cish_ast.Binop (cvar "i", Cish_ast.Plus, cint 1)))),
                          cstmt (Cish_ast.Exp (cexp (Cish_ast.Store (
                              cexp (Cish_ast.Binop (cvar "new_env", Cish_ast.Plus, cexp (Cish_ast.Binop (cexp (Cish_ast.Binop (cvar "i", Cish_ast.Times, cint 4)), Cish_ast.Plus, cint 4)))),
                              cexp (Cish_ast.Load (cexp (Cish_ast.Binop (cvar "env", Cish_ast.Plus, cexp (Cish_ast.Binop(cvar "i", Cish_ast.Times, cint 4)))))))))))),
              cstmt (Cish_ast.Return (cvar "new_env"))))))))));
      pos = 0;
    } in

    (**
        APPLY(closure,arg_val) {
          let env = EXTEND_ENV(arg_val,*(closure+4),*(closure+8)); {
            let tag = *closure; {
              if (tag==1) {
                return lambda1(env);
              } 
              else {
                if (tag==0) {
                  return lambda0(env);
                }
                else {
                  0;
                }
              }
            }
          }
        }
      *)
    let rec build_apply_dispatcher tag_idx max_tags =
      if tag_idx >= max_tags then
          cstmt (Cish_ast.Exp (cint 0))
      else
          cstmt (Cish_ast.If (
            cexp (Cish_ast.Binop (cvar "tag", Cish_ast.Eq, cint tag_idx)),
            cstmt (Cish_ast.Return (cexp (Cish_ast.Call (cvar ("lambda" ^ string_of_int tag_idx), [cvar "env"])))),
            build_apply_dispatcher (tag_idx + 1) max_tags))
    in
  
    let apply_fn = Cish_ast.Fn {
      name = "APPLY";
      args = ["closure"; "arg_val"];
      body =  cstmt (Cish_ast.Let ("env", cexp (Cish_ast.Call (cvar "EXTEND_ENV", [cvar "arg_val"; cexp (Cish_ast.Load (cexp (Cish_ast.Binop (cvar "closure", Cish_ast.Plus, cint 4)))); cexp (Cish_ast.Load (cexp (Cish_ast.Binop (cvar "closure", Cish_ast.Plus, cint 8))))])),
              cstmt (Cish_ast.Let ("tag", cexp (Cish_ast.Load (cvar "closure")), build_apply_dispatcher 0 num_lambdas))));
      pos = 0;
    } in
    
    [closure_fn; extend_env_fn; apply_fn]

let compile_exp (e : Scish_ast.exp) : Cish_ast.program = 
  lambda_counter := 0;
  if_counter := 0;
  let (cish_expr, expr_funcs) = compile_exp_helper e [] in
  let main_stmt = cstmt (Cish_ast.Let ("env", cexp (Cish_ast.Malloc (cint 0)), cstmt (Cish_ast.Return cish_expr))) in
  let main_func = Cish_ast.Fn { name="main"; args=[]; body=main_stmt; pos=0 } in  
  let total_lambdas = !lambda_counter in
  let helper_funcs = cish_helpers total_lambdas in
  helper_funcs @ expr_funcs @ [main_func]