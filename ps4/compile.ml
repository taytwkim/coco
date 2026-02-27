open Riscv

exception IMPLEMENT_ME

type result = { code : Riscv.inst list; data : Riscv.label list }

let w (n:int) : Word32.word = Word32.fromInt n

let z : Word32.word = w 0

let sp = R2         (* stack pointer *)
let ra = R1         (* return addr *)
let fp = R8         (* frame pointer *)
let rv = R10        (* return value *)
let t0 = R11        (* scratch *)
let t1 = R12        (* scratch *)
let t2 = R13        (* scratch *)

let label_counter = ref 0

let new_int () = (incr label_counter; !label_counter)

let new_label () = "L" ^ string_of_int (new_int ())

(*
  +---------------------------+
  | arg(k-1)                   |  fp + 4*(k-1)
  +---------------------------+
  | ...                        |
  +---------------------------+
  | arg2                       |  fp + 8
  +---------------------------+
  | arg1                       |  fp + 4
  +---------------------------+
  | arg0                       |  fp + 0        <-- fp
  +---------------------------+

  | saved ra (x1)              |  fp - 4
  +---------------------------+
  | saved old fp (s0/x8)       |  fp - 8
  +---------------------------+
  | local0                     |  fp - 12
  +---------------------------+
  | local1                     |  fp - 16
  +---------------------------+
  | local2                     |  fp - 20
  +---------------------------+
  | ... (more locals/temps)    |
  +---------------------------+
 *)

module VarSet = Set.Make(String)

module VarMap = Map.Make(String)

type env = int VarMap.t  (* var -> byte offset relative to fp *)

let saved_ra_off = -4
let saved_fp_off = -8
let first_local_off = -12
let word_size = 4

(* ----- First pass: for each function, collect local variables and assign offsets ----- *)

let rec collect_lets_in_stmt ((s, _) : Ast.stmt) : VarSet.t =
  match s with
  | Ast.Exp _ -> VarSet.empty
  | Ast.Return _ -> VarSet.empty
  | Ast.Seq (s1, s2) ->
      VarSet.union (collect_lets_in_stmt s1) (collect_lets_in_stmt s2)
  | Ast.If (_, s1, s2) ->
      VarSet.union (collect_lets_in_stmt s1) (collect_lets_in_stmt s2)
  | Ast.While (_, body) ->
      collect_lets_in_stmt body
  | Ast.For (_, _, _, body) ->
      collect_lets_in_stmt body
  | Ast.Let (x, _init, body) ->
      VarSet.add x (collect_lets_in_stmt body)

type fun_info = {
  frame_size : int;     (* bytes reserved by for saved regs + locals *)
  env : env;            (* BOTH args and locals *)
}

(* ---- for each function, find frame_size and offsets for args and locals ---- *)
let build_fun_info (Ast.Fn (fs : Ast.funcsig)) : fun_info =
  (* args -> offsets *)
  let env_args =
    fs.args
    |> List.mapi (fun i x -> (x, i * word_size))
    |> List.fold_left (fun m (x, off) -> VarMap.add x off m) VarMap.empty
  in
  (* locals -> offsets *)
  let locals =
    fs.body |> collect_lets_in_stmt |> VarSet.elements
  in
  let env =
    locals
    |> List.mapi (fun i x -> (x, first_local_off - (i * word_size)))
    |> List.fold_left (fun m (x, off) -> VarMap.add x off m) env_args
  in
  let nlocals = List.length locals in
  let frame_size = (2 * word_size) + (nlocals * word_size) in
  { frame_size; env }

(* ----- Small helpers ----- *)

let lookup_off (info:fun_info) (x:Ast.var) : int =
  try VarMap.find x info.env
  with Not_found -> failwith ("unbound variable: " ^ x)

(* In PS3, we generated fresh labels to store intermediate results to the .data segment.
  This time, we are taking a different approach, and using stack as our scratch space.
  We can store intermediate results by pushing to the stack, and remove them later by popping.
 *)

(* push word from register to stack *)
let push_word_from_reg (r:reg) : inst list =
  [ Add (sp, sp, Immed (w (-4)));
    Sw (sp, r, z) ]

(* pop from stack and put it in register *)
let pop_word_to_reg (r:reg) : inst list =
  [ Lw (r, sp, z);
    Add (sp, sp, Immed (w 4)) ]

(* store register value at off(fp) *)
let store_at_fp (off:int) (src:reg) : inst list =
  [ Sw (fp, src, w off) ]

(* load from off(fp) to register *)
let load_from_fp (dst:reg) (off:int) : inst list =
  [ Lw (dst, fp, w off) ]

(* ----- Compile Expressions ----- *)

let rec compile_exp (info:fun_info) ((e, _) : Ast.exp) : inst list =
  match e with
  | Ast.Int n ->
      [ Li (rv, w n) ]

  | Ast.Var x ->
      let off = lookup_off info x in
      load_from_fp rv off

  | Ast.Assign (x, e1) ->
      let off = lookup_off info x in
      compile_exp info e1 @ store_at_fp off rv

  | Ast.Not e1 ->
      compile_exp info e1 @ [ Seqz (rv, rv) ]

  | Ast.And (e1, e2) ->
      let lfalse = new_label () in
      let ldone  = new_label () in
      compile_exp info e1
      @ [ Beq (rv, R0, lfalse) ]
      @ compile_exp info e2
      @ [ J ldone;
          Label lfalse;
          Li (rv, w 0);
          Label ldone ]

  | Ast.Or (e1, e2) ->
      let ltrue = new_label () in
      let ldone = new_label () in
      compile_exp info e1
      @ [ Bne (rv, R0, ltrue) ]
      @ compile_exp info e2
      @ [ J ldone;
          Label ltrue;
          Li (rv, w 1);
          Label ldone ]

  | Ast.Binop (e1, bop, e2) ->
      (* Evaluate e1 -> rv, push; eval e2 -> rv; pop e1 into t0; compute rv = t0 (op) rv *)
      let op_code =
        match bop with
        | Ast.Plus  -> [ Add (rv, t0, Reg rv) ]
        | Ast.Minus -> [ Sub (rv, t0, rv) ]
        | Ast.Times -> [ Mul (rv, t0, rv) ]
        | Ast.Div   -> [ Div (rv, t0, rv) ]
        | Ast.Lt    -> [ Slt (rv, t0, rv) ]
        | Ast.Gt    -> [ Slt (rv, rv, t0) ]
        | Ast.Lte   -> [ Slt (rv, rv, t0); Seqz (rv, rv) ]
        | Ast.Gte   -> [ Slt (rv, t0, rv); Seqz (rv, rv) ]
        | Ast.Eq    -> [ Sub (rv, t0, rv); Seqz (rv, rv) ]
        | Ast.Neq   -> [ Sub (rv, t0, rv); Snez (rv, rv) ]
      in
      compile_exp info e1
      @ push_word_from_reg rv
      @ compile_exp info e2
      @ pop_word_to_reg t0
      @ op_code

  | Ast.Call (f, args) ->
      let nargs = List.length args in
      let arg_area = nargs * word_size in

      (* update sp before calling a function and after returning *)
      let alloc = if arg_area = 0 then [] else [ Add (sp, sp, Immed (w (-arg_area))) ] in
      let dealloc = if arg_area = 0 then [] else [ Add (sp, sp, Immed (w arg_area)) ] in
      
      (* store args in allocated space *)
      let store_args =
        args
        |> List.mapi (fun i aexp ->
             compile_exp info aexp @ [ Sw (sp, rv, w (i * word_size)) ])
        |> List.concat
      in
      alloc @ store_args @ [ Jal (ra, f) ] @ dealloc

(* ----- Compile Statements ----- *)

let rec compile_stmt_body (info:fun_info) (exit_lbl:label) ((s, _) : Ast.stmt) : inst list =
  match s with
  | Ast.Exp e ->
      compile_exp info e

  | Ast.Seq (s1, s2) ->
      compile_stmt_body info exit_lbl s1 @ compile_stmt_body info exit_lbl s2

  | Ast.If (cond, s1, s2) ->
      let lelse = new_label () in
      let ldone = new_label () in
      compile_exp info cond
      @ [ Beq (rv, R0, lelse) ]
      @ compile_stmt_body info exit_lbl s1
      @ [ J ldone;
          Label lelse ]
      @ compile_stmt_body info exit_lbl s2
      @ [ Label ldone ]

  | Ast.While (cond, body) ->
      let ltop  = new_label () in
      let ldone = new_label () in
      [ Label ltop ]
      @ compile_exp info cond
      @ [ Beq (rv, R0, ldone) ]
      @ compile_stmt_body info exit_lbl body
      @ [ J ltop;
          Label ldone ]

  | Ast.For (e1, e2, e3, body) ->
      let ltop  = new_label () in
      let ldone = new_label () in
      compile_exp info e1
      @ [ Label ltop ]
      @ compile_exp info e2
      @ [ Beq (rv, R0, ldone) ]
      @ compile_stmt_body info exit_lbl body
      @ compile_exp info e3
      @ [ J ltop;
          Label ldone ]

  | Ast.Return e ->
      compile_exp info e @ [ J exit_lbl ]

  | Ast.Let (x, init, body) ->
      let off = lookup_off info x in
      compile_exp info init
      @ store_at_fp off rv
      @ compile_stmt_body info exit_lbl body

(* ----- Compile Functions ----- *)

let compile_func (info:fun_info) (Ast.Fn (fs : Ast.funcsig)) : inst list =
  let exit_lbl = new_label () in
  let prologue =
    [
      Label fs.name;

      (* copy old fp to t0 *)
      Add (t0, fp, Reg R0);

      (* set the new fp to the current sp *)
      Add (fp, sp, Reg R0);

      (* sp := sp - frame_size *)
      Add (sp, sp, Immed (w (-info.frame_size)));

      (* save ra and old fp *)
      Sw (fp, ra, w saved_ra_off);
      Sw (fp, t0, w saved_fp_off);
    ]
  in
  let body =
    compile_stmt_body info exit_lbl fs.body
    @ [ Li (rv, w 0); J exit_lbl ]  (* return 0 by default *)
  in
  let epilogue =
    [
      Label exit_lbl;

      (* restore old fp into t0, restore ra *)
      Lw (t0, fp, w saved_fp_off);
      Lw (ra, fp, w saved_ra_off);

      (* deallocate frame: sp := fp *)
      Add (sp, fp, Reg R0);

      (* restore fp *)
      Add (fp, t0, Reg R0);

      (* return *)
      Jalr (R0, ra, 0l);
    ]
  in
  prologue @ body @ epilogue

(* ----- Driver ----- *)

let compile (p : Ast.program) : result =
  let code =
    p
    |> List.map (fun fn ->
         let info = build_fun_info fn in
         compile_func info fn)
    |> List.concat
  in
  { code; data = [] }

let result2string (res:result) : string = 
    let code = res.code in
    let data = res.data in
    let strs = List.map (fun x -> (Riscv.inst2string x) ^ "\n") code in
    let vaR8decl x = x ^ ":\t.word 0\n" in
    "\t.text\n" ^
    "\t.align\t2\n" ^
    "\t.globl main\n" ^
    (String.concat "" strs) ^
    "\n\n" ^
    "\t.data\n" ^
    "\t.align 0\n"^
    (String.concat "" (List.map vaR8decl data)) ^
    "\n"
