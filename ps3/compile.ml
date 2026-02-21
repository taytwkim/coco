(* Compile Fish AST to RISC-V AST *)
open Riscv

exception IMPLEMENT_ME

(* result returned by the compiler
 * here, data includes user-defined variables (x, y, ...), 
 * and compiler-generated temp variables (T1, T2, ...) *)
type result = { code : Riscv.inst list;
                data : Riscv.label list }

let label_counter = ref 0

(* generate new int for labels and temps e.g., L1, T2, ... *)
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)

(* Generate fresh labels
 * 
 * Labels can be used as branch or jump targets.
 * Target labels will be encoded in the instructions. See J-type and B-type instructions in riscv.ml.
 * 
 * In addition, the compiler will generate labels for variables (like x and T1).
 * In compile_stmt, we can write instructions like La assuming that these labels exist.
 *)
let new_label() = "L" ^ (string_of_int (new_int()))

(* sets of variables -- Ocaml Set and Set.S *)
module VarSet = Set.Make(struct
                           type t = Ast.var
                           let compare = String.compare
                         end)

(* a table of variables that we need for the code segment *)
let variables : VarSet.t ref = ref (VarSet.empty)

(* generate a fresh temporary variable and store it in the variables set. *)
let rec new_temp() = 
    let t = "T" ^ (string_of_int (new_int())) in
    (* make sure we don't already have a variable with the same name! *)
    if VarSet.mem t (!variables) then new_temp()
    else (variables := VarSet.add t (!variables); t)

(* reset internal state *)
let reset() = (label_counter := 0; variables := VarSet.empty)

(* find all of the variables in a program and add them to
 * the set variables *)
let rec collect_vars (p : Ast.program) : unit = 
  let rec collect_vars_in_exp ((e, _) : Ast.exp) : unit =
    match e with
    | Ast.Int _ -> ()
    | Ast.Var x ->
        variables := VarSet.add x !variables
    | Ast.Binop (e1, _, e2) ->
        collect_vars_in_exp e1; collect_vars_in_exp e2
    | Ast.Not e1 ->
        collect_vars_in_exp e1
    | Ast.And (e1, e2)
    | Ast.Or (e1, e2) ->
        collect_vars_in_exp e1; collect_vars_in_exp e2
    | Ast.Assign (x, e1) ->
        variables := VarSet.add x !variables;
        collect_vars_in_exp e1
  in
  let rec collect_vars_in_stmt ((s, _) : Ast.stmt) : unit =
    match s with
    | Ast.Exp e ->
        collect_vars_in_exp e
    | Ast.Seq (s1, s2) ->
        collect_vars_in_stmt s1; collect_vars_in_stmt s2
    | Ast.If (e, s1, s2) ->
        collect_vars_in_exp e; collect_vars_in_stmt s1; collect_vars_in_stmt s2
    | Ast.While (e, body) ->
        collect_vars_in_exp e; collect_vars_in_stmt body
    | Ast.For (e1, e2, e3, body) ->
        collect_vars_in_exp e1; collect_vars_in_exp e2; collect_vars_in_exp e3; collect_vars_in_stmt body
    | Ast.Return e ->
        collect_vars_in_exp e
  in
  collect_vars_in_stmt p

(* compiles a Fish statement down to a list of RISC-V instructions.
 * Note that a "Return" is accomplished by placing the resulting
 * value in x10 and then doing a jr x1. *)


(* My approach below is to use two registers (R8 and R10).
 * We need at least two registers for binary operations.
 * 
 * While R10 is meant to be overwritten (it holds the final return value),
 * it might not be okay to overwrite R8.
 * 
 * So store R8 to a label before we start computation, 
 * and before we exit, recover R8 to its original value. *)

let w (n : int) : Word32.word = Word32.fromInt n

let z : Word32.word = w 0

(* load label from memory to destination register *)
let load_label (dst : reg) (lbl : string) : inst list = 
    [ 
        La (dst, lbl);      (* load the memory address of the label *)
        Lw (dst, dst, z)    (* load word from memory to register *)
    ]

(* Store register value to label's memory address
 * The reason why we use R8 here is because we often want to 
 * store R10's value to memory, so we can't use R10 *)
let store_label (lbl : string) (src : reg) : inst list = 
    [ 
        La (R8, lbl);       (* load the memory address of the label to R8 *)
        Sw (R8, src, z)     (* store the source register value to label's memory address *)
    ]

let load_label_to_r10 (lbl : string) : inst list = load_label R10 lbl

let load_label_to_r8  (lbl : string) : inst list = load_label R8  lbl

let store_r10_to_label (lbl : string) : inst list = store_label lbl R10

(* load label1 to R8 and label2 to R10
 * used for binary operations *)
let load_operands (label1 : string) (label2 : string) : inst list =
    load_label_to_r8 label1 @ load_label_to_r10 label2

let rec compile_exp ((e, _) : Ast.exp) : inst list =
    match e with
    | Ast.Int n -> [Li (R10, w n)]
    | Ast.Var x -> load_label_to_r10 x

    | Ast.Binop (e1, bop, e2) ->
        let t1 = new_temp () in     (* to store the result of e1 *)
        let t2 = new_temp () in     (* to store the result of e2 *)
        compile_exp e1
        @ store_r10_to_label t1
        @ compile_exp e2
        @ store_r10_to_label t2
        @ load_operands t1 t2       (* load e1 and e2 to R8 and R10 *)
        @ (match bop with
            | Ast.Plus  -> [Add (R10, R8, Reg R10)]
            | Ast.Minus -> [Sub (R10, R8, R10)]
            | Ast.Times -> [Mul (R10, R8, R10)]
            | Ast.Div   -> [Div (R10, R8, R10)]
            | Ast.Eq    -> seq (R10, R8, R10)
            | Ast.Neq   -> sne (R10, R8, R10)
            | Ast.Lt    -> [Slt (R10, R8, R10)]
            | Ast.Gt    -> [Slt (R10, R10, R8)]
            | Ast.Lte   -> [Slt (R10, R10, R8); Seqz (R10, R10)]
            | Ast.Gte   -> [Slt (R10, R8, R10); Seqz (R10, R10)]
        )
    
    | Ast.Not e1 -> compile_exp e1 @ [Seqz (R10, R10)]
    | Ast.Assign (x, e1) -> compile_exp e1 @ store_r10_to_label x
    
    | Ast.And (e1, e2) ->
        let lfalse = new_label () in    (* jump to lfalse if e1 is false (short-circuiting )*)
        let lend   = new_label () in
        compile_exp e1
        @ [Beq (R10, R0, lfalse)]
        @ compile_exp e2                (* at this point, e1 is true, and the result of e2 is in R10 *)
        @ [
            J lend;                     (* we can jump to lend, e2's result is the same as e1 AND e2 *)
            Label lfalse;               (* we end up here only if e1 is false *)
            Li (R10, w 0);              (* in that case, just store 0 to R10 and exit *)
            Label lend
        ]
    
    | Ast.Or (e1, e2) ->
        let ltrue = new_label () in
        let lend  = new_label () in
        compile_exp e1
        @ [Bne (R10, R0, ltrue)]
        @ compile_exp e2
        @ [
            J lend; 
            Label ltrue; 
            Li (R10, w 1); 
            Label lend
        ]

let rec compile_stmt_body (exit_lbl : string) ((s,_) : Ast.stmt) : inst list =
    match s with
    | Ast.Exp e -> compile_exp e
    | Ast.Seq (s1, s2) -> compile_stmt_body exit_lbl s1 @ compile_stmt_body exit_lbl s2
    
    | Ast.If (cond, s_then, s_else) ->
        let lelse = new_label () in
        let lend  = new_label () in
        compile_exp cond
        @ [ Beq (R10, R0, lelse) ]
        @ compile_stmt_body exit_lbl s_then
        @ [ 
            J lend; 
            Label lelse
        ]
        @ compile_stmt_body exit_lbl s_else
        @ [ Label lend ]
    
    | Ast.While (cond, body) ->
        let ltop  = new_label () in
        let ldone = new_label () in
        [ Label ltop ]
        @ compile_exp cond
        @ [ Beq (R10, R0, ldone) ]
        @ compile_stmt_body exit_lbl body
        @ [ 
            J ltop;
            Label ldone
        ]

    | Ast.For (e1, e2, e3, body) ->
        let ltop  = new_label () in
        let ldone = new_label () in
        compile_exp e1
        @ [ Label ltop ]
        @ compile_exp e2
        @ [ Beq (R10, R0, ldone) ]
        @ compile_stmt_body exit_lbl body
        @ compile_exp e3
        @ [ 
            J ltop;
            Label ldone
        ]

    | Ast.Return e ->
        compile_exp e @ [ J exit_lbl ]

let compile_stmt (p : Ast.stmt) : inst list =
  let saved_r8 = new_temp () in     (* label to preserve R8 before we overwrite it *)
  let exit_lbl = new_label () in    (* jump target for return (jump to end) *)
  let prologue : inst list = 
    [   (* store R8 value so that it can be recovered later *)
        La (R10, saved_r8); 
        Sw (R10, R8, z) 
    ] in
  let epilogue : inst list = 
    [   (* recover R8 *)
        La (R8, saved_r8); 
        Lw (R8, R8, z); 
        jr R1 
    ] in
  prologue
  @ compile_stmt_body exit_lbl p
  @ [ 
        Li (R10, w 0); 
        Label exit_lbl 
    ]
  @ epilogue

(* compiles Fish AST down to RISC-V instructions and a list of global vars *)
let compile (p : Ast.program) : result = 
    let _ = reset() in
    let _ = collect_vars(p) in
    let insts = (Label "main") :: (compile_stmt p) in
    { code = insts; data = VarSet.elements (!variables) }

(* converts the output of the compiler to a big string which can be 
 * dumped into a file, assembled, and run in qemu *)
let result2string ({code;data}:result) : string = 
    let strs = List.map (fun x -> (Riscv.inst2string x) ^ "\n") code in
    let var2decl x = x ^ ":\t.word 0\n" in
    "\t.text\n" ^
    "\t.align\t2\n" ^
    "\t.globl main\n\n" ^
    (String.concat "" strs) ^
    "\n\n" ^
    "\t.data\n" ^
    "\t.align 0\n"^
    (String.concat "" (List.map var2decl data)) ^
    "\n"
