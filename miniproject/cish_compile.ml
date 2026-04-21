exception Implement_Me
exception FatalError

open Cish_ast
module CG = Cfg_ast
module CFG = Cfg
module SP = Spill

(* Register Allocation Core *)

type allocation = (var * Riscv.reg) list

let lookup_alloc (alloc : allocation) (v : var) : Riscv.reg =
  try List.assoc v alloc
  with Not_found -> raise FatalError

type allocation_result =
  | Allocated of allocation
  | NeedSpill of var list

(**
    Pre-colored registers
      x0  : hardwired zero register
      x1  : return address
      x2  : stack pointer
      x8  : frame pointer
      x30/x31 : scratch registers reserved by cfg_compile.ml
  *)

let allocatable_registers =
  [ Riscv.R5; Riscv.R6; Riscv.R7; Riscv.R9; Riscv.R10; 
    Riscv.R11; Riscv.R12; Riscv.R13; Riscv.R14; Riscv.R15;
    Riscv.R16; Riscv.R17; Riscv.R18; Riscv.R19; Riscv.R20; 
    Riscv.R21; Riscv.R22; Riscv.R23; Riscv.R24; Riscv.R25; 
    Riscv.R26; Riscv.R27; Riscv.R28; Riscv.R29]

let k = List.length allocatable_registers

(*
  Input: one interference-graph node.
  Output: true iff the node is a physical register with a fixed color already.
*)
let is_precolored_node (node : CFG.igraph_node) : bool =
  match node with
  | CFG.RegNode _ -> true
  | CFG.VarNode _ -> false

(*
  Input: the current reduced interference graph.
  Output: one temp variable to treat as the next spill candidate.
  We conservatively pick a highest-degree non-precolored node.
*)
let choose_spill_candidate (g : CFG.interfere_graph) : var =
  CFG.IUGraph.nodes g
  |> CFG.NodeSet.elements
  |> List.filter (fun node -> not (is_precolored_node node))
  |> List.fold_left
       (fun best node ->
         match (best, node) with
         | None, CFG.VarNode v -> Some (v, CFG.IUGraph.degree node g)
         | Some (best_v, best_deg), CFG.VarNode v ->
             let deg = CFG.IUGraph.degree node g in
             if deg > best_deg then Some (v, deg) else Some (best_v, best_deg)
         | best, CFG.RegNode _ -> best)
       None
  |> function
     | Some (v, _) -> v
     | None -> raise FatalError

(*
  Input: a full interference graph.
  Output: a stack of temp vars in the order we will try to color them.
  It repeatedly removes low-degree temps, or picks a spill candidate if stuck.
*)
let simplify_graph (igraph : CFG.interfere_graph) : var list =
  let rec loop g stack =
    let low_degree_node =
      CFG.IUGraph.nodes g
      |> CFG.NodeSet.elements
      |> List.find_opt
           (fun node ->
             (not (is_precolored_node node)) && CFG.IUGraph.degree node g < k)
    in
    match low_degree_node with
    | Some (CFG.VarNode v) ->
        loop (CFG.IUGraph.rmNode (CFG.VarNode v) g) (v :: stack)
    | Some (CFG.RegNode _) -> raise FatalError
    | None ->
        let remaining_vars =
          CFG.IUGraph.nodes g
          |> CFG.NodeSet.elements
          |> List.filter (fun node -> not (is_precolored_node node))
        in
        (match remaining_vars with
         | [] -> stack
         | _ ->
             let v = choose_spill_candidate g in
             loop (CFG.IUGraph.rmNode (CFG.VarNode v) g) (v :: stack))
  in
  loop igraph []

(*
  Input: the original interference graph, the current partial allocation,
  and one temp variable. Output: the registers already used by its neighbors.
*)
let neighbor_colors (igraph : CFG.interfere_graph) (alloc : allocation) (v : var)
    : Riscv.reg list =
  let reg_of_neighbor (node : CFG.igraph_node) : Riscv.reg option =
    match node with
    | CFG.RegNode r -> Some r
    | CFG.VarNode v' ->
        (try Some (List.assoc v' alloc) with Not_found -> None)
  in
  CFG.IUGraph.adj (CFG.VarNode v) igraph
  |> CFG.NodeSet.elements
  |> List.filter_map reg_of_neighbor

(*
  Input: the interference graph, a partial temp->register assignment,
  and one temp variable. Output: an available register for that temp, if any.
*)
let choose_available_register (igraph : CFG.interfere_graph) (alloc : allocation)
    (v : var) : Riscv.reg option =
  let used = neighbor_colors igraph alloc v in
  List.find_opt (fun r -> not (List.mem r used)) allocatable_registers

(*
  Input: the interference graph and the stack produced by simplify_graph.
  Output: either a finished allocation or the temps that really must be spilled.
*)
let select_allocation (igraph : CFG.interfere_graph) (stack : var list)
    : allocation_result =
  let rec loop remaining alloc spills =
    match remaining with
    | [] ->
        if spills = [] then Allocated alloc else NeedSpill spills
    | v :: vs ->
        (match choose_available_register igraph alloc v with
         | Some r -> loop vs ((v, r) :: alloc) spills
         | None -> loop vs alloc (v :: spills))
  in
  loop stack [] []

(***********************************)

(**
    A future allocator will likely need three conceptual phases:

    1. Analyze the temp-filled CFG and build an interference graph.
    2. Decide which temps get which registers.
    3. Rewrite the CFG by replacing `Var x` operands with `Reg r`.

    These helpers are only scaffolding for now.
  *)

(* takes an interference graph and returns alloc mapping *)
let choose_registers (igraph : CFG.interfere_graph) : allocation_result =
  let stack = simplify_graph igraph in
  select_allocation igraph stack

(* rewrite a CFG temp with its chosen register *)
let rewrite_operand_with_alloc (alloc : allocation) (op : CG.operand)
    : CG.operand =
  match op with
  | CG.Var v -> CG.Reg (lookup_alloc alloc v)
  | _ -> op

(* rewrite a CFG inst with chosen registers *)
let rewrite_inst_with_alloc (alloc : allocation) (inst : CG.inst) : CG.inst =
  match inst with
  | CG.Label l -> CG.Label l
  | CG.Move (dst, src) ->
      CG.Move (rewrite_operand_with_alloc alloc dst,
               rewrite_operand_with_alloc alloc src)
  | CG.Arith (dst, lhs, aop, rhs) ->
      CG.Arith (rewrite_operand_with_alloc alloc dst,
                rewrite_operand_with_alloc alloc lhs,
                aop,
                rewrite_operand_with_alloc alloc rhs)
  | CG.Load (dst, base, off) ->
      CG.Load (rewrite_operand_with_alloc alloc dst,
               rewrite_operand_with_alloc alloc base,
               off)
  | CG.Store (base, off, src) ->
      CG.Store (rewrite_operand_with_alloc alloc base,
                off,
                rewrite_operand_with_alloc alloc src)
  | CG.Call (fn, nargs) ->
      CG.Call (rewrite_operand_with_alloc alloc fn, nargs)
  | CG.Jump l -> CG.Jump l
  | CG.If (lhs, cop, rhs, true_lab, false_lab) ->
      CG.If (rewrite_operand_with_alloc alloc lhs,
             cop,
             rewrite_operand_with_alloc alloc rhs,
             true_lab,
             false_lab)
  | CG.Return -> CG.Return

(* rewrite the entire CFG with chosen registers *)
let rewrite_cfg_with_alloc (alloc : allocation) (blocks : CG.func) : CG.func =
  List.map
    (fun block -> List.map (rewrite_inst_with_alloc alloc) block)
    blocks

(**
   Here is a template for one strategy to get a basic implementation
   working, which would just require implementing reg_alloc to map
   temporaries to registers.

   For each function, it calls Cfg_ast.fn2blocks to convert it into a
   Cfg representation of basic blocks, then calls a register allocator
   to map temporaries to registers, and finally uses Cfg_compile to
   convert the Cfg blocks to RISC-V.

   But you don't have to use this approach!
  *)

(**
    Input and output both have type Cfg_ast.func
    Input might contain temps (e.g., Var "t0"), but the output should
    only contain registers (e.g., Reg R11) before it is converted to RISC-V
  *)

let rec reg_alloc (blocks : Cfg_ast.func) : Cfg_ast.func =
  let igraph = CFG.build_interfere_graph blocks in
  match choose_registers igraph with
  | Allocated alloc -> rewrite_cfg_with_alloc alloc blocks
  | NeedSpill spills -> reg_alloc (SP.spill blocks spills)

let process_fn (fn : func) : Cfg_ast.func =
  (* fn2blocks : Cish_ast.func -> Cfg_ast.func *)
  let curfblocks = Cfg_ast.fn2blocks fn in
  reg_alloc curfblocks

let compile (prog : program) : Riscv.inst list =
  let blocks = List.flatten (List.map (fun fn -> process_fn fn) prog) in
  Cfg_compile.cfg_to_riscv blocks
