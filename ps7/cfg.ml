open Cfg_ast
exception Implement_Me
exception FatalError

type igraph_node = RegNode of Riscv.reg | VarNode of var

let string_of_node (n: igraph_node) : string =
  match n with
  | RegNode r -> "$" ^ Riscv.reg2string r
  | VarNode v -> v
;;

module IGraphNode =
  struct
    type t = igraph_node
    let compare = compare
  end

module NodeSet = Set.Make(IGraphNode)                                                   

(* These are the registers that must be generated / killed as part of
   liveness analysis for call instructions to reflect RISC-V calling
   conventions *)

(* Note that for call_gen_list, if the number of arguments n in the
   call is less than 8, then only the first n of these are actually
   used *)
let call_gen_list = ["x10";"x11";"x12";"x13";"x14";"x15";"x16";"x17";]
let call_kill_list = ["x1";"x5";"x6";"x7";"x10";"x11";"x12";"x13";"x14";"x15";"x16";"x17";"x28";"x29";"x30";"x31"]

(* Undirected graphs where nodes are identified by igraph_node type above. 
  Look at graph.ml for the interface description.  *)

module IUGraph = Graph.UndirectedGraph(IGraphNode)

(* this is a wrapper to addEdge that prevents adding self edges.
   to do all sorts of other complicated stuff for eg coloring *)
let specialAddEdge u v g =
  if (u = v) then
    g
  else
    IUGraph.addEdge u v g

(* An interference graph is an SUGraph where a node is temp variable
   or a register (to be able to handle pre-colored nodes)

   The adjacency set of variable x should be the set of variables
   y such that x and y are live at the same point in time. *)
type interfere_graph = IUGraph.graph

(* To help you printing an igraph for debugging *)
let string_of_igraph (g: interfere_graph) : string =
  let rec string_of_row (n: IUGraph.node) =
    let ns = IUGraph.adj n g in
    Printf.sprintf "  %s\t: {%s}"
      (string_of_node n)
      (String.concat "," (List.map string_of_node (NodeSet.elements ns)))
  in
  let rows = String.concat "\n" (List.map string_of_row (NodeSet.elements (IUGraph.nodes g))) in
  Printf.sprintf "{\n%s\n}\n" rows


(*******************************************************************)
(* PS7 TODO: interference graph construction *)

(* convert a valid cfg operand into an igraph node *)
let igraph_node_of_cfg_operand (op : operand) : igraph_node option =
  match op with
  | Var v -> Some (VarNode v)
  | Reg r -> Some (RegNode r)
  | Int _ | Lab _ -> None   (* integer constants and labels are not nodes in the interference graph *)

(* if cfg operand can be converted to an igraph node, add to a set *)
let add_cfg_operand_to_igraph_node_set (op : operand) (s : NodeSet.t) : NodeSet.t =
  match igraph_node_of_cfg_operand op with
  | Some n -> NodeSet.add n s
  | None -> s

(* given a list of cfg operands, convert valid cfg operands add to one set *)
let igraph_node_set_of_cfg_operands (ops : operand list) : NodeSet.t =
  List.fold_left
    (fun acc op -> add_cfg_operand_to_igraph_node_set op acc)
    NodeSet.empty
    ops

(* create an igraph node for a register *)
let igraph_reg_node_of_string (r : string) : igraph_node =
  RegNode (Riscv.string2reg r)

(* returns the first n elements of a list *)
let rec take n xs =
  if n <= 0 then []
  else
    match xs with
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs

(* 
 * uses: which variables/registers does this instruction need to read?
 * defs: which variables/registers does this instruction overwrite/define?
 * 
 * "use of x generates liveness, while a definition kills it"
 * 
 * For each instruction i,
 * 
 * live_in[i] = uses ∪ (live_out[i] - defs)
 *    Something is live before an instruction if:
 *    1. the instruction uses it right away, or
 *    2. it is live after the instruction and this instruction does not redefine it.
 *
 * live_out[i] = union of live_in[s] for every successor s of i
 *    A variable is live after instruction i if it is live before at least one instruction that can run next.
 *)
let uses (i : inst) : NodeSet.t =
  match i with
  | Label _ -> NodeSet.empty
  | Move (_, src) -> igraph_node_set_of_cfg_operands [src]
  | Arith (_, lhs, _, rhs) -> igraph_node_set_of_cfg_operands [lhs; rhs]
  | Load (_, base, _) -> igraph_node_set_of_cfg_operands [base]
  | Store (base, _, src) -> igraph_node_set_of_cfg_operands [base; src]
  | Call (fn, nargs) ->
      let arg_regs =
        take nargs call_gen_list
        |> List.map igraph_reg_node_of_string
      in
      List.fold_left
        (fun acc n -> NodeSet.add n acc)
        (add_cfg_operand_to_igraph_node_set fn NodeSet.empty)
        arg_regs
  | Jump _ -> NodeSet.empty
  | If (lhs, _, rhs, _, _) -> igraph_node_set_of_cfg_operands [lhs; rhs]
  | Return -> NodeSet.singleton (RegNode Riscv.R10)

let defs (i : inst) : NodeSet.t =
  match i with
  | Label _ -> NodeSet.empty
  | Move (dst, _) -> igraph_node_set_of_cfg_operands [dst]
  | Arith (dst, _, _, _) -> igraph_node_set_of_cfg_operands [dst]
  | Load (dst, _, _) -> igraph_node_set_of_cfg_operands [dst]
  | Store _ -> NodeSet.empty
  | Call _ ->
      List.fold_left
        (fun acc r -> NodeSet.add (igraph_reg_node_of_string r) acc)
        NodeSet.empty
        call_kill_list
  | Jump _ -> NodeSet.empty
  | If _ -> NodeSet.empty
  | Return -> NodeSet.empty

(* 
 * flatten a function into a list of instructions
 * a function is a list of blocks, a block is a list of instructions
 *)
let flatten_func (f : func) : inst array =
  Array.of_list (List.flatten f)

(*
 * Build a table that maps a label to its index (instruction #)
 *
 * This information helps when we build successors, because jumps and branches 
 * go to labels, not the next instruction.
 * 
 * For example:
 *    "main" -> 0
 *    ".L1"  -> 4
 *    ".L0"  -> 6
 *)
let build_label_table (insts : inst array) : (label, int) Hashtbl.t =
  let label_to_index = Hashtbl.create (Array.length insts) in
  Array.iteri
    (fun i inst ->
      match inst with
      | Label lab -> Hashtbl.replace label_to_index lab i
      | _ -> ())
    insts;
  label_to_index

(* label lookup *)
let find_label (label_to_index : (label, int) Hashtbl.t) lab =
  try Hashtbl.find label_to_index lab
  with Not_found -> raise FatalError

(*
 *   successors = [
 *     [successors of instruction 0];
 *     [successors of instruction 1];
 *     [successors of instruction 2];
 *     ...
 *   ]
 * 
 * successors.(0) = [successors of instruction 0]
 *)
let build_successors (insts : inst array) : int list array =
  let n = Array.length insts in
  let label_to_index = build_label_table insts in
  Array.mapi
    (fun i inst ->
      match inst with
      | Jump lab -> [find_label label_to_index lab]
      | If (_, _, _, true_lab, false_lab) ->
          [find_label label_to_index true_lab;
           find_label label_to_index false_lab]
      | Return -> []
      | _ ->
          if i + 1 < n then [i + 1] else [])
    insts

(*
 * returns two arrays (live_in, live_out)
 * live_in.(i) = set of nodes live before instruction i
 * live_out.(i) = set of nodes live after instruction i.
 *)
let solve_liveness (insts : inst array) (successors : int list array)
    : NodeSet.t array * NodeSet.t array =
  let n = Array.length insts in
  let live_in = Array.make n NodeSet.empty in
  let live_out = Array.make n NodeSet.empty in
  let changed = ref true in
  while !changed do
    changed := false;
    for i = n - 1 downto 0 do
      let old_in = live_in.(i) in
      let old_out = live_out.(i) in
      let new_out =
        List.fold_left
          (fun acc succ -> NodeSet.union acc live_in.(succ))
          NodeSet.empty
          successors.(i)
      in
      let new_in =
        NodeSet.union (uses insts.(i)) (NodeSet.diff new_out (defs insts.(i)))
      in
      if not (NodeSet.equal old_in new_in && NodeSet.equal old_out new_out)
      then changed := true;
      live_in.(i) <- new_in;
      live_out.(i) <- new_out
    done
  done;
  (live_in, live_out)

(* create igraph nodes first before adding edges *)
let add_nodes (nodes : NodeSet.t) (g : interfere_graph) : interfere_graph =
  NodeSet.fold (fun node acc -> IUGraph.addNode node acc) nodes g

(*
 * Appel: x and y interfere if y is live when x is defined.
 * 
 * For each instruction i:
 *    For each node d in defs[i]:
 *        For each node y in live_out[i]:
 *            Add edge d -- y
 *)
let build_appel_graph (insts : inst array) (live_in : NodeSet.t array)
    (live_out : NodeSet.t array) : interfere_graph =
  let graph = ref IUGraph.empty in
  for i = 0 to Array.length insts - 1 do
    graph := add_nodes (uses insts.(i)) !graph;
    graph := add_nodes (defs insts.(i)) !graph;
    graph := add_nodes live_in.(i) !graph;
    graph := add_nodes live_out.(i) !graph;
    NodeSet.iter
      (fun defined_node ->
        NodeSet.iter
          (fun live_node ->
            graph := specialAddEdge defined_node live_node !graph)
          live_out.(i))
      (defs insts.(i))
  done;
  !graph

let build_interfere_graph (f : func) : interfere_graph = 
  let insts = flatten_func f in
  let successors = build_successors insts in
  let (live_in, live_out) = solve_liveness insts successors in
  build_appel_graph insts live_in live_out
