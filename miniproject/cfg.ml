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

(* Undirected graphs where nodes are identified by igraph_node type above. Look at
   graph.ml for the interface description.  *)

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
(* PS7 TODO:  interference graph construction *)

(* given a function (i.e., list of basic blocks), construct the
 * interference graph for that function.  This will require that
 * you build a dataflow analysis for calculating what set of variables
 * are live-in and live-out for each program point. *)
let build_interfere_graph (f : func) : interfere_graph = 
    raise Implement_Me
