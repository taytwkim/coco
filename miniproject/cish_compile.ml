exception Implement_Me
exception FatalError

(************************************)

(** This is the hook for your compiler. Please keep your
    implementation contained to this file, plus, optionally the file
    cfg.ml *)

let compile prog =
  raise Implement_Me

(***********************************)

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

(*

let reg_alloc blocks =
  exception FatalError

let process_fn fn =
  let curfblocks = (Cfg_ast.fn2blocks fn) in
  reg_alloc curfblocks

let compile prog =
  let blocks = List.flatten (List.map (fun fn -> process_fn fn) prog) in
  Cfg_compile.cfg_to_riscv blocks

*)
