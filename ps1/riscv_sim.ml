open Riscv_ast
open Riscv_assem
open Byte

exception TODO
exception FatalError

(* Take a look at the definition of the RISC-V AST and machine state in riscv_ast.ml *)

(* 
  Given a starting state, simulate the RISC-V machine code to get a final state;
  a final state is reached if the the next instruction pointed to by the PC is all 0s.

  Each iteration of the simulator consists of three parts:
  1. Load: Load the next 4 bytes (binary) from the PC
  2. Decode: Decode word and get the instruction
  3. Step: Execute the instruction, and update the state. 
    We have a driver called step that dispatches execution to helper functions like step_add.

  We repeat this process until PC reads 0.
 *)

(* load the next 4 bytes from PC and assemble them into a 32-bit word *)
let load (curr_state : state) : int32 = 
  read_word_little_endian curr_state.m curr_state.pc
 
(* Decode a 32-bit word into an instruction. *)
let decode (word : int32) : inst =
  (* All instructions are 32 bits wide. The last 7 bits [0:6] are the opcode. *)
  let opcode = bitrange word 0 6 in

  match opcode with
  (*
    R-type (Arithmetic)
    Layout: [funct7: 7b] [rs2: 5b] [rs1: 5b] [funct3: 3b] [rd: 5b] [opcode: 7b]
    - Used for: Register-to-register operations (Add).
    - Opcode: 0x33l.
   *)
  | 0x33l -> 
      let rd  = ind2reg (bitrange word 7 11) in
      let rs1 = ind2reg (bitrange word 15 19) in
      let rs2 = ind2reg (bitrange word 20 24) in
      let funct3 = bitrange word 12 14 in
      let funct7 = bitrange word 25 31 in

      (match funct3, funct7 with
        | 0x0l, 0x00l -> Add (rd, rs1, rs2)
        | _ -> failwith "Unsupported R-type")

  (* 
    I-type (Immediate/Load)
    Layout: [imm[11:0]: 12b] [rs1: 5b] [funct3: 3b] [rd: 5b] [opcode: 7b]
    - Used for: Arithmetic, Loads, Jumps with immediates (Addi, Ori, Lw, Jalr).
    - Opcode: 0x13l (Arithmetic), 0x03l (Load), 0x67l (Jalr).
   *)
  | 0x13l ->
        let rd  = ind2reg (bitrange word 7 11) in
        let rs1 = ind2reg (bitrange word 15 19) in
        let funct3 = bitrange word 12 14 in
        let imm_raw = bitrange word 20 31 in
        
        (* Arithmetic right shift preserves the sign, fills with leading 0s or 1s *)
        let imm = Int32.shift_right (Int32.shift_left imm_raw 20) 20 in 

        (match funct3 with
          | 0x0l -> Addi (rd, rs1, imm)
          | 0x6l -> Ori (rd, rs1, imm)
          | _ -> failwith "Unsupported Arithmetic I-type")

  | 0x03l ->
      let rd = ind2reg (bitrange word 7 11) in
      let rs1 = ind2reg (bitrange word 15 19) in
      let funct3 = bitrange word 12 14 in
      let imm = Int32.shift_right word 20 in

      (match funct3 with
        | 0x2l -> Lw (rd, rs1, imm)
        | _ -> failwith "Unsupported Load type")

  | 0x67l ->
      let rd = ind2reg (bitrange word 7 11) in
      let rs1 = ind2reg (bitrange word 15 19) in
      let funct3 = bitrange word 12 14 in
      let imm = bitrange word 20 31 in

      (match funct3 with
        | 0x0l -> Jalr (rd, rs1, imm)
        | _ -> failwith "Unsupported Jalr variant")

  (*
    S-type (Store)
    Layout: [imm[11:5]: 7b] [rs2: 5b] [rs1: 5b] [funct3: 3b] [imm[4:0]: 5b] [opcode: 7b]
    - Used for: Storing register values to memory (Sw).
    - Note: The 12-bit immediate is split into two pieces (7 bits and 5 bits).
    - Opcode: 0x23l.
   *)
  | 0x23l ->
        let rs1 = ind2reg (bitrange word 15 19) in
        let rs2 = ind2reg (bitrange word 20 24) in
        let funct3 = bitrange word 12 14 in

        (* 1. Get the high 7 bits and move them to positions 11-5 *)
        let imm_hi = Int32.shift_left (bitrange word 25 31) 5 in
        
        (* 2. Get the low 5 bits (positions 4-0) *)
        let imm_lo = bitrange word 7 11 in
        
        (* 3. Combine them *)
        let imm_combined = Int32.logor imm_hi imm_lo in

        (* 4. Convert a 12-bit immediate into a 32-bit signed integer. *)
        let imm = Int32.shift_right (Int32.shift_left imm_combined 20) 20 in

        (match funct3 with
          | 0x2l -> Sw (rs1, rs2, imm)
          | _ -> failwith "Unknown Store variant")

  (*
    B-type (Branches)
    Layout: [imm[12]] [imm[10:5]: 6b] [rs2: 5b] [rs1: 5b] [funct3: 3b] [imm[4:1]: 4b] [imm[11]] [opcode: 7b]
    - Used for: Conditional jumps (Beq).
    - Note: The immediate is heavily scrambled to keep rs1/rs2 in the same bit positions.
    - Opcode: 0x63l.
  *)
  | 0x63l ->
      let rs1 = ind2reg (bitrange word 15 19) in
      let rs2 = ind2reg (bitrange word 20 24) in
      let funct3 = bitrange word 12 14 in

      (* Reconstructing the B-type immediate bit by bit *)
      let b12 = Int32.shift_left (bitrange word 31 31) 12 in (* Sign bit *)
      let b11 = Int32.shift_left (bitrange word 7 7) 11 in
      let b10_5 = Int32.shift_left (bitrange word 25 30) 5 in
      let b4_1 = Int32.shift_left (bitrange word 8 11) 1 in
      
      let imm_raw = Int32.logor (Int32.logor b12 b11) (Int32.logor b10_5 b4_1) in

      (* Sign-extend the 13-bit value (bit 12 is the sign) to 32 bits *)
      let imm = Int32.shift_right (Int32.shift_left imm_raw 19) 19 in

      (match funct3 with
        | 0x0l -> Beq (rs1, rs2, imm)
        | _ -> failwith "Unsupported Branch type")

  (*
    U-type (Upper Immediate)
    Layout: [imm[31:12]: 20b] [rd: 5b] [opcode: 7b]
    - Used for: Loading large constants into upper bits (Lui).
    - Opcode: 0x37l.
   *)
  | 0x37l -> 
      let rd = ind2reg (bitrange word 7 11) in
      let imm = Int32.shift_left (bitrange word 12 31) 12 in
      Lui (rd, imm)
  
  (*
    J-type (Jump)
    Layout: [imm[20]] [imm[10:1]: 10b] [imm[11]] [imm[19:12]: 8b] [rd: 5b] [opcode: 7b]
    - Used for: Unconditional jumps.
    - Opcode: 0x6fl.
   *)
  | 0x6fl -> 
        let rd = ind2reg (bitrange word 7 11) in
        
        let j20 = Int32.shift_left (bitrange word 31 31) 20 in
        let j19_12 = Int32.shift_left (bitrange word 12 19) 12 in
        let j11 = Int32.shift_left (bitrange word 20 20) 11 in
        let j10_1 = Int32.shift_left (bitrange word 21 30) 1 in
        
        let imm_raw = Int32.logor (Int32.logor j20 j19_12) (Int32.logor j11 j10_1) in
                                  
        (* Sign-extend from 21 bits (bit 20 is sign) to 32 bits *)
        let imm = Int32.shift_right (Int32.shift_left imm_raw 11) 11 in
        Jal (rd, imm)

  | _ -> failwith "Opcode not implemented"

let step_add (rd:reg) (rs1:reg) (rs2:reg) (s:state) : state =
  (* Adds the values in r1 and r2 and store it in rd *)
  let v1 = rf_lookup (reg2ind rs1) s.r in
  let v2 = rf_lookup (reg2ind rs2) s.r in
  let res = Int32.add v1 v2 in
  { s with r = rf_update (reg2ind rd) res s.r; pc = Int32.add s.pc 4l }

let step_addi (rd:reg) (rs1:reg) (imm:int32) (s:state) : state =
  (* Adds rs1 + imm and store it in rd  *)
  let v1 = rf_lookup (reg2ind rs1) s.r in
  let res = Int32.add v1 imm in
  { s with r = rf_update (reg2ind rd) res s.r; pc = Int32.add s.pc 4l }

let step_ori (rd:reg) (rs1:reg) (imm:int32) (s:state) : state =
  (* Bitwise OR between rs1 and imm *)
  let v1 = rf_lookup (reg2ind rs1) s.r in
  let res = Int32.logor v1 imm in
  { s with r = rf_update (reg2ind rd) res s.r; pc = Int32.add s.pc 4l }

let step_lui (rd:reg) (imm:int32) (s:state) : state =
  (* "Load Upper Immediate." Takes a 20-bit constant and places it in the upper bits (31–12) of the rd register, filling the bottom bits with zeros *)
  { s with r = rf_update (reg2ind rd) imm s.r; pc = Int32.add s.pc 4l }

let step_li (rd:reg) (imm:int32) (s:state) : state =
  (* "Load Immediate." Load a 32-bit constant into a register *)
  { s with r = rf_update (reg2ind rd) imm s.r; pc = Int32.add s.pc 4l }

let step_beq (rs1:reg) (rs2:reg) (imm:int32) (s:state) : state =
  (* "Branch if Equal." Compares rs1 and rs2. If they are equal, it jumps to a new address calculated by adding the imm offset to the current PC. *)
  let v1 = rf_lookup (reg2ind rs1) s.r in
  let v2 = rf_lookup (reg2ind rs2) s.r in
  let next_pc = if v1 = v2 then Int32.add s.pc imm else Int32.add s.pc 4l in
  { s with pc = next_pc }

let step_jal (rd:reg) (imm:int32) (s:state) : state =
  (*
    "Jump and Link." Jumps to an address (current PC + imm). 
    Before jumping, it saves the address of the next instruction (the return address) into rd so the program can eventually come back.
  *)
  let ret_addr = Int32.add s.pc 4l in
  let next_pc = Int32.add s.pc imm in
  { s with r = rf_update (reg2ind rd) ret_addr s.r; pc = next_pc }

let step_jalr (rd:reg) (rs1:reg) (imm:int32) (s:state) : state =
  (*
    "Jump and Link Register." Similar to Jal, but the jump target is calculated from a register: rs1 + imm. 
    This is often used for returning from functions or calling function pointers.
   *)
  let ret_addr = Int32.add s.pc 4l in
  let v1 = rf_lookup (reg2ind rs1) s.r in
  let next_pc = Int32.add v1 imm in
  { s with r = rf_update (reg2ind rd) ret_addr s.r; pc = next_pc }

let step_lw (rd:reg) (rs1:reg) (imm:int32) (s:state) : state =
  (* "Load Word." Calculates an address by adding rs1 + imm, goes to that spot in memory, and copies the 32-bit value into rd. *)
  let addr = Int32.add (rf_lookup (reg2ind rs1) s.r) imm in
  let data = read_word_little_endian s.m addr in 
  { s with r = rf_update (reg2ind rd) data s.r; pc = Int32.add s.pc 4l }

let step_sw (rs1:reg) (rs2:reg) (imm:int32) (s:state) : state =
  (* "Store Word." Calculates an address (rs1 + imm) and copies the value from register rs2 into that memory location *)
  let addr = Int32.add (rf_lookup (reg2ind rs1) s.r) imm in
  let data = rf_lookup (reg2ind rs2) s.r in

  let m1 = mem_update addr (getByte data 0) s.m in
  let m2 = mem_update (Int32.add addr 1l) (getByte data 1) m1 in
  let m3 = mem_update (Int32.add addr 2l) (getByte data 2) m2 in
  let m4 = mem_update (Int32.add addr 3l) (getByte data 3) m3 in

  { s with m = m4; pc = Int32.add s.pc 4l }

let step (s : state) : state =
  (* 1. Fetch: Load the 32-bit word from memory at the current PC *)
  let word = load s in
  
  (* 2. Decode: Turn those bits into a high-level instruction type *)
  let instr = decode word in
  
  (* 3. Execute: Dispatch to the helper function *)
  match instr with
  | Add (rd, rs1, rs2)  -> step_add rd rs1 rs2 s
  | Addi (rd, rs1, imm) -> step_addi rd rs1 imm s
  | Beq (rs1, rs2, imm) -> step_beq rs1 rs2 imm s
  | Jal (rd, imm)       -> step_jal rd imm s
  | Jalr (rd, rs1, imm) -> step_jalr rd rs1 imm s
  | Lui (rd, imm)       -> step_lui rd imm s
  | Ori (rd, rs1, imm)  -> step_ori rd rs1 imm s
  | Lw (rd, rs1, imm)   -> step_lw rd rs1 imm s
  | Sw (rs1, rs2, imm)  -> step_sw rs1 rs2 imm s
  | Li (rd, imm)        -> step_li rd imm s

let rec interp (s : state) : state =
  (* 1. Fetch the instruction *)
  let word = load s in

  (* 2. Check the termination condition (all 0s) *)
  if word = 0l then 
    s
  else
    (* 3. Execute instruction and get next state *)
    let s' = step s in
    interp s'

(*
  Here are a few details/assumptions about the assembler and interpreter that the autograder makes:
  * > Little Endian Encoding
  * > Program Data is stored starting at 0x400000
  * > Constants that occur as input to the assembler are passed directly as 32 bit immediates in the AST,
      without any shifting or masking. The assembler then takes subsets of these bits when actually encoding
      an instruction into memory. E.g. an addi can be passed an immediate that 15 bits, but when we encode
      that instruction the encoding only uses bits 0 through 11.
*)
