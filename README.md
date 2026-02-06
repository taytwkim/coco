# Compiler Construction

## `ps0`: OCaml Warm Up

Getting familiar with OCaml.

## `ps1`: RISC-V Simulator

Simulate how a RISC-V CPU executes instructions. The machine's status is represented in a `state` record containing the registers, memory, and Program Counter (PC).

The simulator follows these steps in a loop:

1. Fetch: It reads a 32-bit binary instruction from memory at the location pointed to by the PC.

2. Decode: it breaks down that binary word to figure out which instruction it is (like Add or Beq) and extracts the numbers (immediates) needed.

3. Execute: It updates the CPU state based on the instruction—for example, adding two registers or jumping to a new PC. During this step, we ensure "sign-extension" is handled so that negative numbers are calculated correctly.