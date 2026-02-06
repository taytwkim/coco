# PS 1: RISC-V Simulator

Points: 6

Submit on gradescope: `riscv_sim.ml`

## Instructions

Your job for this assignment is to write a RISC-V interpreter for a
small subset of RV32I machine code. The starter code includes a file
to parse a small subset of RV32I assembly as well as an assembler that
loads the machine code into a simple model of a RV32I processor
state. Please note that the `beq` instruction has been altered to take
an offset directly instead of a label. This is done to keep things
simple.

A state consists of a register file, memory and program counter. Thus,
the `assem` function (in `riscv_assem.ml`) puts the 32-bit encodings of
each assembly instruction in memory, starts the PC at the beginning of
the machine code, and provides an empty register file.

Your job is to write the `interp` function in `riscv_sim.ml`. The `interp`
function takes the initial state provided by the assembler and
produces a final state by "executing" the machine code. That is, it
runs instructions repeatedly until the next instruction it decodes is
all 0s, in which case it stops.

You can assume that the only instructions in memory that `interp` will
have to handle will be the non-pseudo instructions listed in
`riscv_ast.ml`. Please read some of the comments on the data type
for `riscv_ast` to understand some of the choices we make for representing
instructions in the code that parses and assembly file and creates
a machine state.

To compile your code just run `make` from the command-line. This will
produce an executable called `./ps1`. You can execute this in one of two
ways:

1) `./ps1 <name of assembly file>`

   This will load the RISC-V assembly file indicated, assemble it,
   run your interpreter and then print the registers and memory of the
   final state reached. You can find some simple assembly files in the
   `tests/` directory. Note that this "assembler" doesn't understand labels
   for jump and branch instructions, instead we must supply the constants
   we want to use for those instructions directly.

2) just run `./ps1`, without arguments

   This will run a small test suite found in `riscv.ml` and compare your
   program's output with expected values for various programs.


## Hints:

We suggest dividing up your implementation of `interp` into separate
helper functions that can do the following:

  (1) a function to load the next 4 bytes from memory pointed to by
      the PC and assemble them into a word

  (2) a function to decode a word into an instruction, as represented
      in `riscv_ast`

  (3) separate functions to interpret each possible instruction
      e.g. in our reference solution we have `step_add`, `step_beq`, etc.

This makes it easy to test each individual function.
