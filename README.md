# Compiler Construction

Implementing compiler features in OCaml.

### Table of Contents:

- [`ps0`: OCaml Warm Up](#ps0)
- [`ps1`: RISC-V Simulator](#ps1)
- [`ps2`: Lexer and Parser](#ps2)
- [`ps3`: Fortran-ish → RISC-V](#ps3)
- [`ps4`: C-ish → RISC-V](#ps4)

## <a id="ps0"></a> `ps0`: OCaml Warm Up

Getting familiar with OCaml.

## <a id="ps1"></a> `ps1`: RISC-V Simulator

Simulate how a RISC-V CPU executes instructions. The machine's status is represented as a `state`, which includes registers, memory, and the Program Counter (PC).

The simulator follows these steps in a loop:

1. **Fetch**: Reads a 32-bit binary instruction from memory at the location pointed to by the PC.

2. **Decode**: Decodes the binary word to figure out which instruction it is (like `Add` or `Beq`) and extracts the immediates needed.

3. **Execute**: Updates the CPU state (registers, PC) based on the instruction.

## <a id="ps2"></a> `ps2`: Lexer and Parser

Implement a lexer and a parser for the Fish ("Fortran-ish") programming language.

Given the specifications of Fish, implement a lexer and parser that generate a valid AST from raw input source.

We were given a choice between implementing the lexer/parser manually using a combinator-style approach, or using Lex (`ocamllex`) and Yacc (`ocamlyacc`). For this assignment, I used the latter.

## <a id="ps3"></a> `ps3`: Fish → RISC-V

Bridge `ps1` and `ps2` by taking a Fish AST and compiling it into RISC-V assembly.

The tricky parts are generating fresh labels to use as jump targets for branches and loops, and working with a limited set of registers while making sure we don’t accidentally overwrite values we still need.

## <a id="ps4"></a> `ps4`: Cish → RISC-V

Fish has served us well, but it’s time to tackle a slightly more complex programming language. Cish (“C-ish”) is the next step-up from Fish that adds functions, function calls, and local variables.

In implementing function calls, we treat special registers like `sp` and `fp` as anchors and use them as references to store function arguments and local variables at precomputed offsets.
