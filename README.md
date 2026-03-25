# Compiler Construction

This semester, I'm taking a course on Compilers where the problem sets involve incrementally building compiler features in OCaml. Starter code was provided by Professor Sam Westrick and Joseph Tassarotti (NYU).

## Table of Contents

- [OCaml Warm Up](#ps0)
- [RISC-V Simulator](#ps1)
- [Lexer and Parser](#ps2)
- [Fortran-ish → RISC-V](#ps3)
- [C-ish → RISC-V](#ps4)
- [Scheme-ish → C-ish](#ps5)
- [ML-ish → Scheme-ish](#ps6)

## <a id="ps0"></a> `ps0`: OCaml Warm Up

Getting started with OCaml.

## <a id="ps1"></a> `ps1`: RISC-V Simulator

Simulate how a RISC-V CPU executes instructions. The machine's status is represented as a `state`, which includes registers, memory, and the Program Counter (PC).

The simulator follows these steps in a loop:

1. **Fetch**: Reads a 32-bit binary instruction from memory at the location pointed to by the PC.

2. **Decode**: Decodes the binary word to figure out which instruction it is (like `Add` or `Beq`) and extracts immediates.

3. **Execute**: Updates the CPU state (registers, PC) based on the instruction.

## <a id="ps2"></a> `ps2`: Lexer and Parser

Implement a lexer and a parser for the Fish ("Fortran-ish") programming language.

Given the specifications of Fish, implement a lexer and parser that generate a valid AST from Fish source.

We were given a choice between implementing the lexer/parser manually using a combinator-style approach, or using Lex (`ocamllex`) and Yacc (`ocamlyacc`). For this assignment, I used the latter.

## <a id="ps3"></a> `ps3`: Fish → RISC-V

Bridge `ps1` and `ps2` by taking a Fish AST and compiling it into RISC-V assembly.

The tricky parts are generating labels to use as jump targets for branches and loops, and working with a limited set of registers while making sure we don’t accidentally overwrite values we still need.

## <a id="ps4"></a> `ps4`: Cish → RISC-V

Fish has served us well, but it’s time to tackle a slightly more complex programming language. Cish (“C-ish”) is the next step-up from Fish that adds functions, function calls, and local variables.

In implementing function calls, we treat special registers like `sp` and `fp` as anchors and use them as references to store function arguments and local variables at precomputed offsets.

## <a id="ps5"></a> `ps5`: Scish → Cish

We move to a richer programming language, Scish (“Scheme-ish”). Scish is a functional programming language in which functions are first-class values. The main focus of `ps5` is compiling closures. This requires dynamically allocating memory for environments and working with pointers to function bodies and environments.

Instead of compiling directly to RISC-V, we take a Scish AST and translate it into a Cish AST. Since we already implemented a Cish → RISC-V compiler in `ps4`, this means a Scish source program can be compiled in two steps: Scish → Cish → RISC-V. A real compiler would not necessarily take this two-step approach.

## <a id="ps6"></a> `ps6`: MLish → Scish