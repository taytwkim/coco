<h1 align="center">Compiler Construction</h1>

This semester, I'm taking a course on Compilers where the problem sets involve incrementally building compiler features in OCaml. Starters were provided by Professor Sam Westrick (NYU).

## 📚 Study Notes

- [Part 1: Assembly, Lexer, Parser, and Function Calls](https://taytwkim.vercel.app/blog/compiler/000-coco1/)
- [Part 2: Closures and Type Inference](https://taytwkim.vercel.app/blog/compiler/001-coco2/)
- [Part 3: A-Normal Form and Optimizations](https://taytwkim.vercel.app/blog/compiler/002-coco3/)
- [Part 4: Data-flow Analysis on a Control-flow Graph](https://taytwkim.vercel.app/blog/compiler/003-coco4/)

## 🐫 Problem Sets

### 🗂️ Table of Contents

- [RISC-V Simulator](#ps1)
- [Lexer and Parser](#ps2)
- [Fortran-ish → RISC-V](#ps3)
- [C-ish → RISC-V (Function Calls)](#ps4)
- [Scheme-ish → C-ish (Closures)](#ps5)
- [ML-ish → Scheme-ish (Type Inference)](#ps6)
- [Control-flow Graph Analysis](#ps7)
- [Register Allocation](#mini-project)

### <a id="ps1"></a> `ps1`: RISC-V Simulator

Simulate how a RISC-V CPU executes instructions. The machine's status is represented as a `state`, which includes registers, memory, and the Program Counter (PC).

The simulator follows these steps in a loop:

1. **Fetch**: Reads a 32-bit binary instruction from memory at the location pointed to by the PC.

2. **Decode**: Decodes the binary word to figure out which instruction it is (like `Add` or `Beq`) and extracts immediates.

3. **Execute**: Updates the CPU state (registers, PC) based on the instruction.

### <a id="ps2"></a> `ps2`: Lexer and Parser

Implement a lexer and a parser for the Fish ("Fortran-ish") programming language.

Given the specifications of Fish, implement a lexer and parser that generate a valid AST from Fish source.

We were given a choice between implementing the lexer/parser manually in a combinator-style approach, or using Lex (`ocamllex`) and Yacc (`ocamlyacc`). For this assignment, I used the latter.

### <a id="ps3"></a> `ps3`: Fish → RISC-V

Bridge `ps1` and `ps2` by taking a Fish AST and compiling it into RISC-V assembly.

The tricky parts are generating labels to use as jump targets for branches and loops, and working with a limited set of registers while making sure we don’t accidentally overwrite values we still need.

### <a id="ps4"></a> `ps4`: Cish → RISC-V

Fish has served us well, but it’s time to tackle a slightly more complex programming language. Cish (“C-ish”) is the next step-up from Fish that adds functions, function calls, and local variables.

In implementing function calls, we treat special registers like `sp` and `fp` as anchors and use them as references to store function arguments and local variables at precomputed offsets.

### <a id="ps5"></a> `ps5`: Scish → Cish

We move to a richer programming language, Scish (“Scheme-ish”). Scish is a functional programming language in which functions are first-class values. The main focus of `ps5` is compiling closures. This requires dynamically allocating memory for environments and working with pointers to function bodies and environments.

Instead of compiling directly to RISC-V, we take a Scish AST and translate it into a Cish AST. Since we already implemented a Cish → RISC-V compiler in `ps4`, this means a Scish source program can be compiled in two steps: Scish → Cish → RISC-V. A real compiler would not necessarily take this two-step approach.

### <a id="ps6"></a> `ps6`: MLish → Scish

We implement a compiler for an "ML-ish" programming language. As in `ps5`, instead of targeting RISC-V, we compile to a higher-level representation (Scish).

MLish is fairly similar to Scish, but we add another layer of complexity. In addition to compilation, we also type-check/type-infer the given ML program. The difficult part is implementing core set of operations such as `unify`, `generalize`, and `initialize`.

### <a id="ps7"></a> `ps7`: Control-flow Graph Analysis

Until now, we were mostly concerned with the correctness of our compiler. From this point on, we start thinking about optimizations (generating performant RISC-V instructions).

The goal of `ps7` is to run a liveness analysis on a CFG, then use the computed `LiveIn` and `LiveOut` table to build an interference graph. The mini project uses the interference graph constructed here to implement register allocation, a common optimization technique.

### <a id="mini-project"></a> `mini project`: Register Allocation

The goal of the mini project is to optimize our compiler by implementing register allocation. Using the interference graph from `ps7`, we assign registers to temporary variables so that interfering values are not placed in the same register. 

When register pressure is too high (too many temporaries interfere at the same time), some temporaries must be spilled to the stack, which adds expensive memory accesses. The main objective is to minimize spills and improve execution time compared to the `ps4` baseline.