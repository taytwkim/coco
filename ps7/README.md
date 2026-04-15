# PS7: Control-flow Graph Analysis

Submit: `cfg.ml`

For PS7, your job is to construct a liveness dataflow analysis from a
CFG representation and then use the information to build an
interference graph.

The file `cfg_ast.ml` contains the definition of the CFG intermediate form.
At a conceptual level, it's quite close to RISC-V code except that it
allows variables to serve as operands.

Basic blocks are represented as lists of instructions that
includes labels and control-flow operators (e.g., jump, return,
if-then-goto.)  Although this fails to capture the structural
constraints on basic blocks (always start with a label, always end
with control-flow, no-intervening label or control-flow), it will make
compilation and analysis a little easier.

Included in `cfg_ast.ml` is a function `fn2blocks` which translates the
abstract syntax of a Cish function into a list of basic blocks.

**Warning!** This code has not been heavily tested and may have bugs
within it. If you encounter a bug, please let me know and I will post
a fix as soon as possible. Such bugs would not affect the correctness
of your dataflow analysis/interference graph generation, but they may
make it confusing to debug, so let me know!

Your goal is to write the following function in `cfg.ml`

```ocaml
build_interfere_graph : function -> interfere_graph
```

where "function" is a list of CFG basic blocks. When complete, you will upload
`cfg.ml` to GradeScope.

To construct the interference graph, you will have to build a liveness
dataflow analysis that calculates for each instruction a set of
variables that are live coming in to the instruction, and a set of
variables that are live coming out of the instruction. Refer to the
lecture notes for details on the dataflow algorithm for liveness.

If you run `make` it produces a binary called `./ps7_cfg`. Running

```shell
./ps7_cfg [name of cish file]
```

calls the Cish to block converter, then runs your interference graph generator,
and then tries to print the CFG blocks and the interference. Here's some sample
output of the interference graph printer:

```
$x1	  : {t0,t10,t11,t12,t2,t3,t4,t5,t6,t7,t8,t9}
$x8	  : {t1,t10,t11,t12,t2,t3,t4,t5,t6,t7,t8,t9}
$x9	  : {$x18,$x19,$x20,$x21,$x22,$x23,$x24,$x25,$x26,$x27,t0,t1,t10,t11,t12,t3,t4,t5,t6,t7,t8,t9}
$x10	: {t0,t1,t10,t11,t12,t2,t3,t4,t5,t6,t7,t8,t9}
$x18	: {$x9,$x19,$x20,$x21,$x22,$x23,$x24,$x25,$x26,$x27,t0,t1,t10,t11,t12,t2,t4,t5,t6,t7,t8,t9}
$x19	: {$x9,$x18,$x20,$x21,$x22,$x23,$x24,$x25,$x26,$x27,t0,t1,t10,t11,t12,t2,t3,t5,t6,t7,t8,t9}
$x20	: {$x9,$x18,$x19,$x21,$x22,$x23,$x24,$x25,$x26,$x27,t0,t1,t10,t11,t12,t2,t3,t4,t6,t7,t8,t9}
$x21	: {$x9,$x18,$x19,$x20,$x22,$x23,$x24,$x25,$x26,$x27,t0,t1,t10,t11,t12,t2,t3,t4,t5,t7,t8,t9}
$x22	: {$x9,$x18,$x19,$x20,$x21,$x23,$x24,$x25,$x26,$x27,t0,t1,t10,t11,t12,t2,t3,t4,t5,t6,t8,t9}
$x23	: {$x9,$x18,$x19,$x20,$x21,$x22,$x24,$x25,$x26,$x27,t0,t1,t10,t11,t12,t2,t3,t4,t5,t6,t7,t9}
$x24	: {$x9,$x18,$x19,$x20,$x21,$x22,$x23,$x25,$x26,$x27,t0,t1,t10,t11,t12,t2,t3,t4,t5,t6,t7,t8}
$x25	: {$x9,$x18,$x19,$x20,$x21,$x22,$x23,$x24,$x26,$x27,t0,t1,t11,t12,t2,t3,t4,t5,t6,t7,t8,t9}
$x26	: {$x9,$x18,$x19,$x20,$x21,$x22,$x23,$x24,$x25,$x27,t0,t1,t10,t12,t2,t3,t4,t5,t6,t7,t8,t9}
$x27	: {$x9,$x18,$x19,$x20,$x21,$x22,$x23,$x24,$x25,$x26,t0,t1,t10,t11,t2,t3,t4,t5,t6,t7,t8,t9}
t0	  : {$x1,$x9,$x10,$x18,$x19,$x20,$x21,$x22,$x23,$x24,$x25,$x26,$x27,t1,t10,t11,t12,t13,t2,t3,t4,t5,t6,t7,t8,t9}
t1	  : {$x8,$x9,$x10,$x18,$x19,$x20,$x21,$x22,$x23,$x24,$x25,$x26,$x27,t0,t10,t11,t12,t13,t2,t3,t4,t5,t6,t7,t8,t9}
t10	  : {$x1,$x8,$x9,$x10,$x18,$x19,$x20,$x21,$x22,$x23,$x24,$x26,$x27,t0,t1,t11,t12,t13,t2,t3,t4,t5,t6,t7,t8,t9}
t11	  : {$x1,$x8,$x9,$x10,$x18,$x19,$x20,$x21,$x22,$x23,$x24,$x25,$x27,t0,t1,t10,t12,t13,t2,t3,t4,t5,t6,t7,t8,t9}
t12	  : {$x1,$x8,$x9,$x10,$x18,$x19,$x20,$x21,$x22,$x23,$x24,$x25,$x26,t0,t1,t10,t11,t13,t2,t3,t4,t5,t6,t7,t8,t9}
t13	  : {t0,t1,t10,t11,t12,t2,t3,t4,t5,t6,t7,t8,t9}
t2	  : {$x1,$x8,$x10,$x18,$x19,$x20,$x21,$x22,$x23,$x24,$x25,$x26,$x27,t0,t1,t10,t11,t12,t13,t3,t4,t5,t6,t7,t8,t9}
t3	  : {$x1,$x8,$x9,$x10,$x19,$x20,$x21,$x22,$x23,$x24,$x25,$x26,$x27,t0,t1,t10,t11,t12,t13,t2,t4,t5,t6,t7,t8,t9}
t4	  : {$x1,$x8,$x9,$x10,$x18,$x20,$x21,$x22,$x23,$x24,$x25,$x26,$x27,t0,t1,t10,t11,t12,t13,t2,t3,t5,t6,t7,t8,t9}
t5	  : {$x1,$x8,$x9,$x10,$x18,$x19,$x21,$x22,$x23,$x24,$x25,$x26,$x27,t0,t1,t10,t11,t12,t13,t2,t3,t4,t6,t7,t8,t9}
t6	  : {$x1,$x8,$x9,$x10,$x18,$x19,$x20,$x22,$x23,$x24,$x25,$x26,$x27,t0,t1,t10,t11,t12,t13,t2,t3,t4,t5,t7,t8,t9}
t7	  : {$x1,$x8,$x9,$x10,$x18,$x19,$x20,$x21,$x23,$x24,$x25,$x26,$x27,t0,t1,t10,t11,t12,t13,t2,t3,t4,t5,t6,t8,t9}
t8	  : {$x1,$x8,$x9,$x10,$x18,$x19,$x20,$x21,$x22,$x24,$x25,$x26,$x27,t0,t1,t10,t11,t12,t13,t2,t3,t4,t5,t6,t7,t9}
t9	  : {$x1,$x8,$x9,$x10,$x18,$x19,$x20,$x21,$x22,$x23,$x25,$x26,$x27,t0,t1,t10,t11,t12,t13,t2,t3,t4,t5,t6,t7,t8}
```

Each row starts with the name in the graph (so either `$x:` representing
register `x`, or a string, representing a variable). Then, after the colon it
prints the set of all the nodes that are adjacent to that node.

For example, in the above, variable `t0` interferes with (among many other
things) register `$x1` and variable `t1`.

# Hints and Clarification

First, a clarification. There are two sensible ways to define
interference, one used in the reference book by Appel and one used in
the lecture notes:

- Lecture notes definition: `x` and `y` interfere if `x` and `y` are both live
  at the same time

- Appel's definition: `x` and `y` interfere if `y` is live when `x` is defined

These definitions differ under the following edge case: Under Appel's
definition, `x` and `y` will interfere even if `x` is never used after its
definition. Yet if `x` is never used after its definition, it would not
ever be live, so it could not interfere with `y` under the lecture notes
definition.

This discrepancy doesn't really matter too much, particularly if
you have dead code elimination, so we won't stress it; feel free to pick
either choice.

Second, you can take a look at some sample output from the reference
compiler in the `sample/` subdirectory. Each output file is named after
the corresponding Cish test case it was generated from.

Don't worry too much about trying to *exactly* match this sample
output, particularly for the register nodes in the graph. I don't
particularly care if you exactly get the right liveness analysis for
the registers / calling convention of RISC-V.  The autograder is going
to be focused instead on clearly incorrect claims about interference
(or lack of interference) between temporary variables. It's worth
noting that the sample output uses the Appel-style.

In particular, it can be hard to understand what's going on with all
of the conflicts between callee-saved registers and the temps
associated with them that the Cish -> CFG generator inserts in the
prologue/epilogue to save these values. Thus, we provide an option
to simplify the generated CFGs by omitting the code in the prologue/epilogue
to save callee saved registers.

You can enable this optional behavior by passing an additional
argument of "true" to `ps7_cfg`, as in:

```
./ps7_cfg test/01cexpr_01add.cish true
```

Which will generate something like:

```
Omitting callee-saved register handling
==========================
Processing function: main
blocks =
main:
t0 := 12+4
$x10 := t0
jump .L0

.L1:
jump .L0

.L0:
return
```

The sample directory also contains some version of the reference
compiler output when run with this option configured; these examples
end with the suffix `_simple.out`.

**IMPORTANT:**
The autograder will use this option when evaluating your submissions.

**IMPORTANT:**
The autograder will call your `build_interfere_graph` across all of the
examples it tests within a single execution. Thus, if you use mutable
state in your `build_interfere_graph`, it is very important to reset
this mutable state at the beginning of each call. Many times, when
testing on their local machine, students only ever invoke
`build_interfere_graph` once (because they just call `./ps7_cfg` one at a
time on each test case), leading to discrepancies between the
autograder and the local tests.