# PS4: Cish → RISC-V

Submit: `compile.ml`

## Notes

A docker image (provided from class) is needed to simulate RISC-V,
which is not included here because the file size is too big.

1. Build `ps4` via `make`.

2. Generate RISC-V instructions for a given text file.

```shell
./ps4 test/01cexpr_01add.cish > tmp.s
```

3. Open docker daemon and generate binary (`a.out`).

```shell
sh ./docker-gcc.sh tmp.s 
```

4. Simulate instructions in a RISC-V container.

```shell
sh ./docker-qemu.sh a.out
```

5. Check what the program returned.

```shell
echo $?
```

## Instructions

Your job for this assignment is to implement a compiler that maps Cish
source code down to RISC-V assembly.  Cish is quite similar to Fish
except that it adds functions, function calls, and local variables.
You will fill in missing code in `compile.ml` and then submit by uploading
this file to gradescope.

A Cish program is a list of functions.  Each function is of the form

```
var(var1,...,varn) { stmt_list }
```

mimicking functions in C.  Here, `var` is the name of the function and
`var1, ... ,varn` are the formal parameters of the functions.
The `{ stmt_list }` is a statement which should return an integer value to the
caller.  The distinguished function `main` is used to launch the
program.  For Cish, `main` should take no arguments.

To statements, we have added a new form:

```
stmt ::= ... | let var = exp; stmt
```

The intention is that this evaluates the expression `exp`, then declares
a new, locally-scoped variables `var`, and assigns the value of `exp` to
`var` as its initial value.  The scope of `var` extends across the
adjacent statement, and becomes unavailable outside.

To expressions, we have added a new form `var(exp1, ... ,expn)` which
represents a function call.  Here, `var` is the name of the function.

I've provided the abstract syntax, lexer, parser, and updated
interpreter.  You have to provide the compiler.  You'll ideally want
to follow the RISC-V standard calling convention (see the RISC-V manual
and the discussion in the lecture slides) except that you do not need
to worry about keeping the stack-pointer 128 bit aligned.

In particular, when calling a function, make sure to save any
caller-saves registers that you need preserved across the call, and
within a function, make sure to save any caller-saves registers used
by the function. That said, our testing code will NOT check directly
whether you follow a particular calling convention -- but whatever
calling convention you end up implementing ought to at least be
consistent.

A simple strategy for compilation is to keep an environment around
that maps variables (including formal parameters and local variables)
to integer offsets relative to the frame-pointer.  One option is to
make a pass over the code and determine how many distinct variables
and where they will live before compiling the code.  After this pass, 
you will be able to generate the prologue and epilogue.  

Another strategy is to "push" locally-defined variables on the stack and "pop"
them off as you encounter them.  Regardless, you'll need to keep track
of where each variable lives relative to either the stack or the frame
pointer.

I would suggest pushing temporary values on the stack and popping them
off instead of trying to do something fancier (as sketched in the
class notes.)  Get this working first before experimenting with something
more sophisticated!

Running `make` in the current directory generates an executable `ps4`, which
expects a file to compile, i.e. running

```
./ps4 tests/01cexpr_01add.cish
```

will emit the resulting assembly code. You can then save this to a
file and then assemble it using `docker-gcc.sh`, and run it using
`docker-temu.sh`/`docker-qemu.sh` as in PS3. Refer back to the
instructions there if you forgot.
