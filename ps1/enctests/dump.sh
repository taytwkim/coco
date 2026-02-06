#!/bin/bash

# riscv32-unknown-linux-gnu-as $1
# riscv32-unknown-linux-gnu-objdump -d a.out

# translates human-readable assembly code into binary, creates a.out
riscv64-elf-as -march=rv32i -mabi=ilp32 $1 -o a.out

# look at a.out and and disassemble the bytes
riscv64-elf-objdump -d a.out