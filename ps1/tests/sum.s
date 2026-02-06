  li x28, 42
  li x29, 0
  beq x28, x0, 16
  add x29, x29, x28
  addi x28, x28, -1
  jal x0, -12
  li x0, 0
