.section .text
.global _start

_start:
    # Set the stack pointer (sp) to the top of RAM
    # The 'virt' machine has RAM starting at 0x80000000
    # Let's put the stack 64KB above that.
    lui sp, 0x80010      # sp = 0x80010000
    call main            # Jump to your C 'main'

    # a0 = (return_value << 16) | 0x3333
    slli a0, a0, 16      
    li t1, 0x3333
    or a0, a0, t1

    # 4. Shutdown Sequence
    li t0, 0x100000      # SiFive Test device address
    sw a0, 0(t0)         # write ((return_value << 16) | 0x3333) to device

loop: 
    j loop               # Infinite loop if something goes wrong
