SECTIONS
{
  . = 0x80000000;         /* RAM start address */
  .text : { *(.text) }
  .data : { *(.data) }
  .bss  : { *(.bss)  }
}
