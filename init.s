.include "mem.inc"
.include "kernal.inc"

.export init
.import __BSS_RUN__, __BSS_SIZE__, main

.segment "INIT"
.proc init
  memory_fill __BSS_RUN__,__BSS_SIZE__,0
  jmp main
.endproc
