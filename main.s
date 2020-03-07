.include "vera.inc"
.include "data.inc"

.export init

.segment "LOWCODE"
.proc init
  ; init bss here
  ; TODO: copy sw16 to the top of memory
  jmp main
.endproc

.code
.proc main
  vera_init
  blit dc
  init_bitmap_layer 0, 6, 0, 0, 0
  init_bitmap_layer 1, 6, 0, (320*240>>1), 1
  vset VREG::PALETTE
  blit palette
  rts
.endproc
