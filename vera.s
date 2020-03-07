; VERA display routines
; (c) Edward Kmett 2020
; BSD 2-clause license

.include "vera.inc"

reg "RBLIT"

.segment "LOWCODE"
.proc blit_big
  lda (RBLIT),y
  vout
  iny
  bne blit_big
  inc RBLIT_H
  dex
  bne blit_big
  rts
.endproc
