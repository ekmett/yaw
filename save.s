; Apple-style save/restore

.export save, restore

.bss
acc: .res 1
xreg: .res 1
yreg: .res 1
status: .res 1

.code
.proc save
  sta acc
  stx xreg
  sty yreg
  php
  pla
  sta status
  cld
  rts
.endproc

.proc restore
  lda status
  pha
  lda acc
  ldx xreg
  ldy yreg
  plp 
  rts
.endproc
