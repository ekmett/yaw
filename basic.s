.import init

.mac sys addr
.assert addr/10000 = 0,error,"starting address too large"
.byte $9e," "
.lobytes $30 + (addr/1000) .mod 10, $30 + (addr/100) .mod 10, $30 + (addr/10) .mod 10, $30 .mod 10
.endmac

.segment "EXEHDR"
basic:
  .word next, 1
  sys init
  next: .word 0
