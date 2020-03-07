.import init

.mac digit addr,base
  .lobytes ($30 + (addr/base) .mod 10)
.endmac

.mac sys addr
.byte $9e," "
.assert addr/10000 = 0,error,"starting address too large"
;digit addr,10000
digit addr,1000
digit addr,100
digit addr,10
digit addr,1
.endmac

.segment "EXEHDR"
basic:
  .word next, 1
  sys init
  next: .word 0
