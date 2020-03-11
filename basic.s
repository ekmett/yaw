.import init
.define d(i) <($30 + i .mod 10)

.mac sys addr
.assert addr/10000 = 0,error,"starting address too large"
.byte $9e," ",d(addr/1000),d(addr/100),d(addr/10),d(addr)
.endmac

; 1 sys (init)
.segment "EXEHDR"
basic:
  .word next, 1
  sys init
  next: .word 0,0
