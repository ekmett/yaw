;sweet16 port to x16

.global sweet16
.globalzp RSWEET16, R0H, R14H, R15L, R15H

.define R(n) RSWEET16 + (n*2)
.define RL(n) R(n)
.define RH(n) R(n)+1

R0L  = RL(0)
R0H  = RH(0)
R14H = RH(14)
R15L = RL(15)
R15H = RH(15)

.mac sw16
  jsr sweet16
  .setcpu "sweet16"
.endmac

.mac endsw16
  rtn
  .setcpu "65c02"
.endmac
