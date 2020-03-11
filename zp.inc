.ifndef INCLUDED_ZP
INCLUDED_ZP = 1

.mac reg name
  .pushseg
  .segment "ZP2": zeropage
  .align 2
  .ident(name):
    .res 2
  .ident(.concat(name,"_L")) = .ident(name)
  .ident(.concat(name,"_H")) = .ident(name) + 1
  .popseg
.endmac

.mac globalreg name
  .globalzp .ident(name), .ident(.concat(name,"_L")), .ident(.concat(name,"_H"))
.endmac

KERNEL_REGS = 2

.define R(n) KERNEL_REGS + (n*2)
.define RL(n) R(n)
.define RH(n) R(n)+1

; sweet16 and the kernel kind of share the same registers
; one option is to bias the sweet16 regs _down_ by one, then we can call kernel routines and still have our sweet 16 accumulator

R0 = R(0)
R0L = RL(0)
R0H = RH(0)

R1 = R(1)
R1L = RL(1)
R1H = RH(1)

R2 = R(2)
R2L = RL(2)
R2H = RH(2)

R3 = R(3)
R3L = RL(3)
R3H = RH(3)

R4 = R(4)
R4L = RL(4)
R4H = RH(4)

R5 = R(5)
R5L = RL(5)
R5H = RH(5)

R6 = R(6)
R6L = RL(6)
R6H = RH(6)

R7 = R(7)
R7L = RL(7)
R7H = RH(7)

R8 = R(8)
R8L = RL(8)
R8H = RH(8)

R9 = R(9)
R9L = RL(9)
R9H = RH(9)

R10 = R(10)
R10L = RL(10)
R10H = RH(10)

R11 = R(11)
R11L = RL(11)
R11H = RH(11)

R12 = R(12)
R12L = RL(12)
R12H = RH(12)

R13 = R(13)
R13L = RL(13)
R13H = RH(13)

R14 = R(14)
R14L = RL(14)
R14H = RH(14)

R15 = R(15)
R15L = RL(15)
R15H = RH(15)

.endif
