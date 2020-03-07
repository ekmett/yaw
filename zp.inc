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

