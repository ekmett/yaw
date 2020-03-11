.ifndef INCLUDED_MEM
INCLUDED_MEM = 1

.mac rambank n
  poke $9f61, n
.endmac

.mac rombank n
  poke $9f60, n
.endmac

.mac poke addr, value
  .if .const(value) && (value = 0)
    stz addr
  .else
    lda #value
    sta addr
  .endif
.endmac

.mac pokes addr,r1,r2,r3,r4,r5,r6,r7,r8
  .if .blank(r1)
    .exitmacro
  .endif
  poke addr, r1
  pokes addr+1,r2,r3,r4,r5,r6,r7,r8
.endmac

.mac pokew addr, value
  poke addr, <value
  poke addr+1, >value
.endmac

.mac pokews addr,r1,r2,r3,r4,r5,r6,r7,r8
  .if .blank(r1)
    .exitmacro
  .endif
  pokew addr,r1,last
  pokews addr+2,r2,r3,r4,r5,r6,r7,r8
.endmac

.endif
