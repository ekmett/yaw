.mac rambank n, last
  poke $9f61, n, last
.endmac

.mac rombank n, last
  poke $9f60, n, last
.endmac

.mac poke addr, value, last ; last is optional
  .if .const(value) && (value = 0)
    stz addr
;  .elseif .const(value) && .const(last) && .not(.blank(last)) && (value = last)
;    sta addr
  .else
    lda #value
    sta addr
  .endif
.endmac

.mac pokes_after addr, last, r1,r2,r3,r4,r5,r6,r7,r8
  .if .blank(r1)
    .exitmacro
  .endif
  poke addr, r1, last
  pokes_after addr+1,r1,r2,r3,r4,r5,r6,r7,r8
.endmac

.mac pokes addr,r1,r2,r3,r4,r5,r6,r7,r8
  pokes_after addr,,r1,r2,r3,r4,r5,r6,r7,r8
.endmac

.mac pokew addr, value, last
  poke addr, <value, last
  poke addr, >value, <value
.endmac

.mac pokews_after addr, last, r1,r2,r3,r4,r5,r6,r7,r8
  .if .blank(r1)
    .exitmacro
  .endif
  pokew addr, r1, last
  pokews_after addr+2,>(r1),r2,r3,r4,r5,r6,r7,r8
.endmac

.mac pokews addr,r1,r2,r3,r4,r5,r6,r7,r8
  pokews_after addr,,r1,r2,r3,r4,r5,r6,r7,r8
.endmac

