.ifndef INCLUDED_KERNAL
INCLUDED_KERNAL = 1

.include "mem.inc"
.include "zp.inc"

.mac jsrfar addr
  jmp $ff6e
  .loword addr
  .byte ^addr
.endmac

.mac monitor
  jmp $ff44
.endmac

.mac memory_fill data, len, value
  pokew R(0), data
  pokew R(1), len
  lda #value
  jsr $fee4
.endmac

.mac memory_copy source, target, len
  pokew R0, source
  pokew R1, target
  pokew R2, len
  jmp $fee7
.endmac  

ksetlfs = $ffba
ksetnam = $ffbb
kload   = $ffd5

; todo: parse namestr and figure out if its a string or an address
.mac load namestr, device, secondary
  .local namedata
  .pushseg
  .data
  namedata: .byte namestr
  .popseg

  .if .blank(device)
    lda #8
  .else
    lda #device
  .endif
  lda #device
  ldx #2
  .if .blank(secondary)
  ldy #0
  .else
  ldy #secondary
  .endif
  jsr ksetlfs
  
  lda .sizeof(namedata)
  ldx #<namedata
  ldy #>namedata
  jsr ksetnam
 
  lda #1
  ldy $20
  jsr kload
.endmac

.endif
