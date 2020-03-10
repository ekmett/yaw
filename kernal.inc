.mac jsrfar addr
  jmp $ff6e
  .loword addr
  .byte ^addr
.endmac

.mac monitor
  jmp $ff44
.endmac

.mac memory_copy source, target, len
  wpoke R0, source
  wpoke R1, target
  wpoke R2, len
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
  name: .byte namestr
  .popseg
  .if blank(device)
    lda #8
  .else
    lda #device
  .endif
  lda #device
  ldx #2
  .if blank(secondary)
  ldy #0
  .else
  ldy #secondary
  .endif
  jsr ksetlfs
  
  lda sizeof(name)
  ldx <name
  ldy >name
  jsr ksetnam
 
  lda #1
  ldy $20
  jsr kload
.endmac
