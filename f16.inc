; f16: freshman 16 -- a lot heavier than sweet 16

; ldax R(0)
; adcw R(1)
; stax R(2)

.define is_imm(src) .match(.left(1,{src}),#)
.define as_imm(src) .right(.tcount({src})-1,{src})
; movw R(0), addr
; movw addr, R(0)
; movw R(1), R(3)
; movw R(4), #$1234
; movew addr, #$1234
.mac movw dst, src
  .if is_imm(src)
     pokew dst, as_imm(src)
  .else
     lda src
     sta dst
     lda 1+src
     sta 1+dst
  .endif
.endmac


; i'd like to use these as a building block, but it just seems to make me push more
.mac ldax src
  .if is_imm(src)
    lda #<as_imm(src)
    ldx #>as_imm(src)
  .else
    lda src
    ldx 1+src
  .endif
.endmac

; stax R(n); 4 bytes, 6 cycles
; stax addr; 6 bytes; 8 cycles
.mac stax dst
  sta dst
  stx 1+dst
.endmac

; cmpw (R(n)|addr|imm), (R(m)|add|imm)
; trashes eax, loads flags
.mac cmpw dst, src
  .if is_imm(dst) && is_imm(src)
    .if as_imm(dst) -- forcibly set n,z,c
  .if is_imm(src)
    .local done
    cmp 1+dst, >as_imm(src)
    bne done
    cmp dst, <as_imm(src)
    done:
  .else
    .local done
    lda 1+dst,
    cmp dst
   
  .endif
.endmac
 
; adcw #$1234 ; 14 cycles, ...
.mac adcw r1, r2
  .if .blank(r2)
    .if is_imm(r1)
      adc #<as_imm(r1)
      pha
      txa
      adc #>as_imm(r1)
    .else
      adc r1
      pha
      txa
      adc r1+1
    .endif
    tax
    pla
  .else
    .if is_imm(r2)
      
      adc #<as_imm(r1)
  .endif
.endmac 
     
.mac aslax
.endmac  

; aslw      ; 15 cycles, 6 bytes -- ax
; aslw R(n) ; 10 cycles, 4 bytes -- zeropage
; aslw addr ; 12 cycles, 6 bytes -- addr
.mac aslw dst
  .if blank(dst)
    asl a ; 2
    pha ; 3
    txa ; 2
    rol a ; 2
    tax ; 2
    pla ; 4
  .else
    asl dst
    rol 1+dst
  .endif
