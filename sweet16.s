  ; APPLE II PSEUDO MACHINE INTERPRETER
  ; COPYRIGHT (C) 1977
; APPLE COMPUTER INC.
; STEVE WOZNIAK
; TITLE: SWEET 16 INTERPRETER

.include "sweet16.inc"

.pc02

.bss
acc: .res 1
xreg: .res 1
yreg: .res 1
status: .res 1

.segment "ZP2": zeropage
.align 2
RSWEET16: .res 32

R12 = R(12)

.segment "SW16"

.proc save
  sta acc
  stx xreg
  sty yreg
  php
  pla
  sta status
  cld
  rts
.endproc

.proc restore
  lda status
  pha
  lda acc
  ldx xreg
  ldy yreg
  plp 
  rts
.endproc

ZP_SWEET16 = 2

.proc sweet16
  jsr save
  pla
  sta R15L
  pla
  sta R15H
sw16b:
  jsr sw16c
  jmp sw16b
sw16c:
  inc R15L
  bne sw16d
  inc R15H
sw16d:
  lda #>set
  pha
  ldy $0
  lda (R15L),y
  and $F
  asl
  tax
  lsr
  eor (R15L),y
  beq tobr
  stx R14H
  lsr
  lsr
  lsr
  tay
  lda optbl-2,y
  pha
  rts
tobr:
  inc R15L
  bne tobr2
  inc R15H
tobr2:
  lda brtbl,x
  pha
  lda R14H
  lsr
  rts
rtnz:
  pla
  pla
  jsr restore
  jmp (R15L)
setz:
  lda (R15L),y
  sta R0H,x
  dey
  lda (R15L),y
  sta R0L,x
  tya
  sec
  adc R15L
  sta R15L
  bcc set2
  inc R15H
set2: rts

optbl:
  .lobytes set-1 ; 1x
brtbl:
  .lobytes rtn-1 ;0
  .lobytes ld-1 ;2x 
  .lobytes br-1 ;1
  .lobytes st-1 ;3x
  .lobytes bnc-1 ;2
  .lobytes ldat-1 ;4X
  .lobytes bc-1 ;3
  .lobytes stat-1 ;5X
  .lobytes bp-1 ;4
  .lobytes lddat-1 ;6X
  .lobytes bm-1 ;5
  .lobytes stdat-1 ;7X
  .lobytes bz-1 ;6
  .lobytes pop-1 ;8X
  .lobytes bnz-1 ;7
  .lobytes stpat-1 ;9X
  .lobytes bm1-1 ;8
  .lobytes add-1 ;AX
  .lobytes bnm1-1 ;9
  .lobytes sub-1 ;BX
  .lobytes bk-1 ;A
  .lobytes popd-1 ;CX
  .lobytes rs-1 ;B
  .lobytes cpr-1 ;DX
  .lobytes bs-1 ;C
  .lobytes inr-1 ;EX
  .lobytes nul-1 ;D
  .lobytes dcr-1 ;FX
  .lobytes nul-1 ;E
  .lobytes nul-1 ;UNUSED
  .lobytes nul-1 ;F

; must fit within a page
set:    bpl setz           ; bpl always taken, bra on 65c02?
ld:     lda R0L,x
        sta R0L
        lda R0H,x          ;MOVE RX TO R0
        sta R0H
        rts
bk:     brk ; = *-1                   ;MOVED BECAUSE R0L <> 0 -- EAK
st:     lda R0L
        sta R0L,x          ;MOVE R0 TO RX
        lda R0H
        sta R0H,x
        rts
stat:   lda R0L
stat2:  sta (R0L,x)        ;STORE BYTE INDIRECT
        ldy $0
stat3:  sty R14H           ;INDICATE R0 IS RESULT NEG
inr:    inc R0L,x
        bne inr2           ;INCR RX
        inc R0H,x
inr2:   rts
ldat:   lda (R0L,x)        ;LOAD INDIRECT (RX)
        sta R0L            ;TO R0
        ldy $0
        sty R0H            ;ZERO HIGH ORDER R0 BYTE
        beq stat3          ;ALWAYS TAKEN
pop:    ldy $0             ;HIGH ORDER BYTE = 0
        beq pop2           ;ALWAYS TAKEN
popd:   jsr dcr            ;DECR RX
        lda (R0L,x)        ;POP HIGH ORDER BYTE @RX
        tay                ;SAVE IN Y REG
pop2:   jsr dcr            ;DECR RX
        lda (R0L,x)        ;LOW ORDER BYTE
        sta R0L            ;TO R0
        sty R0H
pop3:   ldy $0             ;INDICATE R0 AS LAST RESULT REG
        sty R14H
        rts
lddat:  jsr ldat           ;LOW ORDER BYTE TO R0, INCR RX
        lda (R0L,x)        ;HIGH ORDER BYTE TO R0
        sta R0H
        jmp inr            ;INCR RX
stdat:  jsr stat           ;STORE INDIRECT LOW ORDER
        lda R0H            ;BYTE AND INCR RX. THEN
        sta (R0L,x)        ;STORE HIGH ORDER BYTE.
        jmp inr            ;INCR RX AND RETURN
stpat:  jsr dcr            ;DECR RX
        lda R0L
        sta (R0L,x)        ;STORE R0 LOW BYTE @RX
        jmp pop3           ;INDICATE R0 AS LAST RESULT REG
dcr:    lda R0L,x
        bne dcr2           ;DECR RX
        dec R0H,x
dcr2:   dec R0L,x
        rts
sub:    ldy $0             ;RESULT TO R0
cpr:    sec                ;NOTE Y REG = 13*2 FOR CPR
        lda R0L
        sbc R0L,x
        sta R0L,y          ;R0-RX TO RY
        lda R0H
        sbc R0H,x
sub2:   sta R0H,y
        tya                 ;LAST RESULT REG*2
        adc $0             ;CARRY TO LSB
        sta R14H
        rts
add:    lda R0L
        adc R0L,x
        sta R0L            ;R0+RX TO R0
        lda R0H
        adc R0H,x
        ldy $0             ;R0 FOR RESULT
        beq sub2           ;FINISH ADD
bs:     lda R15L           ;NOTE X REG IS 12*2!
        jsr stat2          ;PUSH LOW PC BYTE VIA R12
        lda R15H
        jsr stat2          ;PUSH HIGH ORDER PC BYTE
br:     clc
bnc:    bcs bnc2           ;NO CARRY TEST
br1:    lda (R15L),y       ;DISPLACEMENT BYTE
        bpl br2
        dey
br2:    adc R15L           ;ADD TO PC
        sta R15L
        tya
        adc R15H
        sta R15H
bnc2:   rts
bc:     bcs br
        rts
bp:     asl                 ;DOUBLE RESULT-REG INDEX
        tax                 ;TO X REG FOR INDEXING
        lda R0H,x          ;TEST FOR PLUS
        bpl br1            ;BRANCH IF SO
        rts
bm:     asl                 ;DOUBLE RESULT-REG INDEX
        tax
        lda R0H,x          ;TEST FOR MINUS
        bmi br1
        rts
bz:     asl                 ;DOUBLE RESULT-REG INDEX
        tax
        lda R0L,x          ;TEST FOR ZERO
        ora R0H,x          ;(BOTH BYTES)
        beq br1            ;BRANCH IF SO
        rts
bnz:    asl                 ;DOUBLE RESULT-REG INDEX
        tax
        lda R0L,x          ;TEST FOR NON-ZERO
        ora R0H,x          ;(BOTH BYTES)
        bne br1            ;BRANCH IF SO
        rts
bm1:    asl                 ;DOUBLE RESULT-REG INDEX
        tax
        lda  R0L,x          ;CHECK BOTH BYTES
        and  R0H,x          ;FOR $FF (MINUS 1)
        eor  $ff
        beq  br1            ;BRANCH IF SO
        rts
bnm1:   asl                 ;DOUBLE RESULT-REG INDEX
        tax
        lda  R0L,x
        and  R0H,x          ;CHECK BOTH BYTES FOR NO $FF
        eor  $ff
        bne  br1            ;BRANCH IF NOT MINUS 1
nul:    rts
rs:     ldx  R12            ;12*2 FOR R12 AS STACK POINTER
        jsr  dcr            ;DECR STACK POINTER
        lda  (R0L,x)        ;POP HIGH RETURN ADDRESS TO PC
        sta  R15H
        jsr  dcr            ;SAME FOR LOW ORDER BYTE
        lda  (R0L,x)
        sta  R15L
        rts
rtn:    jmp  rtnz
.assert .hibyte(rtn) = .hibyte(set), error, "sweet16 ops not on the same page"

.endproc

