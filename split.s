.include "vera.inc"
.include "kernal.inc"

brk_handler = $0316
irq_handler = $0314

.export main

.code
.proc main
  sei
  movw default_irq_handler, irq_handler
  vset DC::IRQ_LINE_L|INC1
  vput 240 ; at scanline 240
  vput 0
  lda VERA::IEN
  ora #$03 ; enable line and vsync interrupts
  sta VERA::IEN
  ; set up line irq
  pokew irq_handler, my_irq_handler
  cli
  ; do stuff here?
  rts
;:
;  jmp :-
.endproc

.bss
default_irq_handler: .res 2
saved_addr0: .res 2
saved_bank0: .res 1
saved_addr1: .res 2
saved_bank1: .res 1
saved_ctrl: .res 1
.code

; save data port 0 location, and current ADDR_SEL, and set ADDR_SEL to 0
; if both = 1 then we'll save both port 0 and port 1's addresses

; this is stubbed out in the kernel, so i'll do it myself:w
.mac vsave both
  mov saved_ctrl,VERA::CTRL
  .if .not(.blank(both)) && both = 1
    poke VERA::CTRL,1
    movw saved_addr1,VERA::ADDR
    mov saved_bank1,VERA::ADDR_H
  .endif
  poke VERA::CTRL,0
  movw saved_addr0,VERA::ADDR
  mov saved_bank0,VERA::ADDR_H
.endmac

; restore data port locations, so they aren't corrupted by the irq handler
; this is stubbed out in the kernel, so i'll do it myself
.mac vrestore both
  .if .not(.blank(both)) && both = 1
    poke VERA::CTRL,1
    movw VERA::ADDR,saved_addr1
    mov VERA::ADDR_H,saved_bank1
  .endif
  poke VERA::CTRL,0
  movw VERA::ADDR,saved_addr0
  mov VERA::ADDR_H,saved_bank0
  mov VERA::CTRL,saved_ctrl
.endmac

.proc my_irq_handler
  lda VERA::ISR
  and #$03 ; either vblank or line
  bne :+
  jmp (default_irq_handler) ; no? bail
: tay
  vsave 1; could save tay/tya by making this trash x not a
  tya
  and #$01
  beq on_line
  ; vsync irq
  vpoke DC::HSCALE,128 ; e.g. restore width
  poke VERA::ISR,$01 ; clear vsync irq
  bra done
  ; end vsync handler
on_line:
  vpoke DC::HSCALE,64 ; 320 columns below the line
  poke VERA::ISR,$02 ; clear line irq
  ; end line handler, fall through
done:
  vrestore 1
  jmp (default_irq_handler)
  ;ply
  ;plx
  ;pla
  ;rti
.endproc
