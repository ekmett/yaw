.include "vera.inc"
.include "data.inc"
.include "kernal.inc"

.export main

.code
.proc main
  ; load resources
  ;puts "loading bank 1"
  ;rambank 1
  ;load "bank-1.bin",8,1
  ;puts "loading bank 2"
  ;rambank 2
  ;load "bank-2.bin",8,1
  ;rambank 1
  vera_init
  ; blit dc
  init_bitmap_layer 0, 6, 0, 0, 0
  init_bitmap_layer 1, 6, 0, (320*240>>1), 1
  vset VREG::PALETTE
  ; blit palette
  rts
.endproc

.mac foreach_with_len data,len
  .scope
    ldx #>len
    ldy #<len
    pokew RBLIT, data
  ; now put the body
loop:
    lda (RBLIT),y
.endmac

.mac foreach data,len
  .ifblank(len)
     foreach_with_len data,sizeof(data)
  .else
     foreach_with_len data,len
  .endif
.endmac


.mac endforeach
    iny
    bne blit_big
    inc RBLIT_H
    dex
    bne blit_big
    rts
  .endscope
.endmac

;.proc console
;  lda #$80
;  jsr kscreen_set_mode
;.endproc
