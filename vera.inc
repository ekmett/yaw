.ifndef VERA_INCLUDED
.define VERA_INCLUDED 1

; VERA display routines
; (c) Edward Kmett 2020
; BSD 2-clause license

.include "zp.inc"
.include "mem.inc"

.enum VERA
  BASE = $9f20
  ADDR_L = BASE
  ADDR_M
  ADDR_H
  DATA0
  DATA1
  CTRL
  IEN
  ISR
.endenum

.enum VREG
  DC      = $f0000
  PALETTE = $f1000
  LAYER0  = $f2000
  LAYER1  = $f3000
.endenum

INC1  = $100000
INC4K = $d00000

.mac vset addr, last
  pokew VERA::ADDR_L, .loword(addr), last
  poke VERA::ADDR_H, .bankbyte(addr), .hibyte(addr)
.endmac

.mac vpoke addr, value, last
  vset addr, last
  poke VERA::DATA0, value, .hibyte(addr)
.endmac

.mac vpokes addr, r1,r2,r3,r4,r5,r6,r7
  vset addr
  vputs, r1,r2,r3,r4,r5,r6,r7
.endmac

.mac vin
	lda VERA::DATA0
.endmac

.mac vout
  sta VERA::DATA0
.endmac

.mac vput value, last
  poke VERA::DATA0, value, last
.endmac

.mac vputs_after last,r1,r2,r3,r4,r5,r6,r7
  .ifblank r1
    .exitmacro
  .endif
  vput r1,last
  vputs_after r1,r2,r3,r4,r5,r6,r7
.endmac

.mac vputs r1,r2,r3,r4,r5,r6,r7
  vputs_after 0,r1,r2,r3,r4,r5,r6,r7
.endmac

; display controller

.enum DC
  VIDEO = VREG::DC
  HSCALE
  VSCALE
  BORDER_COLOR
  HSTART_L
  HSTOP_L
  VSTART_L
  VSTOP_L
  STARTSTOP_H
  IRQ_LINE_L
  IRQ_LINE_H
.endenum

.mac vera_init
	stz VERA::CTRL
  vpoke (DC::VIDEO + INC1), 1
.endmac

; layer 0/1 registers

.enum Ln
  CTRL0
  CTRL1
  MAP_BASE_L
  MAP_BASE_H
  TILE_BASE_L
  TILE_BASE_H
  HSCROLL_L
  HSCROLL_H
  VSCROLL_L
  VSCROLL_H
  BM_PAL_OFFS = HSCROLL_H
.endenum

; layer = 0 or 1
; mode = 5,6,7
; tilew = 0 means 320 columns, tilew = 1 means 640 columns
; palette offset in 0..15
.mac init_bitmap_layer layer, mode, tilew, base, palette_offset
  .assert (mode >= 5) && (mode <= 7), error, "not a bitmap mode"
  .assert tilew < 2, error, "invalid tile width"
  .assert layer < 2, error, "invalid layer"
  .assert palette_offset < 16, error, "invalid bitmap palette offset"
  vset VREG::LAYER0 + layer<<12 + INC1
  vputs mode << 5 + 1, tilew << 4
  vputs 0, 0
  vputs <(base >> 2), <(base >> 10)
  vputs 0, palette_offset
.endmac

.mac init_tiled_layer layer, mode, 

globalreg "RBLIT"
.import blit_big

.mac blit_with_len data, len
  .assert .bankbyte(len) = 0, error, "too much"
  .if .hibyte(len) > 0
    ldx #>len
    ldy #<len
    pokew RBLIT, data
    jsr blit_big
  .else
    ldy #<(256-len)
  : lda .loword(data+len), y
    vout
    iny
    bmi :-
  .endif
.endmac

.mac blit data, len
  .ifblank len
     blit_with_len data, .sizeof(data)
  .else
     blit_with_len data, len
  .endif
.endmac

.import PORT_LZSA_SRC_LO, PORT_LZSA_SRC_HI
.import PORT_LZSA_DST_LO, PORT_LZSA_DST_HI

;.define is_imm(src) .match(.left(1,{src}),#)
;.define as_imm(src) .right(.tcount({src})-1,{src})
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

; blit data from an lzsa compressed location in memory to vera
.mac vlzsa data
  pokew PORT_LZSA_DST, VERA::DATA0
  movw PORT_LZSA_SRC, data
.endmac

.endif
