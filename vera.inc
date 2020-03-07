; VERA display routines
; (c) Edward Kmett 2020
; BSD 2-clause license

.include "zp.inc"

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

.mac poke addr, value
  .if .const(value) && (value = 0)
    stz addr
  .else
    lda #value
    sta addr
  .endif
.endmac

.mac vset addr
  poke VERA::ADDR_L, .lobyte(addr)
  poke VERA::ADDR_M, .hibyte(addr)
  poke VERA::ADDR_H, .bankbyte(addr)
.endmac

.mac vpoke addr, value
  vset addr
  poke VERA::DATA0, value
.endmac

.mac vin
	lda VERA::DATA0
.endmac

.mac vout
  sta VERA::DATA0
.endmac

.mac vput value
  poke VERA::DATA0, value
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
  vput mode << 5 + 1
  vput tilew << 4
  vput 0
  vput 0
  vput <(base >> 2)
  vput <(base >> 10)
  vput 0
  vput palette_offset
.endmac

globalreg "RBLIT"

.mac blit_with_len data, len
  .assert .bankbyte(len) = 0, error, "too much"
  .if .hibyte(len) > 0
    .import blit_big
    ; setup up slow blit
    ldx #>len
    ldy #<len
    lda #<data
    sta RBLIT_L
    lda #>data
    sta RBLIT_H
    jsr blit_big
  .else
    .local loop
    ldy #<(256-len)
    loop:
      lda .loword(data+len), y
      vout
      iny
      bmi loop
  .endif
.endmac

; send n bytes of data to VERA
.mac blit data, len
  .ifblank len
     blit_with_len data, .sizeof(data)
  .else
     blit_with_len data, len
  .endif
.endmac
