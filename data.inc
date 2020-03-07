.data

.proc palette
  .word $000, $111, $222, $333, $444, $555, $666, $777, $888, $999, $aaa, $bbb, $ccc, $ddd, $eee, $fff
  .word $000, $001, $002, $003, $004, $005, $006, $007, $008, $009, $00a, $00b, $00c, $00d, $00e, $00f
  .word $000, $100, $200, $300, $400, $500, $600, $700, $800, $900, $a00, $b00, $c00, $d00, $e00, $f00
  .word $0f0
.endproc

.proc dc
  border_color = 10
  .byte 64,64,border_color,0,128,0,224,40
.endproc
