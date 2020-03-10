.ifndef SWEET16_INCLUDED
.define SWEET16_INCLUDED 1

.global sweet16

.mac sw16
  jsr sweet16
  .setcpu "sweet16"
.endmac

.mac endsw16
  rtn
  .setcpu "65c02"
.endmac

.endif
