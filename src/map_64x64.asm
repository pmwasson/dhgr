;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; Example 64x64 map
;-----------------------------------------------------------------------------

.align 256

map_64x64:


;row 00
.word $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.word $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.word $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.word $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;row 01
.word $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.word $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.word $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.word $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;row 02
.word $00,$00,$05,$04,$04,$00,$00,$00,$06,$06,$06,$06,$06,$06,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$04,$00,$00,$00,$15,$00,$00,$00,$04,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00
;row 03
.word $00,$00,$05,$02,$05,$00,$2C,$00,$06,$06,$06,$06,$06,$06,$05,$01
.word $03,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$05
.word $01,$01,$01,$01,$01,$01,$01,$04,$00,$00,$00,$00,$04,$01,$01,$01
.word $01,$01,$05,$01,$01,$01,$01,$01,$01,$08,$01,$01,$01,$01,$00,$00
;row 04
.word $00,$00,$02,$04,$02,$04,$00,$2C,$06,$06,$06,$06,$06,$06,$01,$01
.word $01,$01,$01,$01,$01,$01,$03,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$04,$01,$01,$00,$00,$00,$00,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$08,$01,$00,$00
;row 05
.word $00,$00,$05,$04,$09,$04,$00,$00,$06,$06,$06,$06,$06,$06,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$05,$01,$01,$01
.word $04,$01,$04,$01,$01,$01,$01,$01,$15,$00,$00,$04,$01,$01,$04,$01
.word $01,$04,$04,$05,$01,$01,$01,$01,$01,$01,$08,$01,$01,$01,$00,$00
;row 06
.word $00,$00,$04,$04,$09,$04,$04,$00,$06,$06,$06,$06,$06,$06,$05,$01
.word $05,$01,$01,$01,$01,$01,$01,$01,$03,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$04,$00,$00,$00,$04,$01,$01,$01,$01
.word $01,$04,$04,$01,$01,$08,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00
;row 07
.word $00,$00,$04,$04,$09,$09,$04,$00,$15,$12,$12,$12,$12,$12,$02,$01
.word $01,$01,$01,$01,$03,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$12,$01,$04,$01,$01
.word $01,$01,$04,$01,$01,$01,$01,$01,$05,$01,$01,$01,$01,$01,$00,$00
;row 08
.word $00,$00,$04,$01,$04,$09,$01,$2C,$00,$00,$00,$2C,$00,$00,$05,$05
.word $01,$01,$01,$01,$01,$01,$01,$01,$03,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$04,$01,$01,$01,$01,$01,$00,$00,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00
;row 09
.word $00,$00,$01,$04,$01,$09,$08,$03,$01,$01,$01,$01,$01,$15,$00,$00
.word $12,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$03,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$15,$00,$04,$01,$01,$01
.word $01,$01,$01,$01,$05,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00
;row 0A
.word $00,$00,$05,$01,$04,$09,$09,$01,$02,$01,$02,$01,$01,$01,$01,$15
.word $00,$12,$01,$01,$12,$12,$12,$01,$01,$01,$01,$03,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$12,$12,$12,$00,$00,$01,$01,$01,$01
.word $01,$08,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$08,$01,$00,$00
;row 0B
.word $00,$00,$09,$03,$01,$03,$09,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $15,$00,$12,$12,$00,$00,$00,$12,$01,$01,$01,$01,$01,$01,$01,$01
.word $05,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00
;row 0C
.word $00,$00,$01,$09,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$00,$00,$00,$00,$00,$00,$12,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$05,$01,$01,$00,$00,$00,$00,$00,$04,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00
;row 0D
.word $00,$00,$01,$01,$03,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$15,$00,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$08,$01,$01,$00,$00
;row 0E
.word $00,$00,$04,$01,$09,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$08,$01,$01,$01,$01,$00,$00,$00,$00,$00,$04,$01,$01,$08
.word $01,$01,$01,$01,$01,$01,$01,$01,$05,$01,$01,$01,$01,$01,$00,$00
;row 0F
.word $00,$00,$04,$05,$09,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $12,$01,$01,$00,$00,$01,$01,$00,$00,$01,$01,$01,$01,$01,$01,$02
.word $08,$01,$01,$01,$08,$01,$01,$01,$01,$04,$00,$00,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$05,$01,$01,$00,$00
;row 10
.word $00,$00,$05,$04,$05,$01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$12
.word $00,$12,$01,$01,$01,$01,$01,$01,$00,$01,$01,$03,$01,$01,$01,$01
.word $01,$01,$08,$01,$01,$01,$01,$01,$01,$01,$00,$00,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00
;row 11
.word $00,$00,$05,$04,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00
.word $00,$00,$12,$01,$01,$01,$01,$12,$00,$00,$01,$01,$01,$01,$03,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$00,$15,$01,$01,$04,$01
.word $01,$01,$01,$01,$01,$01,$05,$01,$01,$01,$01,$01,$01,$01,$00,$00
;row 12
.word $00,$00,$05,$04,$04,$04,$01,$01,$05,$01,$01,$01,$01,$01,$01,$09
.word $00,$00,$00,$01,$01,$01,$12,$00,$00,$00,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00
;row 13
.word $00,$00,$04,$05,$04,$05,$01,$01,$01,$01,$01,$04,$01,$01,$01,$01
.word $00,$00,$00,$12,$12,$12,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01
.word $05,$01,$01,$02,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$0C
.word $01,$01,$01,$01,$05,$01,$01,$01,$01,$01,$05,$01,$01,$01,$00,$00
;row 14
.word $00,$00,$05,$04,$04,$05,$09,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $09,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$03,$01,$01
.word $01,$01,$01,$05,$01,$01,$01,$01,$01,$19,$19,$19,$19,$19,$02,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00
;row 15
.word $00,$00,$04,$04,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$12,$12,$12,$01,$01,$01,$01,$01,$00,$00
;row 16
.word $00,$00,$04,$04,$01,$01,$01,$01,$04,$01,$01,$01,$01,$05,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$03,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$02,$01,$01,$01,$12,$00,$01,$02,$01,$01
.word $01,$01,$01,$12,$12,$12,$00,$00,$00,$12,$12,$12,$01,$01,$00,$00
;row 17
.word $00,$00,$04,$05,$01,$01,$09,$01,$01,$01,$01,$01,$01,$05,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$03,$01,$05,$01,$01
.word $01,$01,$01,$01,$01,$01,$04,$01,$01,$01,$00,$00,$01,$01,$08,$12
.word $12,$12,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$12,$12,$00,$00
;row 18
.word $00,$00,$04,$01,$01,$01,$01,$01,$03,$01,$04,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$03,$03,$01,$01,$01,$01,$01,$01,$01,$01,$03
.word $01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$00,$00,$12,$12,$12,$00
.word $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;row 19
.word $00,$00,$09,$09,$01,$09,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$05,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00
.word $00,$00,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$18,$00,$00
;row 1A
.word $00,$00,$05,$09,$09,$01,$01,$01,$01,$01,$01,$03,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$03,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$18,$18,$18,$18
.word $18,$18,$18,$18,$1D,$1F,$1C,$1E,$1D,$1C,$1F,$1C,$1E,$18,$00,$00
;row 1B
.word $00,$00,$01,$01,$09,$01,$04,$01,$05,$01,$02,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$05,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$18,$1D,$1E,$18
.word $1D,$1C,$1E,$18,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$18,$00,$00
;row 1C
.word $00,$00,$01,$01,$09,$01,$01,$01,$02,$01,$01,$01,$01,$03,$01,$01
.word $01,$01,$01,$01,$01,$03,$01,$01,$01,$09,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$18,$1C,$1C,$18
.word $1C,$1C,$1C,$18,$1D,$1C,$1E,$1C,$1C,$1D,$1C,$1C,$1E,$18,$00,$00
;row 1D
.word $00,$00,$0D,$01,$01,$01,$01,$01,$02,$02,$03,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$03,$01,$01,$0C,$01
.word $05,$01,$01,$01,$01,$01,$01,$12,$12,$12,$12,$00,$18,$1C,$1C,$18
.word $1C,$1F,$1C,$18,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$18,$00,$00
;row 1E
.word $00,$00,$08,$0D,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$12,$00,$00,$00,$00,$00,$18,$1C,$1F,$18
.word $1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$18,$00,$00
;row 1F
.word $00,$00,$0D,$0D,$01,$01,$01,$01,$01,$01,$01,$02,$01,$01,$05,$01
.word $01,$05,$01,$01,$01,$04,$03,$01,$01,$09,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$12,$00,$00,$01,$01,$01,$01,$18,$1C,$1C,$18
.word $18,$18,$18,$18,$18,$18,$18,$18,$1A,$18,$18,$18,$18,$18,$00,$00
;row 20
.word $00,$00,$0D,$0D,$0D,$01,$01,$08,$11,$01,$01,$01,$01,$01,$02,$01
.word $01,$01,$01,$01,$09,$01,$01,$01,$01,$01,$01,$01,$01,$01,$03,$01
.word $01,$01,$05,$01,$12,$00,$00,$01,$01,$01,$01,$01,$18,$1C,$1C,$18
.word $18,$18,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$18,$18,$18,$00,$00
;row 21
.word $00,$00,$11,$0E,$0F,$0D,$10,$11,$11,$11,$08,$01,$01,$01,$02,$01
.word $05,$05,$05,$04,$01,$01,$01,$01,$01,$01,$01,$09,$01,$01,$01,$01
.word $01,$01,$01,$01,$00,$00,$01,$01,$01,$01,$01,$01,$18,$1D,$1C,$1A
.word $1C,$1A,$1C,$1C,$1C,$00,$00,$00,$00,$00,$1C,$18,$03,$05,$00,$00
;row 22
.word $00,$00,$11,$10,$11,$0E,$0E,$11,$11,$11,$11,$11,$11,$11,$11,$11
.word $06,$06,$06,$04,$01,$05,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$01,$18,$1C,$1C,$18
.word $18,$18,$1C,$1C,$1C,$00,$15,$00,$15,$00,$1C,$18,$01,$01,$00,$00
;row 23
.word $00,$00,$11,$11,$11,$11,$11,$11,$11,$11,$11,$06,$06,$06,$06,$06
.word $06,$06,$06,$05,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$05,$01
.word $01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$01,$18,$1F,$1C,$18
.word $18,$18,$1C,$1C,$1C,$00,$00,$00,$00,$00,$1C,$18,$01,$01,$00,$00
;row 24
.word $00,$00,$11,$11,$11,$11,$11,$0E,$0E,$11,$06,$06,$06,$06,$06,$06
.word $11,$04,$04,$04,$01,$01,$01,$01,$09,$05,$01,$01,$01,$01,$01,$01
.word $01,$01,$04,$01,$00,$12,$01,$01,$01,$01,$01,$01,$18,$1C,$1C,$18
.word $05,$18,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$18,$01,$01,$00,$00
;row 25
.word $00,$00,$11,$11,$0D,$0F,$0E,$0E,$0F,$11,$06,$06,$06,$06,$06,$11
.word $11,$01,$01,$01,$01,$01,$01,$01,$04,$05,$05,$07,$07,$07,$07,$01
.word $01,$01,$01,$01,$00,$00,$01,$01,$01,$01,$01,$01,$18,$1D,$1C,$18
.word $01,$18,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$1C,$18,$01,$05,$00,$00
;row 26
.word $00,$00,$11,$11,$11,$11,$0E,$0E,$0D,$11,$11,$06,$06,$06,$11,$11
.word $01,$01,$01,$01,$01,$09,$01,$01,$09,$04,$07,$07,$07,$07,$07,$07
.word $07,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$18,$18,$18,$18
.word $03,$18,$18,$18,$18,$1C,$18,$18,$18,$18,$18,$18,$01,$01,$00,$00
;row 27
.word $00,$00,$11,$0E,$0F,$11,$0F,$0E,$11,$11,$11,$11,$11,$11,$11,$01
.word $01,$01,$01,$01,$01,$04,$09,$09,$01,$07,$07,$07,$12,$12,$07,$07
.word $07,$01,$01,$01,$12,$00,$01,$01,$01,$01,$01,$01,$01,$01,$03,$03
.word $01,$03,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$05,$00,$00
;row 28
.word $00,$00,$11,$0E,$11,$11,$0E,$0E,$0D,$11,$11,$11,$11,$05,$01,$01
.word $01,$01,$01,$05,$09,$09,$05,$01,$01,$01,$07,$07,$00,$00,$07,$07
.word $07,$01,$01,$01,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$05,$05,$00,$00
;row 29
.word $00,$00,$11,$0E,$0E,$0E,$0E,$0E,$0D,$0D,$11,$11,$05,$05,$05,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$07,$07,$00,$00,$07,$07
.word $01,$01,$01,$01,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$05,$05,$0B,$00,$00
;row 2A
.word $00,$00,$11,$11,$0E,$0E,$0E,$0E,$0E,$0E,$11,$11,$11,$01,$08,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$04,$01,$01,$07,$01,$02,$07,$01
.word $01,$01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$09,$09,$01,$05,$05,$0A,$0B,$00,$00
;row 2B
.word $00,$00,$0D,$11,$11,$0F,$0E,$0E,$0F,$0E,$0E,$0D,$0D,$0D,$0A,$0A
.word $0A,$01,$01,$08,$01,$01,$01,$01,$01,$01,$01,$08,$01,$05,$04,$01
.word $01,$01,$01,$01,$12,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$0A,$09,$01,$01,$0B,$0B,$0B,$00,$00
;row 2C
.word $00,$00,$0D,$11,$11,$11,$0E,$0F,$0D,$11,$11,$11,$01,$0A,$0A,$0A
.word $0A,$09,$09,$01,$01,$01,$01,$01,$01,$05,$01,$05,$01,$04,$08,$01
.word $01,$01,$01,$12,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$09,$0A,$01,$01,$01,$01,$0A,$0B,$00,$00
;row 2D
.word $00,$00,$0D,$0D,$11,$11,$11,$11,$11,$11,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$09,$09,$01,$01,$01,$01,$01,$01,$05,$01,$01,$01,$01
.word $01,$01,$01,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$0A,$09,$01,$01,$01,$01,$0B,$00,$00
;row 2E
.word $00,$00,$0D,$0D,$0D,$11,$11,$11,$11,$01,$01,$08,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$09,$01,$01,$01,$01,$04,$05,$05,$04,$05,$08
.word $01,$01,$01,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$0A,$0A,$01,$05,$01,$01,$08,$00,$00
;row 2F
.word $00,$00,$0D,$0D,$01,$01,$01,$01,$01,$01,$08,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$08,$01,$01,$09,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$09,$0A,$01,$01,$01,$01,$01,$00,$00
;row 30
.word $00,$00,$0D,$0D,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$12,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$0A,$09,$01,$01,$05,$01,$00,$00
;row 31
.word $00,$00,$0D,$01,$01,$01,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$09,$0A,$09,$01,$01,$01,$00,$00
;row 32
.word $00,$00,$0D,$01,$01,$04,$01,$05,$04,$04,$09,$09,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$00,$00,$12,$12,$12,$12,$12,$12,$12,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$0A,$0A,$01,$01,$01,$00,$00
;row 33
.word $00,$00,$0D,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$05,$01,$01,$04
.word $01,$05,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$09,$01,$01,$01
.word $01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$09,$0A,$09,$01,$05,$00,$00
;row 34
.word $00,$00,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B
.word $0B,$0B,$0A,$01,$01,$01,$01,$01,$01,$09,$09,$09,$01,$01,$01,$01
.word $01,$01,$01,$01,$00,$00,$00,$01,$01,$00,$00,$15,$01,$01,$00,$00
.word $00,$01,$01,$01,$01,$01,$01,$01,$01,$09,$0A,$0A,$09,$01,$00,$00
;row 35
.word $00,$00,$0B,$0B,$08,$08,$01,$04,$01,$05,$0B,$0B,$0B,$0B,$0B,$0B
.word $0B,$0B,$0B,$0B,$0B,$0A,$01,$09,$09,$0A,$0A,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$15,$01,$01,$01,$01,$01,$01
.word $00,$00,$00,$01,$01,$01,$01,$01,$01,$09,$0A,$0A,$0A,$09,$00,$00
;row 36
.word $00,$00,$0B,$0B,$08,$01,$01,$01,$01,$01,$01,$09,$01,$01,$01,$01
.word $0A,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0A,$09,$09,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$00,$00,$01,$01,$01,$01,$01,$09,$09,$0A,$0A,$01,$00,$00
;row 37
.word $00,$00,$08,$08,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$0A,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B
.word $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0A,$09,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$00,$00,$00,$00,$01,$01,$01,$09,$0A,$0A,$09,$00,$00
;row 38
.word $00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$0A,$0B,$0B,$0B,$0B,$0B,$0B,$0B
.word $0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0A,$09,$01,$01,$01
.word $01,$01,$01,$01,$01,$00,$00,$01,$0C,$01,$0A,$0A,$09,$01,$00,$00
;row 39
.word $00,$00,$01,$01,$01,$29,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$09,$09,$0A,$0A,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B,$0B
.word $0B,$0B,$0A,$19,$19,$19,$19,$19,$19,$0A,$0A,$09,$09,$09,$00,$00
;row 3A
.word $00,$00,$01,$01,$01,$01,$01,$27,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$09,$0A,$0B,$0B,$0B,$0B,$0B,$0B
.word $0B,$0B,$0A,$19,$19,$19,$19,$19,$19,$0A,$0A,$0A,$09,$08,$00,$00
;row 3B
.word $15,$00,$01,$01,$27,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$00,$00,$01,$01,$09,$0A,$0A,$09,$01,$00,$00
;row 3C
.word $00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$12,$12,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.word $01,$01,$01,$01,$12,$00,$00,$12,$01,$09,$09,$09,$09,$09,$00,$00
;row 3D
.word $00,$00,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12
.word $12,$12,$00,$00,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12
.word $12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12,$12
.word $12,$12,$12,$12,$00,$00,$00,$00,$12,$12,$09,$09,$09,$05,$00,$00
;row 3E
.word $00,$15,$00,$00,$00,$00,$00,$15,$00,$00,$00,$00,$00,$00,$00,$00
.word $00,$00,$00,$00,$00,$15,$00,$00,$00,$00,$15,$00,$00,$00,$00,$00
.word $00,$00,$00,$00,$00,$00,$15,$00,$00,$00,$00,$00,$00,$00,$15,$00
.word $00,$00,$00,$00,$00,$00,$15,$00,$00,$00,$12,$12,$12,$12,$00,$00
;row 3F
.word $00,$00,$00,$15,$00,$00,$00,$00,$00,$00,$00,$00,$15,$00,$00,$00
.word $00,$15,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$15,$00
.word $00,$00,$00,$00,$15,$00,$00,$00,$00,$00,$15,$00,$00,$00,$00,$00
.word $00,$00,$00,$15,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00