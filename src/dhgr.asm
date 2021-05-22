;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; DHGR BW Demo
;

;------------------------------------------------
; Constants
;------------------------------------------------

.include "defines.asm"
.include "macros.asm"

;------------------------------------------------
; Zero page usage
;------------------------------------------------

; Safe zero page locations from Inside the Apple IIe:
;
;                         $06 $07 
; $08 $09
;     $19 $1A $1B $1C $1D $1E
;                         $CE $CF
;                             $D7
;             $E3
; $E8 
;                 $EC $ED $EE $EF
;         $FA $FB $FC $FD $FE $FF 
;
; Reserve $FE/$FF for inline print

tilePtr0        :=  $06     ; Tile pointer
tilePtr1        :=  $07
screenPtr0      :=  $08     ; Screen pointer
screenPtr1      :=  $09

color           :=  $EB


; Colors
;---------------------
; 0 - black
; 1 - dark blue
; 2 - dark green
; 3 - medium blue
; 4 - brown
; 5 - gray (1)
; 6 - light green
; 7 - aqua
; 8 - red
; 9 - purple
; A - gray (2)
; B - light blue
; C - orange
; D - pink
; E - yellow
; F - white

.segment "CODE"
.org    $4000

.proc main

    jsr     $c300       ; 80 column mode
    jsr     HOME        ; clear screen
    lda     #23         ; put cursor on last line
    sta     CV
    jsr     VTAB

    ; display a greeting
    jsr     inline_print
    .byte   "DHGR tile edit - ? for help",13,0

    jsr     dhgr_init

    ; clear screens
    lda     #$20    ; page1
    sta     screenPage
    lda     #$55    ; black
    sta     color
    jsr     clearScreen
    jsr     draw_color_key

command_loop:
    jsr     inline_print
    .byte   "Command:",0

skip_prompt:
    jsr     getInput    ; Wait for a keypress

    ; Parse command

    ;------------------
    ; RIGHT (arrow)
    ;------------------
    cmp     #KEY_RIGHT
    bne     :+
    jsr     inline_print
    .byte   "Right ",0
    inc     curX
    lda     width
    cmp     curX
    bne     right_good
    lda     #0
    sta     curX
right_good:
    jmp     finish_move
:

    ;------------------
    ; LEFT (arrow)
    ;------------------
    cmp     #KEY_LEFT
    bne     :+
    jsr     inline_print
    .byte   "Left  ",0
    dec     curX
    lda     curX
    bpl     left_good
    lda     width_m1
    sta     curX
left_good:
    jmp     finish_move
:

    ;------------------
    ; UP (arrow)
    ;------------------
    cmp     #KEY_UP
    bne     :+
    jsr     inline_print
    .byte   "Up    ",0
    dec     curY
    lda     curY
    bpl     up_good
    lda     height_m1
    sta     curY
up_good:
    jmp     finish_move
:
    ;------------------
    ; DOWN (arrow)
    ;------------------
    cmp     #KEY_DOWN
    bne     :+
    jsr     inline_print
    .byte   "Down  ",0
    inc     curY
    lda     height
    cmp     curY
    bne     down_good
    lda     #0
    sta     curY
down_good:
    jmp     finish_move
:

    ;------------------
    ; Q = QUIT
    ;------------------
    cmp     #$80 | 'Q'
    bne     :+
    jsr     inline_print
    .byte   "Quit",13,0
    bit     TXTSET
    jmp     MONZ        ; enter monitor
:

    ;------------------
    ; 2 = page 2
    ;------------------
    cmp     #$80 | '2'
    bne     :+
    jsr     inline_print
    .byte   "Page 2",13,0
    sta     CLR80COL    ; Use RAMWRT for aux mem
    sta     HISCR
    sta     MIXCLR
screenPause:
    lda     KBD
    bpl     screenPause
    sta     KBDSTRB
    sta     LOWSCR
    sta     MIXSET
    jmp     command_loop
:

    ;------------------
    ; ESC = Toggle Text
    ;------------------
    cmp     #KEY_ESC
    bne     :+
    ; dont display anything
    lda     TEXTMODE
    bmi     toggle_text_off
    bit     TXTSET    
    jmp     skip_prompt
toggle_text_off:
    bit     TXTCLR    
    jmp     skip_prompt
:

    ;------------------
    ; ? = HELP
    ;------------------
    cmp     #$80 + '?'
    bne     :+
    jsr     inline_print
    .byte   "Help (ESC when done)",13,0
    jsr     printHelp
    jmp     command_loop

:

    ;------------------
    ; Unknown
    ;------------------
    jsr     inline_print
    .byte   "Unknown command (? for help)",13,0
    jmp     command_loop

; jump to after changing coordinates
finish_move:
    jsr     inline_print
    .byte   "X/Y:",0
    lda     curX
    jsr     PRBYTE
    lda     #$80 + ','
    jsr     COUT
    lda     curY
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    jmp     command_loop

.endproc

;-----------------------------------------------------------------------------
; printHelp
;-----------------------------------------------------------------------------
.proc printHelp
    bit     TXTSET
    jsr     inline_print
    .byte   " ?:   HELP",13
    .byte   " Q:   Quit",13  
    .byte   " Escape: Toggle text/graphics",13
    .byte   0
    rts
.endproc

;-----------------------------------------------------------------------------
; DHGR clear screen
;
;   screenPage - set to either 2000 or 4000
;   color - set colors in upper and lower nibble
;-----------------------------------------------------------------------------

.proc clearScreen
    lda     #$00
    sta     screenPtr0
    lda     screenPage
    sta     screenPtr1

    clc
    adc     #$20
    sta     screenEnd

loop:
    ldy     #0
yloop:

    lda     color
    sta     RAMWRTON  
    sta     (screenPtr0),y
    sta     RAMWRTOFF
    rol     ; put bit 7 into carry
    rol     color

    lda     color
    sta     (screenPtr0),y
    rol     ; put bit 7 into carry
    rol     color

    iny

    lda     color
    sta     RAMWRTON  
    sta     (screenPtr0),y
    sta     RAMWRTOFF
    rol     ; put bit 7 into carry
    rol     color

    lda     color
    sta     (screenPtr0),y
    rol     ; put bit 7 into carry
    rol     color

    iny

    bne     yloop

    inc     screenPtr1
    lda     screenEnd
    cmp     screenPtr1
    bne     loop
    rts

.endproc

;-----------------------------------------------------------------------------
; Init double hi-res
;-----------------------------------------------------------------------------

.proc dhgr_init

    sta     TXTCLR      ; Graphics
    sta     HIRES       ; Hi-res
    sta     MIXSET      ; Mixed
    sta     LOWSCR      ; Display page 1
    sta     DHIRESON    ; Annunciator 2 On
    sta     SET80VID    ; 80 column on
    rts

.endproc

;-----------------------------------------------------------------------------
; Draw color key
;   Note: uses drawPixel so had to deal with offset
;-----------------------------------------------------------------------------

.proc draw_color_key

    ; set up coordinates
    lda     #0
    sta     curX
    lda     #17
    sta     curY

    ; loop through colors
:
    ldx     curX
    lda     colorTable,x
    sta     color
    jsr     drawPixel
    inc     curX
    lda     curX
    cmp     #16
    bne     :-

    lda     #0
    sta     curX
    sta     curY

    rts
.endproc

;-----------------------------------------------------------------------------
; setScreenPtr
;   Based on X (vertical) and A (horizontal)
;-----------------------------------------------------------------------------
.proc setScreenPtr
    ; calculate screen pointer
    clc
    adc     lineOffset,x    ; lineOffset(curY)
    sta     screenPtr0    
    lda     linePage,x
    sta     screenPtr1

    rts
.endproc

;-----------------------------------------------------------------------------
; DrawPixel
;   A = color (byte copied for each line)
;   Based on curX, curY
;-----------------------------------------------------------------------------
.proc drawPixel

    sta     CLR80COL        ; Use RAMWRT for aux mem

    ; set screen pointer
    ldx     curY            ; Putting Y in X is confusing, sorry
    inx                     ; offset by 1
    lda     curX
    clc
    adc     #1              ; offset by 1
    asl                     ; x*2
    adc     lineOffset,x    ; lineOffset(curY)
    sta     screenPtr0    
    lda     linePage,x
    sta     screenPtr1

    ldx     #8

pixel_loop:
    ldy     #0

    ; Byte 0: AUX memory
    lda     color
    sta     RAMWRTON  
    sta     (screenPtr0),y
    sta     RAMWRTOFF
    rol     ; put bit 7 into carry
    rol     color

    ; Byte 1: Main memory
    lda     color
    sta     (screenPtr0),y
    rol     ; put bit 7 into carry
    rol     color

    ; Byte 2: AUX memory
    iny
    lda     color
    sta     RAMWRTON  
    sta     (screenPtr0),y
    sta     RAMWRTOFF
    rol     ; put bit 7 into carry
    rol     color

    ; Byte 3: Main memory
    lda     color
    sta     (screenPtr0),y
    rol     ; put bit 7 into carry
    rol     color

    clc
    lda     screenPtr1
    adc     #$04
    sta     screenPtr1
    dex
    bne     pixel_loop
    rts

.endproc

;-----------------------------------------------------------------------------
; getInput
;   Blink cursors and wait for keypress
;   Return key in A (upper bit set)
;-----------------------------------------------------------------------------
.proc getInput

cursor_loop:
    ; Strobe keyboard
    bit     KBDSTRB 

    ; Display cursor
    lda     #$FF
    jsr     COUT

    lda     #$ff        ; FIXME cursor color
    sta     color
    jsr     drawPixel

    ; Wait (on)
    jsr     wait

    ; Restore
    lda     #$88        ; backspace
    jsr     COUT
    lda     #$A0        ; space
    jsr     COUT
    lda     #$88        ; backspace
    jsr     COUT

    lda     #$00        ; FIXME tile color
    sta     color
    jsr     drawPixel

    ; check for keypress
    lda     KBD 
    bmi     exit

    ; Strobe keyboard
    bit     KBDSTRB 

    ; Wait (off)
    jsr     wait

    ; check for keypress
    lda     KBD 
    bpl     cursor_loop

exit:
    bit     KBDSTRB     ; clean up

    rts

; Wait loop that can be interrupted by key-press
wait:
    ldx     #$80
wait_x:
    ldy     #0
wait_y:
    lda     KBD
    bmi     waitExit
    dey
    bne     wait_y
    dex
    bne     wait_x
waitExit:
    rts

.endproc


;-----------------------------------------------------------------------------
; drawTile
;  Assume 14x16, where 14 is 14*4 pixels = 56 -> 8 bytes
;    8*16 = 128, so 2 tiles per page
;  tileIndex - tile to draw
;  tileX     - byte offset of tile, should be /4
;  tileY     - 8-line offset of tile, should be /2
;-----------------------------------------------------------------------------
.proc drawTile_14x16

    sta     CLR80COL        ; Use RAMWRT for aux mem

    ; calculate tile pointer
    lda     tileIndex
    lsr                     ; *128
    lda     #0
    ror
    sta     tilePtr0
    lda     tileIndex
    lsr                     ; /2
    clc
    adc     #>tileSheet_14x16
    sta     tilePtr1

    ; calculate screen pointer
    ldx     tileY
    lda     tileX
    clc
    adc     lineOffset,x    ; + lineOffset
    sta     screenPtr0    
    lda     linePage,x
    sta     screenPtr1

    clc     ; no carry generated inside of loop
    ldx     #8
drawLoop1:
    ; Bytes 0-3 in AUX memory
    ;
    sta     RAMWRTON   
    ldy     #0
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #2
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #3
    lda     (tilePtr0),y
    sta     (screenPtr0),y

    ; Bytes 4-7 in MAIN memory
    ;
    sta     RAMWRTOFF

    lda     tilePtr0    ; offset tile pointer by 4
    adc     #4
    sta     tilePtr0

    ldy     #0
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #2
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #3
    lda     (tilePtr0),y
    sta     (screenPtr0),y

    ; assumes aligned such that there are no page crossing
    lda     tilePtr0
    adc     #4
    sta     tilePtr0

    lda     screenPtr1
    adc     #4
    sta     screenPtr1

    dex
    bne     drawLoop1

    ; move to second half
    lda     screenPtr0
    clc
    adc     #$80
    sta     screenPtr0
    lda     screenPtr1
    sbc     #$1f        ; subtract 20 if no carry, 19 if carry
    sta     screenPtr1

    clc     ; Carry not set in loop, so clear outside of loop
    ldx     #8
drawLoop2:
    ; Bytes 0-3 in AUX memory
    ;
    sta     RAMWRTON   
    ldy     #0
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #2
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #3
    lda     (tilePtr0),y
    sta     (screenPtr0),y

    ; Bytes 4-7 in MAIN memory
    ;
    sta     RAMWRTOFF

    lda     tilePtr0    ; offset tile pointer by 4
    adc     #4
    sta     tilePtr0

    ldy     #0
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #2
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #3
    lda     (tilePtr0),y
    sta     (screenPtr0),y

    ; assumes aligned such that there are no page crossing
    lda     tilePtr0
    adc     #4
    sta     tilePtr0

    lda     screenPtr1
    adc     #4
    sta     screenPtr1

    dex
    bne     drawLoop2

    rts    

; locals
temp0:  .byte   0

.endproc

;-----------------------------------------------------------------------------
; drawTile
;  Assume 7x8, where 7 is 7*4 pixels = 28 -> 4 bytes
;    4*8 = 32, so 8 tiles per page
;  tileIndex - tile to draw
;  tileX     - byte offset of tile, should be /2
;  tileY     - 8-line offset of tile
;-----------------------------------------------------------------------------
.proc drawTile_7x8

    sta     CLR80COL        ; Use RAMWRT for aux mem

    ; calculate tile pointer
    lda     tileIndex
    asl                     ; *32
    asl
    asl
    asl
    asl
    sta     tilePtr0
    lda     tileIndex
    lsr                     ; /8
    lsr
    lsr
    clc
    adc     #>tileSheet_7x8
    sta     tilePtr1

    ; calculate screen pointer
    ldx     tileY
    lda     tileX
    clc
    adc     lineOffset,x    ; + lineOffset
    sta     screenPtr0    
    lda     linePage,x
    sta     screenPtr1

    clc     ; no carry generated inside of loop
    ldx     #8
drawLoop1:
    ; Bytes 0-1 in AUX memory
    ;
    sta     RAMWRTON   
    ldy     #0
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (tilePtr0),y
    sta     (screenPtr0),y

    ; Bytes 2-3 in MAIN memory
    ;
    sta     RAMWRTOFF

    lda     tilePtr0    ; offset tile pointer by 2
    adc     #2
    sta     tilePtr0

    ldy     #0
    lda     (tilePtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (tilePtr0),y
    sta     (screenPtr0),y

    ; assumes aligned such that there are no page crossing
    lda     tilePtr0
    adc     #2
    sta     tilePtr0

    lda     screenPtr1
    adc     #4
    sta     screenPtr1

    dex
    bne     drawLoop1

    rts    

; locals
temp0:  .byte   0

.endproc

;-----------------------------------------------------------------------------
; Utilies

.include "inline_print.asm"

; Global Variables
;-----------------------------------------------------------------------------

screenPage: .byte   $20
screenEnd:  .byte   $40
tileX:      .byte   0
tileY:      .byte   0
tileIndex:  .byte   0
curX:       .byte   0
curY:       .byte   0

; Make dimensions a variable incase we want variable tile size
width:      .byte   14
width_m1:   .byte   13
height:     .byte   16
height_m1:  .byte   15

; Lookup tables
;-----------------------------------------------------------------------------

.align      64

lineOffset:
    .byte   <$2000
    .byte   <$2080
    .byte   <$2100
    .byte   <$2180
    .byte   <$2200
    .byte   <$2280
    .byte   <$2300
    .byte   <$2380
    .byte   <$2028
    .byte   <$20A8
    .byte   <$2128
    .byte   <$21A8
    .byte   <$2228
    .byte   <$22A8
    .byte   <$2328
    .byte   <$23A8
    .byte   <$2050
    .byte   <$20D0
    .byte   <$2150
    .byte   <$21D0
    .byte   <$2250
    .byte   <$22D0
    .byte   <$2350
    .byte   <$23D0

linePage:
    .byte   >$2000
    .byte   >$2080
    .byte   >$2100
    .byte   >$2180
    .byte   >$2200
    .byte   >$2280
    .byte   >$2300
    .byte   >$2380
    .byte   >$2028
    .byte   >$20A8
    .byte   >$2128
    .byte   >$21A8
    .byte   >$2228
    .byte   >$22A8
    .byte   >$2328
    .byte   >$23A8
    .byte   >$2050
    .byte   >$20D0
    .byte   >$2150
    .byte   >$21D0
    .byte   >$2250
    .byte   >$22D0
    .byte   >$2350
    .byte   >$23D0

colorTable:
    .byte   $0 * $11
    .byte   $1 * $11
    .byte   $2 * $11
    .byte   $3 * $11
    .byte   $4 * $11
    .byte   $5 * $11
    .byte   $6 * $11
    .byte   $7 * $11
    .byte   $8 * $11
    .byte   $9 * $11
    .byte   $A * $11
    .byte   $B * $11
    .byte   $C * $11
    .byte   $D * $11
    .byte   $E * $11
    .byte   $F * $11


MAX_TILES = 64

; number of tiles should be /8
.align 256
tileSheet_7x8:
    .res    32*MAX_TILES

; number of tiles should be
.align 256
tileSheet_14x16:
    .res    128*MAX_TILES

.align 256
mapSheet:
    .res    32*32