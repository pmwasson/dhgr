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

color           :=  $E8

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

    jsr     dhgrInit

    ; default size
    jsr     setSize_7x8

    ; Populate pixels
    jsr     updatePixels

reset_loop:
    jsr     resetScreen

command_loop:
    jsr     inline_print
    .byte   "Command:",0

skip_prompt:
    jsr     getInput    ; Wait for a keypress

    ; Parse command

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
    ; SP = Set Pixel
    ;------------------
    cmp     #KEY_SPACE
    bne     :+
    jsr     inline_print
    .byte   "Set pixel to ",0
    lda     paintColor
    and     #$f
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    ldx     paintColor
    jsr     setPixelColor
    lda     size
    jsr     updateTile
    jsr     drawPreview
    jmp     command_loop
:

    ;------------------
    ; ^F = Fill Color
    ;------------------
    cmp     #KEY_CTRL_F
    bne     :+
    jsr     inline_print
    .byte   "Set all pixels to ",0
    lda     paintColor
    and     #$f
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    jsr     fillPixels
    jsr     updateAll
    jsr     drawTilePixels
    jsr     drawPreview
    jmp     command_loop
:

    ;------------------
    ; ^T = Toggle Size
    ;------------------
    cmp     #KEY_CTRL_T
    bne     :+
    jsr     inline_print
    .byte   "Toggle size.  New size: ",0
    lda     size
    bne     set_small
; set large
    jsr     inline_print
    .byte   "14x16",13,0
    jsr     setSize_14x16
    jsr     updatePixels_14x16
    jmp     reset_loop
set_small:
    jsr     inline_print
    .byte   "7x8",13,0
    jsr     setSize_7x8
    jsr     updatePixels_7x8
    jmp     reset_loop
:

; DHGR Colors
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

    ;------------------
    ; 0 = black
    ;------------------
    cmp     #$80 | '0'
    bne     :+
    jsr     inline_print
    .byte   "Color = black ($0)",13,0
    lda     #$00
    jmp     finish_color    
:

    ;------------------
    ; 1 = dark blue
    ;------------------
    cmp     #$80 | '1'
    bne     :+
    jsr     inline_print
    .byte   "Color = dark blue ($1)",13,0
    lda     #$11
    jmp     finish_color    
:

    ;------------------
    ; 2 = dark green
    ;------------------
    cmp     #$80 | '2'
    bne     :+
    jsr     inline_print
    .byte   "Color = dark green ($2)",13,0
    lda     #$22
    jmp     finish_color    
:

    ;------------------
    ; 3 = medium blue
    ;------------------
    cmp     #$80 | '3'
    bne     :+
    jsr     inline_print
    .byte   "Color = medium blue ($3)",13,0
    lda     #$33
    jmp     finish_color    
:

    ;------------------
    ; 4 = brown
    ;------------------
    cmp     #$80 | '4'
    bne     :+
    jsr     inline_print
    .byte   "Color = brown ($4)",13,0
    lda     #$44
    jmp     finish_color    
:

    ;------------------
    ; 5 = gray (1)
    ;------------------
    cmp     #$80 | '5'
    bne     :+
    jsr     inline_print
    .byte   "Color = gray-1 ($5)",13,0
    lda     #$55
    jmp     finish_color    
:

    ;------------------
    ; 6 = light green
    ;------------------
    cmp     #$80 | '6'
    bne     :+
    jsr     inline_print
    .byte   "Color = light green ($6)",13,0
    lda     #$66
    jmp     finish_color    
:

    ;------------------
    ; 7 = Aqua
    ;------------------
    cmp     #$80 | '7'
    bne     :+
    jsr     inline_print
    .byte   "Color = aqua ($7)",13,0
    lda     #$77
    jmp     finish_color    
:

    ;------------------
    ; 8 = Red
    ;------------------
    cmp     #$80 | '8'
    bne     :+
    jsr     inline_print
    .byte   "Color = red ($8)",13,0
    lda     #$88
    jmp     finish_color    
:

    ;------------------
    ; 9 = Purple
    ;------------------
    cmp     #$80 | '9'
    bne     :+
    jsr     inline_print
    .byte   "Color = purple ($9)",13,0
    lda     #$99
    jmp     finish_color    
:

    ;------------------
    ; A = Gray (2)
    ;------------------
    cmp     #$80 | 'A'
    bne     :+
    jsr     inline_print
    .byte   "Color = gray-2 ($A)",13,0
    lda     #$AA
    jmp     finish_color    
:

    ;------------------
    ; B = Light blue
    ;------------------
    cmp     #$80 | 'B'
    bne     :+
    jsr     inline_print
    .byte   "Color = light blue ($B)",13,0
    lda     #$BB
    jmp     finish_color    
:

    ;------------------
    ; C = Orange
    ;------------------
    cmp     #$80 | 'C'
    bne     :+
    jsr     inline_print
    .byte   "Color = orange ($C)",13,0
    lda     #$CC
    jmp     finish_color    
:

    ;------------------
    ; D = Pink
    ;------------------
    cmp     #$80 | 'D'
    bne     :+
    jsr     inline_print
    .byte   "Color = pink ($D)",13,0
    lda     #$DD
    jmp     finish_color    
:

    ;------------------
    ; E = Yellow
    ;------------------
    cmp     #$80 | 'E'
    bne     :+
    jsr     inline_print
    .byte   "Color = yellow ($E)",13,0
    lda     #$EE
    jmp     finish_color    
:

    ;------------------
    ; F = white
    ;------------------
    cmp     #$80 | 'F'
    bne     :+
    jsr     inline_print
    .byte   "Color = white ($F)",13,0
    lda     #$FF
    jmp     finish_color    
:


    ;------------------
    ; D = Dump
    ;------------------
    cmp     #$80 + '!' 
    bne     :+
    bit     TXTSET
    jsr     inline_print
    .byte   "Dump (ESC when done) ",13,0
    jsr     printDump
    jmp     command_loop
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

; jump to after changing color
finish_color:
    sta     paintColor
    jsr     drawPreview
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
    jsr     inline_print
    .byte   " Color:",0
    jsr     getPixelColor
    and     #%00001111
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
; printDump
;-----------------------------------------------------------------------------
.proc printDump

    jsr     setTilePointer

    ldx     tilePtr0
    ldy     tilePtr1
    jsr     PRINTXY
    jsr     inline_print
    .byte   ":",13,".byte ",0

    lda     #0
    sta     dump_count
    jmp     dump_loop
dump_comma:
    lda     #$80 + ','
    jsr     COUT
dump_loop:
    lda     #$80 + '$'
    jsr     COUT
    ldy     dump_count
    lda     (tilePtr0),y
    jsr     PRBYTE
    inc     dump_count
    lda     dump_count
    cmp     length
    beq     dump_finish
    lda     dump_count
    and     #$f
    bne     dump_comma
    jsr     inline_print
    .byte   13,".byte ",0
    jmp     dump_loop

dump_finish:
    lda     #13
    jsr     COUT
    rts

dump_count: .byte   0

.endproc

;-----------------------------------------------------------------------------
; Reset screen
;
;   Clear screen and redraw
;-----------------------------------------------------------------------------

.proc resetScreen

    ; clear screens
    lda     #$20    ; page1
    sta     screenPage
    lda     #$55    ; black
    sta     color
    jsr     clearScreen

    jsr     drawColorKey
    jsr     drawPreview

    jsr     drawTilePixels

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

screenEnd:      .byte   $40

.endproc

;-----------------------------------------------------------------------------
; Init double hi-res
;-----------------------------------------------------------------------------

.proc dhgrInit

    sta     TXTCLR      ; Graphics
    sta     HIRES       ; Hi-res
    sta     MIXSET      ; Mixed
    sta     LOWSCR      ; Display page 1
    sta     DHIRESON    ; Annunciator 2 On
    sta     SET80VID    ; 80 column on
    rts

.endproc

;-----------------------------------------------------------------------------
; Get pixel color
;   color = pixel at curX,curY
;-----------------------------------------------------------------------------

.proc getPixelColor

    lda     curY
    asl             ; y*16
    asl
    asl
    asl
    clc
    adc     curX    ; +x
    tay
    lda     pixelData,y
    sta     color
    rts

.endproc

;-----------------------------------------------------------------------------
; Set pixel color
;   Set pixel at cursor to the value in X
;-----------------------------------------------------------------------------

.proc setPixelColor

    lda     curY
    asl             ; y*16
    asl
    asl
    asl
    clc
    adc     curX    ; +x
    tay
    txa
    sta     pixelData,y
    rts

.endproc


;-----------------------------------------------------------------------------
; Fill pixels
;   Set all pixel to paint color
;-----------------------------------------------------------------------------

.proc fillPixels

    ldx     #0
    lda     paintColor
:
    sta     pixelData,x
    inx
    bne     :-

    ; this is setting extra bytes, but no harm

    rts

.endproc


;-----------------------------------------------------------------------------
; Draw color key
;   Note: uses drawPixel so had to deal with offset
;-----------------------------------------------------------------------------

.proc drawColorKey

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
; Draw preview
;-----------------------------------------------------------------------------

.proc drawPreview
    
    ; current paint color
    jsr     saveCursor  ; save cursor position
    lda     #15
    sta     curX
    lda     #0
    sta     curY
    lda     paintColor
    sta     color
    jsr     drawPixel
    jsr     saveCursor  ; restore cursor position

    ; large tile
    lda     #40-8
    sta     tileX
    lda     #4
    sta     tileY
    jsr drawTile_14x16   

    ; small tile
    lda     #8
    sta     tileY
    jsr drawTile_7x8   

    rts
.endproc

;-----------------------------------------------------------------------------
; Draw tile pixels
;-----------------------------------------------------------------------------

.proc drawTilePixels
    
    jsr     saveCursor  ; save cursor position

    lda     #0
    sta     curY

loopY:
    lda     #0
    sta     curX

loopX:
    jsr     getPixelColor
    jsr     drawPixel

    inc     curX
    lda     curX
    cmp     width
    bne     loopX

    inc     curY
    lda     curY
    cmp     height
    bne     loopY

    jsr     saveCursor ; restor cursor
rts

.endproc


;-----------------------------------------------------------------------------
; Save cursor
;   Swap between current and backup cursor coordinates
;-----------------------------------------------------------------------------
.proc saveCursor

    ldx     curX
    lda     saveCurX
    sta     curX
    stx     saveCurX

    ldx     curY
    lda     saveCurY
    sta     curY
    stx     saveCurY

    rts

saveCurX:   .byte   0
saveCurY:   .byte   0

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

    jsr     getPixelColor
    lda     color
    lda     #$ff        ; default cursor == white
    cmp     color       ; if pixel white ...
    bne     :+
    lda     #0          ; ... set color to black
:
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

    jsr     getPixelColor
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
;
;            0 1 2 3 4 5 6 7 8 9 10 11 12 13 ; 4-bit pixels in 7-bit byes (MSB ignored)
;            ---   -----   ---   --------
;              -----   ---   ------    -----
;  Storage:  000 004 001 005 002 006 003 007 ; line 0
;            008 012 009 013 010 014 011 015 ; line 1
;            ..
;            120 124 121 125 122 126 123 127 ; line 15

;-----------------------------------------------------------------------------
.proc drawTile_14x16

    jsr     setTilePointer_14x16

    ; calculate screen pointer
    ldx     tileY
    lda     tileX
    clc
    adc     lineOffset,x    ; + lineOffset
    sta     screenPtr0    
    lda     linePage,x
    sta     screenPtr1

    sta     CLR80COL        ; Use RAMWRT for aux mem

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

.proc setTilePointer_14x16

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
    rts

.endproc

;-----------------------------------------------------------------------------
; drawTile
;  Assume 7x8, where 7 is 7*4 pixels = 28 -> 4 bytes
;    4*8 = 32, so 8 tiles per page
;  tileIndex - tile to draw
;  tileX     - byte offset of tile, should be /2
;  tileY     - 8-line offset of tile
;
;            0 1 2  3 4 5 6  ; 4-bit pixels in 7-bit byes (MSB ignored)
;            ---    -----
;              ------   ---
;  Storage:  00  02  01  03  ; line 0
;            04  06  05  07  ; line 1
;            ..
;            28  30  29  31  ; line 7
;
;-----------------------------------------------------------------------------
.proc drawTile_7x8

    jsr     setTilePointer_7x8

    sta     CLR80COL        ; Use RAMWRT for aux mem

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

.proc setTilePointer_7x8

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

    rts

.endproc

;-----------------------------------------------------------------------------
; setTilePointer
;
;-----------------------------------------------------------------------------

.proc setTilePointer

    lda     size
    beq     :+
    jmp     setTilePointer_14x16
:
    jmp     setTilePointer_7x8
.endproc

;-----------------------------------------------------------------------------
; updateTile
;  Update the tile pointed to by tileIndex with the current pixel data
;  To save time, only the 4 bytes pointed to by curX & Y are updated.
;
;-----------------------------------------------------------------------------

.proc updateTile
    lda     size
    beq     :+
    jmp     updateTile_14x16
:
    jmp     updateTile_7x8
.endproc

.proc updateTile_14x16

    jsr     setTilePointer_14x16

    jsr     updateSetOffsets_14x16

    ldx     pixelOffset
    jsr     pixelsToBytes

    ldy     tileOffset
    lda     pixelByte0
    sta     (tilePtr0),y
    iny                     ; +1
    lda     pixelByte2
    sta     (tilePtr0),y
    iny
    iny
    iny                     ; +4
    lda     pixelByte1
    sta     (tilePtr0),y
    iny                     ; +5
    lda     pixelByte3
    sta     (tilePtr0),y

    rts

.endproc


.proc updateTile_7x8

    jsr     setTilePointer_7x8

    jsr     updateSetOffsets_7x8

    ldx     pixelOffset
    jsr     pixelsToBytes

    ldy     tileOffset
    lda     pixelByte0
    sta     (tilePtr0),y
    iny                     ; +1
    lda     pixelByte2
    sta     (tilePtr0),y
    iny                     ; +2
    lda     pixelByte1
    sta     (tilePtr0),y
    iny                     ; +3
    lda     pixelByte3
    sta     (tilePtr0),y

    rts

.endproc


.proc updateSetOffsets_14x16
    ; tile  y offset = curY * 8 
    ; pixel y offset = curY * 16 
    lda     curY
    asl 
    asl
    asl     ; *8
    sta     tileOffset
    asl     ; *16
    sta     pixelOffset

    ; if curX < 7, pixel X offset 0, tile X offset 0 (0,4,1,5) or (0,2,1,3)
    ; else         pixel X offset 7, tile X offset 2 (2,6,3,7)

    lda     curX
    cmp     #7
    bmi     :+

    clc
    lda     tileOffset
    adc     #2
    sta     tileOffset
    lda     pixelOffset
    adc     #7
    sta     pixelOffset
:
    rts
.endproc

.proc updateSetOffsets_7x8
    ; tile  y offset = curY * 4 
    ; pixel y offset = curY * 16 
    lda     curY
    asl 
    asl     ; *4
    sta     tileOffset
    asl 
    asl     ; *16
    sta     pixelOffset
    rts
.endproc

;-----------------------------------------------------------------------------
; pixelsToBytes
;  Generate 4 bytes based on cursor position
;
; x = offset into pixel data
; results saved to global pixelByte0..3
;-----------------------------------------------------------------------------

.proc pixelsToBytes

    ; Set four bytes based on an offset into the pixel data in X
    ; byte 0 = x1110000
    lda     pixelData,x     ; pixel 0 bit 0:3 -> byte 0 bits 0:3
    and     #%00001111
    sta     temp
    lda     pixelData+1,x   ; pixel 1 bits 0:2 -> byte 0 bits 4:6
    and     #%01110000      ; dont set bit 8
    ora     temp
    sta     pixelByte0

    ; byte 1 = x3322221
    lda     pixelData+1,x   ; pixel 1 bit 3 -> byte 1 bit 0
    rol                     ; put bit 8 into carry
    lda     pixelData+2,x   ; pixel 2 bit 0:3 -> byte 1 bits 1:4
    and     #%00001111
    rol     
    sta     temp
    lda     pixelData+3,x   ; pixel 3 bits 0:1 -> byte 1 bits 5:6
    rol
    and     #%01100000      ; don't set bit 8
    ora     temp
    sta     pixelByte1

    ; byte 2 = x5444433
    lda     pixelData+3,x   ; pixel 3 bits 2:3 -> byte 2 bit 0:1
    ror
    ror
    and     #%00000011
    sta     temp
    lda     pixelData+4,x   ; pixel 4 bits 0:3 -> byte 2 bits 2:5
    rol
    rol
    and     #%00111100
    ora     temp
    sta     temp
    lda     pixelData+5,x   ; pixel 5 bit 0 -> byte 2 bit 6
    rol
    rol
    and     #%01000000
    ora     temp
    sta     pixelByte2

    ; byte 3 = x6666555
    lda     pixelData+5,x   ; pixel 5 bit 1:3 -> byte 3 bits 0:2
    ror
    and     #%00000111
    sta     temp
    lda     pixelData+6,x   ; pixel 6 bits 0:3 -> byte 3 bits 3:6
    ror
    and     #%01111000
    ora     temp
    sta     pixelByte3

    rts

temp:       .byte   0

.endproc

;-----------------------------------------------------------------------------
; Update pixels
;  Populate pixel array from shape table bytes
;-----------------------------------------------------------------------------

.proc updatePixels
    lda     size
    beq     :+
    jmp     updatePixels_14x16
:
    jmp     updatePixels_7x8
.endproc


.proc updatePixels_7x8

    jsr     saveCursor
    jsr     setTilePointer_7x8

    lda     #0
    sta     curY
    sta     tempIndex

yloop:
    lda     #0
    sta     curX

    ; read 4 bytes - bytes are interleaved
    ldy     tempIndex
    lda     (tilePtr0),y
    sta     pixelByte0
    iny
    lda     (tilePtr0),y
    sta     pixelByte2
    iny
    lda     (tilePtr0),y
    sta     pixelByte1
    iny
    lda     (tilePtr0),y
    sta     pixelByte3
    iny
    sty     tempIndex

    jsr     bytesToPixels

    inc     curY
    lda     curY
    cmp     #8
    bne     yloop

    jsr     saveCursor  ; restore cursor
    rts

tempIndex:  .byte   0

.endproc

.proc updatePixels_14x16

    jsr     saveCursor
    jsr     setTilePointer_14x16

    lda     #0
    sta     curY
    sta     tempIndex

yloop:
    lda     #0
    sta     curX

    ; read 4 bytes - bytes are interleaved
    ; first 4 are at y 0, 4, 1, 5

    ldy     tempIndex
    lda     (tilePtr0),y
    sta     pixelByte0
    iny     ; +1
    lda     (tilePtr0),y
    sta     pixelByte2
    iny     ; +2
    iny     ; +3
    iny     ; +4
    lda     (tilePtr0),y
    sta     pixelByte1
    iny     ; +5
    lda     (tilePtr0),y
    sta     pixelByte3
    dey     ; 4
    dey     ; 3
    dey     ; 2
    sty     tempIndex

    jsr     bytesToPixels

    ; next 4 are at y 2, 6, 3, 7

    ldy     tempIndex
    lda     (tilePtr0),y
    sta     pixelByte0
    iny     ; +3
    lda     (tilePtr0),y
    sta     pixelByte2
    iny     ; +4
    iny     ; +5
    iny     ; +6
    lda     (tilePtr0),y
    sta     pixelByte1
    iny     ; +7
    lda     (tilePtr0),y
    sta     pixelByte3
    iny     ; +8 -> next row
    sty     tempIndex

    inc     curY
    lda     curY
    cmp     #16
    bne     yloop

    jsr     saveCursor  ; restore cursor
    rts

tempIndex:  .byte   0

.endproc

;-----------------------------------------------------------------------------
; Bytes to pixels
;  Convert 4 bytes into pixel data using curX & Y
;-----------------------------------------------------------------------------

.proc bytesToPixels

    ; Pixel 0 - byte0 xxxx0000
    lda     pixelByte0
    and     #%00001111
    tay
    lda     colorTable,y    ; replicate nibbles
    tax
    jsr     setPixelColor
    inc     curX

    ; Pixel 1 - byte1,0 xxxxxxx1 x111xxxx
    lda     pixelByte0
    ror
    ror                     
    ror
    ror
    and     #%00000111
    sta     temp
    lda     pixelByte1
    rol
    rol
    rol
    and     #%00001000
    ora     temp
    tay
    lda     colorTable,y    ; replicate nibbles
    tax
    jsr     setPixelColor
    inc     curX

    ; Pixel 2 - byte1 xxx2222x
    lda     pixelByte1
    ror
    and     #%00001111
    tay
    lda     colorTable,y    ; replicate nibbles
    tax
    jsr     setPixelColor
    inc     curX


    ; Pixel 3 - byte2,1 xxxxxx33 x33xxxxx
    lda     pixelByte1
    rol     ; 4 rotate left (would be 5 ror)
    rol
    rol
    rol
    and     #%00000011
    sta     temp
    lda     pixelByte2
    rol
    rol
    and     #%00001100
    ora     temp
    tay
    lda     colorTable,y    ; replicate nibbles
    tax
    jsr     setPixelColor
    inc     curX

    ; Pixel 4 - byte2 xx4444xx
    lda     pixelByte2
    ror
    ror    
    and     #%00001111
    tay
    lda     colorTable,y    ; replicate nibbles
    tax
    jsr     setPixelColor
    inc     curX

    ; Pixel 5 - byte3,2 xxxxx555 x5xxxxxx
    lda     pixelByte2
    rol     ; shifting left through carry is 3 instruction, right would be 5
    rol
    rol
    and     #%00000001
    sta     temp
    lda     pixelByte3
    rol
    and     #%00001110
    ora     temp
    tay
    lda     colorTable,y    ; replicate nibbles
    tax
    jsr     setPixelColor
    inc     curX

    ; Pixel 6 - byte3 x6666xxx
    lda     pixelByte3
    ror
    ror    
    ror    
    and     #%00001111
    tay
    lda     colorTable,y    ; replicate nibbles
    tax
    jsr     setPixelColor
    inc     curX

    rts

temp:       .byte   0

.endproc


;-----------------------------------------------------------------------------
; Update all
;  Call update tile for all pixels
;
;-----------------------------------------------------------------------------

.proc updateAll
    lda     size
    beq     :+
    jmp     updateAll_14x16
:
    jmp     updateAll_7x8
.endproc


.proc updateAll_14x16

    jsr     saveCursor

    lda     #0
    sta     curY

:
    lda     #0
    sta     curX
    jsr     updateTile_14x16
    lda     #7
    sta     curX
    jsr     updateTile_14x16

    inc     curY
    lda     curY
    cmp     height
    bne     :-

    jsr     saveCursor

    rts

.endproc

.proc updateAll_7x8

    jsr     saveCursor

    lda     #0
    sta     curY

:
    lda     #0
    sta     curX
    jsr     updateTile_7x8

    inc     curY
    lda     curY
    cmp     height
    bne     :-

    jsr     saveCursor

    rts

.endproc

;-----------------------------------------------------------------------------
; Set size
;  Call update tile for all pixels
;
;-----------------------------------------------------------------------------

.proc setSize_14x16
    lda     #1
    sta     size
    lda     #14
    sta     width
    lda     #13
    sta     width_m1
    lda     #16
    sta     height
    lda     #15
    sta     height_m1
    lda     #8*16
    sta     length

    rts
.endproc

.proc setSize_7x8
    lda     #0
    sta     size
    lda     #7
    sta     width
    lda     #6
    sta     width_m1
    lda     #8
    sta     height
    lda     #7
    sta     height_m1
    lda     #4*8
    sta     length

    rts
.endproc

;-----------------------------------------------------------------------------
; Utilies

.include "inline_print.asm"

; Global Variables
;-----------------------------------------------------------------------------

screenPage:     .byte   $20
tileX:          .byte   0
tileY:          .byte   0
tileIndex:      .byte   0
curX:           .byte   0
curY:           .byte   0

paintColor:     .byte   $FF

; Make dimensions a variable incase we want variable tile size
size:           .byte   0   ; 0=7x8, 1=14x16
width:          .byte   0
width_m1:       .byte   0
height:         .byte   0
height_m1:      .byte   0
length:         .byte   0

; Conversion from pixels to bytes
pixelOffset:    .byte   0
tileOffset:     .byte   0

pixelByte0:     .byte   0
pixelByte1:     .byte   0
pixelByte2:     .byte   0
pixelByte3:     .byte   0

; Just store 1 pixel per byte and pad out to power of 2
.align      256
pixelData:
    .res    16*16

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

; Replicate color into both nibbles

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


; Tile Storage
;---------------------

MAX_TILES = 64

; number of tiles should be /8
.align 256
tileSheet_7x8:
    ; upper-left
    .byte   $0f,$00,$00,$00,$00,$00,$00,$00                                           
    .byte   $00,$55,$20,$2A,$00,$55,$2A,$2A                                           
    .byte   $00,$55,$2A,$2A,$00,$55,$2A,$2A                                           
    .byte   $00,$15,$2A,$00,$00,$15,$2A,$00                                           
    .res    32*(MAX_TILES-1)

; number of tiles should be
.align 256
tileSheet_14x16:
    .res    128*MAX_TILES

.align 256
mapSheet:
    .res    32*32