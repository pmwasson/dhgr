;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; DHGR Tile Editor
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

MAX_TILES       = 64
BOX_HORZ        = 48
BOX_VERT        = 49
BOX_UPPER_LEFT  = 50
BOX_UPPER_RIGHT = 51
BOX_LOWER_LEFT  = 52
BOX_LOWER_RIGHT = 53


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
    .byte   "DHGR tile editor - ? for help",13,0

    jsr     dhgrInit
    jsr     clearScreen

    ; default size
    jsr     setSize_14x16


reset_loop:
    jsr     updatePixels    ; populate pixels
    jsr     drawScreen

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
    ; - = Previous
    ;------------------
    cmp     #$80 | '-'
    bne     :+
    jsr     inline_print
    .byte   "Previous tile: ",0

    lda     tileIndex
    bne     previous_continue
    lda     #MAX_TILES
    sta     tileIndex
previous_continue:
    dec     tileIndex
    lda     tileIndex
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    jmp     reset_loop
:

    ;------------------
    ; _ = Previous 8
    ;------------------
    cmp     #$80 | '_'
    bne     :+
    jsr     inline_print
    .byte   "Previous 8 tiles: ",0

    lda     tileIndex
    sec     
    sbc     #8
    bpl     previous8_continue
    clc
    adc     #MAX_TILES
previous8_continue:
    sta     tileIndex
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    jmp     reset_loop
:

    ;------------------
    ; = = Next
    ;------------------
    cmp     #$80 | '='
    bne     :+
    jsr     inline_print
    .byte   "Next tile: ",0

    inc     tileIndex
    lda     tileIndex
    cmp     #MAX_TILES
    bne     next_continue
    lda     #0
    sta     tileIndex
next_continue:
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    jmp     reset_loop
:

    ;------------------
    ; + = Next 8
    ;------------------
    cmp     #$80 | '+'
    bne     :+
    jsr     inline_print
    .byte   "Next 8 tiles: ",0

    lda     tileIndex
    clc
    adc     #8
    cmp     #MAX_TILES
    bmi     next_continue8
    sec
    sbc     #MAX_TILES
next_continue8:
    sta     tileIndex
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    jmp     reset_loop
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
    jsr     clearScreen
    jmp     reset_loop
set_small:
    jsr     inline_print
    .byte   "7x8",13,0
    jsr     setSize_7x8
    jsr     clearScreen
    jmp     reset_loop
:

    ;------------------
    ; ^B = Toggle B&W
    ;------------------
    cmp     #KEY_CTRL_B
    bne     :+
    jsr     inline_print
    .byte   "Toggle mode.  New mode: ",0
    lda     colorMode
    cmp     #$10
    bne     set_color_mode
; set B&W mode
    jsr     inline_print
    .byte   "Binary",13,0
    lda     #$20
    sta     colorMode
    jmp     reset_loop
set_color_mode:
    jsr     inline_print
    .byte   "Color",13,0
    lda     #$10
    sta     colorMode
    jmp     reset_loop
:


    ;------------------
    ; ^R = Rotate
    ;------------------
    cmp     #KEY_CTRL_R
    bne     rotate_after
    jsr     inline_print
    .byte   "Rotate Direction (or cancel):",0
    jsr     getInputDirection
    bmi     rotate_cancel

    cmp     #DIR_UP
    bne     :+
    jsr     rotateUp
    jmp     rotate_done
:

    cmp     #DIR_DOWN
    bne     :+
    jsr     rotateDown
    jmp     rotate_done
:
    cmp     #DIR_LEFT
    bne     :+
    jsr     rotateLeft
    jmp     rotate_done
:
    ; must be right
    jsr     rotateRight

rotate_done:
    jsr     updateAll
    jsr     reset_loop

rotate_cancel:
    jmp     command_loop

rotate_after:  

    ;-----------------------
    ; ^D = Direction Mirror
    ;-----------------------
    cmp     #KEY_CTRL_D
    bne     mirror_after
    jsr     inline_print
    .byte   "Mirror Direction (or cancel):",0
    jsr     getInputDirection
    bmi     rotate_cancel   ; share code

    cmp     #DIR_UP         ; warning depends on order of enum
    bpl     :+
    jsr     mirrorHorz
    jmp     rotate_done     ; share code
:

    ; must be vertical
    jsr     mirrorVert
    jmp     rotate_done     ; share code

mirror_after:  



    ;------------------
    ; ! = Dump
    ;------------------
    cmp     #$80 + '!' 
    bne     :+
    bit     TXTSET
    jsr     inline_print
    .byte   "Dump Tile ",0
    lda     tileIndex
    jsr     PRBYTE
    jsr     inline_print
    .byte   " (ESC when done) ",13,0
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
    ; ^C = Color Swap
    ;------------------
    cmp     #KEY_CTRL_C
    bne     :+
    jsr     inline_print
    .byte   "Color swap",13,"First color (or cancel):",0
    jsr     getInput
    jsr     getInputColor
    sta     swapColor1
    cmp     #$80
    beq     color_swap_cancel
    jsr     inline_print
    .byte   "Second color (or cancel):",0
    jsr     getInput
    jsr     getInputColor
    sta     swapColor2
    cmp     #$80
    beq     color_swap_cancel
    jsr     swapColors
    jsr     updateAll
    jmp     reset_loop
color_swap_cancel:
    jsr     inline_print
    .byte   "Cancel",13,0
    jmp     command_loop

:

    ;---------------------
    ; 0-9A-Z = Pick color
    ;---------------------

    ; This should be the last input choice
    ; as it corrupts the keystroke if it fails

    jsr     getInputColor
    cmp     #$80    ; did it fail
    beq     :+
    ; finish color
    sta     paintColor
    jsr     drawColorKey
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
; Get input direction
;   Pick and diplay 1 of 4 directions or cancel
;-----------------------------------------------------------------------------
.proc getInputDirection
    jsr     getInput
    cmp     #KEY_LEFT
    bne     :+
    jsr     inline_print
    .byte   "Left ",13,0
    lda     #DIR_LEFT
    rts
:
    cmp     #KEY_RIGHT
    bne     :+
    jsr     inline_print
    .byte   "Right",13,0
    lda     #DIR_RIGHT
    rts
:
    cmp     #KEY_UP
    bne     :+
    jsr     inline_print
    .byte   "Up   ",13,0
    lda     #DIR_UP
    rts
:
    cmp     #KEY_DOWN
    bne     :+
    jsr     inline_print
    .byte   "Down ",13,0
    lda     #DIR_DOWN
    rts
:
    jsr     inline_print
    .byte   "Cancel",13,0
    LDA     #$FF
    rts
.endproc


;-----------------------------------------------------------------------------
; Get input color
;   Pick and diplay color
;   return $80 if invalid color picked
;-----------------------------------------------------------------------------
.proc getInputColor

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
    rts  
:

    ;------------------
    ; 1 = dark blue
    ;------------------
    cmp     #$80 | '1'
    bne     :+
    jsr     inline_print
    .byte   "Color = dark blue ($1)",13,0
    lda     #$11
    rts     
:

    ;------------------
    ; 2 = dark green
    ;------------------
    cmp     #$80 | '2'
    bne     :+
    jsr     inline_print
    .byte   "Color = dark green ($2)",13,0
    lda     #$22
    rts   
:

    ;------------------
    ; 3 = medium blue
    ;------------------
    cmp     #$80 | '3'
    bne     :+
    jsr     inline_print
    .byte   "Color = medium blue ($3)",13,0
    lda     #$33
    rts   
:

    ;------------------
    ; 4 = brown
    ;------------------
    cmp     #$80 | '4'
    bne     :+
    jsr     inline_print
    .byte   "Color = brown ($4)",13,0
    lda     #$44
    rts    
:

    ;------------------
    ; 5 = gray (1)
    ;------------------
    cmp     #$80 | '5'
    bne     :+
    jsr     inline_print
    .byte   "Color = gray-1 ($5)",13,0
    lda     #$55
    rts    
:

    ;------------------
    ; 6 = light green
    ;------------------
    cmp     #$80 | '6'
    bne     :+
    jsr     inline_print
    .byte   "Color = light green ($6)",13,0
    lda     #$66
    rts  
:

    ;------------------
    ; 7 = Aqua
    ;------------------
    cmp     #$80 | '7'
    bne     :+
    jsr     inline_print
    .byte   "Color = aqua ($7)",13,0
    lda     #$77
    rts    
:

    ;------------------
    ; 8 = Red
    ;------------------
    cmp     #$80 | '8'
    bne     :+
    jsr     inline_print
    .byte   "Color = red ($8)",13,0
    lda     #$88
    rts     
:

    ;------------------
    ; 9 = Purple
    ;------------------
    cmp     #$80 | '9'
    bne     :+
    jsr     inline_print
    .byte   "Color = purple ($9)",13,0
    lda     #$99
    rts    
:

    ;------------------
    ; A = Gray (2)
    ;------------------
    cmp     #$80 | 'A'
    bne     :+
    jsr     inline_print
    .byte   "Color = gray-2 ($A)",13,0
    lda     #$AA
    rts     
:

    ;------------------
    ; B = Light blue
    ;------------------
    cmp     #$80 | 'B'
    bne     :+
    jsr     inline_print
    .byte   "Color = light blue ($B)",13,0
    lda     #$BB
    rts     
:

    ;------------------
    ; C = Orange
    ;------------------
    cmp     #$80 | 'C'
    bne     :+
    jsr     inline_print
    .byte   "Color = orange ($C)",13,0
    lda     #$CC
    rts    
:

    ;------------------
    ; D = Pink
    ;------------------
    cmp     #$80 | 'D'
    bne     :+
    jsr     inline_print
    .byte   "Color = pink ($D)",13,0
    lda     #$DD
    rts   
:

    ;------------------
    ; E = Yellow
    ;------------------
    cmp     #$80 | 'E'
    bne     :+
    jsr     inline_print
    .byte   "Color = yellow ($E)",13,0
    lda     #$EE
    rts   
:

    ;------------------
    ; F = white
    ;------------------
    cmp     #$80 | 'F'
    bne     :+
    jsr     inline_print
    .byte   "Color = white ($F)",13,0
    lda     #$FF
    rts   
:

    ; Cancel!
    lda     #$80
    rts
.endproc

;-----------------------------------------------------------------------------
; printHelp
;-----------------------------------------------------------------------------
.proc printHelp
    bit     TXTSET
    jsr     inline_print
    .byte   "  Arrows:  Move cursor",13
    .byte   "  0-9,A-F: Set paint color",13
    .byte   "  Space:   Paint pixel",13
    .byte   "  Ctrl-F:  Fill tile with paint color",13
    .byte   "  Ctrl-C:  Swap 2 specified colors",13
    .byte   "  Ctrl-D:  Directional Mirror: mirror tile in an arrow key direction",13
    .byte   "  Ctrl-R:  Rotate pixels in a direction specified by an arrow key",13
    .byte   "  Ctrl-T:  Toggle between 7x8 and 14x16 tile size",13
    .byte   "  Ctrl-B:  Toggle between color and binary mode",13
    .byte   "  !:       Dump bytes",13
    .byte   "  -,=:     Go to previous/next tile",13
    .byte   "  _,+:     Go to previous/next 8 tiles",13
    .byte   "  ?:       This help screen",13
    .byte   "  Q:       Quit",13  
    .byte   "  Escape:  Toggle text/graphics",13
    ;.byte   "There are 2 tiles sheets in memory, one for 14x16 tiles and one for 7x8 tiles.",13
    ;.byte   "Each sheet contains 64 tiles that can be editted.",13
    .byte   0

    rts
.endproc


;-----------------------------------------------------------------------------
; printDump
;-----------------------------------------------------------------------------
.proc printDump

    lda     tileIndex
    jsr     setTilePointer

    jsr     inline_print
    .byte   ".byte ",0

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
; Rotate up 
;  Rotate all pixels based on tile size
;-----------------------------------------------------------------------------
.proc rotateUp

    ldx     #0      ; destination row
    ldy     #16     ; source row

loop:
    lda     height_m1
    sta     loopCount

    ; remember first data
    lda     pixelData,x
    sta     temp

col_loop:
    lda     pixelData,y
    sta     pixelData,x

    tya
    tax     ; copy Y -> X
    clc
    adc     #16
    tay     ; y += 16 (next row)
    dec     loopCount
    bne     col_loop

    ; write back first data
    lda     temp
    sta     pixelData,x

    ; calc next column
    iny     ; +1 (next column)
    tya
    sec     ; jump back to start of column
    sbc     height_x16
    tax     ; put into X
    clc
    adc     #16
    tay     ; y = next row

    cpx     width
    bne     loop

    rts

temp:       .byte   0
loopCount:  .byte   0

.endproc

;-----------------------------------------------------------------------------
; Rotate down
;  Rotate all pixels based on tile size
;-----------------------------------------------------------------------------
.proc rotateDown

    lda     height_x16
    sec
    sbc     #16
    tax             ; destination row
    sec
    sbc     #16
    tay             ; source row

    lda     width
    sta     loopCountRow


loop:
    lda     height_m1
    sta     loopCountCol

    ; remember first data
    lda     pixelData,x
    sta     temp

col_loop:
    lda     pixelData,y
    sta     pixelData,x

    tya
    tax     ; copy Y -> X
    sec
    sbc     #16
    tay     ; y -= 16 (next row)
    dec     loopCountCol
    bne     col_loop

    ; write back first data
    lda     temp
    sta     pixelData,x

    ; calc next column
    iny     ; +1 (next column)
    tya
    clc     ; jump back to start of column
    adc     height_x16
    tax     ; put into X
    sec
    sbc     #16
    tay     ; y = next row

    dec     loopCountRow
    bne     loop

    rts

temp:           .byte   0
loopCountCol:   .byte   0
loopCountRow:   .byte   0

.endproc


;-----------------------------------------------------------------------------
; Rotate Left
;  Rotate all pixels based on tile size
;-----------------------------------------------------------------------------
.proc rotateLeft

    lda     height
    sta     loopCount
    ldx     #0

loop:
    ; remember first data
    lda     pixelData,x
    sta     temp

    ldy     width_m1

row_loop:
    inx
    lda     pixelData,x
    dex
    sta     pixelData,x
    inx
    dey
    bne     row_loop

    lda     temp
    sta     pixelData,x

    txa
    clc
    adc     #16
    and     #$F0
    tax

    dec     loopCount
    bne     loop

    rts

temp:       .byte   0
loopCount:  .byte   0

.endproc

;-----------------------------------------------------------------------------
; Rotate Right
;  Rotate all pixels based on tile size
;-----------------------------------------------------------------------------
.proc rotateRight

    lda     height
    sta     loopCount
    ldx     width_m1

loop:
    ; remember first data
    lda     pixelData,x
    sta     temp

    ldy     width_m1

row_loop:
    dex
    lda     pixelData,x
    inx
    sta     pixelData,x
    dex
    dey
    bne     row_loop

    lda     temp
    sta     pixelData,x

    txa
    clc
    adc     #16
    adc     width_m1
    tax

    dec     loopCount
    bne     loop

    rts

temp:       .byte   0
loopCount:  .byte   0

.endproc


;-----------------------------------------------------------------------------
; Mirror horizontal
;   Swap bytes
;-----------------------------------------------------------------------------
.proc mirrorHorz

    lda     #0
    sta     rowCount

row_loop:

    lda     width
    lsr     ; /2
    sta     colCount

    ; set up offsets

    lda     rowCount
    asl
    asl
    asl
    asl     ; *16
    tax
    clc
    adc     width_m1
    tay


col_loop:
    ; swap data
    lda     pixelData,x
    sta     temp
    lda     pixelData,y
    sta     pixelData,x
    lda     temp
    sta     pixelData,y

    inx
    dey
    dec     colCount

    bne     col_loop

    inc     rowCount
    lda     rowCount
    cmp     height
    bne     row_loop
    rts

temp:       .byte   0
rowCount:   .byte   0
colCount:   .byte   0

.endproc

;-----------------------------------------------------------------------------
; Mirror vertically
;   Swap bytes
;-----------------------------------------------------------------------------
.proc mirrorVert

    lda     #0
    sta     colCount

col_loop:

    lda     height
    lsr     ; /2
    sta     rowCount

    ; set up offsets

    lda     colCount
    tax
    adc     height_x16
    sec
    sbc     #16
    tay


row_loop:
    ; swap data
    lda     pixelData,x
    sta     temp
    lda     pixelData,y
    sta     pixelData,x
    lda     temp
    sta     pixelData,y

    txa
    clc
    adc     #16
    tax

    tya
    sec
    sbc     #16
    tay

    dec     rowCount
    bne     row_loop

    inc     colCount
    lda     colCount
    cmp     width
    bne     col_loop
    rts

temp:       .byte   0
rowCount:   .byte   0
colCount:   .byte   0

.endproc


;-----------------------------------------------------------------------------
; Swap Colors
;
;   Just go through whole buffer
;-----------------------------------------------------------------------------

.proc swapColors

    ldx     #0

loop:
    lda     pixelData,x
    cmp     swapColor1
    bne     :+
    lda     swapColor2
    sta     pixelData,x
    jmp     continue
:
    cmp     swapColor2
    bne     continue
    lda     swapColor1
    sta     pixelData,x
continue:
    inx
    bne     loop
    rts

.endproc

;-----------------------------------------------------------------------------
; Draw screen
;
;   Redraw screen
;-----------------------------------------------------------------------------

.proc drawScreen

    jsr     drawColorKey
    jsr     drawPreview
    jsr     drawTilePixels

    ; Draw preview box
    lda     #0
    sta     boxLeft
    sta     boxTop
    lda     width
    asl                 ; *2
    adc     #2
    sta     boxRight
    lda     height
    sta     boxBottom
    inc     boxBottom
    jsr     drawBox

    rts
.endproc


;-----------------------------------------------------------------------------
; Draw box
;
;   Redraw screen
;-----------------------------------------------------------------------------

.proc drawBox

    ; Draw corners
    lda     boxLeft
    sta     tileX
    lda     boxTop
    sta     tileY
    lda     #BOX_UPPER_LEFT
    jsr     drawTile_7x8

    lda     boxRight
    sta     tileX    
    lda     #BOX_UPPER_RIGHT
    jsr     drawTile_7x8

    lda     boxBottom
    sta     tileY
    lda     #BOX_LOWER_RIGHT
    jsr     drawTile_7x8

    lda     boxLeft
    sta     tileX
    lda     #BOX_LOWER_LEFT
    jsr     drawTile_7x8

    ; Draw horizontal

    inc     tileX
    inc     tileX
:
    lda     boxTop
    sta     tileY
    lda     #BOX_HORZ
    jsr     drawTile_7x8
    
    lda     boxBottom
    sta     tileY
    lda     #BOX_HORZ
    jsr     drawTile_7x8
    
    inc     tileX
    inc     tileX
    lda     boxRight
    cmp     tileX
    bne     :-

    ; Draw vertical

    lda     boxTop
    sta     tileY
    inc     tileY

:
    lda     boxLeft
    sta     tileX
    lda     #BOX_VERT
    jsr     drawTile_7x8
    
    lda     boxRight
    sta     tileX
    lda     #BOX_VERT
    jsr     drawTile_7x8
    
    inc     tileY
    lda     boxBottom
    cmp     tileY
    bne     :-

    rts

.endproc



;-----------------------------------------------------------------------------
; DHGR clear screen
;-----------------------------------------------------------------------------

.proc clearScreen
    lda     #$00
    sta     screenPtr0
    lda     #$20
    sta     screenPtr1

    sta     CLR80COL        ; Use RAMWRT for aux mem

loop:
    ldy     #0

    ; aux mem
    lda     #0
    sta     RAMWRTON  

:
    sta     (screenPtr0),y
    iny
    bne     :-    

    sta     RAMWRTOFF

    ; main mem
:
    sta     (screenPtr0),y
    iny
    bne     :-    

    inc     screenPtr1
    lda     #$40
    cmp     screenPtr1
    bne     loop
    rts

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

    lda     #0
    sta     colorX

loopx:
    lda     #18
    sta     tileY

    lda     colorX
    asl     ; *2
    sta     tileX

    lda     colorX
    ora     colorMode
    jsr     drawTile_7x8

    inc     tileY

    lda     paintColor
    and     #$0F
    cmp     colorX
    bne     :+

    lda     #$FF
    sta     invMask

:
    lda     colorX
    jsr     drawTile_7x8

    lda     #0
    sta     invMask

    inc     colorX
    lda     colorX
    cmp     #16
    bne     loopx
    rts



    jsr     saveCursor

    ; set up coordinates
    lda     #0
    sta     curX
    lda     #17
    sta     curY

    ; loop through colors
loop:

    ; Draw color pixel
    ldx     curX
    lda     colorTable,x
    sta     color
    jsr     drawPixel

    lda     #19
    sta     tileY

    ; Calc label X
    lda     curX
    clc
    adc     #1      ; + offset
    asl             ; *2
    sta     tileX

    ; Set mask
    lda     #$00
    sta     invMask
    lda     paintColor
    and     #$0f
    cmp     curX
    bne     :+
    lda     #$ff
    sta     invMask
:

    lda     curX
    jsr     drawTile_7x8

    lda     #$00
    sta     invMask

    inc     curX
    lda     curX
    cmp     #16
    bne     loop

    jsr     saveCursor      ; restore cursor

    rts

colorX:     .byte 0

.endproc




;-----------------------------------------------------------------------------
; Draw preview
;-----------------------------------------------------------------------------

.proc drawPreview
    

    lda     width
    asl             ; *2
    adc     #4
    sta     tileX
    lda     #1
    sta     tileY

    lda     size
    beq     :+

    ; large tiles

    ; 1 preview tile
    lda     tileIndex
    jsr     drawTile_14x16

    ; 2x2 tile grid
    lda     #4
    sta     tileY
    lda     tileIndex
    jsr     drawTile_14x16

    lda     #6
    sta     tileY
    lda     tileIndex
    jsr     drawTile_14x16

    lda     tileX
    clc
    adc     #4
    sta     tileX
    lda     tileIndex
    jsr     drawTile_14x16

    lda     #4
    sta     tileY
    lda     tileIndex
    jsr     drawTile_14x16

    rts
:

    ; current preview tile
    lda     tileIndex
    jsr     drawTile_7x8

    ; FIXME - display all tiles!

    lda     #0
    sta     index

loopy:
    lda     #24
    sta     tileX

loopx:
    lda     index
    cmp     tileIndex
    bne     :+

    lda     #$ff
    sta     invMask
    lda     tileIndex

:
    jsr     drawTile_7x8

    lda     #0
    sta     invMask

    inc     index
    inc     tileX
    inc     tileX
    lda     tileX
    cmp     #40
    bne     loopx

    inc     tileY
    lda     index
    cmp     #MAX_TILES
    bne     loopy

    rts

index:  .byte   0

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
;   Based on curX, curY and color
;-----------------------------------------------------------------------------
.proc drawPixel

    sta     CLR80COL        ; Use RAMWRT for aux mem

    lda     curY
    sta     tileY
    inc     tileY           ; offset by 1

    lda     curX
    asl                     ; *2
    adc     #2              ; add offset
    sta     tileX

    lda     color
    and     #$f
    ora     colorMode
    jsr     drawTile_7x8

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

    jsr     setCursorColor
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
; Set cursor color
;   Default to paint color
;   If pixel == painter color and pixel != white, use white
;   Else use black
;-----------------------------------------------------------------------------

.proc setCursorColor

    jsr     getPixelColor
    lda     paintColor
    cmp     color
    bne     done        ; A = paintColor

    ; check if current is white
    lda     #$ff
    cmp     paintColor
    bne     done        ; A = white

    ; set to black
    lda     #0

done:
    sta     color
    rts

.endproc


;-----------------------------------------------------------------------------
; drawTile
;  Assume 14x16, where 14 is 14*4 pixels = 56 -> 8 bytes
;    8*16 = 128, so 2 tiles per page
;  A         - tile to draw
;  tileX     - byte offset of tile, should be /4
;  tileY     - 8-line offset of tile, should be /2
;
;            0 1 2 3 4 5 6 7 8 9 10 11 12 13 ; 4-bit pixels in 7-bit byes (MSB ignored)
;            -0-   --2--   -4-   ----6---    ; AUX memory
;              --1--   -3-   ---5--    --7-- ; Main memory
;
;  Storage:  000 004 001 005 002 006 003 007 ; line 0
;            008 012 009 013 010 014 011 015 ; line 1
;            ..
;            120 124 121 125 122 126 123 127 ; line 15
;-----------------------------------------------------------------------------

.proc drawTile_14x16

    ; tile index passed in A
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
    tay     ; save A
    lsr                     ; *128
    lda     #0
    ror
    sta     tilePtr0
    tya     ; restore A
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
;  A         - tile to draw
;  tileX     - byte offset of tile, should be /2
;  tileY     - 8-line offset of tile
;
;            0 1 2  3 4 5 6  ; 4-bit pixels in 7-bit byes (MSB ignored)
;            -0-    --2--    ; AUX memory
;              ---1--   -3-  ; Main memory
;
;  Storage:  00  02  01  03  ; line 0
;            04  06  05  07  ; line 1
;            ..
;            28  30  29  31  ; line 7
;
;-----------------------------------------------------------------------------
.proc drawTile_7x8

    ; tile index passes in A
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
    eor     invMask
    sta     (screenPtr0),y
    ldy     #1
    lda     (tilePtr0),y
    eor     invMask
    sta     (screenPtr0),y

    ; Bytes 2-3 in MAIN memory
    ;
    sta     RAMWRTOFF

    lda     tilePtr0    ; offset tile pointer by 2
    adc     #2
    sta     tilePtr0

    ldy     #0
    lda     (tilePtr0),y
    eor     invMask
    sta     (screenPtr0),y
    ldy     #1
    lda     (tilePtr0),y
    eor     invMask
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

; Index passed in A
.proc setTilePointer_7x8

    tay     ; copy A
    ; calculate tile pointer
    asl                     ; *32
    asl
    asl
    asl
    asl
    sta     tilePtr0
    tya     ; restore A
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
    lda     tileIndex
    jmp     setTilePointer_14x16
:
    lda     tileIndex
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

    lda     tileIndex
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

    lda     tileIndex
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
    lda     tileIndex
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
    lda     tileIndex
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

    jsr     bytesToPixels

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
    lda     #0          ; height * 16
    sta     height_x16
    lda     #8*16
    sta     length

    lda     #0
    sta     curX
    sta     curY

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
    lda     #8*16
    sta     height_x16
    lda     #4*8
    sta     length

    lda     #0
    sta     curX
    sta     curY

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

invMask:        .byte   0
paintColor:     .byte   $FF
colorMode:      .byte   $10     ; $10 = color, $20 = B&W

swapColor1:     .byte   $0
swapColor2:     .byte   $0

; Make dimensions a variable incase we want variable tile size
size:           .byte   0   ; 0=7x8, 1=14x16
width:          .byte   0
width_m1:       .byte   0
height:         .byte   0
height_m1:      .byte   0
height_x16:     .byte   0
length:         .byte   0

; Conversion from pixels to bytes
pixelOffset:    .byte   0
tileOffset:     .byte   0

pixelByte0:     .byte   0
pixelByte1:     .byte   0
pixelByte2:     .byte   0
pixelByte3:     .byte   0

; Box routine
boxLeft:        .byte   0
boxRight:       .byte   0
boxTop:         .byte   0
boxBottom:      .byte   0

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


.align 256

tileSheet_7x8:

    ; Color Keys
    ; Tiles 00..0F
    ;-------------------


    ; 0
    .byte $40,$7F,$7F,$01,$70,$7C,$1F,$07,$70,$7C,$1F,$07,$70,$7C,$1F,$07           
    .byte $70,$7C,$1F,$07,$70,$7C,$1F,$07,$40,$7F,$7F,$01,$00,$00,$00,$00           

    ; 1
    .byte $00,$0F,$78,$00,$00,$0F,$7E,$00,$40,$0F,$7F,$00,$00,$0F,$78,$00           
    .byte $00,$0F,$78,$00,$00,$0F,$78,$00,$40,$7F,$7F,$01,$00,$00,$00,$00           

    ; 2
    .byte $70,$7F,$7F,$01,$00,$7C,$00,$07,$00,$7C,$00,$07,$40,$7F,$7F,$01           
    .byte $70,$00,$1F,$00,$70,$00,$1F,$00,$70,$7F,$7F,$07,$00,$00,$00,$00           

    ; 3
    .byte $70,$7F,$7F,$01,$00,$7C,$00,$07,$00,$7C,$00,$07,$00,$7F,$7E,$01           
    .byte $00,$7C,$00,$07,$00,$7C,$00,$07,$70,$7F,$7F,$01,$00,$00,$00,$00           

    ;4
    .byte $70,$7C,$1F,$07,$70,$7C,$1F,$07,$70,$7C,$1F,$07,$70,$7F,$7F,$07           
    .byte $00,$7C,$00,$07,$00,$7C,$00,$07,$00,$7C,$00,$07,$00,$00,$00,$00           

    ; 5
    .byte $70,$7F,$7F,$07,$70,$00,$1F,$00,$70,$00,$1F,$00,$70,$7F,$7F,$01           
    .byte $00,$7C,$00,$07,$00,$7C,$00,$07,$70,$7F,$7F,$01,$00,$00,$00,$00           

    ; 6
    .byte $40,$7F,$7F,$01,$70,$00,$1F,$00,$70,$00,$1F,$00,$70,$7F,$7F,$01           
    .byte $70,$7C,$1F,$07,$70,$7C,$1F,$07,$40,$7F,$7F,$01,$00,$00,$00,$00           

    ; 7
    .byte $70,$7F,$7F,$07,$00,$7C,$00,$07,$00,$7C,$00,$07,$00,$7F,$00,$01           
    .byte $00,$3F,$60,$00,$00,$0F,$78,$00,$00,$0F,$78,$00,$00,$00,$00,$00           

    ; 8
    .byte $40,$7F,$7F,$01,$70,$7C,$1F,$07,$70,$7C,$1F,$07,$40,$7F,$7F,$01           
    .byte $70,$7C,$1F,$07,$70,$7C,$1F,$07,$40,$7F,$7F,$01,$00,$00,$00,$00           

    ; 9
    .byte $40,$7F,$7F,$01,$70,$7C,$1F,$07,$70,$7C,$1F,$07,$40,$7F,$7F,$07           
    .byte $00,$7C,$00,$07,$00,$7C,$00,$07,$40,$7F,$7F,$01,$00,$00,$00,$00           

    ; A
    .byte $00,$0F,$78,$00,$40,$7F,$7F,$01,$70,$7C,$1F,$07,$70,$7C,$1F,$07           
    .byte $70,$7F,$7F,$07,$70,$7C,$1F,$07,$70,$7C,$1F,$07,$00,$00,$00,$00           

    ; B
    .byte $70,$7F,$7F,$01,$70,$7C,$1F,$07,$70,$7C,$1F,$07,$70,$7F,$7F,$01           
    .byte $70,$7C,$1F,$07,$70,$7C,$1F,$07,$70,$7F,$7F,$01,$00,$00,$00,$00           

    ; C
    .byte $40,$7F,$7F,$01,$70,$7C,$1F,$07,$70,$00,$1F,$00,$70,$00,$1F,$00           
    .byte $70,$00,$1F,$00,$70,$7C,$1F,$07,$40,$7F,$7F,$01,$00,$00,$00,$00           

    ; D
    .byte $70,$7F,$7F,$01,$70,$7C,$1F,$07,$70,$7C,$1F,$07,$70,$7C,$1F,$07           
    .byte $70,$7C,$1F,$07,$70,$7C,$1F,$07,$70,$7F,$7F,$01,$00,$00,$00,$00           

    ; E
    .byte $70,$7F,$7F,$07,$70,$00,$1F,$00,$70,$00,$1F,$00,$70,$3F,$7F,$00           
    .byte $70,$00,$1F,$00,$70,$00,$1F,$00,$70,$7F,$7F,$07,$00,$00,$00,$00           

    ; F
    .byte $70,$7F,$7F,$07,$70,$00,$1F,$00,$70,$00,$1F,$00,$70,$3F,$7F,$00           
    .byte $70,$00,$1F,$00,$70,$00,$1F,$00,$70,$00,$1F,$00,$00,$00,$00,$00           

    ; Color pixels
    ; Tiles 10..1F
    ;-------------------
                                  
    ; black ($0)   
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           

    ; dark blue ($1)     
    .byte $11,$44,$22,$08,$11,$44,$22,$08,$11,$44,$22,$08,$11,$44,$22,$08           
    .byte $11,$44,$22,$08,$11,$44,$22,$08,$11,$44,$22,$08,$11,$44,$22,$08           

    ; dark green ($2)  
    .byte $22,$08,$44,$11,$22,$08,$44,$11,$22,$08,$44,$11,$22,$08,$44,$11           
    .byte $22,$08,$44,$11,$22,$08,$44,$11,$22,$08,$44,$11,$22,$08,$44,$11           

    ; medium blue ($3)
    .byte $33,$4C,$66,$19,$33,$4C,$66,$19,$33,$4C,$66,$19,$33,$4C,$66,$19           
    .byte $33,$4C,$66,$19,$33,$4C,$66,$19,$33,$4C,$66,$19,$33,$4C,$66,$19           

    ; brown ($4) 
    .byte $44,$11,$08,$22,$44,$11,$08,$22,$44,$11,$08,$22,$44,$11,$08,$22           
    .byte $44,$11,$08,$22,$44,$11,$08,$22,$44,$11,$08,$22,$44,$11,$08,$22           

    ; gray-1 ($5)
    .byte $55,$55,$2A,$2A,$55,$55,$2A,$2A,$55,$55,$2A,$2A,$55,$55,$2A,$2A           
    .byte $55,$55,$2A,$2A,$55,$55,$2A,$2A,$55,$55,$2A,$2A,$55,$55,$2A,$2A           

    ; light green ($6) 
    .byte $66,$19,$4C,$33,$66,$19,$4C,$33,$66,$19,$4C,$33,$66,$19,$4C,$33           
    .byte $66,$19,$4C,$33,$66,$19,$4C,$33,$66,$19,$4C,$33,$66,$19,$4C,$33           

    ; aqua ($7)  
    .byte $77,$5D,$6E,$3B,$77,$5D,$6E,$3B,$77,$5D,$6E,$3B,$77,$5D,$6E,$3B           
    .byte $77,$5D,$6E,$3B,$77,$5D,$6E,$3B,$77,$5D,$6E,$3B,$77,$5D,$6E,$3B           

    ; red ($8)   
    .byte $08,$22,$11,$44,$08,$22,$11,$44,$08,$22,$11,$44,$08,$22,$11,$44           
    .byte $08,$22,$11,$44,$08,$22,$11,$44,$08,$22,$11,$44,$08,$22,$11,$44           

    ; purple ($9)  
    .byte $19,$66,$33,$4C,$19,$66,$33,$4C,$19,$66,$33,$4C,$19,$66,$33,$4C           
    .byte $19,$66,$33,$4C,$19,$66,$33,$4C,$19,$66,$33,$4C,$19,$66,$33,$4C           

    ; gray-2 ($A)   
    .byte $2A,$2A,$55,$55,$2A,$2A,$55,$55,$2A,$2A,$55,$55,$2A,$2A,$55,$55           
    .byte $2A,$2A,$55,$55,$2A,$2A,$55,$55,$2A,$2A,$55,$55,$2A,$2A,$55,$55           

    ; light blue ($B)  
    .byte $3B,$6E,$77,$5D,$3B,$6E,$77,$5D,$3B,$6E,$77,$5D,$3B,$6E,$77,$5D           
    .byte $3B,$6E,$77,$5D,$3B,$6E,$77,$5D,$3B,$6E,$77,$5D,$3B,$6E,$77,$5D           

    ; orange ($C)     
    .byte $4C,$33,$19,$66,$4C,$33,$19,$66,$4C,$33,$19,$66,$4C,$33,$19,$66           
    .byte $4C,$33,$19,$66,$4C,$33,$19,$66,$4C,$33,$19,$66,$4C,$33,$19,$66           

    ; pink ($D)  
    .byte $5D,$77,$3B,$6E,$5D,$77,$3B,$6E,$5D,$77,$3B,$6E,$5D,$77,$3B,$6E           
    .byte $5D,$77,$3B,$6E,$5D,$77,$3B,$6E,$5D,$77,$3B,$6E,$5D,$77,$3B,$6E           

    ; yellow ($E)   
    .byte $6E,$3B,$5D,$77,$6E,$3B,$5D,$77,$6E,$3B,$5D,$77,$6E,$3B,$5D,$77           
    .byte $6E,$3B,$5D,$77,$6E,$3B,$5D,$77,$6E,$3B,$5D,$77,$6E,$3B,$5D,$77           

    ; white ($F)      
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           

    ; Binary pixels
    ; Tiles 20..2F
    ;-------------------

    ; Binary 0
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$70,$3F,$7F,$00,$00,$00,$00,$00           

    ; Binary 1
    .byte $10,$00,$00,$00,$10,$00,$00,$00,$10,$00,$00,$00,$10,$00,$00,$00           
    .byte $10,$00,$00,$00,$10,$00,$00,$00,$00,$3F,$7E,$00,$00,$00,$00,$00           

    ; Binary 2
    .byte $00,$00,$04,$00,$00,$00,$04,$00,$00,$00,$04,$00,$00,$00,$04,$00           
    .byte $00,$00,$04,$00,$00,$00,$04,$00,$70,$3F,$61,$00,$00,$00,$00,$00           

    ; Binary 3
    .byte $30,$00,$06,$00,$30,$00,$06,$00,$30,$00,$06,$00,$30,$00,$06,$00           
    .byte $30,$00,$06,$00,$30,$00,$06,$00,$00,$3F,$60,$00,$00,$00,$00,$00           

    ; Binary 4
    .byte $00,$01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00,$00,$01,$00,$00           
    .byte $00,$01,$00,$00,$00,$01,$00,$00,$70,$3C,$1F,$00,$00,$00,$00,$00           


    ; Binary 5
    .byte $50,$01,$20,$00,$50,$01,$20,$00,$50,$01,$20,$00,$50,$01,$20,$00           
    .byte $50,$01,$20,$00,$50,$01,$20,$00,$00,$3C,$1E,$00,$00,$00,$00,$00           

    ; Binary 6
    .byte $00,$01,$4C,$00,$00,$01,$4C,$00,$00,$01,$4C,$00,$00,$01,$4C,$00           
    .byte $00,$01,$4C,$00,$00,$01,$4C,$00,$70,$3C,$01,$00,$00,$00,$00,$00           

    ; Binary 7
    .byte $70,$01,$6E,$00,$70,$01,$6E,$00,$70,$01,$6E,$00,$70,$01,$6E,$00           
    .byte $70,$01,$6E,$00,$70,$01,$6E,$00,$00,$3C,$00,$00,$00,$00,$00,$00           

    ; Binary 8
    .byte $00,$20,$00,$00,$00,$20,$00,$00,$00,$20,$00,$00,$00,$20,$00,$00           
    .byte $00,$20,$00,$00,$00,$20,$00,$00,$70,$03,$7F,$00,$00,$00,$00,$00           

    ; Binary 9
    .byte $10,$24,$01,$00,$10,$24,$01,$00,$10,$24,$01,$00,$10,$24,$01,$00           
    .byte $10,$24,$01,$00,$10,$24,$01,$00,$00,$03,$7E,$00,$00,$00,$00,$00           

    ; Binary A
    .byte $00,$28,$14,$00,$00,$28,$14,$00,$00,$28,$14,$00,$00,$28,$14,$00           
    .byte $00,$28,$14,$00,$00,$28,$14,$00,$70,$03,$61,$00,$00,$00,$00,$00           

    ; Binary B
    .byte $30,$2C,$17,$00,$30,$2C,$17,$00,$30,$2C,$17,$00,$30,$2C,$17,$00           
    .byte $30,$2C,$17,$00,$30,$2C,$17,$00,$00,$03,$60,$00,$00,$00,$00,$00           

    ; Binary C
    .byte $00,$33,$00,$00,$00,$33,$00,$00,$00,$33,$00,$00,$00,$33,$00,$00           
    .byte $00,$33,$00,$00,$00,$33,$00,$00,$70,$00,$1F,$00,$00,$00,$00,$00           

    ; Binary D
    .byte $50,$37,$21,$00,$50,$37,$21,$00,$50,$37,$21,$00,$50,$37,$21,$00           
    .byte $50,$37,$21,$00,$50,$37,$21,$00,$00,$00,$1E,$00,$00,$00,$00,$00           

    ; Binary E
    .byte $00,$3B,$5C,$00,$00,$3B,$5C,$00,$00,$3B,$5C,$00,$00,$3B,$5C,$00           
    .byte $00,$3B,$5C,$00,$00,$3B,$5C,$00,$70,$00,$01,$00,$00,$00,$00,$00           

    ; Binary F
    .byte $70,$3F,$7F,$00,$70,$3F,$7F,$00,$70,$3F,$7F,$00,$70,$3F,$7F,$00           
    .byte $70,$3F,$7F,$00,$70,$3F,$7F,$00,$00,$00,$00,$00,$00,$00,$00,$00           

    ; Screen Element
    ; Tiles 30..35
    ;-------------------


    ; Horizontal pipe
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$55,$55,$2A,$2A           
    .byte $55,$55,$2A,$2A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           

    ; Vertical pipe
    .byte $00,$01,$20,$00,$00,$01,$20,$00,$00,$01,$20,$00,$00,$01,$20,$00           
    .byte $00,$01,$20,$00,$00,$01,$20,$00,$00,$01,$20,$00,$00,$01,$20,$00           

    ; Upper-left pipe
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$54,$00,$2A           
    .byte $00,$55,$20,$2A,$00,$15,$20,$00,$00,$01,$20,$00,$00,$01,$20,$00           

    ; Upper-right pipe
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$55,$00,$0A,$00           
    .byte $55,$01,$2A,$00,$00,$01,$2A,$00,$00,$01,$20,$00,$00,$01,$20,$00           

    ; Lower-left pipe
    .byte $00,$01,$20,$00,$00,$01,$20,$00,$00,$15,$20,$00,$00,$55,$20,$2A           
    .byte $00,$54,$00,$2A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           

    ; Lower-right pipe
    .byte $00,$01,$20,$00,$00,$01,$20,$00,$00,$01,$2A,$00,$55,$01,$2A,$00           
    .byte $55,$00,$0A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           

    .res    32*(MAX_TILES-54)

.align 256

tileSheet_14x16:

    ; Grass
    .byte $22,$08,$22,$08,$44,$11,$44,$11,$22,$08,$22,$08,$44,$11,$44,$11           
    .byte $22,$08,$26,$08,$44,$11,$44,$11,$22,$08,$22,$08,$44,$11,$44,$11           
    .byte $22,$08,$22,$08,$44,$11,$44,$11,$22,$08,$22,$08,$44,$11,$44,$11           
    .byte $22,$08,$22,$08,$44,$11,$44,$11,$22,$08,$22,$08,$4C,$11,$44,$11           
    .byte $22,$08,$22,$08,$44,$11,$44,$11,$22,$08,$22,$08,$44,$11,$44,$11           
    .byte $22,$08,$22,$08,$44,$11,$44,$11,$22,$08,$22,$09,$44,$11,$44,$11           
    .byte $22,$08,$22,$08,$44,$11,$44,$11,$22,$08,$22,$08,$44,$11,$44,$11           
    .byte $22,$08,$22,$08,$44,$11,$44,$11,$22,$08,$22,$08,$44,$11,$44,$11           

    ; Bricks
    .byte $2A,$2A,$2A,$2A,$55,$55,$55,$55,$4C,$33,$4C,$33,$15,$66,$15,$66           
    .byte $4C,$33,$4C,$33,$15,$66,$15,$66,$4C,$33,$4C,$33,$15,$66,$15,$66           
    .byte $2A,$2A,$2A,$2A,$55,$55,$55,$55,$4A,$33,$4A,$33,$19,$66,$19,$66           
    .byte $4A,$33,$4A,$33,$19,$66,$19,$66,$4A,$33,$4A,$33,$19,$66,$19,$66           
    .byte $2A,$2A,$2A,$2A,$55,$55,$55,$55,$4C,$32,$4C,$32,$59,$66,$59,$66           
    .byte $4C,$32,$4C,$32,$59,$66,$59,$66,$4C,$32,$4C,$32,$59,$66,$59,$66           
    .byte $2A,$2A,$2A,$2A,$55,$55,$55,$55,$4C,$33,$4C,$33,$19,$56,$19,$56           
    .byte $4C,$33,$4C,$33,$19,$56,$19,$56,$4C,$33,$4C,$33,$19,$56,$19,$56           


    ; Tree
    .byte $22,$08,$22,$08,$44,$11,$44,$11,$22,$19,$66,$09,$44,$33,$4C,$11           
    .byte $22,$1D,$66,$19,$4C,$33,$4E,$11,$62,$59,$77,$19,$4C,$3B,$4C,$13           
    .byte $62,$19,$66,$1D,$4E,$33,$4C,$13,$62,$1D,$66,$19,$6C,$3B,$6E,$13           
    .byte $62,$19,$77,$19,$4C,$33,$4C,$13,$62,$19,$66,$1D,$6E,$33,$4C,$13           
    .byte $62,$19,$66,$19,$4C,$3B,$6C,$13,$22,$1D,$77,$19,$4C,$33,$4E,$11           
    .byte $22,$19,$66,$09,$44,$33,$4C,$11,$22,$08,$66,$08,$44,$31,$44,$11           
    .byte $22,$08,$44,$08,$44,$21,$44,$11,$22,$08,$44,$08,$44,$21,$44,$11           
    .byte $22,$08,$44,$08,$44,$21,$44,$11,$22,$08,$22,$08,$44,$11,$44,$11           


    ; Girl-1
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$6E,$00,$00,$77,$1D,$00           
    .byte $00,$38,$6E,$03,$00,$77,$5D,$00,$00,$78,$4D,$03,$00,$66,$5B,$00           
    .byte $00,$78,$1D,$03,$00,$0F,$5E,$00,$00,$78,$5D,$03,$00,$6E,$5B,$00           
    .byte $00,$3B,$5C,$3B,$40,$68,$41,$00,$00,$3B,$0B,$3B,$40,$00,$40,$00           
    .byte $00,$00,$3B,$00,$00,$58,$01,$00,$00,$40,$3B,$00,$00,$5E,$1B,$00           
    .byte $00,$34,$3B,$03,$00,$78,$21,$00,$00,$43,$3B,$34,$20,$5D,$17,$00           
    .byte $00,$43,$7B,$34,$20,$5D,$17,$00,$00,$40,$3B,$00,$00,$5D,$17,$00           
    .byte $00,$00,$50,$00,$00,$68,$01,$00,$00,$40,$70,$00,$00,$7F,$1F,$00           

    ; Girl-2
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$6E,$00,$00,$77,$1D,$00           
    .byte $00,$38,$6E,$03,$00,$77,$5D,$00,$00,$78,$5D,$03,$00,$6E,$5B,$00           
    .byte $00,$38,$4D,$03,$00,$66,$59,$00,$00,$78,$5D,$03,$00,$6E,$5B,$00           
    .byte $00,$3B,$5C,$3B,$40,$68,$41,$00,$00,$3B,$0B,$3B,$40,$00,$40,$00           
    .byte $00,$00,$3B,$00,$00,$58,$01,$00,$00,$40,$3B,$00,$00,$5E,$1B,$00           
    .byte $00,$34,$3B,$03,$00,$78,$21,$00,$00,$43,$3B,$34,$20,$5D,$17,$00           
    .byte $00,$43,$7B,$34,$20,$5D,$17,$00,$00,$40,$3B,$00,$00,$5D,$17,$00           
    .byte $00,$00,$50,$00,$00,$68,$01,$00,$00,$40,$70,$00,$00,$7F,$1F,$00           

    .res    128*(MAX_TILES-5)

end_of_program:
    .byte 0
