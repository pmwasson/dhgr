;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; DHGR Tile Editor
;

;------------------------------------------------
; Global scope (for tile edit)
;------------------------------------------------
.proc tile_edit
;------------------------------------------------

; Only called once
.proc init
    ; default size
    jsr     setSize_14x16
    rts
.endproc

; Call to enter tool
.proc main

    bit     TXTCLR      ; Make sure displaying graphics

    ; display a greeting
    jsr     inline_print
    .byte   "DHGR tile editor - ? for help",13,0

    jsr     clearScreen

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
    ; ^C = Copy Tile
    ;------------------
    cmp     #KEY_CTRL_C
    bne     :+
    jsr     inline_print
    .byte   "Copy tile to clipboard",13,0
    jsr     copyTile
    jmp     command_loop
:

    ;------------------
    ; ^V = Paste Tile
    ;------------------
    cmp     #KEY_CTRL_V
    bne     :+
    jsr     inline_print
    .byte   "Paste tile from clipboard",13,0
    jsr     pasteTile
    jsr     updateAll
    jmp     reset_loop
:

    ;------------------
    ; ^A = Add Tile
    ;------------------
    cmp     #KEY_CTRL_A
    bne     :+
    jsr     inline_print
    .byte   "Add clipboard to tile",13,0
    jsr     addTile
    jsr     updateAll
    jmp     reset_loop
:

    ;------------------
    ; ^F = Fill Color
    ;------------------
    cmp     #KEY_CTRL_F
    bne     :+
    jsr     inline_print


    .byte   "Fill Color. Pick color (or cancel):",0
    jsr     getInput
    jsr     getInputColor
    cmp     #$80
    beq     fill_cancel
    jsr     fillPixels
    jsr     updateAll
    jmp     reset_loop
fill_cancel:
    jsr     inline_print
    .byte   "Cancel",13,0
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
    jmp     reset_loop

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
    ; ^L = Load
    ;------------------
    cmp     #KEY_CTRL_L
    bne     :+
    jsr     inline_print
    .byte   "Read slot (0-7):",0
    lda     #8
    jsr     getInputNumber
    bmi     load_exit
    jsr     loadSheet

    ; redraw the screen
    jmp     reset_loop

load_exit:
    jmp     command_loop
:    

    ;------------------
    ; ^S = Save
    ;------------------
    cmp     #KEY_CTRL_S
    bne     :+
    jsr     inline_print
    .byte   "Save slot (0-7):",0
    lda     #8
    jsr     getInputNumber
    bmi     save_exit
    jsr     saveSheet

save_exit:
    jmp     command_loop
: 

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
    cmp     #KEY_CTRL_Q
    bne     :+
    jsr     inline_print
    .byte   "Quit",13,0
    bit     TXTSET
    jmp     quit
:

    ;------------------
    ; \ = Monitor
    ;------------------
    cmp     #$80 | '\'
    bne     :+
    jsr     inline_print
    .byte   "Monitor",13,"(enter CTRL-Y to return)",13,0

    jsr     inline_print
    .byte   "Tilesheet address = $",0
    ldx     #<TILESHEET
    ldy     #>TILESHEET
    jsr     PRINTXY
    lda     #13
    jsr     COUT

    ; Set ctrl-y vector
    lda     #$4c        ; JMP
    sta     $3f8
    lda     #<main
    sta     $3f9
    lda     #>main
    sta     $3fa

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
    ; ^E = Exchange colors
    ;------------------
    cmp     #KEY_CTRL_E
    bne     :+
    jsr     inline_print
    .byte   "Exchange colors",13,"First color (or cancel):",0
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

    ;-------------------
    ; TAB = Switch tool
    ;-------------------
    cmp     #KEY_TAB
    bne     :+
    jsr     inline_print
    .byte   "Switch tool",13,0
    rts     ; return to main

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
; getInputNumber
;   Get input for a number 0..max+1, where A == max+1
;   Display number or cancel and return result in A (-1 for cancel)
;-----------------------------------------------------------------------------
.proc getInputNumber
    clc
    adc     #$80 + '0'  ; convert A to ascii number
    sta     max_digit     
    jsr     getInput
    cmp     #$80 + '0'
    bmi     cancel
    cmp     max_digit
    bpl     cancel
    jsr     COUT
    sec
    sbc     #$80 + '0'
    rts
cancel:
    jsr     inline_print
    .byte   "Cancel",13,0
    lda     #$ff
    rts

; local variable
max_digit:  .byte   0

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
    .byte   "  Ctrl-F:  Fill tile with specified color (overwrites current tile)",13
    .byte   "  Ctrl-C:  Copy tile to clipboard",13
    .byte   "  Ctrl-V:  Paste tile from clipboard (overwrites current tile)",13
    .byte   "  Ctrl-A:  Add clipboard to current tile (black pixels ignored)",13
    .byte   "  Ctrl-E:  Exchange 2 specified colors",13
    .byte   "  Ctrl-D:  Directional Mirror: mirror tile in an arrow key direction",13
    .byte   "  Ctrl-R:  Rotate pixels in a direction specified by an arrow key",13
    .byte   "  Ctrl-T:  Toggle between 7x8 and 14x16 tile size",13
    .byte   "  Ctrl-B:  Toggle between color and binary mode",13
    .byte   "  -,=:     Go to previous/next tile (holding shift moves 8 tile)",13
    .byte   "  Ctrl-L:  Load tile set",13
    .byte   "  Ctrl-S:  Save tile set",13
    .byte   "  !:       Dump bytes",13
    .byte   "  Tab:     Switch tool",13
    .byte   "  ?:       This help screen",13
    .byte   "  \:       Monitor",13
    .byte   "  Ctrl-Q:  Quit",13
    .byte   "  Escape:  Toggle text/graphics",13
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
; Copy tile
;  Copy pixels to clipboard
;-----------------------------------------------------------------------------
.proc copyTile

    lda     size
    beq     small

    ; for large, copy all bytes

    ldy     #0
:
    lda     pixelData,y
    sta     clipboardData,y
    iny
    bne     :-

    rts

small:
    ; for the smaller tile set unused bytes to zero

    ldy     #0

loop0:    
    ldx     #0

loop1:
    lda     pixelData,y
    sta     clipboardData,y
    iny
    inx
    cpx     #7
    bne     loop1

    lda     #0
loop2:
    sta     clipboardData,y
    iny
    inx
    cpx     #16
    bne     loop2

    cpy     #8*16   ; height * 16
    bne     loop0

    ; fill rest with zeros
:
    sta     clipboardData,y
    iny
    bne     :-

    rts
.endproc

;-----------------------------------------------------------------------------
; Paste tile
;  Copy pixels from clipboard
;-----------------------------------------------------------------------------
.proc pasteTile
    ldy     #0
:
    lda     clipboardData,y
    sta     pixelData,y
    iny
    bne     :-
    rts
.endproc

;-----------------------------------------------------------------------------
; Add tile
;  Copy non-black pixels from clipboard
;-----------------------------------------------------------------------------
.proc addTile
    ldy     #0
loop:
    lda     clipboardData,y
    beq     :+
    sta     pixelData,y
:
    iny
    bne     loop
    rts
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
;   Set all pixel to A
;-----------------------------------------------------------------------------

.proc fillPixels

    ldx     #0
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
    jsr     drawInterfaceTile_7x8

    inc     tileY

    lda     paintColor
    and     #$0F
    cmp     colorX
    bne     :+

    lda     #$FF
    sta     invMask

:
    lda     colorX
    jsr     drawInterfaceTile_7x8

    lda     #0
    sta     invMask

    inc     colorX
    lda     colorX
    cmp     #16
    bne     loopx

    lda     #18
    sta     tileY
    lda     #34
    sta     tileX
    lda     tileIndex
    jsr     drawTileNumber

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
    jsr     drawInterfaceTile_7x8

    rts

.endproc

;-----------------------------------------------------------------------------
; getInput
;   Blink cursors and wait for keypress
;   Return key in A (upper bit set)
;-----------------------------------------------------------------------------
.proc getInput

cursor_loop:
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
; Load sheet
;
;   Load sheet using ProDOS
;-----------------------------------------------------------------------------
.proc loadSheet

    ; set filename
    clc
    adc     #'0'
    sta     pathname_end-1

    lda     #13
    jsr     COUT

    ; open file
    jsr     MLI
    .byte   CMD_OPEN
    .word   open_params
    bcc     :+

    jsr    PRBYTE
    jsr    inline_print
    .byte  ":unable to open file",13,0
    rts
:
    ;jsr    inline_print
    ;.byte  "File open",13,0

    ; set reference number 
    lda     open_params+5
    sta     read_params+1
    sta     close_params+1

    ; read data
    jsr    MLI
    .byte  CMD_READ
    .word  read_params
    bcc    :+

    jsr    PRBYTE
    jsr    inline_print
    .byte  ":unable to read data",13,0
:
    ;jsr    inline_print
    ;.byte  "Data read",13,0

    jsr    MLI
    .byte  CMD_CLOSE
    .word  close_params
    bcc    :+
    jsr    PRBYTE
    jsr    inline_print
    .byte  ":unable to close file",13,0
:
    jsr    inline_print
    .byte  "Load complete",13,0

    rts
    
open_params:
    .byte   $3
    .word   pathname      
    .word   FILEBUFFER
    .byte   $0                  ; reference number

read_params:
    .byte   $4
    .byte   $0                  ; reference number
    .word   TILESHEET           ; address of data buffer
    .word   TILESHEET_SIZE      ; number of bytes to read
    .word   $0                  ; number of bytes read

close_params:
    .byte   $1
    .byte   $0                  ; reference number

.endproc

; Use a common pathname for load and store

pathname:
    .byte   14,"/DHGR/TILESET0"
pathname_end:


;-----------------------------------------------------------------------------
; Save sheet
;
;   Use prodos to save tile data
;-----------------------------------------------------------------------------
.proc saveSheet

    ; set filename
    clc
    adc     #'0'
    sta     pathname_end-1

    lda     #13
    jsr     COUT

    ; open file
    jsr     MLI
    .byte   CMD_OPEN
    .word   open_params
    bcc     open_good
    
    jsr    PRBYTE
    jsr    inline_print
    .byte  ":unable to open file, creating new",13,0

    ; create file
     jsr     MLI
    .byte   CMD_CREATE
    .word   create_params
    bcc     :+   

    jsr    PRBYTE
    jsr    inline_print
    .byte  ":unable to create file",13,0
    rts    ; give up!
:

    ; open file again!
    jsr     MLI
    .byte   CMD_OPEN
    .word   open_params
    bcc     open_good

    jsr    PRBYTE
    jsr    inline_print
    .byte  ":still unable to open file",13,0
    rts    ; give up

open_good:
    ;jsr    inline_print
    ;.byte  "File open",13,0

    ; set reference number 
    lda     open_params+5
    sta     write_params+1
    sta     close_params+1

    ; write data
    jsr    MLI
    .byte  CMD_WRITE
    .word  write_params
    bcc    :+
    jsr    PRBYTE
    jsr    inline_print
    .byte  ":unable to write data",13,0
:
    ;jsr    inline_print
    ;.byte  "Data written",13,0

    jsr    MLI
    .byte  CMD_CLOSE
    .word  close_params
    bcc    :+
    jsr    PRBYTE
    jsr    inline_print
    .byte  ":unable to close file",13,0
:
    jsr    inline_print
    .byte  "Save complete",13,0

    rts
    
open_params:
    .byte   $3
    .word   pathname      
    .word   FILEBUFFER
    .byte   $0                  ; reference number

create_params:
    .byte   $7
    .word   pathname
    .byte   $C3                 ; access bits (full access)
    .byte   $6                  ; file type (binary)
    .word   TILESHEET
    .byte   $1                  ; storage type (standard)
    .word   $0                  ; creation date
    .word   $0                  ; creation time

write_params:
    .byte   $4
    .byte   $0                  ; reference number
    .word   TILESHEET           ; address of data buffer
    .word   TILESHEET_SIZE      ; number of bytes to write
    .word   $0                  ; number of bytes written

close_params:
    .byte   $1
    .byte   $0                  ; reference number

.endproc


;-----------------------------------------------------------------------------
; Global Variables
;-----------------------------------------------------------------------------

color:              .byte   0
curX:               .byte   0
curY:               .byte   0
tileIndex:          .byte   0

paintColor:         .byte   $FF
colorMode:          .byte   $10     ; $10 = color, $20 = B&W
    
swapColor1:         .byte   $0
swapColor2:         .byte   $0
    
; Make dimensions a variable incase we want variable tile size
size:               .byte   0   ; 0=7x8, 1=14x16
width:              .byte   0
width_m1:           .byte   0
height:             .byte   0
height_m1:          .byte   0
height_x16:         .byte   0
length:             .byte   0
    
; Conversion from pixels to bytes
pixelOffset:        .byte   0
tileOffset:         .byte   0
    
pixelByte0:         .byte   0
pixelByte1:         .byte   0
pixelByte2:         .byte   0
pixelByte3:         .byte   0

; Just store 1 pixel per byte and pad out to power of 2
.align      256
pixelData:
    .res    16*16

clipboardData:
    .res    16*16

; Lookup tables
;-----------------------------------------------------------------------------

.align 16

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

;------------------------------------------------
; Global scope (for tile edit)
;------------------------------------------------
.endproc
;------------------------------------------------
