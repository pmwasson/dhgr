;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; DHGR Map Editor
;


MAP_CURSOR          =   MAX_TILES
MAP_X_OFFSET        =   10
MAP_Y_OFFSET        =   2
MAP_SCREEN_WIDTH    =   7
MAP_SCREEN_HEIGHT   =   7

MAP_WIDTH           =   64
MAP_HEIGHT          =   64

;------------------------------------------------
; Global scope (for map edit)
;------------------------------------------------
.proc map_edit
;------------------------------------------------

; Only called once
.proc init

    jsr     updateWorld
    jsr     getTile
    sta     cursorTile

    rts
.endproc

; Call to enter tool
.proc main

    bit     TXTCLR      ; Make sure displaying graphics

    ; display a greeting
    jsr     inline_print
    .byte   "DHGR map editor - ? for help",13,0

    jsr     clearScreen

reset_loop:

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
    ; A - select up
    ;------------------
    cmp     #KEY_A
    bne     :+
    jsr     inline_print
    .byte   "Select up 1",13,0
    lda     #256-1
    jmp     finish_select
:

    ;------------------
    ; A - select up more
    ;------------------
    cmp     #KEY_CTRL_A
    bne     :+
    jsr     inline_print
    .byte   "Select up 5",13,0
    lda     #256-5
    jmp     finish_select
:

    ;------------------
    ; Z - select down
    ;------------------
    cmp     #KEY_Z
    bne     :+
    jsr     inline_print
    .byte   "Select down 1",13,0
    lda     #1
    jmp     finish_select
:

    ;------------------
    ; Z - select down 5
    ;------------------
    cmp     #KEY_CTRL_Z
    bne     :+
    jsr     inline_print
    .byte   "Select down 5",13,0
    lda     #5
    jmp     finish_select
:

    ;------------------
    ; SP = Set tile
    ;------------------
    cmp     #KEY_SPACE
    bne     :+
    jsr     inline_print
    .byte   "Set tile to ",0
    lda     selectTile
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    lda     selectTile
    jsr     setTile
    jmp     command_loop
:

    ;------------------
    ; 0..9
    ; Quick bar set tile
    ;------------------
    ; if less than 0 or greater than 9, skip ahead
    cmp     #KEY_0
    bmi     :+
    cmp     #KEY_9+1
    bpl     :+
    ; zero upper bits
    and     #$f
    bne     key_not_zero
    lda     #10
key_not_zero:
    sec
    sbc     #1
    tax
    lda     quickBar,x
    sta     cursorTile      ; its going to get overwritten    
    jsr     inline_print
    .byte   "Set tile to ",0
    lda     cursorTile
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    lda     cursorTile
    jsr     setTile
    jmp     command_loop
:

    ;------------------
    ; RIGHT (arrow)
    ;------------------
    cmp     #KEY_RIGHT
    bne     :+
    jsr     inline_print
    .byte   "Right ",0
    lda     curX
    cmp     #MAP_SCREEN_WIDTH-1
    beq     pan_right
    inc     curX
    jmp     finish_move
pan_right:
    lda     mapX
    cmp     #MAP_WIDTH-MAP_SCREEN_WIDTH
    beq     move_fail
    inc     mapX
    jmp     finish_pan
:

    ;------------------
    ; LEFT (arrow)
    ;------------------
    cmp     #KEY_LEFT
    bne     :+
    jsr     inline_print
    .byte   "Left  ",0
    lda     curX
    beq     pan_left
    dec     curX
    jmp     finish_move
pan_left:
    lda     mapX
    beq     move_fail
    dec     mapX
    jmp     finish_pan
move_fail:
    jsr     sound_click
    jmp     finish_move
:
    ;------------------
    ; UP (arrow)
    ;------------------
    cmp     #KEY_UP
    bne     :+
    jsr     inline_print
    .byte   "Up    ",0
    lda     curY
    beq     pan_up
    dec     curY
    jmp     finish_move
pan_up:
    lda     mapY
    beq     move_fail
    dec     mapY
    jmp     finish_pan
:
    ;------------------
    ; DOWN (arrow)
    ;------------------
    cmp     #KEY_DOWN
    bne     :+
    jsr     inline_print
    .byte   "Down  ",0
    lda     curY
    cmp     #MAP_SCREEN_HEIGHT-1
    beq     pan_down
    inc     curY
    jmp     finish_move
pan_down:
    lda     mapY
    cmp     #MAP_HEIGHT-MAP_SCREEN_HEIGHT
    beq     move_fail
    inc     mapY
    jmp     finish_pan
:

    ;------------------
    ; ^P = Printer dump
    ;------------------
    cmp     #KEY_CTRL_P
    bne     :+
    bit     TXTSET
    jsr     inline_print
    .byte   "Dump Map to printer ",13,13,0
    jsr     printerDump
    jmp     command_loop
:

    ;------------------
    ; ^Q = QUIT
    ;------------------
    cmp     #KEY_CTRL_Q
    bne     :+
    jsr     inline_print
    .byte   "Quit",13,0
    bit     TXTSET
    jmp     quit
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

    ;-------------------
    ; TAB = Switch tool
    ;-------------------
    cmp     #KEY_TAB
    bne     :+
    jsr     inline_print
    .byte   "Switch tool",13,0
    rts     ; return to main

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
    ; Set quick bar
    ; !@#$%^&*()
    ;------------------
    cmp     #$80 | '!'
    bne     :+
    ldx     #0
    jmp     set_key
:
    cmp     #$80 | '@'
    bne     :+
    ldx     #1
    jmp     set_key
:
    cmp     #$80 | '#'
    bne     :+
    ldx     #2
    jmp     set_key
:
    cmp     #$80 | '$'
    bne     :+
    ldx     #3
    jmp     set_key
:
    cmp     #$80 | '%'
    bne     :+
    ldx     #4
    jmp     set_key
:
    cmp     #$80 | '^'
    bne     :+
    ldx     #5
    jmp     set_key
:
    cmp     #$80 | '&'
    bne     :+
    ldx     #6
    jmp     set_key
:
    cmp     #$80 | '*'
    bne     :+
    ldx     #7
    jmp     set_key
:
    cmp     #$80 | '('
    bne     :+
    ldx     #8
    jmp     set_key
:
    cmp     #$80 | ')'
    bne     :+
    ldx     #9
    jmp     set_key
:

    ;------------------
    ; Unknown
    ;------------------
    jsr     inline_print
    .byte   "Unknown command (? for help)",13,0
    jmp     command_loop

; jump to to change key
set_key:
    lda     selectTile
    sta     quickBar,x
    jsr     drawQuickBar
    jmp     command_loop


; jump to after changing select
; amount to move select in A
finish_select:
    clc
    adc     selectOffset
    and     #MAX_TILES-1
    sta     selectOffset
    jsr     drawSelectBar
    jmp     command_loop

finish_pan:
    jsr     updateWorld
    jsr     drawMap
    jmp     finish_move2

; jump to after changing coordinates
finish_move:
    ; update world coordinates
    jsr     updateWorld

finish_move2:    
    jsr     inline_print
    .byte   "X/Y:",0

    ; calc cursor position as part of print
    lda     worldX
    jsr     PRBYTE
    lda     #$80 + ','
    jsr     COUT
    lda     worldY
    jsr     PRBYTE
    jsr     inline_print
    .byte   " Tile:",0
    jsr     getTile
    sta     cursorTile
    jsr     PRBYTE
    lda     #13
    jsr     COUT
    jmp     command_loop

.endproc

;-----------------------------------------------------------------------------
; Update world
;   Set world coordinates
;-----------------------------------------------------------------------------
.proc updateWorld
    lda     mapX
    clc
    adc     curX
    sta     worldX
    lda     mapY
    clc
    adc     curY
    sta     worldY
    rts
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
; printHelp
;-----------------------------------------------------------------------------
.proc printHelp
    bit     TXTSET
    jsr     inline_print
    .byte   "  Arrows:  Move cursor",13
    .byte   "  Space:      Set location of cursor to selected tile",13
    .byte   "  0..9:       Set location of cursor to quick-bar tile",13
    .byte   "  Shift-0..9: Assign quick-bar to selected tile",13
    .byte   "  A,Z:        Scroll tile selection by 1",13
    .byte   "  Ctrl-A,Z:   Scroll tile selection by 5",13
    .byte   "  Ctrl-P:     Print map to output (printer)",13
    .byte   "              (Do a 1^P in monitor first!)",13
    .byte   "  Tab:        Switch tool",13
    .byte   "  ?:          This help screen",13
    .byte   "  \:          Monitor",13
    .byte   "  Ctrl-Q:     Quit",13
    .byte   "  Escape:     Toggle text/graphics",13
    .byte   0

    rts
.endproc

;-----------------------------------------------------------------------------
; Draw screen
;
;   Redraw screen
;-----------------------------------------------------------------------------

.proc drawScreen

    ;----------------
    ; Static content
    ;----------------
    ; Map box
    lda     #1
    sta     boxTop
    lda     #16
    sta     boxBottom
    lda     #8
    sta     boxLeft
    lda     #38
    sta     boxRight
    jsr     drawBox

    ; Select outline
    jsr     drawSelectBox

    ;----------------
    ; Dynamic content
    ;----------------
    jsr     drawSelectBar
    jsr     drawQuickBar
    jsr     drawMap

    rts
.endproc

.proc drawSelectBox
    lda     #4
    sta     index

    lda     #BOX_RIGHT_TEE
    sta     shape

loop:
    lda     index
    asl     ;*2
    sta     tileX
    lda     #5
    sta     tileY
    lda     shape
    jsr     drawInterfaceTile_7x8
    lda     #8
    sta     tileY
    lda     shape
    jsr     drawInterfaceTile_7x8

    lda     #BOX_HORZ
    sta     shape

    dec     index
    bpl     loop

    rts

index:      .byte   0
shape:      .byte   0

.endproc

;-----------------------------------------------------------------------------
; Draw Select Bar
;
;-----------------------------------------------------------------------------

.proc drawSelectBar
    lda     selectOffset
    sta     index

    lda     #0
    sta     tileX
    lda     #0
    sta     tileY
    jsr     drawNumberedTile
    jsr     incIndex
    jsr     drawNumberedTile
    jsr     incIndex

    sta     selectTile
    jsr     drawSelectedTile
    jsr     incIndex

    jsr     drawNumberedTile
    jsr     incIndex
    jsr     drawNumberedTile
    jsr     incIndex
    jsr     drawNumberedTile
    jsr     incIndex

    rts

incIndex:
    lda     index
    clc
    adc     #1
    and     #MAX_TILES-1
    sta     index
    rts


drawNumberedTile:
    lda     index
    jsr     drawTileNumber
    inc     tileX
    inc     tileX
    lda     index
    jsr     drawTile_14x16
    lda     tileX
    sec
    sbc     #4
    sta     tileX
    inc     tileY
    inc     tileY
    rts

drawSelectedTile:
    inc     tileY
    inc     tileY
    lda     index
    jsr     drawTileNumberSelected
    inc     tileX
    inc     tileX
    lda     index
    jsr     drawTile_14x16
    lda     tileX
    sec
    sbc     #4
    sta     tileX
    lda     tileY
    clc
    adc     #4
    sta     tileY
    rts

index:      .byte   0

.endproc


;-----------------------------------------------------------------------------
; Draw Quick Bar
;
;-----------------------------------------------------------------------------

.proc drawQuickBar

    lda     #0
    sta     index

loop:
    lda     index
    asl     ;
    asl     ; *4
    sta     tileX

    lda     #17
    sta     tileY

    ldx     index
    lda     quickBar,x
    jsr     drawTile_14x16

    inc     tileX

    lda     #19
    sta     tileY

    lda     index
    clc
    adc     #1
    cmp     #10
    bne     :+
    lda     #0
:
    jsr     drawInterfaceTile_7x8

    inc     index
    lda     index
    cmp     #10
    bne     loop

    rts

index:      .byte   0

.endproc


;-----------------------------------------------------------------------------
; Draw Map
;
;-----------------------------------------------------------------------------

.proc drawMap

    lda     #0
    sta     indexY

loop_y:

    ; set pointer
    lda     mapY
    clc
    adc     indexY
    sta     temp
    lsr
    lsr     ; /4
    clc
    adc     #>MAPSHEET
    sta     mapPtr1

    lda     temp
    asl
    asl
    asl
    asl
    asl
    asl
    sta     mapPtr0     ; assume 256 aligned

    lda     #0
    sta     indexX

    lda     indexY
    asl
    adc     #MAP_Y_OFFSET
    sta     tileY

loop_x:

    lda     indexX
    asl
    asl     ;*4
    adc     #MAP_X_OFFSET
    sta     tileX

    lda     mapX
    clc
    adc     indexX
    tay
    lda     (mapPtr0),y
    jsr     drawTile_14x16

    inc     indexX
    lda     indexX
    cmp     #MAP_SCREEN_WIDTH   
    bne     loop_x

    inc     indexY
    lda     indexY
    cmp     #MAP_SCREEN_HEIGHT
    bne     loop_y

    rts

temp:       .byte   0
indexX:     .byte   0
indexY:     .byte   0

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
; getInput
;   Blink cursors and wait for keypress
;   Return key in A (upper bit set)
;-----------------------------------------------------------------------------
.proc getInput

    ; calc tile cordinates once

    lda     curX
    asl
    asl     ;*4
    adc     #MAP_X_OFFSET
    sta     tileX

    lda     curY
    asl
    adc     #MAP_Y_OFFSET
    sta     tileY

cursor_loop:
    ; Display cursor
    lda     #$FF
    jsr     COUT

    lda     #MAP_CURSOR
    jsr     drawTile_14x16

    ; Wait (on)
    jsr     wait

    ; Restore
    lda     #$88        ; backspace
    jsr     COUT
    lda     #$A0        ; space
    jsr     COUT
    lda     #$88        ; backspace
    jsr     COUT

    lda     cursorTile
    jsr     drawTile_14x16

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
; setWorldMapPtr
;   Set map pointer based on world X,Y
;-----------------------------------------------------------------------------

.proc setWorldMapPtr

    lda     worldY
    lsr
    lsr     ; /4
    clc
    adc     #>MAPSHEET
    sta     mapPtr1

    lda     worldY
    asl
    asl
    asl
    asl
    asl
    asl
    sta     mapPtr0     ; assume 256 aligned
    rts

.endproc

;-----------------------------------------------------------------------------
; getTile
;   Return tile byte at world coordinates
;-----------------------------------------------------------------------------

.proc getTile

    jsr     setWorldMapPtr
    ldy     worldX
    lda     (mapPtr0),y
    rts

.endproc

;-----------------------------------------------------------------------------
; setTile
;   Set map tile to A
;-----------------------------------------------------------------------------

.proc setTile
    sta     temp
    jsr     setWorldMapPtr
    ldy     worldX
    lda     temp
    sta     (mapPtr0),y
    sta     cursorTile      ; update cursor
    rts

temp:   .byte   0

.endproc


;-----------------------------------------------------------------------------
; printerDump
;
;  Dump map to printer
;-----------------------------------------------------------------------------
.proc printerDump

    bit     TXTSET

    ;jsr     $c100       ; connect output to printer

    lda     #0
    sta     rowCount

    ; set map ptr
    sta     mapPtr0         ; zero
    lda     #>MAPSHEET
    sta     mapPtr1         ; page

print_loop:
    jsr     inline_print
    .byte   ";row ",0
    lda     rowCount
    jsr     PRBYTE

    jsr     printRow

    lda     mapPtr0
    clc
    adc     #MAP_WIDTH
    bne     :+
    inc     mapPtr1
:
    inc     rowCount
    lda     rowCount
    cmp     #MAP_HEIGHT
    bne     print_loop

    ;jsr     $c300       ; recoonect output?

    rts

rowCount:   .byte   0

printRow:
    jsr     inline_print
    .byte   13,".byte ",0

    lda     #0
    sta     dumpCount
    jmp     dump_loop
dump_comma:
    lda     #$80 + ','
    jsr     COUT
dump_loop:
    lda     #$80 + '$'
    jsr     COUT
    ldy     dumpCount
    lda     (mapPtr0),y
    jsr     PRBYTE
    inc     dumpCount
    lda     dumpCount
    cmp     #MAP_WIDTH
    beq     dump_finish
    lda     dumpCount
    and     #$f
    bne     dump_comma
    jsr     inline_print
    .byte   13,".byte ",0
    jmp     dump_loop

dump_finish:
    lda     #13
    jsr     COUT
    rts

dumpCount: .byte   0

.endproc

;-----------------------------------------------------------------------------
; Global Variables
;-----------------------------------------------------------------------------

; map = world offset
mapX:               .byte   0
mapY:               .byte   0

; cur = offset on screen
curX:               .byte   3       ; start in middle of the screen
curY:               .byte   3
cursorTile:         .byte   0

; word = map + cur
worldX:             .byte   0
worldY:             .byte   0

selectOffset:       .byte   0
selectTile:         .byte   0

height:             .byte   64
height_m1:          .byte   63
width:              .byte   64
width_m1:           .byte   63

; Saved indexes
quickBar:           .byte   0,1,2,3,4,5,6,7,8,9

;------------------------------------------------
; Global scope (for tile edit)
;------------------------------------------------
.endproc
;------------------------------------------------
