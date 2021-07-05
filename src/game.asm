;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; DHGR Toolbox -- Play
;  Game engine for DHGR maps and tiles

;------------------------------------------------
; Constants
;------------------------------------------------

.include "defines.asm"
.include "macros.asm"

;------------------------------------------------
; Constants
;------------------------------------------------

BOX_HORZ        = $40+0
BOX_VERT        = $40+1
BOX_UPPER_LEFT  = $40+2
BOX_UPPER_RIGHT = $40+3
BOX_LOWER_LEFT  = $40+4
BOX_LOWER_RIGHT = $40+5
BOX_RIGHT_TEE   = $40+6

DIALOG_UL       = $40+7
DIALOG_UP       = $40+8
DIALOG_UR       = $40+9
DIALOG_LF       = $40+10
DIALOG_RT       = $40+11
DIALOG_LL       = $40+12
DIALOG_LO       = $40+13
DIALOG_LR       = $40+14
DIALOG_RS1      = $40+15
DIALOG_RS2      = $40+16
DIALOG_LS1      = $40+17
DIALOG_LS2      = $40+18

MAP_WIDTH           =  64
MAP_HEIGHT          =  64
MAP_X_OFFSET        =   6
MAP_Y_OFFSET        =   6   ; must be even
MAP_SCREEN_WIDTH    =   7
MAP_SCREEN_HEIGHT   =   7
MAP_CENTER          =   (3*7)+3

;------------------------------------------------

.segment "CODE"
.org    $6000

;=============================================================================
; Main program
;=============================================================================

; Main
;------------------------------------------------
.proc main

    jsr     dhgrInit    ; Turn on dhgr

    lda     #$00        ; Clear both pages
    sta     drawPage        
    jsr     clearScreen
    jsr     drawFrame

    lda     #$20        ; Clear both pages
    sta     drawPage        
    jsr     clearScreen
    jsr     drawFrame

    lda     #$00
    sta     drawPage


    ; set initial coordinates
    lda     #(64-7)/2
    sta     mapWindowX
    sta     mapWindowY
    jsr     DHGR_READ_MAP


    ; set up dialog

    lda     #0
    sta     dialogTop

    lda     #2
    sta     dialogLeft

    lda     #26
    sta     dialogRight

    lda     #6
    sta     dialogX

    lda     #0
    sta     dialogDir

    lda     #<dialog
    sta     stringPtr0
    lda     #>dialog
    sta     stringPtr1
    jsr     drawDialog

    lda     #$20
    sta     drawPage

    lda     #<dialog
    sta     stringPtr0
    lda     #>dialog
    sta     stringPtr1
    jsr     drawDialog


gameLoop:

    inc     gameTime
    lda     gameTime
    lsr
    sta     animateTime

    jsr     drawScreen

    lda     KBD
    bpl     gameLoop
    sta     KBDSTRB

    cmp     #KEY_ESC
    bne     :+

    ; Set ctrl-y vector
    lda     #$4c        ; JMP
    sta     $3f8
    lda     #<quit
    sta     $3f9
    lda     #>quit
    sta     $3fa

    bit     TXTSET
    jmp     MONZ        ; enter monitor
:

    cmp     #KEY_UP
    bne     :+
    lda     mapWindowY
    beq     done_up
    dec     mapWindowY
done_up:
    jmp     gameLoop

:
 
    cmp     #KEY_DOWN
    bne     :+
    lda     mapWindowY
    cmp     #MAP_HEIGHT-MAP_SCREEN_HEIGHT
    beq     done_down
    inc     mapWindowY
done_down:
    jmp     gameLoop
:

    cmp     #KEY_LEFT
    bne     :+
    lda     #1
    sta     fgTile
    lda     mapWindowX
    beq     done_left
    dec     mapWindowX
done_left:
    jmp     gameLoop
:

    cmp     #KEY_RIGHT
    bne     :+
    lda     #0
    sta     fgTile
    lda     mapWindowX
    cmp     #MAP_WIDTH-MAP_SCREEN_WIDTH
    beq     done_right
    inc     mapWindowX
done_right:
    jmp     gameLoop
:

    jmp     gameLoop


dialog:     .byte   "DIALOG!",0

.endproc

;------------------------------------------------
; Draw Frame
;------------------------------------------------

.proc drawFrame

    ; Draw title box
    lda     #MAP_X_OFFSET-2
    sta     boxLeft
    lda     #MAP_X_OFFSET+MAP_SCREEN_WIDTH*4
    sta     boxRight
    lda     #MAP_Y_OFFSET-1
    sta     boxTop
    lda     #MAP_Y_OFFSET+MAP_SCREEN_HEIGHT*2
    sta     boxBottom
    jsr     drawBox

    rts   

.endproc

;-----------------------------------------------------------------------------
; drawScreen
;-----------------------------------------------------------------------------

.proc drawScreen

    ; Alternate page to draw
    ;-------------------------------------------------------------------------
    lda     #$00    ; if showing page 2, draw on page 1
    ldx     PAGE2
    bmi     pageSelect
    lda     #$20    ; displaying page 1, draw on page 2
pageSelect:
    sta     drawPage


    ; Draw map
    ;-------------------------------------------------------------------------

    jsr     drawMap


    ; Set display page
    ;-------------------------------------------------------------------------

flipPage:
    ; flip page
    ldx     PAGE2
    bmi     flipToPage1
    sta     HISCR           ; display page 2
    rts

flipToPage1:
    sta     LOWSCR          ; diaplay page 1
    rts

.endproc

;-----------------------------------------------------------------------------
; Draw box
;
;-----------------------------------------------------------------------------

.proc drawBox

    ; Draw corners
    lda     boxLeft
    sta     tileX
    lda     boxTop
    sta     tileY
    lda     #BOX_UPPER_LEFT
    sta     bgTile
    jsr     DHGR_DRAW_7X8

    lda     boxRight
    sta     tileX    
    lda     #BOX_UPPER_RIGHT
    sta     bgTile
    jsr     DHGR_DRAW_7X8

    lda     boxBottom
    sta     tileY
    lda     #BOX_LOWER_RIGHT
    sta     bgTile
    jsr     DHGR_DRAW_7X8

    lda     boxLeft
    sta     tileX
    lda     #BOX_LOWER_LEFT
    sta     bgTile
    jsr     DHGR_DRAW_7X8

    ; Draw horizontal

    inc     tileX
    inc     tileX
:
    lda     boxTop
    sta     tileY
    lda     #BOX_HORZ
    sta     bgTile
    jsr     DHGR_DRAW_7X8
    
    lda     boxBottom
    sta     tileY
    lda     #BOX_HORZ
    sta     bgTile
    jsr     DHGR_DRAW_7X8
    
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
    sta     bgTile
    jsr     DHGR_DRAW_7X8
    
    lda     boxRight
    sta     tileX
    lda     #BOX_VERT
    sta     bgTile
    jsr     DHGR_DRAW_7X8
    
    inc     tileY
    lda     boxBottom
    cmp     tileY
    bne     :-

    rts

.endproc

;-----------------------------------------------------------------------------
; Draw dialog
;
;-----------------------------------------------------------------------------

.proc drawDialog

    lda     #0
    sta     stringIndex

    lda     dialogTop
    sta     tileY

    ; Upper-left
    lda     dialogLeft
    sta     tileX
    lda     #DIALOG_UL
    sta     bgTile
    jsr     DHGR_DRAW_7X8

    ; Top
    inc     tileX
    inc     tileX
:
    lda     #DIALOG_UP
    sta     bgTile
    jsr     DHGR_DRAW_7X8
    
    inc     tileX
    inc     tileX
    lda     tileX
    cmp     dialogRight
    bne     :-

    ; Upper-right
    lda     #DIALOG_UR
    sta     bgTile
    jsr     DHGR_DRAW_7X8

    ; Dialog Row

dialog_row:

    inc     tileY
    lda     dialogLeft
    sta     tileX

    ; left - edge
    lda     #DIALOG_LF
    sta     bgTile
    jsr     DHGR_DRAW_7X8

    inc     tileX
    inc     tileX

dialog_string:
    ldy     stringIndex
    lda     (stringPtr0),y
    beq     next_row
    cmp     #13
    beq     next_row

    and     #$3f
    sta     bgTile
    jsr     DHGR_DRAW_7X8
    inc     tileX
    inc     tileX
    inc     stringIndex
    jmp     dialog_string

finish_row:

    lda     #32     ; space
    sta     bgTile
    jsr     DHGR_DRAW_7X8
    inc     tileX 
    inc     tileX 

next_row:

    lda     tileX
    cmp     dialogRight
    bne     finish_row

    lda     #DIALOG_RT
    sta     bgTile
    jsr     DHGR_DRAW_7X8

    ldy     stringIndex
    lda     (stringPtr0),y
    beq     bottom_row

    inc     stringIndex
    jmp     dialog_row


bottom_row:

    inc     tileY

    ; Lower-left
    lda     dialogLeft
    sta     tileX
    lda     #DIALOG_LL
    sta     bgTile
    jsr     DHGR_DRAW_7X8

    inc     tileX
    inc     tileX

bottom_loop:

    lda     tileX
    cmp     dialogX
    bne     :+

    lda     #32         ;space
    sta     bgTile
    jsr     DHGR_DRAW_7X8

    inc     tileX
    inc     tileX

    lda     #32         ;space
    sta     bgTile
    jsr     DHGR_DRAW_7X8

    inc     tileX
    inc     tileX

:
    lda     #DIALOG_LO
    sta     bgTile
    jsr     DHGR_DRAW_7X8
    
    inc     tileX
    inc     tileX
    lda     dialogRight
    cmp     tileX
    bne     bottom_loop

    ; Lower-right
    lda     #DIALOG_LR
    sta     bgTile
    jsr     DHGR_DRAW_7X8

    inc     tileY

    lda     dialogX
    sta     tileX

    lda     dialogDir
    beq     :+

    lda     #DIALOG_RS1
    sta     bgTile
    jsr     DHGR_DRAW_7X8
    inc     tileX
    inc     tileX
    lda     #DIALOG_RS2
    sta     bgTile
    jsr     DHGR_DRAW_7X8
    rts

:

    lda     #DIALOG_LS1
    sta     bgTile
    jsr     DHGR_DRAW_7X8
    inc     tileX
    inc     tileX
    lda     #DIALOG_LS2
    sta     bgTile
    jsr     DHGR_DRAW_7X8
    rts

stringIndex:    .byte   0

.endproc


;-----------------------------------------------------------------------------
; Draw String
;
;   Use tileX and tileY for start and string inlined
;-----------------------------------------------------------------------------

.proc drawString

    ; Pop return address to find string
    pla
    sta     stringPtr0
    pla
    sta     stringPtr1
    ldy     #0

    lda     tileX
    sta     offset

    ; Print characters until 0 (end-of-string)
printLoop:
    iny
    bne     :+              ; Allow strings > 255
    inc     stringPtr1
:
    tya
    pha
    lda     (stringPtr0),y
    beq     printExit
    cmp     #13
    bne     :+
    inc     tileY
    lda     offset
    sta     tileX
    jmp     continue
:
    and     #$3f
    sta     bgTile
    jsr     DHGR_DRAW_7X8

    inc     tileX
    inc     tileX
continue:
    pla
    tay
    jmp     printLoop

printExit:    
    pla                 ; clean up stack
    ; calculate return address after print string
    clc
    tya
    adc     stringPtr0  ; add low-byte first
    tax                 ; save in X
    lda     stringPtr1  ; carry to high-byte
    adc     #0          
    pha                 ; push return high-byte
    txa
    pha                 ; push return low-byte
    rts                 ; return

char:   .byte   0
offset: .byte   0
.endproc

;-----------------------------------------------------------------------------
; DHGR clear screen
;-----------------------------------------------------------------------------

.proc clearScreen
    lda     #$00
    sta     screenPtr0
    clc
    lda     #$20
    adc     drawPage
    sta     screenPtr1
    adc     #$20
    sta     nextPage

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
    lda     nextPage
    cmp     screenPtr1
    bne     loop
    rts

nextPage:   .byte   0

.endproc

;-----------------------------------------------------------------------------
; Init double hi-res
;-----------------------------------------------------------------------------

.proc dhgrInit

    sta     TXTCLR      ; Graphics
    sta     HIRES       ; Hi-res
    sta     MIXCLR      ; Full Screen
    sta     LOWSCR      ; Display page 1
    sta     DHIRESON    ; Annunciator 2 On
    sta     SET80VID    ; 80 column on
    rts

.endproc

;-----------------------------------------------------------------------------
; Draw Map
;
;-----------------------------------------------------------------------------

.proc drawMap

    ; Read map buffer
    jsr     DHGR_READ_MAP

    lda     #0
    sta     mapIndex

    lda     #MAP_Y_OFFSET
    sta     tileY

loopy:

    lda     #MAP_X_OFFSET
    sta     tileX

loopx:


    ;---------------------
    ; Set background tile
    ;---------------------

    ; Update animation index
    clc 
    lda     tileY
    adc     tileX           ; x+y should never carry
    adc     animateTime
    and     #$F
    sta     animateIndex

    ; Apply animation
    ldy     mapIndex
    lda     MAP_BUFFER,y        
    tax                         ; x = tile
    ldy     animateTable,x      ; y = animation offst table base
    beq     :+
    clc
    tya
    adc     animateIndex
    tay                         ; y = animation delta
    txa                         ; a = tile
    adc     animateOffset,y     ; add offset
 :

    sta     bgTile

    ;---------------------
    ; Draw tile
    ;---------------------

    lda     mapIndex
    cmp     #MAP_CENTER
    bne     draw_background

    jsr     DHGR_DRAW_FG_14X16
    jmp     continue

draw_background:
    jsr     DHGR_DRAW_14X16

continue:
    inc     mapIndex

    lda     tileX
    clc
    adc     #4
    sta     tileX
    cmp     #MAP_X_OFFSET + MAP_SCREEN_WIDTH*4
    bne     loopx

    inc     tileY
    inc     tileY
    lda     tileY
    cmp     #MAP_Y_OFFSET + MAP_SCREEN_HEIGHT*2
    bne     loopy

    rts

animateIndex:   .byte   0
mapIndex:       .byte   0

.endproc

;-----------------------------------------------------------------------------
; Quit
;
;   Exit to ProDos
;-----------------------------------------------------------------------------
.proc quit

    jsr     MLI
    .byte   CMD_QUIT
    .word  quit_params

quit_params:
    .byte   4               ; 4 parameters
    .byte   0               ; 0 is the only quit type
    .word   0               ; Reserved pointer for future use (what future?)
    .byte   0               ; Reserved byte for future use (what future?)
    .word   0               ; Reserved pointer for future use (what future?)

.endproc


;-----------------------------------------------------------------------------
; Utilies

.include "sounds.asm"

; Global Variables
;-----------------------------------------------------------------------------

gameTime:           .byte   0
animateTime:        .byte   0

; Box routine   
boxLeft:            .byte   0
boxRight:           .byte   0
boxTop:             .byte   0
boxBottom:          .byte   0

; Dialog
dialogLeft:         .byte   0
dialogRight:        .byte   0
dialogTop:          .byte   0
dialogX:            .byte   0
dialogDir:          .byte   0

animateTable:
; Animation offset table for each tile
    .byte   $00     ; 00
    .byte   $00     ; 01
    .byte   $00     ; 02
    .byte   $00     ; 03
    .byte   $00     ; 04
    .byte   $00     ; 05
    .byte   $00     ; 06
    .byte   $00     ; 07
    .byte   $00     ; 08
    .byte   $00     ; 09
    .byte   $00     ; 0a
    .byte   $00     ; 0b
    .byte   $00     ; 0c
    .byte   $00     ; 0d
    .byte   $00     ; 0e
    .byte   $00     ; 0f
    .byte   $00     ; 10
    .byte   $00     ; 11
    .byte   $10     ; 12 - water
    .byte   $00     ; 13
    .byte   $00     ; 14
    .byte   $10     ; 15 - pond rock
    .byte   $00     ; 16
    .byte   $00     ; 17
    .byte   $00     ; 18
    .byte   $00     ; 19
    .byte   $00     ; 1a
    .byte   $00     ; 1b
    .byte   $00     ; 1c
    .byte   $00     ; 1d
    .byte   $00     ; 1e
    .byte   $00     ; 1f
    .byte   $00     ; 20
    .byte   $20     ; 21 - Girl
    .byte   $00     ; 22
    .byte   $20     ; 23 - Bee Boy
    .byte   $00     ; 24
    .byte   $20     ; 25 - Octo
    .byte   $00     ; 26
    .byte   $20     ; 27 - Pig
    .byte   $00     ; 28
    .byte   $30     ; 29 - Doll
    .byte   $00     ; 2a
    .byte   $00     ; 2b
    .byte   $10     ; 2c - Lily Pad
    .byte   $00     ; 2d
    .byte   $00     ; 2e
    .byte   $00     ; 2f
animateOffset:
    .byte   $00     ; 30
    .byte   $00     ; 31
    .byte   $00     ; 32
    .byte   $00     ; 33
    .byte   $00     ; 34
    .byte   $00     ; 35 
    .byte   $00     ; 36
    .byte   $00     ; 37
    .byte   $00     ; 38
    .byte   $00     ; 39
    .byte   $00     ; 3a
    .byte   $00     ; 3b
    .byte   $00     ; 3c
    .byte   $00     ; 3d
    .byte   $00     ; 3e
    .byte   $00     ; 3f

; Overlap animate offset with animate table since 0-f invalid

    ; 10 - water
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   1
    .byte   1
    .byte   1
    .byte   2
    .byte   2
    .byte   2
    .byte   2
    .byte   2
    .byte   1
    .byte   1
    .byte   1

    ; 20 - blink
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   1

    ; 30 - alternate
    .byte   0
    .byte   1
    .byte   0
    .byte   1
    .byte   0
    .byte   1
    .byte   0
    .byte   1
    .byte   0
    .byte   1
    .byte   0
    .byte   1
    .byte   0
    .byte   1
    .byte   0
    .byte   1
