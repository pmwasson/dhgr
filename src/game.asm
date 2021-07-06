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
MAP_UP              =   MAP_CENTER-MAP_SCREEN_WIDTH
MAP_DOWN            =   MAP_CENTER+MAP_SCREEN_WIDTH
MAP_LEFT            =   MAP_CENTER-1
MAP_RIGHT           =   MAP_CENTER+1

PLAYER_RIGHT        =   0
PLAYER_LEFT         =   1

DIALOG_BOTTOM       =   MAP_Y_OFFSET + 3*2
;DIALOG_LEFT         =   MAP_X_OFFSET
;DIALOG_RIGHT        =   MAP_X_OFFSET + MAP_SCREEN_WIDTH*4 - 2
DIALOG_LEFT         =   2
DIALOG_RIGHT        =   36
DIALOG_X_LEFT       =   10
DIALOG_X_RIGHT      =   26

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

    ; set up map
    lda     #MAP_SCREEN_WIDTH
    sta     DHGR_MAP_BUFFER_WIDTH

    lda     #MAP_SCREEN_HEIGHT
    sta     DHGR_MAP_BUFFER_HEIGHT


    ; set initial coordinates
    lda     #(64-7)/2
    sta     mapWindowX
    sta     mapWindowY
    jsr     DHGR_READ_MAP

gameLoop:

    jsr     flipPage        ; display final drawing from last iteration of game loop

skipFlip:

    ; clock tick
    inc     gameTime
    lda     gameTime
    lsr
    sta     animateTime

    ; update screen
    jsr     drawScreen

    ;-------------------
    ; Dialog
    ;-------------------

    lda     dialogAction
    beq     normalInput

    jsr     drawDialog

    jsr     flipPage        ; display dialog

    ; any key to clear
:
    lda     KBD
    cmp     #KEY_SPACE
    bne     :-
    sta     KBDSTRB

    jsr     flipPage        ; display previous non-dialog page while we clean up

    ; refresh
    jsr     clearScreen
    jsr     drawFrame

    lda     #0
    sta     dialogAction

    ; Check for link

    lda     dialogNext
    beq     skipFlip

    lda     dialogNext
    sta     dialogIndex

    jsr     readDialog

    jmp     skipFlip

normalInput:
    ;-------------------
    ; Input
    ;-------------------

    ; check user input
    lda     KBD
    bpl     gameLoop
    sta     KBDSTRB


    ;
    ; Movement
    ;

    cmp     #KEY_UP
    bne     :+
    lda     mapWindowY
    beq     done_up
    ldy     MAP_BUFFER+MAP_UP
    lda     bgInfoTable,y
    and     #1
    bne     done_up
    dec     mapWindowY
done_up:
    jmp     gameLoop
:
 
    cmp     #KEY_DOWN
    bne     :+
    lda     mapWindowY
    cmp     #MAP_HEIGHT-MAP_SCREEN_HEIGHT
    beq     done_down
    ldy     MAP_BUFFER+MAP_DOWN
    lda     bgInfoTable,y
    and     #1
    bne     done_down
    inc     mapWindowY
done_down:
    jmp     gameLoop
:

    cmp     #KEY_LEFT
    bne     :+
    lda     #PLAYER_LEFT
    sta     playerTile
    lda     mapWindowX
    beq     done_left
    ldy     MAP_BUFFER+MAP_LEFT
    lda     bgInfoTable,y
    and     #1
    bne     done_left
    dec     mapWindowX
done_left:
    jmp     gameLoop
:

    cmp     #KEY_RIGHT
    bne     :+
    lda     #PLAYER_RIGHT
    sta     playerTile
    lda     mapWindowX
    cmp     #MAP_WIDTH-MAP_SCREEN_WIDTH
    beq     done_right
    ldy     MAP_BUFFER+MAP_RIGHT
    lda     bgInfoTable,y
    and     #1
    bne     done_right
    inc     mapWindowX
done_right:
    jmp     gameLoop
:

    ;
    ; Action
    ;

    cmp     #KEY_SPACE
    bne     :+

    lda     #0
    sta     dialogIndex

    jsr     readDialog

    jmp     gameLoop


:
    ;
    ; Exit
    ;

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

    jmp     gameLoop

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

    ; Draw components
    ;-------------------------------------------------------------------------

    jsr     drawMap

    jsr     drawCoordinates

    rts
.endproc

;-----------------------------------------------------------------------------
; flipPage
;-----------------------------------------------------------------------------

.proc flipPage
    ; flip page
    ldx     PAGE2
    bmi     flipToPage1
    sta     HISCR           ; display page 2
    lda     #0
    sta     drawPage        ; draw on page 1
    rts

flipToPage1:
    sta     LOWSCR          ; diaplay page 1
    lda     #$20
    sta     drawPage        ; draw on page 2
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

    lda     #DIALOG_LS1
    sta     bgTile
    jsr     DHGR_DRAW_7X8
    inc     tileX
    inc     tileX
    lda     #DIALOG_LS2
    sta     bgTile
    jsr     DHGR_DRAW_7X8
    rts

:

    lda     #DIALOG_RS1
    sta     bgTile
    jsr     DHGR_DRAW_7X8
    inc     tileX
    inc     tileX
    lda     #DIALOG_RS2
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
; Draw coordinates (for debug)
;-----------------------------------------------------------------------------

.proc drawCoordinates

    ; draw coordinates

    ; FIXME: should make default font normal and inverse for dialog
    lda     #$ff
    sta     invMask

    lda     #MAP_Y_OFFSET + MAP_SCREEN_HEIGHT*2 - 1    ; lined up with bottom
    sta     tileY

    lda     #0
    sta     tileX
    lda     mapWindowX
    lsr
    lsr
    lsr
    lsr                 ; / 16
    tax
    lda     hexOffset,x
    sta     bgTile
    jsr     DHGR_DRAW_7X8

    lda     #2
    sta     tileX
    lda     mapWindowX
    and     #$f
    tax
    lda     hexOffset,x
    sta     bgTile
    jsr     DHGR_DRAW_7X8


    lda     #MAP_Y_OFFSET + MAP_SCREEN_HEIGHT*2 + 1    ; below bottom
    sta     tileY


    lda     #6
    sta     tileX
    lda     mapWindowY
    lsr
    lsr
    lsr
    lsr                 ; / 16
    tax
    lda     hexOffset,x
    sta     bgTile
    jsr     DHGR_DRAW_7X8

    lda     #8
    sta     tileX
    lda     mapWindowY
    and     #$f
    tax
    lda     hexOffset,x
    sta     bgTile
    jsr     DHGR_DRAW_7X8

    lda     #$0
    sta     invMask

    rts

hexOffset: .byte    $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$1,$2,$3,$4,$5,$6

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
    ldy     bgInfoTable,x      ; y = animation offst table base
    bpl     :+
    clc
    tya
    and     #$70
    adc     animateIndex
    tay                         
    txa                         
    adc     animateOffset,y     ; add offset
 :

    sta     bgTile

    ;---------------------
    ; Draw tile
    ;---------------------

    lda     mapIndex
    cmp     #MAP_CENTER
    bne     draw_background

    lda     playerTile
    sta     fgTile
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
; Process dialog
;
;-----------------------------------------------------------------------------

dialogPtr       = A2              ; Use A2 for temp pointer

.proc readDialog

    ; Set up pointer
    lda     dialogIndex
    asl                     ; *8
    asl                     
    asl                     
    sta     dialogPtr

    ; FIXME, need more than 32 dialogs

    lda     #>dialogTable   ; Table must be page aligned 
    sta     dialogPtr+1

    ; set string pointer

    ldy     #0
    lda     (dialogPtr),y
    sta     stringPtr0
    iny
    lda     (dialogPtr),y
    sta     stringPtr1

    ; action
    iny
    lda     (dialogPtr),y
    sta     dialogAction

    ; next
    ldy     #5
    lda     (dialogPtr),y
    sta     dialogNext

    ; Top = bottom - height

    lda     #DIALOG_BOTTOM
    ldy     #4              ; height
    sec
    sbc     (dialogPtr),y
    sta     dialogTop

    ; Left / Right based on direction player is facing

    lda     playerTile
    sta     dialogDir
    cmp     #PLAYER_RIGHT
    bne     dialog_left

    lda     #DIALOG_RIGHT
    sta     dialogRight

    ldy     #3              ; width
    sec
    sbc     (dialogPtr),y
    sta     dialogLeft

    lda     #DIALOG_X_RIGHT
    sta     dialogX

    rts

dialog_left:

    lda     #DIALOG_LEFT
    sta     dialogLeft

    ldy     #3              ; width
    clc
    adc     (dialogPtr),y
    sta     dialogRight

    lda     #DIALOG_X_LEFT
    sta     dialogX

    rts

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

;-----------------------------------------------------------------------------
; Global Variables

gameTime:           .byte   0
animateTime:        .byte   0
playerTile:         .byte   0

; Box routine   
boxLeft:            .byte   0
boxRight:           .byte   0
boxTop:             .byte   0
boxBottom:          .byte   0

; Dialog
dialogIndex:        .byte   0
dialogLeft:         .byte   0
dialogRight:        .byte   0
dialogTop:          .byte   0
dialogX:            .byte   0
dialogDir:          .byte   0
dialogAction:       .byte   0
dialogNext:         .byte   0


;-----------------------------------------------------------------------------
; Tile Dynamic Info

COLLISION           = $01
ANIMATE_WATER       = $80+$10
ANIMATE_BLINK       = $80+$20
ANIMATE_ALTERNATE   = $80+$30

bgInfoTable:
; Collision and animation offset for each tile
    .byte   COLLISION                   ; 00 - Water
    .byte   $00                         ; 01 - Grass 1
    .byte   $00                         ; 02 - Grass 2
    .byte   $00                         ; 03 - Grass Flowers
    .byte   COLLISION                   ; 04 - Tree 1
    .byte   COLLISION                   ; 05 - Tree 2
    .byte   COLLISION                   ; 06 - Forest
    .byte   COLLISION                   ; 07 - Hedge
    .byte   COLLISION                   ; 08 - Rock
    .byte   $00                         ; 09 - Forest Path 1
    .byte   $00                         ; 0a - Forest Path 2
    .byte   $00                         ; 0b - Forest Path 3
    .byte   COLLISION                   ; 0c - Wooden Sign
    .byte   $00                         ; 0d - Gravel Path
    .byte   $00                         ; 0e - Snow Cover
    .byte   COLLISION                   ; 0f - Snow Tree
    .byte   COLLISION                   ; 10 - Snow Rock
    .byte   COLLISION                   ; 11 - Mountains
    .byte   COLLISION+ANIMATE_WATER     ; 12 - Shore 1
    .byte   COLLISION+ANIMATE_WATER     ; 13 - Shore 2
    .byte   COLLISION+ANIMATE_WATER     ; 14 - Shore 3
    .byte   COLLISION+ANIMATE_WATER     ; 15 - Pond Rock 1
    .byte   COLLISION+ANIMATE_WATER     ; 16 - Pond Rock 2
    .byte   COLLISION+ANIMATE_WATER     ; 17 - Pond Rock 3
    .byte   COLLISION                   ; 18 - Bricks
    .byte   $00                         ; 19 - Boards
    .byte   COLLISION                   ; 1a - Door Closed
    .byte   $00                         ; 1b - Door Open
    .byte   $00                         ; 1c - Store floor
    .byte   COLLISION                   ; 1d - Chair - right
    .byte   COLLISION                   ; 1e - Chair - left
    .byte   COLLISION                   ; 1f - Table
    .byte   $00                         ; 20 - Tile
    .byte   COLLISION+ANIMATE_BLINK     ; 21 - Girl-1
    .byte   COLLISION+ANIMATE_BLINK     ; 22 - Girl-2
    .byte   COLLISION+ANIMATE_BLINK     ; 23 - Bee Boy-1
    .byte   COLLISION+ANIMATE_BLINK     ; 24 - Bee Boy-2
    .byte   COLLISION+ANIMATE_BLINK     ; 25 - Octo-1
    .byte   COLLISION+ANIMATE_BLINK     ; 26 - Octo-2
    .byte   COLLISION+ANIMATE_BLINK     ; 27 - Pig-1
    .byte   COLLISION+ANIMATE_BLINK     ; 28 - Pig-2
    .byte   COLLISION+ANIMATE_ALTERNATE ; 29 - Doll-1
    .byte   COLLISION+ANIMATE_ALTERNATE ; 2a - Doll-2
    .byte   COLLISION                   ; 2b - Snail
    .byte   COLLISION+ANIMATE_WATER     ; 1c - Lily Pad 1
    .byte   COLLISION+ANIMATE_WATER     ; 1d - Lily Pad 2
    .byte   COLLISION+ANIMATE_WATER     ; 1e - Lily Pad 3
    .byte   $00                         ; 2f - Small mushroooms
animateOffset:
    .byte   COLLISION                   ; 30 - Large mushroom
    .byte   $00                         ; 31
    .byte   $00                         ; 32
    .byte   $00                         ; 33
    .byte   $00                         ; 34
    .byte   $00                         ; 35 
    .byte   $00                         ; 36
    .byte   $00                         ; 37
    .byte   $00                         ; 38
    .byte   $00                         ; 39
    .byte   $00                         ; 3a
    .byte   $00                         ; 3b
    .byte   $00                         ; 3c
    .byte   $00                         ; 3d
    .byte   $00                         ; 3e
    .byte   $00                         ; 3f

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


;-----------------------------------------------------------------------------
; Dialog

.align 256

DIALOG_ACTION_NONE      = 0
DIALOG_ACTION_DISPLAY   = 1

dialogTable:

; width =  (1+maxcol)*2 (range = 10 - 26)
; height = 2 + rows

; 0
    .word   dialogString0
    .byte   DIALOG_ACTION_DISPLAY
    .byte   14,3                    ; width, height
    .byte   1                       ; Next dialog
    .byte   0,0                     ; Padding

; 1
    .word   dialogString1
    .byte   DIALOG_ACTION_DISPLAY
    .byte   34,12                   ; width, height
    .byte   0                       ; Next dialog
    .byte   0,0                     ; Padding


 dialogString0:
    String      "HEY!"   

 dialogString1:
    StringCont  "MAXIMUM STRING"   
    StringCont  "0123456789ABCDEF"   
    StringCont  "BEEP"   
    StringCont  "THIS IS A TEST"   
    StringCont  "BOOP"   
    StringCont  "BZZZ"   
    StringCont  "     HMMMMM"   
    StringCont  "     HMMMMM"   
    StringCont  "     HMMMMM"   
    String      "     HMMMMM"   
   

