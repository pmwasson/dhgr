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
DIALOG_LS       = 0
DIALOG_RS       = 1

MAP_WIDTH           =  64
MAP_HEIGHT          =  64
MAP_X_OFFSET        =   6
MAP_Y_OFFSET        =   6   ; must be even
MAP_SCREEN_WIDTH    =   7
MAP_SCREEN_HEIGHT   =   7
MAP_CENTER          =   2*((3*7)+3)
MAP_UP              =   MAP_CENTER-2*MAP_SCREEN_WIDTH
MAP_DOWN            =   MAP_CENTER+2*MAP_SCREEN_WIDTH
MAP_LEFT            =   MAP_CENTER-2
MAP_RIGHT           =   MAP_CENTER+2

; Tile for player direction
PLAYER_RIGHT        =   4
PLAYER_LEFT         =   5
PLAYER_DOWN         =   6
PLAYER_UP           =   7

DIALOG_BOTTOM       =   MAP_Y_OFFSET + 3*2
DIALOG_LEFT         =   2
DIALOG_RIGHT        =   36
DIALOG_X_LEFT       =   10
DIALOG_X_RIGHT      =   26

SIGN_BOTTOM         =   MAP_Y_OFFSET + 3*2

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
    lda     #$04
    sta     mapWindowX
    lda     #$25
    sta     mapWindowY
    jsr     DHGR_READ_MAP



    ;-----------------------
    ; Game Loop
    ;-----------------------
    ;
    ; - Page flip
    ; - If refresh, clear screen and draw boarder
    ; - Draw screen
    ; - Mode
    ;   - Wait: wait for space-bar, process next
    ;   - Continue: process next right away
    ;   - User: get user input
    ;     - If start new action, process new
    ;
    ; Processing an action means to draw action and update state

    ; - Get input based on mode
    ;   - If player input, can trigger new action
    ; - Process next
    ; - If action is triggered
    ;   - Draw action
    ;   - If drawing > map, trigger refresh


gameLoop:

    ;-----------------------
    ; Page Flip
    ;-----------------------
    jsr     flipPage        ; display final drawing from last iteration of game loop


    ;-----------------------
    ; Clear screen (if needed)
    ;-----------------------
    lda     refresh
    beq     :+

    jsr     clearScreen
    jsr     drawFrame
    lda     #0
    sta     refresh
:

    ;-----------------------
    ; clock tick
    ;-----------------------
    inc     gameTime
    lda     gameTime
    lsr
    sta     animateTime         ; animate slower

    ;-----------------------
    ; update screen
    ;-----------------------
    jsr     drawScreen

    ;-------------------
    ; action
    ;-------------------

    lda     actionType
    beq     checkPassive

    lda     actionRefresh
    sta     refresh         ; set if need to clear screen next time

    lda     actionWait      ; set if need to wait for spacebar to continue
    beq     action_continue

    ; any key to clear
:
    lda     KBD
    cmp     #KEY_SPACE
    bne     :-
    sta     KBDSTRB
 
 action_continue:

    ; clear current action
    lda     #0
    sta     actionType

    ; Check for link

    lda     actionNext
    beq     gameLoop
    sta     actionCommand

    jsr     readAction

    jmp     gameLoop


    ;-------------------
    ; Check passive
    ;-------------------
checkPassive:
    ldx     #MAP_CENTER
    ldy     MAP_BUFFER+1,x
    lda     actionState,y
    and     #ACTION_PASSIVE
    beq     playerInput

    sty     actionIndex
    lda     actionCommandTable,y
    sta     actionCommand
    jsr     readAction
    jmp     gameLoop


playerInput:
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
    lda     #PLAYER_UP
    sta     playerTile
    lda     mapWindowY
    beq     done_up
    ldy     #MAP_UP
    jsr     collisionCheck
    bcs     done_up
    dec     mapWindowY
done_up:
    jmp     gameLoop
:
 
    cmp     #KEY_DOWN
    bne     :+
    lda     #PLAYER_DOWN
    sta     playerTile
    lda     mapWindowY
    cmp     #MAP_HEIGHT-MAP_SCREEN_HEIGHT
    beq     done_down
    ldy     #MAP_DOWN
    jsr     collisionCheck
    bcs     done_down
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
    ldy     #MAP_LEFT
    jsr     collisionCheck
    bcs     done_left
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
    ldy     #MAP_RIGHT
    jsr     collisionCheck
    bcs     done_right
    inc     mapWindowX
done_right:
    jmp     gameLoop
:


    ;
    ; Action
    ;

    cmp     #KEY_SPACE
    bne     :+

    ldy     playerTile
    ldx     directionMapOffset-PLAYER_RIGHT,y
    ldy     MAP_BUFFER,x
    jmp     select_action
:
    ;
    ; Exit
    ;

    cmp     #KEY_ESC
    bne     :+

    jsr    inline_print
    StringCR "Press CTRL-Y for ProDOS program launcher"

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

    ;
    ; Default
    ;
    jmp     gameLoop

    ;---------------------------------

    ; Pass action index in Y
select_action:
    lda     actionState,y
    and     #ACTION_SELECT
    bne     do_action
    jmp     gameLoop

do_action:
    sty     actionIndex
    lda     actionCommandTable,y
    sta     actionCommand
    jsr     readAction
    jmp     gameLoop


directionMapOffset:
    .byte   MAP_RIGHT+1
    .byte   MAP_LEFT+1
    .byte   MAP_DOWN+1
    .byte   MAP_UP+1

.endproc

;------------------------------------------------
; Collision check
;
;  Pass location to check in Y
;  Set carry if collision
;------------------------------------------------

.proc collisionCheck

    ldx     MAP_BUFFER,y
    lda     bgInfoTable,x
    and     #COLLISION
    bne     collision

    clc
    rts

collision:
    sec
    rts

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
    bne     :+
    rts
:
    sta     tileX

    lda     dialogDir
    beq     :+

    lda     #DIALOG_LS
    sta     fgTile
    jsr     DHGR_DRAW_FG_14X16
    rts
:
    lda     #DIALOG_RS
    sta     fgTile
    jsr     DHGR_DRAW_FG_14X16
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
    lda     clearColor
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

    ;------------------
    ; X coord

    lda     #MAP_Y_OFFSET + MAP_SCREEN_HEIGHT*2 - 1    ; lined up with bottom
    sta     tileY
    lda     #0
    sta     tileX
    lda     mapWindowX
    jsr     drawHex

    ;------------------
    ; Y coord

    lda     #MAP_Y_OFFSET + MAP_SCREEN_HEIGHT*2 + 1    ; below bottom
    sta     tileY
    lda     #6
    sta     tileX
    lda     mapWindowY
    jsr     drawHex

    ;------------------
    ; Frame count

    lda     #0
    sta     tileX
    sta     tileY
    lda     gameTime
    jsr     drawHex
    


    ;------------------

    lda     #$0
    sta     invMask

    rts


drawHex:
    sta     temp
    lsr
    lsr
    lsr
    lsr                 ; / 16
    tax
    lda     hexOffset,x
    sta     bgTile
    jsr     DHGR_DRAW_7X8

    inc     tileX
    inc     tileX

    lda     temp
    and     #$f
    tax
    lda     hexOffset,x
    sta     bgTile
    jsr     DHGR_DRAW_7X8

    rts


hexOffset:  .byte   $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$1,$2,$3,$4,$5,$6
temp:       .byte   0

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

    lda     tileY
    lsr             ; /2
    clc
    adc     animateTime
    sta     animateY        ; Y component for animation

loopx:

    ; Check for modifications
    ldx     mapIndex
    ldy     MAP_BUFFER+1,x
    lda     actionState,y
    and     #ACTION_FLIP_BG
    beq     :+

    ; Add 1 to BG tile
    inc     MAP_BUFFER,x
:

    ;---------------------
    ; Draw background tile
    ;---------------------

    ; Update animation index
    lda     tileX
    lsr
    lsr
    clc
    adc     animateY            ; X component for animation
    and     #$1F
    sta     animateIndex

    ; Apply animation
    ldy     mapIndex
    lda     MAP_BUFFER,y        
    tax                         ; x = tile
    ldy     bgInfoTable,x       ; y = animation base
    bpl     :+

    tya
    clc
    and     #$E0
    adc     animateIndex
    tay
    txa
    clc
    adc     animateOffset,y     ; add offset
:

    sta     bgTile
    jsr     DHGR_DRAW_14X16

    ;---------------------
    ; Draw foreground tile
    ;---------------------

    inc     mapIndex

    ldy     mapIndex
    ldx     MAP_BUFFER,y

    lda     actionState,x
    and     #ACTION_FG_TILE
    beq     draw_player

    ora     #$10

    ; Apply animation
    tax                         ; x = tile
    ldy     fgInfoTable,x       ; y = animation base
    bpl     :+

    tya
    clc
    and     #$E0
    adc     animateIndex
    tay
    txa
    clc
    adc     animateOffset,y     ; add offset
:

    sta     fgTile

    jsr     DHGR_DRAW_FG_14X16


    ;---------------------
    ; Draw player
    ;---------------------
draw_player:

    lda     mapIndex
    cmp     #MAP_CENTER+1
    bne     continue

    lda     playerTile
    sta     fgTile
    jsr     DHGR_DRAW_FG_14X16

    ;---------------------
    ; Update pointers
    ;---------------------

continue:
    inc     mapIndex

    lda     tileX
    clc
    adc     #4
    sta     tileX
    cmp     #MAP_X_OFFSET + MAP_SCREEN_WIDTH*4
    beq     :+
    jmp     loopx
:

    inc     tileY
    inc     tileY
    lda     tileY
    cmp     #MAP_Y_OFFSET + MAP_SCREEN_HEIGHT*2
    bne     :+
    rts
: 
    jmp     loopy


mapIndex:       .byte   0
animateY:       .byte   0
animateIndex:   .byte   0

.endproc



;-----------------------------------------------------------------------------
; Read action
;
;   Read entry and process action
;-----------------------------------------------------------------------------

actionPtr       = A2              ; Use A2 for temp pointer

.proc readAction

    ; Set up pointer
    lda     actionCommand
    asl                     ; *8
    asl                     
    asl                     
    sta     actionPtr

    ; FIXME, need more than 32 actions

    lda     #>actionTable   ; Table must be page aligned 
    sta     actionPtr+1

    ; Set output defaults
    lda     #0
    sta     actionWait
    sta     actionRefresh

    ; next
    ldy     #1
    lda     (actionPtr),y
    sta     actionNext

    ; type
    ldy     #0
    lda     (actionPtr),y
    sta     actionType

    cmp     #ACTION_TYPE_DIALOG
    bne     :+

    jmp     readActionDialog    ; link return
:

    cmp     #ACTION_TYPE_SIGN
    bne     :+

    jmp     readActionSign      ; link return
:

    cmp     #ACTION_TYPE_FLASH
    bne     :+

    lda     #$ff
    sta     clearColor
    jsr     clearScreen
    lda     #0
    sta     clearColor
    inc     actionRefresh

    rts

:

    cmp     #ACTION_TYPE_BG_STATE
    bne     pickup

    ldy     #3                  ; state
    lda     (actionPtr),y
    tax
    bne     :+
    ldx     actionIndex         ; 0 = me!
:

    ldy     #2                  ; mode
    lda     (actionPtr),y

    bne     :+

    ; 0 = ACTION_MODE_CLEAR
    lda     actionState,x
    and     #255-ACTION_FLIP_BG
    sta     actionState,x
    rts
:
    cmp     #ACTION_MODE_SET
    bne     :+

    lda     actionState,x
    ora     ACTION_FLIP_BG
    sta     actionState,x

    rts
:
    ; 2 = ACTION_MODE_TOGGLE
    lda     actionState,x
    eor     #ACTION_FLIP_BG
    sta     actionState,x
    rts

pickup:
    cmp     #ACTION_TYPE_PICKUP
    bne     :+

    ldx     actionIndex
    lda     actionState,x
    and     #255-(ACTION_FG_TILE+ACTION_PASSIVE)
    sta     actionState,x


    rts

:
    ; Default to nothing
    rts

.endproc

;-----------------------------------------------------------------------------
; Read action: Dialog
;-----------------------------------------------------------------------------

.proc readActionDialog

    ; set string pointer
    ldy     #2
    lda     (actionPtr),y
    sta     stringPtr0
    iny     ; 3
    lda     (actionPtr),y
    sta     stringPtr1


    ; Top = bottom - height

    ldx     playerTile
    lda     dialogBottom-PLAYER_RIGHT,x
    ldy     #5              ; height
    sec
    sbc     (actionPtr),y
    sta     dialogTop

    ; Left / Right based on direction player is facing

    lda     playerTile
    cmp     #PLAYER_RIGHT
    bne     dialog_left

    ; Right justified

    lda     #0
    sta     dialogDir

    lda     #DIALOG_RIGHT
    sta     dialogRight

    ldy     #4              ; width
    sec
    sbc     (actionPtr),y
    sta     dialogLeft

    lda     #DIALOG_X_RIGHT
    sta     dialogX

    jmp     finish

dialog_left:

    cmp     #PLAYER_LEFT
    bne     dialog_updown

    ; Left justified

    lda     #DIALOG_LEFT
    sta     dialogLeft

    ldy     #4              ; width
    clc
    adc     (actionPtr),y
    sta     dialogRight

    lda     #1
    sta     dialogDir

    lda     #DIALOG_X_LEFT
    sta     dialogX

    jmp     finish

dialog_updown:

    ; Left justified (but shifted right)

    lda     #DIALOG_LEFT+4
    sta     dialogLeft

    ldy     #4              ; width
    clc
    adc     (actionPtr),y
    sta     dialogRight

    lda     #1
    sta     dialogDir

    lda     #DIALOG_X_LEFT+4
    sta     dialogX

finish:
    lda     #1
    sta     actionWait
    sta     actionRefresh

    jsr     drawDialog

    rts

halfWidth:  .byte   0

.endproc


; Make global to share with sign
dialogBottom:
    .byte   DIALOG_BOTTOM
    .byte   DIALOG_BOTTOM
    .byte   DIALOG_BOTTOM+2
    .byte   DIALOG_BOTTOM-2

;-----------------------------------------------------------------------------
; Read action: Sign
;-----------------------------------------------------------------------------

.proc readActionSign

    ; set string pointer
    ldy     #2
    lda     (actionPtr),y
    sta     stringPtr0
    iny     ; 3
    lda     (actionPtr),y
    sta     stringPtr1

    ; Top = bottom - height
    ldx     playerTile
    lda     dialogBottom-PLAYER_RIGHT,x
    ldy     #5              ; height
    sec
    sbc     (actionPtr),y
    sta     dialogTop

    ; Left = 20 - width/2
    ; Right = left + width

    ldy     #4              ; width
    lda     (actionPtr),y
    lsr
    sta     halfWidth
    lda     #20
    sec
    sbc     halfWidth
    sta     dialogLeft
    clc
    adc     (actionPtr),y
    sta     dialogRight

    lda     #0
    sta     dialogX         ; don't draw a stem

    lda     #1
    sta     actionWait
    sta     actionRefresh

    lda     #$ff            ; invert
    sta     invMask
    jsr     drawDialog
    lda     #0
    sta     invMask

    rts

halfWidth:  .byte   0

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
.include "inline_print.asm"

;-----------------------------------------------------------------------------
; Global Variables

refresh:            .byte   0
gameTime:           .byte   0
animateTime:        .byte   0
playerTile:         .byte   PLAYER_RIGHT
clearColor:         .byte   0

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

; Action
actionIndex:        .byte   0       ; Part of map - index into state
actionCommand:      .byte   0       ; Current command index
actionType:         .byte   0       ; Current action type
actionNext:         .byte   0
actionWait:         .byte   0
actionRefresh:      .byte   0


;-----------------------------------------------------------------------------
; Tile Dynamic Info

COLLISION           = $01
ANIMATE_WATER       = $80
ANIMATE_BLINK       = $a0
ANIMATE_ALTERNATE   = $c0
ANIMATE_GLITTER     = $e0

.align 256

animateOffset:                          ; overlap animate with BG
bgInfoTable:
; Collision and animation offset for each tile
    .byte   COLLISION                                       ; 00 - Water
    .byte   $00                                             ; 01 - Grass 1
    .byte   $00                                             ; 02 - Grass 2
    .byte   $00                                             ; 03 - Grass Flowers
    .byte   COLLISION                                       ; 04 - Tree 1
    .byte   COLLISION                                       ; 05 - Tree 2
    .byte   COLLISION                                       ; 06 - Forest
    .byte   COLLISION                                       ; 07 - Hedge
    .byte   COLLISION                                       ; 08 - Rock
    .byte   $00                                             ; 09 - Forest Path 1
    .byte   $00                                             ; 0a - Forest Path 2
    .byte   $00                                             ; 0b - Forest Path 3
    .byte   COLLISION                                       ; 0c - Wooden Sign
    .byte   $00                                             ; 0d - Gravel Path
    .byte   $00                                             ; 0e - Snow Cover
    .byte   COLLISION                                       ; 0f - Snow Tree
    .byte   COLLISION                                       ; 10 - Snow Rock
    .byte   COLLISION                                       ; 11 - Mountains
    .byte   COLLISION+ANIMATE_WATER                         ; 12 - Shore 1
    .byte   COLLISION                                       ; 13 - Shore 2
    .byte   COLLISION                                       ; 14 - Shore 3
    .byte   ANIMATE_WATER                                   ; 15 - Pond Rock 1
    .byte   ANIMATE_WATER                                   ; 16 - Pond Rock 2
    .byte   ANIMATE_WATER                                   ; 17 - Pond Rock 3
    .byte   COLLISION                                       ; 18 - Bricks
    .byte   $00                                             ; 19 - Boards
    .byte   COLLISION                                       ; 1a - Door Closed
    .byte   $00                                             ; 1b - Door Open
    .byte   $00                                             ; 1c - Store floor
    .byte   $00                                             ; 1d - Chair - right
    .byte   $00                                             ; 1e - Chair - left
    .byte   COLLISION                                       ; 1f - Table
    .byte   $00                                             ; 20 - Tile
    .byte   COLLISION+ANIMATE_BLINK                         ; 21 - Girl-1
    .byte   COLLISION                                       ; 22 - Girl-2
    .byte   COLLISION+ANIMATE_BLINK                         ; 23 - Bee Boy-1
    .byte   COLLISION                                       ; 24 - Bee Boy-2
    .byte   COLLISION+ANIMATE_BLINK                         ; 25 - Octo-1
    .byte   COLLISION                                       ; 26 - Octo-2
    .byte   COLLISION+ANIMATE_BLINK                         ; 27 - Pig-1
    .byte   COLLISION                                       ; 28 - Pig-2
    .byte   COLLISION+ANIMATE_ALTERNATE                     ; 29 - Doll-1
    .byte   COLLISION                                       ; 2a - Doll-2
    .byte   COLLISION                                       ; 2b - Snail
    .byte   COLLISION+ANIMATE_WATER                         ; 1c - Lily Pad 1
    .byte   COLLISION                                       ; 1d - Lily Pad 2
    .byte   COLLISION                                       ; 1e - Lily Pad 3
    .byte   $00                                             ; 2f - Small mushroooms
    .byte   COLLISION                                       ; 30 - Large mushroom
    .byte   COLLISION                                       ; 31 - Ice
    .byte   $00                                             ; 32 - Ice (melted)
    .byte   $00                                             ; 33 - Fire (out)
    .byte   COLLISION+ANIMATE_ALTERNATE                     ; 34 - Fire lit 1
    .byte   COLLISION                                       ; 35 - Fire lit 2
    .byte   COLLISION+ANIMATE_BLINK                         ; 36 - Cold guy 1
    .byte   COLLISION                                       ; 37 - Cold guy 2
    .byte   $00                                             ; 38
    .byte   $00                                             ; 39
    .byte   $00                                             ; 3a
    .byte   $00                                             ; 3b
    .byte   $00                                             ; 3c
    .byte   $00                                             ; 3d
    .byte   $00                                             ; 3e
    .byte   $00                                             ; 3f

fgInfoTable:
; Just animation
    .byte   $00                                             ; 00
    .byte   $00                                             ; 01
    .byte   $00                                             ; 02
    .byte   $00                                             ; 03
    .byte   $00                                             ; 04
    .byte   $00                                             ; 05
    .byte   $00                                             ; 06
    .byte   $00                                             ; 07
    .byte   $00                                             ; 08
    .byte   $00                                             ; 09
    .byte   $00                                             ; 0a
    .byte   $00                                             ; 0b
    .byte   $00                                             ; 0c
    .byte   $00                                             ; 0d
    .byte   $00                                             ; 0e
    .byte   $00                                             ; 0f
    .byte   ANIMATE_GLITTER                                 ; 10 - Coin
    .byte   $00                                             ; 11
    .byte   $00                                             ; 12
    .byte   $00                                             ; 13
    .byte   $00                                             ; 14
    .byte   $00                                             ; 15
    .byte   $00                                             ; 16
    .byte   $00                                             ; 17
    .byte   $00                                             ; 18
    .byte   $00                                             ; 19
    .byte   $00                                             ; 1a
    .byte   $00                                             ; 1b
    .byte   $00                                             ; 1c
    .byte   $00                                             ; 1d
    .byte   $00                                             ; 1e
    .byte   $00                                             ; 1f
    .byte   $00                                             ; 20
    .byte   $00                                             ; 21
    .byte   $00                                             ; 22
    .byte   $00                                             ; 23
    .byte   $00                                             ; 24
    .byte   $00                                             ; 25
    .byte   $00                                             ; 26
    .byte   $00                                             ; 27
    .byte   $00                                             ; 28
    .byte   $00                                             ; 29
    .byte   $00                                             ; 2a
    .byte   $00                                             ; 2b
    .byte   $00                                             ; 1c
    .byte   $00                                             ; 1d
    .byte   $00                                             ; 1e
    .byte   $00                                             ; 2f
    .byte   $00                                             ; 30
    .byte   $00                                             ; 31
    .byte   $00                                             ; 32
    .byte   $00                                             ; 33
    .byte   $00                                             ; 34
    .byte   $00                                             ; 35 
    .byte   $00                                             ; 36
    .byte   $00                                             ; 37
    .byte   $00                                             ; 38
    .byte   $00                                             ; 39
    .byte   $00                                             ; 3a
    .byte   $00                                             ; 3b
    .byte   $00                                             ; 3c
    .byte   $00                                             ; 3d
    .byte   $00                                             ; 3e
    .byte   $00                                             ; 3f

    ; 80 - water
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

    ; a0 - blink
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
    .byte   0
    .byte   1

    ; c0 - alternate
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

    ; e0 - glitter
    .byte   1
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   1
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   1
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   1
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   1
    .byte   0
    .byte   0
    .byte   0
    .byte   0
    .byte   0


;-----------------------------------------------------------------------------

.include "action.asm"


