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

; DHGR
tilePtr0        :=  $06     ; Tile pointer
tilePtr1        :=  $07
screenPtr0      :=  $08     ; Screen pointer
screenPtr1      :=  $09

; Map
mapPtr0         :=  $EC
mapPtr1         :=  $ED

; memory map
FILEBUFFER      =   $800        ; PRODOS filebuffer

;------------------------------------------------
; Constants
;------------------------------------------------

BOX_HORZ        = $40
BOX_VERT        = $41
BOX_UPPER_LEFT  = $42
BOX_UPPER_RIGHT = $43
BOX_LOWER_LEFT  = $44
BOX_LOWER_RIGHT = $45
BOX_RIGHT_TEE   = $46

DIALOG_UL       = 60
DIALOG_UP       = 30
DIALOG_UR       = 62
DIALOG_LF       = 27
DIALOG_RT       = 29
DIALOG_LL       = 38
DIALOG_LO       = 31
DIALOG_LR       = 39
DIALOG_RS1      = 58
DIALOG_RS2      = 59
DIALOG_LS1      = 42
DIALOG_LS2      = 47

MAP_WIDTH           =  64
MAP_HEIGHT          =  64
MAP_X_OFFSET        =   6
MAP_Y_OFFSET        =   6   ; must be even
MAP_SCREEN_WIDTH    =   7
MAP_SCREEN_HEIGHT   =   7

;------------------------------------------------

.segment "CODE"
.org    $6000

;=============================================================================
; Main program
;=============================================================================

; Main
;------------------------------------------------
.proc main

    jsr     $c300       ; 80 column mode
    jsr     HOME        ; clear screen
    lda     #23         ; put cursor on last line
    sta     CV
    jsr     VTAB

    jsr     dhgrInit    ; Turn on dhgr

    lda     #$00        ; Clear both pages
    sta     page        
    jsr     clearScreen
    jsr     drawFrame

    lda     #$20        ; Clear both pages
    sta     page        
    jsr     clearScreen
    jsr     drawFrame

    lda     #$00
    sta     page


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
    sta     page

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
    lda     mapY
    beq     done_up
    dec     mapY
done_up:
    jmp     gameLoop

:
 
    cmp     #KEY_DOWN
    bne     :+
    lda     mapY
    cmp     #MAP_HEIGHT-MAP_SCREEN_HEIGHT
    beq     done_down
    inc     mapY
done_down:
    jmp     gameLoop
:

    cmp     #KEY_LEFT
    bne     :+
    lda     mapX
    beq     done_left
    dec     mapX
done_left:
    jmp     gameLoop
:

    cmp     #KEY_RIGHT
    bne     :+
    lda     mapX
    cmp     #MAP_WIDTH-MAP_SCREEN_WIDTH
    beq     done_right
    inc     mapX
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
    sta     page


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
    jsr     drawTile_7x8

    ; Top
    inc     tileX
    inc     tileX
:
    lda     #DIALOG_UP
    jsr     drawTile_7x8
    
    inc     tileX
    inc     tileX
    lda     tileX
    cmp     dialogRight
    bne     :-

    ; Upper-right
    lda     #DIALOG_UR
    jsr     drawTile_7x8

    ; Dialog Row

dialog_row:

    inc     tileY
    lda     dialogLeft
    sta     tileX

    ; left - edge
    lda     #DIALOG_LF
    jsr     drawTile_7x8

    inc     tileX
    inc     tileX

dialog_string:
    ldy     stringIndex
    lda     (stringPtr0),y
    beq     next_row
    cmp     #13
    beq     next_row

    and     #$3f
    jsr     drawTile_7x8
    inc     tileX
    inc     tileX
    inc     stringIndex
    jmp     dialog_string

finish_row:

    lda     #32     ; space
    jsr     drawTile_7x8
    inc     tileX 
    inc     tileX 

next_row:

    lda     tileX
    cmp     dialogRight
    bne     finish_row

    lda     #DIALOG_RT
    jsr     drawTile_7x8

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
    jsr     drawTile_7x8

    inc     tileX
    inc     tileX

bottom_loop:

    lda     tileX
    cmp     dialogX
    bne     :+

    lda     #32         ;space
    jsr     drawTile_7x8

    inc     tileX
    inc     tileX

    lda     #32         ;space
    jsr     drawTile_7x8

    inc     tileX
    inc     tileX

:
    lda     #DIALOG_LO
    jsr     drawTile_7x8
    
    inc     tileX
    inc     tileX
    lda     dialogRight
    cmp     tileX
    bne     bottom_loop

    ; Lower-right
    lda     #DIALOG_LR
    jsr     drawTile_7x8

    inc     tileY

    lda     dialogX
    sta     tileX

    lda     dialogDir
    beq     :+

    lda     #DIALOG_RS1
    jsr     drawTile_7x8
    inc     tileX
    inc     tileX
    lda     #DIALOG_RS2
    jsr     drawTile_7x8
    rts

:

    lda     #DIALOG_LS1
    jsr     drawTile_7x8
    inc     tileX
    inc     tileX
    lda     #DIALOG_LS2
    jsr     drawTile_7x8
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
    jsr     drawTile_7x8

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
    adc     page
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

    lda     #0
    sta     indexY

loop_y:

    ; set pointer
    lda     mapY
    clc
    adc     indexY
    sta     worldY
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
    asl                 ; * MAP_WIDTH (64)
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
    adc     worldY
    adc     animateTime
    and     #$F
    sta     animateIndex
    lda     (mapPtr0),y
    tax
    ldy     animateTable,x      ; a = animation offst table base
    beq     :+
    clc
    tya
    adc     animateIndex
    tay
    txa
    adc     animateOffset,y     ; add offset


 ;   bit     TXTSET
 ;   brk

 :
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

worldY:         .byte   0
indexX:         .byte   0
indexY:         .byte   0
animateIndex:   .byte   0

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
    ; calculate tile pointer
    tay     ; save A
    lsr                     ; *128
    lda     #0
    ror
    sta     tilePtr0
    tya     ; restore A
    lsr                     ; /2
    clc
    adc     currentSheet_14x16+1
    sta     tilePtr1

    ; calculate screen pointer
    ldx     tileY
    lda     tileX
    clc
    adc     lineOffset,x    ; + lineOffset
    sta     screenPtr0    
    lda     linePage,x
    adc     page            ; assume carry still cleared
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
    adc     currentSheet_7x8+1
    sta     tilePtr1

    sta     CLR80COL        ; Use RAMWRT for aux mem

    ; calculate screen pointer
    ldx     tileY
    lda     tileX
    clc
    adc     lineOffset,x    ; + lineOffset
    sta     screenPtr0    
    lda     linePage,x
    adc     page
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
; Load Data
;   Load data using ProDOS
;-----------------------------------------------------------------------------
.proc loadData

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

.endproc

;-----------------------------------------------------------------------------
; Save data
;
;   Use prodos to save data
;-----------------------------------------------------------------------------
.proc saveData

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
    
.endproc

;-----------------------------------------------------------------------------
; Global ProDos parameters
;-----------------------------------------------------------------------------

open_params:
    .byte   $3
    .word   defaultPathname     ; *OVERWRITE* pathname     
    .word   FILEBUFFER
    .byte   $0                  ;             reference number

read_params:
    .byte   $4
    .byte   $0                  ;             reference number
    .word   $0                  ; *OVERWRITE* address of data buffer
    .word   $0                  ; *OVERWRITE* number of bytes to read
    .word   $0                  ;             number of bytes read

; Note, not using the real address for the binary file load address as
; the data buffer may move around with re-compiles, so we don't
; want to rely on it.

create_params:
    .byte   $7
    .word   defaultPathname     ; *OVERWRITE* pathname
    .byte   $C3                 ;             access bits (full access)
    .byte   $6                  ;             file type (binary)
    .word   $6000               ;             binary file load address, default to $6000 (above hires pages)
    .byte   $1                  ;             storage type (standard)
    .word   $0                  ;             creation date
    .word   $0                  ;             creation time

write_params:
    .byte   $4
    .byte   $0                  ;             reference number
    .word   MAPSHEET            ; *OVERWRITE* address of data buffer
    .word   MAPSHEET_SIZE       ;             number of bytes to write
    .word   $0                  ;             number of bytes written

close_params:
    .byte   $1
    .byte   $0                  ;             reference number

defaultPathname:
    .byte   7,"UNKNOWN"


;-----------------------------------------------------------------------------
; Utilies

.include "inline_print.asm"
.include "sounds.asm"

; Global Variables
;-----------------------------------------------------------------------------

page:               .byte   0
gameTime:           .byte   0
animateTime:        .byte   0

mapX:               .byte   0
mapY:               .byte   0

currentSheet_7x8:   .word   tileSheet_7x8
currentSheet_14x16: .word   tileSheet_14x16

tileX:              .byte   0
tileY:              .byte   0
invMask:            .byte   0

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

;--------------------------------------------------------------------------
; Default tiles
;--------------------------------------------------------------------------

.align 256

.include "tileSheet_dialog_7x8.asm"

; Add screen elements

    ; Screen Element
    ; Tiles $40 - $46
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

    ; Right Tee -|
    .byte $00,$01,$20,$00,$00,$01,$20,$00,$00,$01,$20,$00,$55,$01,$2A,$00           
    .byte $55,$01,$2A,$00,$00,$01,$20,$00,$00,$01,$20,$00,$00,$01,$20,$00           

;--------------------------------------------------------------------------

MAX_TILES       = 64
.include "tileSheet_14x16.asm"

; Append player

; Old-man right
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$40,$5D,$00,$00,$6E,$01,$00
.byte $00,$7C,$5D,$00,$00,$6E,$1B,$00,$00,$7C,$3F,$00,$00,$6E,$1B,$00
.byte $00,$7C,$5D,$03,$00,$6E,$3B,$00,$00,$7C,$0D,$00,$00,$6E,$11,$00
.byte $00,$7C,$5D,$00,$00,$6E,$1F,$00,$00,$7C,$7F,$00,$00,$7F,$1F,$00
.byte $00,$00,$7F,$00,$00,$7F,$1F,$00,$00,$00,$7A,$00,$00,$55,$1F,$00
.byte $00,$00,$2A,$00,$00,$45,$01,$00,$00,$00,$58,$00,$00,$45,$01,$00
.byte $00,$00,$6E,$00,$00,$77,$01,$00,$00,$40,$55,$00,$00,$2A,$00,$00
.byte $00,$40,$45,$00,$00,$2A,$08,$00,$00,$00,$00,$00,$00,$00,$00,$00

; Old-man right (mask)
.byte $7F,$3F,$00,$7F,$7F,$00,$7E,$7F,$7F,$03,$00,$7F,$7F,$00,$60,$7F
.byte $7F,$00,$00,$7C,$1F,$00,$00,$7F,$7F,$00,$00,$7C,$1F,$00,$00,$7F
.byte $7F,$00,$00,$40,$1F,$00,$00,$7F,$7F,$00,$00,$7C,$1F,$00,$00,$7F
.byte $7F,$00,$00,$7C,$1F,$00,$00,$7F,$7F,$00,$00,$7C,$1F,$00,$00,$7F
.byte $7F,$03,$00,$7C,$7F,$00,$00,$7F,$7F,$03,$00,$7C,$7F,$00,$00,$7F
.byte $7F,$03,$00,$7F,$7F,$00,$60,$7F,$7F,$03,$00,$7F,$7F,$00,$60,$7F
.byte $7F,$03,$00,$7F,$7F,$00,$60,$7F,$7F,$03,$00,$7F,$7F,$00,$60,$7F
.byte $7F,$03,$00,$7C,$7F,$00,$00,$7F,$7F,$3F,$00,$7F,$7F,$00,$60,$7F

; Old-man left
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$40,$5D,$00,$00,$6E,$01,$00
.byte $00,$74,$5D,$00,$00,$6E,$1F,$00,$00,$74,$5D,$00,$00,$7D,$1F,$00
.byte $00,$77,$5D,$00,$20,$6E,$1F,$00,$00,$20,$5D,$00,$00,$6C,$1F,$00
.byte $00,$7C,$5D,$00,$00,$6E,$1F,$00,$00,$7C,$7F,$00,$00,$7F,$1F,$00
.byte $00,$7C,$6F,$00,$00,$7F,$01,$00,$00,$7C,$2A,$00,$00,$57,$01,$00
.byte $00,$00,$28,$00,$00,$55,$01,$00,$00,$40,$28,$00,$00,$46,$01,$00
.byte $00,$00,$6E,$00,$00,$77,$01,$00,$00,$40,$55,$00,$00,$2A,$00,$00
.byte $00,$10,$55,$00,$00,$2A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; Old-man left (mask)
.byte $7F,$3F,$00,$7F,$7F,$00,$7E,$7F,$7F,$03,$00,$7F,$7F,$00,$60,$7F
.byte $7F,$00,$00,$7C,$1F,$00,$00,$7F,$7F,$00,$00,$7C,$1F,$00,$00,$7F
.byte $7F,$00,$00,$7C,$01,$00,$00,$7F,$7F,$00,$00,$7C,$1F,$00,$00,$7F
.byte $7F,$00,$00,$7C,$1F,$00,$00,$7F,$7F,$00,$00,$7C,$1F,$00,$00,$7F
.byte $7F,$00,$00,$7F,$1F,$00,$60,$7F,$7F,$00,$00,$7F,$1F,$00,$60,$7F
.byte $7F,$03,$00,$7F,$7F,$00,$60,$7F,$7F,$03,$00,$7F,$7F,$00,$60,$7F
.byte $7F,$03,$00,$7F,$7F,$00,$60,$7F,$7F,$03,$00,$7F,$7F,$00,$60,$7F
.byte $7F,$00,$00,$7F,$1F,$00,$60,$7F,$7F,$03,$00,$7F,$7F,$00,$7E,$7F

;--------------------------------------------------------------------------

.align 256

MAPSHEET_SIZE = MAPSHEET_END - MAPSHEET

MAPSHEET:
.include "map_64x64.asm"
MAPSHEET_END:

    .dword  .time   ; Time of compilation

;--------------------------------------------------------------------------
