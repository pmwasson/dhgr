;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; DHGR Toolbox
;  Tile editor
;  Map editor

;------------------------------------------------
; Constants
;------------------------------------------------

.include "defines.asm"
.include "macros.asm"

;------------------------------------------------
; Constants
;------------------------------------------------

MAX_TILES       = 64
BOX_HORZ        = 48
BOX_VERT        = 49
BOX_UPPER_LEFT  = 50
BOX_UPPER_RIGHT = 51
BOX_LOWER_LEFT  = 52
BOX_LOWER_RIGHT = 53
BOX_RIGHT_TEE   = 54
BOX_TILE        = 55 ; +1
BOX_ARROW       = 57 ; +1

;------------------------------------------------

.segment "CODE"
.org    $4000

;=============================================================================
; Initial jump vector
;=============================================================================

.proc main
    jmp     toolbox
.endproc

;=============================================================================
; Include tools
;=============================================================================

.include "tile_edit.asm"

.include "map_edit.asm"

;=============================================================================
; Toolbox program
;=============================================================================

;------------------------------------------------
; Global Scope
.proc   toolbox
;------------------------------------------------

; Title loop
;------------------------------------------------
.proc titleLoop

    jsr     tile_edit::init     ; Init tile editor
    jsr     map_edit::init      ; Init map editor

    jsr     $c300       ; 80 column mode
    jsr     HOME        ; clear screen
    lda     #23         ; put cursor on last line
    sta     CV
    jsr     VTAB

    jsr     dhgrInit    ; Turn on dhgr

    jsr     clearScreen
    jsr     displayTitle

loop:
    jsr     map_edit::main
    jsr     tile_edit::main
    jmp     loop

.endproc

;------------------------------------------------
; Display title
;------------------------------------------------

.proc displayTitle

    ; Draw title box
    lda     #0
    sta     boxLeft
    lda     #38
    sta     boxRight
    lda     #4
    sta     boxTop
    lda     #14
    sta     boxBottom
    jsr     drawBox

    lda     #2
    sta     tileX
    lda     #5
    sta     tileY

    jsr     drawString
    .byte "  DHGR TOOLBOX:",13,13
    .byte " ] TILE EDITOR [",13
    .byte " ] MAP EDITOR  [",13,13
    .byte "  BY PAUL WASSON",13
    .byte "  JUNE 2021",13,13
    .byte "PRESS TAB TO BEGIN",0

:
    lda     KBD
    bpl     :-

    sta     KBDSTRB
    cmp     #KEY_TAB
    bne     :-

    rts   

.endproc

;------------------------------------------------
; Global Scope End
;------------------------------------------------
.endproc

;=============================================================================
; Common routines to all tools
;=============================================================================

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
    jsr     drawInterfaceTile_7x8

    lda     boxRight
    sta     tileX    
    lda     #BOX_UPPER_RIGHT
    jsr     drawInterfaceTile_7x8

    lda     boxBottom
    sta     tileY
    lda     #BOX_LOWER_RIGHT
    jsr     drawInterfaceTile_7x8

    lda     boxLeft
    sta     tileX
    lda     #BOX_LOWER_LEFT
    jsr     drawInterfaceTile_7x8

    ; Draw horizontal

    inc     tileX
    inc     tileX
:
    lda     boxTop
    sta     tileY
    lda     #BOX_HORZ
    jsr     drawInterfaceTile_7x8
    
    lda     boxBottom
    sta     tileY
    lda     #BOX_HORZ
    jsr     drawInterfaceTile_7x8
    
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
    jsr     drawInterfaceTile_7x8
    
    lda     boxRight
    sta     tileX
    lda     #BOX_VERT
    jsr     drawInterfaceTile_7x8
    
    inc     tileY
    lda     boxBottom
    cmp     tileY
    bne     :-

    rts

.endproc

;-----------------------------------------------------------------------------
; Draaw String
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
; Draaw Tile Number
;
;   Draw 14x16 tile index with number passed in A
;-----------------------------------------------------------------------------

.proc drawTileNumber
    sta     tileIndex

    lda     #BOX_TILE
    jsr     drawInterfaceTile_7x8

    inc     tileY
    lda     tileIndex
    lsr     
    lsr     
    lsr     
    lsr     
    jsr     drawInterfaceTile_7x8

    inc     tileX
    inc     tileX
    lda     tileIndex
    and     #$f
    jsr     drawInterfaceTile_7x8

    dec     tileY
    lda     #BOX_TILE+1
    jsr     drawInterfaceTile_7x8

    rts

tileIndex:  .byte   0

.endproc

.proc drawTileNumberSelected
    sta     tileIndex

    lda     #BOX_ARROW
    jsr     drawInterfaceTile_7x8

    inc     tileY
    lda     tileIndex
    lsr     
    lsr     
    lsr     
    lsr     
    jsr     drawInterfaceTile_7x8

    inc     tileX
    inc     tileX
    lda     tileIndex
    and     #$f
    jsr     drawInterfaceTile_7x8

    dec     tileY
    lda     #BOX_ARROW+1
    jsr     drawInterfaceTile_7x8

    rts

tileIndex:  .byte   0

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
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #2
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #3
    lda     (bgPtr0),y
    sta     (screenPtr0),y

    ; Bytes 4-7 in MAIN memory
    ;
    sta     RAMWRTOFF

    lda     bgPtr0    ; offset tile pointer by 4
    adc     #4
    sta     bgPtr0

    ldy     #0
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #2
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #3
    lda     (bgPtr0),y
    sta     (screenPtr0),y

    ; assumes aligned such that there are no page crossing
    lda     bgPtr0
    adc     #4
    sta     bgPtr0

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
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #2
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #3
    lda     (bgPtr0),y
    sta     (screenPtr0),y

    ; Bytes 4-7 in MAIN memory
    ;
    sta     RAMWRTOFF

    lda     bgPtr0    ; offset tile pointer by 4
    adc     #4
    sta     bgPtr0

    ldy     #0
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #2
    lda     (bgPtr0),y
    sta     (screenPtr0),y
    ldy     #3
    lda     (bgPtr0),y
    sta     (screenPtr0),y

    ; assumes aligned such that there are no page crossing
    lda     bgPtr0
    adc     #4
    sta     bgPtr0

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
    sta     bgPtr0
    tya     ; restore A
    lsr                     ; /2
    clc
    adc     currentSheet_14x16+1
    sta     bgPtr1
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

bypassTilePointer:

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
    lda     (bgPtr0),y
    eor     invMask
    sta     (screenPtr0),y
    ldy     #1
    lda     (bgPtr0),y
    eor     invMask
    sta     (screenPtr0),y

    ; Bytes 2-3 in MAIN memory
    ;
    sta     RAMWRTOFF

    lda     bgPtr0    ; offset tile pointer by 2
    adc     #2
    sta     bgPtr0

    ldy     #0
    lda     (bgPtr0),y
    eor     invMask
    sta     (screenPtr0),y
    ldy     #1
    lda     (bgPtr0),y
    eor     invMask
    sta     (screenPtr0),y

    ; assumes aligned such that there are no page crossing
    lda     bgPtr0
    adc     #2
    sta     bgPtr0

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
    sta     bgPtr0
    tya     ; restore A
    lsr                     ; /8
    lsr
    lsr
    clc
    adc     currentSheet_7x8+1
    sta     bgPtr1

    rts

.endproc

;-----------------------------------------------------------------------------
; draw interface tile 7x8
;
; Special version of draw tile for the interface
;-----------------------------------------------------------------------------

.proc drawInterfaceTile_7x8
    ; calc tile ptr using interface tilesheet

    tay     ; copy A
    ; calculate tile pointer
    asl                     ; *32
    asl
    asl
    asl
    asl
    sta     bgPtr0
    tya     ; restore A
    lsr                     ; /8
    lsr
    lsr
    clc
    adc     #>interface_7x8
    sta     bgPtr1

    jmp     drawTile_7x8::bypassTilePointer
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

currentSheet_7x8:   .word   tileSheet_7x8
currentSheet_14x16: .word   tileSheet_14x16

; Box routine   
boxLeft:            .byte   0
boxRight:           .byte   0
boxTop:             .byte   0
boxBottom:          .byte   0

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

;--------------------------------------------------------------------------
; Internal tile sheet for interface
;--------------------------------------------------------------------------

.align 256
interface_7x8:

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

    ; 4
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

    ; Right Tee -|
    .byte $00,$01,$20,$00,$00,$01,$20,$00,$00,$01,$20,$00,$55,$01,$2A,$00           
    .byte $55,$01,$2A,$00,$00,$01,$20,$00,$00,$01,$20,$00,$00,$01,$20,$00           

    ;; "TI" - White
    ;.byte $00,$00,$00,$00,$00,$00,$00,$00,$7F,$3C,$1F,$78,$70,$3C,$01,$78           
    ;.byte $70,$3C,$01,$78,$70,$3C,$01,$78,$70,$3C,$01,$78,$00,$00,$00,$00           
    ;
    ;; "LE" - White
    ;.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$7F,$60,$07,$00,$03,$60,$00           
    ;.byte $00,$3F,$60,$00,$00,$03,$60,$00,$7F,$7F,$61,$07,$00,$00,$00,$00           

    ; "TI" - Gray
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$2A,$28,$15,$50,$20,$28,$01,$50           
    .byte $20,$28,$01,$50,$20,$28,$01,$50,$20,$28,$01,$50,$00,$00,$00,$00           

    ; "LE" - Gray
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$2A,$40,$05,$00,$02,$40,$00           
    .byte $00,$2A,$40,$00,$00,$02,$40,$00,$2A,$2A,$41,$05,$00,$00,$00,$00           

    ; Arrow 0
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$60,$3B,$5D,$77           
    .byte $20,$2A,$55,$55,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00           

    ; Arrow 1
    .byte $00,$00,$00,$00,$00,$03,$40,$00,$00,$3B,$40,$00,$6E,$3B,$5D,$07           
    .byte $2A,$3B,$55,$05,$00,$2B,$40,$00,$00,$02,$40,$00,$00,$00,$00,$00           

;--------------------------------------------------------------------------
; Default tiles
;--------------------------------------------------------------------------

.align 256

; Calculate size of tilesheets
TILESHEET_SIZE = TILESHEET_END - TILESHEET

; Start of tilesheet
TILESHEET:

.include "tileSheet_7x8.asm"

.include "tileSheet_14x16.asm"

; End of tilesheet
TILESHEET_END:

; Include a "cursor" tile for map edit after the user tiles
    ; 14x16 Cursor
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$14,$50,$41,$0A,$28,$20,$02           
    .byte $50,$41,$05,$14,$20,$02,$0A,$00,$00,$14,$50,$41,$0A,$28,$20,$02           
    .byte $50,$41,$05,$14,$20,$02,$0A,$00,$00,$14,$50,$41,$0A,$28,$20,$02           
    .byte $50,$41,$05,$14,$20,$02,$0A,$00,$00,$14,$50,$41,$0A,$28,$20,$02           
    .byte $50,$41,$05,$14,$20,$02,$0A,$00,$00,$14,$50,$41,$0A,$28,$20,$02           
    .byte $50,$41,$05,$14,$20,$02,$0A,$00,$00,$14,$50,$41,$0A,$28,$20,$02           
    .byte $50,$41,$05,$14,$20,$02,$0A,$00,$00,$14,$50,$41,$0A,$28,$20,$02           
    .byte $50,$41,$05,$14,$20,$02,$0A,$00,$00,$00,$00,$00,$00,$00,$00,$00           


.align 256

MAPSHEET_SIZE = MAPSHEET_END - MAPSHEET

MAPSHEET:
.include "map_64x64.asm"
MAPSHEET_END:

    .dword  .time   ; Time of compilation

;--------------------------------------------------------------------------
