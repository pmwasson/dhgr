;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
;  Game engine

;------------------------------------------------
; Constants
;------------------------------------------------

.include "defines.asm"
.include "macros.asm"

.segment "CODE"
.org    $C00

; Jump table (in fixed locations)
    jmp     drawTest            ; Remove once game calls engine
    jmp     drawInit
    jmp     drawTile_7x8
    jmp     drawTile_14x16
    jmp     drawTileFG_14x16
    jmp     readMap

; Variables (in fixed locations)
.align 32
bgSheet_14x16:  .word   $9000
fgSheet_14x16:  .word   $A000
bgSheet_7x8:    .word   $B000
mapSheet:       .word   $6000
mapWindowWidth: .byte   7
mapWindowHeight:.byte   7

;-----------------------------------------------------------------------------
; drawInit
;
; Set up aux memory
;-----------------------------------------------------------------------------
.proc drawInit

    ; copy code to aux memory
    lda     #<auxMemStart
    sta     A1
    sta     A4
    lda     #>auxMemStart
    sta     A1+1
    sta     A4+1
    lda     #<auxMemEnd
    sta     A2
    lda     #>auxMemEnd
    sta     A2+1
    sec                     ; copy from main to aux
    jsr     AUXMOVE

    ; init variables
    lda     #0
    sta     drawPage
    sta     invMask
    rts

.endproc

auxMemStart:
; This code is copied such that it exists in both main and aux memory.

;-----------------------------------------------------------------------------
; drawTile (14x16)
;  Assume 14x16, where 14 is 14*4 pixels = 56 -> 8 bytes
;    8*16 = 128 (split main/aux), so 4 tiles per page
;
;            0 1 2 3 4 5 6 7 8 9 10 11 12 13 ; 4-bit pixels in 7-bit byes (MSB ignored)
;            -0-   --1--   -2-   ----3---    ; AUX memory
;              --0--   -1-   ---2--    --3-- ; Main memory
;-----------------------------------------------------------------------------

.proc drawTile_14x16

    sta     CLR80COL        ; Use RAMWRT for aux mem

    lda     bgTile
    ror                     ; *64
    ror
    ror
    and     #$c0
    sta     bgPtr0
    sta     bgPtr0Copy

    lda     bgTile
    lsr
    lsr
    clc
    adc     bgSheet_14x16+1
    sta     bgPtr1

    ; calculate screen pointer
    ldx     tileY
    lda     tileX
    clc
    adc     lineOffset,x    ; + lineOffset
    sta     screenPtr0    
    sta     screenPtr0Copy    
    lda     linePage,x
    adc     drawPage
    sta     screenPtr1
    sta     screenPtr1Copy    

    ; draw half of tile in main mem
    jsr     drawTile

    ; restore tile pointers (page byte doesn't change)
    lda     bgPtr0Copy
    sta     bgPtr0

    ; restore screen pointer
    lda     screenPtr0Copy
    sta     screenPtr0
    lda     screenPtr1Copy
    sta     screenPtr1

    ; transfer to aux memory
    sta     RAMWRTON  
    sta     RAMRDON  

    ; draw half of tile in aux mem
    jsr     drawTile    ; AUX

    sta     RAMWRTOFF   ; AUX
    sta     RAMRDOFF    ; AUX

    rts

drawTile:

    ldx     #16             ; 8 lines

drawLoop:
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

    ; Add 4 to tile pointers
    ; assumes aligned such that there are no page crossing
    clc
    lda     bgPtr0
    adc     #4
    sta     bgPtr0

    ; Go to next line

    lda     screenPtr1
    adc     #4
    sta     screenPtr1

    dex
    beq     done            ; did 16 lines

    cpx     #8              ; after 8 lines, adjust screen pointer
    bne     drawLoop

    ; move to second half
    lda     screenPtr0
    clc
    adc     #$80
    sta     screenPtr0
    lda     screenPtr1
    sbc     #$1f            ; subtract 20 if no carry, 19 if carry
    sta     screenPtr1
    jmp     drawLoop

done:
    rts    

; locals
bgPtr0Copy:     .byte   0
screenPtr0Copy: .byte   0
screenPtr1Copy: .byte   0

.endproc


;-----------------------------------------------------------------------------
; draw tile foreground (14x16)
;
;   Draw a foreground tile on top of the screen.
;   It is assumed that the foreground tile comes first and the
;   mask follows in aligned memory.
;-----------------------------------------------------------------------------

.proc drawTileFG_14x16

    sta     CLR80COL        ; Use RAMWRT for aux mem

    ; Assume fg tile is followed by mask
    ; calculate tile pointer
    lda     fgTile
    lsr                     ; *128
    lda     #0
    ror
    sta     fgPtr0
    sta     fgPtr0Copy
    ora     #$40            ; add 64
    sta     maskPtr0
    sta     maskPtr0Copy

    lda     fgTile
    lsr                     ; /2
    clc
    adc     fgSheet_14x16+1
    sta     fgPtr1
    sta     maskPtr1

    ; calculate screen pointer
    ldx     tileY
    lda     tileX
    clc
    adc     lineOffset,x    ; + lineOffset
    sta     screenPtr0    
    sta     screenPtr0Copy    
    lda     linePage,x
    adc     drawPage
    sta     screenPtr1
    sta     screenPtr1Copy    

    ; draw half of tile in main mem
    jsr     drawTile

    ; restore tile pointers (page byte doesn't change)
    lda     fgPtr0Copy
    sta     fgPtr0
    lda     maskPtr0Copy
    sta     maskPtr0

    ; restore screen pointer
    lda     screenPtr0Copy
    sta     screenPtr0
    lda     screenPtr1Copy
    sta     screenPtr1

    ; transfer to aux memory
    sta     RAMWRTON  
    sta     RAMRDON  

    ; draw half of tile in aux mem
    jsr     drawTile    ; AUX

    sta     RAMWRTOFF   ; AUX
    sta     RAMRDOFF    ; AUX

    rts

drawTile:

    ldx     #16             ; 16 lines

drawLoop:
    ldy     #0
    lda     (screenPtr0),y
    and     (maskPtr0),y
    ora     (fgPtr0),y
    sta     (screenPtr0),y
    ldy     #1
    lda     (screenPtr0),y
    and     (maskPtr0),y
    ora     (fgPtr0),y
    sta     (screenPtr0),y
    ldy     #2
    lda     (screenPtr0),y
    and     (maskPtr0),y
    ora     (fgPtr0),y
    sta     (screenPtr0),y
    ldy     #3
    lda     (screenPtr0),y
    and     (maskPtr0),y
    ora     (fgPtr0),y
    sta     (screenPtr0),y

    ; Add 4 to tile pointers
    ; assumes aligned such that there are no page crossing
    clc
    lda     maskPtr0
    adc     #4
    sta     maskPtr0

    lda     fgPtr0
    adc     #4
    sta     fgPtr0

    ; Go to next line

    lda     screenPtr1
    adc     #4
    sta     screenPtr1

    dex
    beq     done            ; did 16 lines

    cpx     #8              ; after 8 lines, adjust screen pointer
    bne     drawLoop

    ; move to second half
    lda     screenPtr0
    clc
    adc     #$80
    sta     screenPtr0
    lda     screenPtr1
    sbc     #$1f            ; subtract 20 if no carry, 19 if carry
    sta     screenPtr1
    jmp     drawLoop

done:
    rts    

; locals
fgPtr0Copy:     .byte   0
maskPtr0Copy:   .byte   0
screenPtr0Copy: .byte   0
screenPtr1Copy: .byte   0

.endproc

;-----------------------------------------------------------------------------
; drawTile (7x8)
;  Assume 7x8, where 7 is 7*4 pixels = 28 -> 4 bytes
;    4*8 = 32 (split main/aux), so 16 tiles per page
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

    sta     CLR80COL        ; Use RAMWRT for aux mem

    lda     bgTile
    ; calculate tile pointer
    asl                     ; *16
    asl
    asl
    asl
    sta     bgPtr0
    sta     bgPtr0Copy
    lda     bgTile
    lsr                     ; /16
    lsr
    lsr
    lsr
    clc
    adc     bgSheet_7x8+1
    sta     bgPtr1

    ; calculate screen pointer
    ldx     tileY
    lda     tileX
    clc
    adc     lineOffset,x    ; + lineOffset
    sta     screenPtr0    
    sta     screenPtr0Copy
    lda     linePage,x
    adc     drawPage
    sta     screenPtr1
    sta     screenPtr1Copy

    jsr     drawTile

    ; restore tile pointers (page byte doesn't change)
    lda     bgPtr0Copy
    sta     bgPtr0

    ; restore screen pointer
    lda     screenPtr0Copy
    sta     screenPtr0
    lda     screenPtr1Copy
    sta     screenPtr1

    ; transfer to aux memory
    sta     RAMWRTON  
    sta     RAMRDON  

    ; draw half of tile in aux mem
    jsr     drawTile    ; AUX

    sta     RAMWRTOFF   ; AUX
    sta     RAMRDOFF    ; AUX

    rts

drawTile:

    clc     ; no carry generated inside of loop
    ldx     #8  ; 8 lines

drawLoop:
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
    bne     drawLoop

    rts    

; locals
bgPtr0Copy:     .byte   0
screenPtr0Copy: .byte   0
screenPtr1Copy: .byte   0

.endproc


;-----------------------------------------------------------------------------
; readMap
;   Copy the visible portion of the map into a buffer
;
;   This function reads from aux memory and writes to main memory
;-----------------------------------------------------------------------------

.proc readMap

    ; calc map pointer
    lda     mapWindowY
    lsr                 ; /2
    clc
    adc     mapSheet+1
    sta     mapPtr1


    lda     mapWindowY
    ror                 ; * 128
    ror
    and     #$80
    clc
    adc     mapWindowX
    adc     mapWindowX  ; * 2
    sta     mapPtr0     ; assume 256 aligned


    ; Loop over height x width
    ; Use X as pointer into buffer
    ; Use Y for indirect index

    ldx     #0

    lda     mapWindowHeight
    sta     loopCountY

loopy:
    lda     mapWindowWidth
    asl     ; *2
    sta     loopCountX

    ldy     #0

loopx:

    ; transfer to aux memory for read
    sta     RAMRDON  
    lda     (mapPtr0),y     ; AUX
    sta     RAMRDOFF        ; AUX

    sta     MAP_BUFFER,x

    iny
    inx

    dec     loopCountX
    bne     loopx

    ; move to next line
    clc
    lda     mapPtr0
    adc     #128
    sta     mapPtr0

    lda     mapPtr1
    adc     #0
    sta     mapPtr1

    dec     loopCountY
    bne     loopy

    rts

loopCountX:     .byte   0
loopCountY:     .byte   0

.endproc

auxMemEnd:

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

;-----------------------------------------------------------------------------
; drawTest
;
; Set up aux memory
;-----------------------------------------------------------------------------
.proc drawTest

    ; Set ctrl-y vector
    lda     #$4c        ; JMP
    sta     $3f8
    lda     #<quit
    sta     $3f9
    lda     #>quit
    sta     $3fa

    jsr     clearScreen

    jsr     drawInit    ; init code

    ; init DHGR
    sta     TXTCLR      ; Graphics
    sta     HIRES       ; Hi-res
    sta     MIXSET      ; Mixed
    sta     LOWSCR      ; Display page 1
    sta     DHIRESON    ; Annunciator 2 On
    sta     SET80VID    ; 80 column on

    ; test font

    lda     #0    
    sta     bgTile
    sta     tileX
    sta     tileY

:
    jsr     drawTile_7x8
    inc     tileX
    inc     tileX
    inc     bgTile

    lda     bgTile
    and     #$0F
    bne     :-

    lda     #0
    sta     tileX
    inc     tileY

    lda     tileY
    cmp     #4
    bne     :-

    lda     #0
    sta     bgTile


:
    jsr     drawTile_14x16
    inc     tileX
    inc     tileX
    inc     tileX
    inc     tileX
    inc     bgTile

    lda     bgTile
    and     #$07
    bne     :-

    lda     #0
    sta     tileX
    inc     tileY
    inc     tileY

    lda     tileY
    cmp     #8
    bne     :-


    lda     #0
    sta     bgTile
    sta     fgTile

:
    jsr     drawTileFG_14x16
    inc     tileX
    inc     tileX
    inc     tileX
    inc     tileX
    inc     bgTile
    inc     fgTile

    lda     bgTile
    and     #$07
    bne     :-

    lda     #0
    sta     tileX
    inc     tileY
    inc     tileY

    lda     tileY
    cmp     #12
    bne     :-



    ; Exit to monitor
    jmp     MONZ        ; enter monitor

clearScreen:
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
; Quit
;
;   Exit to ProDos
;-----------------------------------------------------------------------------
.proc quit

    jsr     MLI
    .byte   CMD_QUIT
    .word   quit_params

quit_params:
    .byte   4               ; 4 parameters
    .byte   0               ; 0 is the only quit type
    .word   0               ; Reserved pointer for future use (what future?)
    .byte   0               ; Reserved byte for future use (what future?)
    .word   0               ; Reserved pointer for future use (what future?)


.endproc