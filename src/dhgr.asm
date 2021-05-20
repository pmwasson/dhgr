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

spritePtr0      :=  $06     ; Sprite pointer
spritePtr1      :=  $07
screenPtr0      :=  $08     ; Screen pointer
screenPtr1      :=  $09

color           =   14
color0          =   $7f & (color + color<<4)  
color1          =   $7f & (color>>3 + color<<1 + color<<5)  
color2          =   $7f & (color>>2 + color<<2 + color<<6)  
color3          =   $7f & (color>>1 + color<<3)

.segment "CODE"
.org    $C00

.proc main

    jsr     init

    sta     SET80COL 

    sta     HISCR
    lda     #color0
    sta     colorA
    lda     #color2
    sta     colorB
    jsr     clearScreen

    sta     LOWSCR
    lda     #color1
    sta     colorA
    lda     #color3
    sta     colorB
    jsr     clearScreen

    ; wait for keypress
:
    lda     KBD
    bpl     :-

    sta     TXTSET

    ; exit
    jmp     MONZ

.endproc

.proc clearScreen
    lda     #$00
    sta     screenPtr0
    lda     #$20
    sta     screenPtr1

loop:
    ldy     #0
xloop:
    lda     colorA
    sta     (screenPtr0),y
    iny
    lda     colorB
    sta     (screenPtr0),y
    iny
    bne     xloop

    inc     screenPtr1
    lda     #$40
    cmp     screenPtr1
    bne     loop
    rts

.endproc

.proc init
    jsr   $c300         ; 80-column firmware
    jsr   $fc58

    sta     MIXCLR      ; Full Screen
    sta     HIRES       ; Hi-res
    sta     TXTCLR      ; Graphics
    sta     LOWSCR      ; Display page 1
    sta     DHIRESON    ; Annunciator 2 On
    
    sta     SET80COL    ; 80STOREON (c000 = off)

    rts

.endproc


.proc init_bw

; Not sure how this code works, but we end up in DHGR B&W mode
; Code provided by Antoine Vignau.  Thank you!

    jsr   $c300         ; 80-column firmware
    jsr   $fc58

    ; FORCE MONOCHROME 560*192
    sta     MIXCLR      ; Full Screen
    sta     HIRES       ; Hi-res
    sta     TXTCLR      ; Graphics
    sta     LOWSCR      ; Display page 1
    
    ldx     #2          ; Do it twice?
DCKD:   
    sta     SET80COL    ; 80STOREON (c000 = off)
    sta     DHIRESON    ; Annunciator 2 On
    sta     $c00c       ; 40-columns
    sta     $c05e       ; double hi-res on
    sta     $c05f       ; double hi-res off
    sta     $c00d       ; 80-columns
    sta     $c05e       ; double hi-res on
    dex
    bne     DCKD

    rts

.endproc

colorA:     .byte   color0
colorB:     .byte   color1
colorC:     .byte   color2
colorD:     .byte   color3
