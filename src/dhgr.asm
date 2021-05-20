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


.segment "CODE"
.org    $C00


.proc main

    jsr     init


; Not sure how this code works, but we end up in DHGR B&W mode
; Code provided by Antoine Vignau.  Thank you!

init:
;    lda   #$21
;    sta   $c029        ; new video -- apple IIgs
;    lda   $c034        ; clock control -- apple IIgs
;    and   #$f0
;    ora   #$0f
;    sta   $c034        ; clock control -- apple IIgs

    jsr   $c300         ; 80-column firmware
    jsr   $fc58

    ; FORCE MONOCHROME 560*192
    sta     $c052       ; Full Screen
    sta     $c057       ; Hi-res
    sta     $c050       ; Graphics
    sta     $c054       ; Display page 1
    
    ldx     #2          ; Do it twice?
DCKD:   
    sta     $c001       ; 80STOREON (c000 = off)
    sta     $c05d       ; Annunciator 2 On
    sta     $c00c       ; 40-columns
    sta     $c05e       ; double hi-res on
    sta     $c05f       ; double hi-res off
    sta     $c00d       ; 80-columns
    sta     $c05e       ; double hi-res on
    dex
    bne     DCKD

    rts




gameLoop:
    inc     gameClock

    jsr     draw_screen

    jsr     get_input

    cmp     #KEY_QUIT
    bne     :+
    jmp     quit_game
:

    ; Update actors
    jsr     update_actors

    ; Bullet
    bit     bulletY
    bpl     update_bullet

    bit     BUTTON0
    bpl     :+
    clc
    lda     playerX
    adc     #2
    sta     bulletX
    lda     playerY
    sta     bulletY
    jsr     sound_shoot

update_bullet:
    lda     gameClock
    and     #1
    bne     :+
    dec     bulletY
:


    ; Movement
    lda     paddlePosition
    bmi     paddle_left
    beq     paddle_middle

    ; must be right
    ;lda     #0; 2
    ;sta     playerSprite

    lda     playerX
    cmp     #39-4
    bpl     gameLoop
    inc     playerX
    jmp     gameLoop

paddle_left:
    ;lda     #0; 1
    ;sta     playerSprite

    lda     playerX
    beq     gameLoop
    dec     playerX
    jmp     gameLoop

paddle_middle:
    ;lda     #0
    ;sta     playerSprite
    jmp     gameLoop

.endproc

;-----------------------------------------------------------------------------
; quit_game
;-----------------------------------------------------------------------------

.proc quit_game   

    sta     LOWSCR      ; Make sure exit onto screen 1
    jmp     MONZ

.endproc

;-----------------------------------------------------------------------------
; set_level_1
;-----------------------------------------------------------------------------
; Reset game state for level 1

.proc set_level_1

    ; copy level1 data
    ldx     #0
:
    lda     level1,x
    sta     actors,x
    inx
    cpx     #ACTOR_SIZE*ACTOR_MAX_COUNT
    bne     :-

    rts

;   state           - 0=inactive
;   shape           -
;   path_index      -
;   x_lo            - decimal
;   x_hi            - screen coordinate
;   y_lo            - decimal
;   y_hi            - screen coordinate
;   count           -

level1:
    ;       active,shape,path,xlo,xhi,ylo,yhi,count
    .byte   1,     10,   0,   0,  2,  0,  256-7,  0   
    .byte   1,     6,    0,   0,  8,  0,  256-9,  0   
    .byte   1,     6,    0,   0,  14, 0,  256-10,  0   
    .byte   1,     6,    0,   0,  20, 0,  256-10,  0   
    .byte   1,     6,    0,   0,  26, 0,  256-9,  0   
    .byte   1,     10,   0,   0,  31, 0,  256-7,  0   
    .byte   1,     8,    0,   0,  16, 0,  256-5,  0   
    .res    (8*(8-7))    
.endproc

;-----------------------------------------------------------------------------
; get_input
;-----------------------------------------------------------------------------
; Return key with bit 7 clear, or -1
; Also read paddle 0

.proc get_input

    ; assume middle to start with
    lda     #0
    sta     paddlePosition

    ; read paddle
    ldx     #0
    jsr     PREAD
    tya
    bmi     check_right

    ; LEFT
    cmp     #64
    bpl     keyboard
    lda     #$FF
    sta     paddlePosition
    jmp     keyboard

    ; RIGHT
check_right:
    cmp     #192
    bmi     keyboard
    lda     #1
    sta     paddlePosition

keyboard:
    lda     KBD
    bmi     gotKey

    ; exit with no key
    lda     #$ff
    rts

gotKey: 
    sta     KBDSTRB
    and     #$7f
    rts

.endproc

;-----------------------------------------------------------------------------
; update_actors
;-----------------------------------------------------------------------------
; actor:
;   state           - 0=inactive
;   shape           -
;   path_index      -
;   x_lo            - decimal
;   x_hi            - screen coordinate
;   y_lo            - decimal
;   y_hi            - screen coordinate
;   count           -
;
; path:
;   x
;   y
;   count
;   next_path

.proc update_actors

    lda     #0              ; point to first actor

actor_loop:
    tax
    lda     actors,x        ; state
    bne     :+

    ; skip to next actor
    txa
    clc
    adc     #ACTOR_SIZE
    cmp     #ACTOR_MAX_COUNT*ACTOR_SIZE
    bne     actor_loop

    ; done with list
    rts

:
    ldy     actors+ACTOR_PATH,x
    lda     path+PATH_X,y
    asl
    bcs     x_neg
    clc
    adc     actors+ACTOR_X_LO,x        
    sta     actors+ACTOR_X_LO,x     ; x_lo = path_x + x_lo

    lda     actors+ACTOR_X_HI,x
    adc     #0
    sta     actors+ACTOR_X_HI,x     ; x_hi = x_hi + carry

    jmp     do_y

x_neg:
    ; carry already set
    sta     temp2                   ; remember value to subtract
    lda     actors+ACTOR_X_LO,x        
    sbc     temp2
    sta     actors+ACTOR_X_LO,x     ; x_lo = x_lo - path_x

    lda     actors+ACTOR_X_HI,x
    sbc     #0
    sta     actors+ACTOR_X_HI,x     ; x_hi = x_hi - !carry

do_y:

    lda     path+PATH_Y,y           ; path_y
    asl
    bcs     y_neg
    clc
    adc     actors+ACTOR_Y_LO,x        
    sta     actors+ACTOR_Y_LO,x     ; y_lo = path_y + y_lo

    lda     actors+ACTOR_Y_HI,x
    adc     #0
    sta     actors+ACTOR_Y_HI,x     ; y_hi = y_hi + carry
    jmp     do_path

y_neg:
    ; carry already set
    sta     temp2                   ; remember value to subtract
    lda     actors+ACTOR_Y_LO,x        
    sbc     temp2     
    sta     actors+ACTOR_Y_LO,x     ; y_lo = y_lo - path_y

    lda     actors+ACTOR_Y_HI,x
    sbc     #0
    sta     actors+ACTOR_Y_HI,x     ; y_hi = y_hi - !carry

do_path:
    inc     actors+ACTOR_COUNT,x    ; increment count
    lda     actors+ACTOR_COUNT,x    ; actor_count
    cmp     path+PATH_COUNT,y       ; path_count
    bne     path_good

    lda     #0
    sta     actors+ACTOR_X_LO,x     ; reset x_lo to avoid accumulated error
    sta     actors+ACTOR_Y_LO,x     ; reset y_lo to avoid accumulated error
    sta     actors+ACTOR_COUNT,x    ; reset count
    lda     path+PATH_NEXT,y        ; next_path
    sta     actors+ACTOR_PATH,x     ; path = next_path

path_good:
    ; skip to next actor
    txa
    clc
    adc     #ACTOR_SIZE
    cmp     #ACTOR_MAX_COUNT*ACTOR_SIZE
    beq     :+
    jmp     actor_loop
:
    rts

temp1:  .byte   0
temp2:  .byte   0

.endproc


;-----------------------------------------------------------------------------
; draw_actors
;-----------------------------------------------------------------------------
; actor:
;   state           - 0=inactive
;   shape           -
;   path_index      -
;   x_lo            - decimal
;   x_hi            - screen coordinate
;   y_lo            - decimal
;   y_hi            - screen coordinate
;   count           -

.proc draw_actors

    ; set up animation value
    lda     gameClock
    lsr
    lsr
    lsr
    lsr
    and     #1
    sta     animate

    lda     #0              ; point to first actor

actor_loop:
    sta     actor_index
    tax
    lda     actors,x        ; state
    beq     :+

    lda     actors+ACTOR_X_HI,x
    sta     spriteX
    lda     actors+ACTOR_Y_HI,x
    sta     spriteY
    lda     actors+ACTOR_SHAPE,x
    eor     animate
    jsr     draw_sprite


    ldx     actor_index
    lda     actors+ACTOR_X_HI,x
    tax
    ldy     #0
    lda     #0
    jsr     draw_value

    ldx     actor_index
    lda     actors+ACTOR_Y_HI,x
    tax
    ldy     #0
    lda     #3
    jsr     draw_value

:
    ; next actor
    clc
    lda     actor_index
    adc     #ACTOR_SIZE
    cmp     #ACTOR_MAX_COUNT*ACTOR_SIZE
    bne     actor_loop

    ; done with list
    rts

actor_index:    .byte   0
animate:        .byte   0
.endproc


;-----------------------------------------------------------------------------
; draw_screen
;-----------------------------------------------------------------------------

.proc draw_screen

    ; Alternate page to draw
    ;-------------------------------------------------------------------------
    lda     #4      ; if showing page 2, draw on page 1
    ldx     PAGE2
    bmi     pageSelect
    lda     #8      ; displaying page 1, draw on page 2
pageSelect:
    sta     drawPage
    clc
    adc     #4
    sta     drawNextPage


    ; screen screen and draw stars
    jsr     star_screen

    ; draw actors
    jsr     draw_actors

    ; draw ship
    lda     playerX
    sta     spriteX
    lda     playerY
    sta     spriteY  
    lda     #0
    bit     BUTTON0
    bpl     :+
    lda     #1
:
    jsr     draw_sprite


    ; draw bullet
    lda     bulletX
    ldy     bulletY
    ldx     #'*' | $80
    jsr     draw_char

    ; draw lives
    lda     #0
    sta     spriteX
    lda     #23
    sta     spriteY
    lda     #5
    jsr     draw_sprite



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

alienAnimate:   .byte   0

.endproc

;-----------------------------------------------------------------------------
; star_screen
; clear screen with stars!
;-----------------------------------------------------------------------------

.proc star_screen

    ; set up pointer to first line
    clc
    lda     #0
    sta     screenPtr0
    lda     drawPage            ; either 04 or 08
    sta     screenPtr1

    ; load position in star field
    ldx     starOffset

rowLoop:
    ; erase line
    lda     #$a0
    ldy     #39
:
    sta     (screenPtr0),y
    dey
    bpl     :-

    ; draw 1 star per line

    lda     #'.' | $80
    ldy     starTable,x
    bpl     :+
    tya
    and     #$7f        ; clear bit 7
    tay
    lda     #$ac        ; slightly different star
:
    sta     (screenPtr0),y

    ; increment to the next line

    inx     ; next star

    lda     screenPtr0
    adc     #$80
    sta     screenPtr0
    lda     screenPtr1
    adc     #0
    sta     screenPtr1
    cmp     drawNextPage
    bne     rowLoop

    ; next 1/3 of screen
    lda     drawPage
    sta     screenPtr1
    clc
    lda     screenPtr0
    adc     #40
    sta     screenPtr0
    cmp     #40*3
    bne     rowLoop

    dec     starOffset
    rts

.endproc

;-----------------------------------------------------------------------------
; draw_char
; y = row
; a = col
; x = character
;-----------------------------------------------------------------------------
.proc draw_char
    clc
    adc     lineOffset,y    ; + lineOffset
    sta     screenPtr0    
    lda     linePage,y
    adc     drawPage        ; previous carry should be clear
    sta     screenPtr1
    ldy     #0
    txa
    sta     (screenPtr0),y
    rts
.endproc

;-----------------------------------------------------------------------------
; draw_value
; y = row
; a = col
; x = value
;-----------------------------------------------------------------------------
.proc draw_value
    stx     temp
    clc
    adc     lineOffset,y    ; + lineOffset
    sta     screenPtr0    
    lda     linePage,y
    adc     drawPage        ; previous carry should be clear
    sta     screenPtr1
    ldy     #0
    txa
    lsr
    lsr
    lsr
    lsr
    tax
    lda     asciiNibble,x
    sta     (screenPtr0),y
    iny
    lda     temp
    and     #$f
    tax
    lda     asciiNibble,x
    sta     (screenPtr0),y
    rts

temp:
    .byte   0

asciiNibble:    
    .byte   '0'+$80
    .byte   '1'+$80
    .byte   '2'+$80
    .byte   '3'+$80
    .byte   '4'+$80
    .byte   '5'+$80
    .byte   '6'+$80
    .byte   '7'+$80
    .byte   '8'+$80
    .byte   '9'+$80
    .byte   'A'+$80
    .byte   'B'+$80
    .byte   'C'+$80
    .byte   'D'+$80
    .byte   'E'+$80
    .byte   'F'+$80

.endproc


;-----------------------------------------------------------------------------
; draw_sprite
;-----------------------------------------------------------------------------
; Sprite format
;   row0 bytes      - width bytes of data
;   ...
;   rowN bytes      - width bytes of data, where N is height-1
;   padding         - (64*2) - w*h
;   width, height   - 2 bytes (always byte 62 and 63)

;
; Sprites must fit within 64 bytes, including the 2-byte coda.  So width*height + 2 =< 64
; So an 8x8 sprite is not allowed, but 7x8, 8x7 or 6x10 are fine.
;
; This routine can handle off-screen sprites in Y but not in X.

.proc draw_sprite

    ; calculate sprite pointer
    sta     temp            ; Save a copy of A

    ror
    ror
    ror                     ; Multiply by 64
    and     #$c0
    clc
    adc     #<spriteSheet
    sta     spritePtr0

    lda     #>spriteSheet
    sta     spritePtr1
    lda     temp 
    lsr
    lsr                     ; Divide by 4
    clc
    adc     spritePtr1
    sta     spritePtr1

    ; Read header
    ldy     #62
    lda     (spritePtr0),y
    sta     width
    sta     width_m1        ; width-1
    dec     width_m1

    ldy     #63
    lda     (spritePtr0),y
    tax                     ; x == height

    ; copy spriteY so as to not modify
    lda     spriteY
    sta     drawY

loopy:
    ; check if Y is on screen
    lda     drawY
    bmi     skipY
    cmp     #24
    bpl     skipY


    ; calculate screen pointer
    ldy     drawY
    lda     spriteX
    clc
    adc     lineOffset,y    ; + lineOffset
    sta     screenPtr0    
    lda     linePage,y
    adc     drawPage        ; previous carry should be clear
    sta     screenPtr1

    ; display row
    ldy     width_m1
loopx:
    lda     (spritePtr0),y
    beq     skipX           ; if data is zero, don't draw
    sta     (screenPtr0),y
skipX:
    dey
    bpl     loopx
skipY:
    ; assumes aligned such that there are no page crossing
    lda     spritePtr0
    adc     width           ; carry should still be clear
    sta     spritePtr0

    inc     drawY           ; next line

    dex
    bne     loopy

    rts    

; locals
temp:       .byte   0
width:      .byte   0
width_m1:   .byte   0
drawY:      .byte   0

.endproc


; Libraries
;-----------------------------------------------------------------------------

; add utilies
.include "inline_print.asm"
.include "sounds.asm"

; Globals
;-----------------------------------------------------------------------------

gameClock:      .byte   0

drawPage:       .byte   4   ; 4 or 8
drawNextPage:   .byte   8   ; 8 or C

spriteX:        .byte   0
spriteY:        .byte   0

starOffset:     .byte   0

playerX:        .byte   (40-5)/2
playerY:        .byte   23-3
playerSprite:   .byte   0

paddlePosition: .byte   0

bulletX:        .byte   0
bulletY:        .byte   $ff

actors:         .res    ACTOR_SIZE*ACTOR_MAX_COUNT

; Lookup tables
;-----------------------------------------------------------------------------

lineOffset:
    .byte   <$0000
    .byte   <$0080
    .byte   <$0100
    .byte   <$0180
    .byte   <$0200
    .byte   <$0280
    .byte   <$0300
    .byte   <$0380
    .byte   <$0028
    .byte   <$00A8
    .byte   <$0128
    .byte   <$01A8
    .byte   <$0228
    .byte   <$02A8
    .byte   <$0328
    .byte   <$03A8
    .byte   <$0050
    .byte   <$00D0
    .byte   <$0150
    .byte   <$01D0
    .byte   <$0250
    .byte   <$02D0
    .byte   <$0350
    .byte   <$03D0

linePage:
    .byte   >$0000
    .byte   >$0080
    .byte   >$0100
    .byte   >$0180
    .byte   >$0200
    .byte   >$0280
    .byte   >$0300
    .byte   >$0380
    .byte   >$0028
    .byte   >$00A8
    .byte   >$0128
    .byte   >$01A8
    .byte   >$0228
    .byte   >$02A8
    .byte   >$0328
    .byte   >$03A8
    .byte   >$0050
    .byte   >$00D0
    .byte   >$0150
    .byte   >$01D0
    .byte   >$0250
    .byte   >$02D0
    .byte   >$0350
    .byte   >$03D0

    ; 256 random number from 0-39 + a few bit 7s set
    ; Not sure I like the different star, may remove
starTable:
    .byte   $18, $08, $0B, $15, $0B, $15, $1E, $0A, $0B, $23, $25, $10, $1F, $05, $13, $23
    .byte   $1B, $08, $11, $23, $24, $18, $15, $1C, $14, $12, $14, $1B, $0B, $23, $13, $0C
    .byte   $26, $08, $00, $03, $12, $14, $0F, $13, $07, $21, $02, $1A, $0C, $1D, $1B, $20
    .byte   $08, $20, $0A, $15, $08, $25, $1B, $05, $19, $08, $03, $0B, $11, $27, $0F, $0D
    .byte   $26, $15, $23, $1B, $1A, $0E, $19, $0B, $13, $14, $27, $07, $16, $26, $01, $10
    .byte   $19, $0C, $03, $07, $04, $12, $15, $22, $21, $15, $05, $07, $16, $02, $07, $14
    .byte   $04, $1B, $26, $04, $15, $1F, $12, $1F, $20, $0F, $1B, $1A, $16, $09, $0A, $0B
    .byte   $1D, $20, $20, $06, $06, $1C, $12, $01, $05, $1F, $26, $01, $1D, $20, $17, $02
    .byte   $0A, $08, $13, $21, $23, $21, $08, $0D, $1B, $15, $1D, $18, $1A, $07, $1C, $0D
    .byte   $18, $0E, $23, $04, $10, $0C, $01, $02, $01, $1F, $19, $14, $1C, $08, $21, $16
    .byte   $0E, $16, $0C, $14, $0B, $00, $21, $10, $0A, $19, $01, $1A, $09, $1A, $26, $02
    .byte   $27, $02, $1B, $02, $09, $1F, $00, $1D, $21, $05, $0F, $03, $17, $1A, $17, $1D
    .byte   $06, $11, $1C, $0D, $06, $15, $04, $0E, $13, $1A, $0C, $1E, $16, $13, $0A, $01
    .byte   $1A, $25, $1F, $1F, $0E, $13, $25, $27, $04, $1C, $07, $1F, $05, $1F, $1E, $12
    .byte   $15, $0C, $12, $23, $24, $15, $14, $12, $0D, $17, $12, $16, $25, $1F, $03, $06
    .byte   $10, $25, $14, $21, $25, $1D, $11, $08, $20, $02, $1D, $02, $22, $1D, $0E, $15


;-----------------------------------------------------------------------------
; Paths
;-----------------------------------------------------------------------------

.include "path.asm"

;-----------------------------------------------------------------------------
; Game Sprites
;-----------------------------------------------------------------------------

.include "sprites.asm"
