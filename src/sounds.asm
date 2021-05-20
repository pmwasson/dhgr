;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; A collection of sounds
;
; Look into improving sounds using:
;  https://www.applefritter.com/appleii-box/H218ArcadeSoundEditor.htm

;-----------------------------------------------------------------------------
; sound_tone
;-----------------------------------------------------------------------------
; A = tone
; X = duration
.proc sound_tone
loop1:
    sta     SPEAKER
    tay
loop2:
    nop
    nop
    nop
    nop             ; add some delay for lower notes
    dey
    bne     loop2
    dex
    bne     loop1
    rts

.endproc

;-----------------------------------------------------------------------------
; sound_shoot
;-----------------------------------------------------------------------------
.proc sound_shoot
    lda     #50         ; tone
    ldx     #25         ; duration
    jsr     sound_tone
    lda     #190        ; tone
    ldx     #3          ; duration
    jmp     sound_tone  ; link returns
.endproc

;-----------------------------------------------------------------------------
; sound_bump
;-----------------------------------------------------------------------------
.proc sound_bump
    lda     #100        ; tone
    ldx     #20         ; duration
    jsr     sound_tone
    lda     #90         ; tone
    ldx     #10         ; duration
    jmp     sound_tone  ; link returns
.endproc

;-----------------------------------------------------------------------------
; sound_talk
;-----------------------------------------------------------------------------
.proc sound_talk
    lda     #60         ; tone
    ldx     #15         ; duration
    jsr     sound_tone  ; link returns
    lda     #40         ; tone
    ldx     #10         ; duration
    jmp     sound_tone  ; link returns
.endproc

;-----------------------------------------------------------------------------
; sound_bark
;-----------------------------------------------------------------------------
.proc sound_bark
    lda     #20         ; tone
    ldx     #40         ; duration
    jsr     sound_tone
    lda     #200        ; tone
    ldx     #5          ; duration
    jsr     sound_tone
    lda     #50         ; tone
    ldx     #40         ; duration
    jmp     sound_tone  ; link returns
.endproc

;-----------------------------------------------------------------------------
; sound_quack
;-----------------------------------------------------------------------------
.proc sound_quack
    lda     #35         ; tone
    ldx     #40         ; duration
    jsr     sound_tone
    lda     #200        ; tone
    ldx     #60         ; duration
    jsr     sound_tone
    lda     #35         ; tone
    ldx     #40         ; duration
    jmp     sound_tone  ; link returns
.endproc

;-----------------------------------------------------------------------------
; sound_door
;-----------------------------------------------------------------------------
.proc sound_door
    lda     #200        ; tone
    ldx     #4          ; duration
    jmp     sound_tone  ; link returns
.endproc

;-----------------------------------------------------------------------------
; sound_pickup
;-----------------------------------------------------------------------------
.proc sound_pickup
    lda     #200        ; tone
    ldx     #25         ; duration
    jsr     sound_tone 
    lda     #100        ; tone
    ldx     #20         ; duration
    jsr     sound_tone 
    lda     #35         ; tone
    ldx     #100        ; duration
    jmp     sound_tone  ; link return
.endproc

;-----------------------------------------------------------------------------
; sound_crash
;-----------------------------------------------------------------------------
.proc sound_crash
    lda     #35         ; tone
    ldx     #100        ; duration
    jsr     sound_tone 
    lda     #100        ; tone
    ldx     #20         ; duration
    jsr     sound_tone 
    lda     #200        ; tone
    ldx     #25         ; duration
    jmp     sound_tone  ; link return
.endproc

;-----------------------------------------------------------------------------
; sound_timer
;-----------------------------------------------------------------------------
.proc sound_timer
    lda     #150        ; tone
    ldx     #200        ; duration
    jmp     sound_tone  ; link return
.endproc

