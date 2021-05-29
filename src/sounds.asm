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
.proc sound_click
    lda     #50         ; tone
    ldx     #10         ; duration
    jsr     sound_tone
    lda     #190        ; tone
    ldx     #3          ; duration
    jmp     sound_tone  ; link returns
.endproc