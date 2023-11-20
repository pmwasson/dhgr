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
; sound_passive
;-----------------------------------------------------------------------------
.proc sound_passive
    lda     #10         ; tone
    ldx     #80         ; duration
    jmp     sound_tone  ; link returns
.endproc

;-----------------------------------------------------------------------------
; sound_action
;-----------------------------------------------------------------------------
.proc sound_action
    lda     #20         ; tone
    ldx     #20         ; duration
    jsr     sound_tone
    lda     #40        ; tone
    ldx     #20         ; duration
    jmp     sound_tone  ; link returns
.endproc

;-----------------------------------------------------------------------------
; sound_fail
;-----------------------------------------------------------------------------
.proc sound_fail
    lda     #50         ; tone
    ldx     #20         ; duration
    jsr     sound_tone
    lda     #190        ; tone
    ldx     #20         ; duration
    jmp     sound_tone  ; link returns
.endproc

;-----------------------------------------------------------------------------
; sound_walk
;-----------------------------------------------------------------------------
.proc sound_walk
    lda     #80         ; tone
    ldx     #04         ; duration
    jmp     sound_tone
.endproc