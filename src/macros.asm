;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; Macros
;-----------------------------------------------------------------------------


.macro  SpriteInfo w,h
        .res    (64-2)-w*h
        .byte   w,h
.endmacro

.macro  StringInv s
        .repeat .strlen(s), I
        .byte   .strat(s, I) & $3f
        .endrep
.endmacro

.macro  StringHi s
        .repeat .strlen(s), I
        .byte   .strat(s, I) | $80
        .endrep
.endmacro

.macro  StringHiBG s,bg
        .repeat .strlen(s), I
        .if(.strat(s,I) = bg)
        .byte 0
        .else
        .byte   .strat(s, I) | $80
        .endif
        .endrep
.endmacro

; Converts @ to " for ASCII art
.macro  StringQuote s
        .repeat .strlen(s), I
        .if(.strat(s,I) = '@')
        .byte '"' | $80
        .else
        .byte   .strat(s, I) | $80
        .endif
        .endrep
.endmacro

; also adds a return to the end of each line
.macro  StringQuoteReturn s
        StringQuote s
        .byte   13
.endmacro

; Converts B to inverse-space and % to inverse _
.macro  StringBlock s
        .repeat .strlen(s), I
        .if(.strat(s,I) = 'B')
        .byte ' '
        .elseif(.strat(s,I) = '%')
        .byte '_' & $1f
        .else
        .byte   .strat(s, I) | $80
        .endif
        .endrep
.endmacro

; also adds a return to the end of each line
.macro  StringBlockReturn s
        StringBlock s
        .byte   $8d
.endmacro