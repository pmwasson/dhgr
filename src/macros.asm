;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; Macros
;-----------------------------------------------------------------------------

; Add 0 to the end of a string
.macro  String s
        .byte   s
        .byte   0
.endmacro

; Prepend the string length
.macro  StringLen s
        .byte   .strlen(s)
        .byte   s
.endmacro

; Add CR + 0 to the end of a string
.macro  StringCR s
        .byte   s
        .byte   13,0
.endmacro

; Add CR to the end of a string
.macro  StringCont s
        .byte   s
        .byte   13
.endmacro

