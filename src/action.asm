;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; Action
;
; Data for game actions
;-----------------------------------------------------------------------------


ACTION_TYPE_NONE        = 0
ACTION_TYPE_DIALOG      = 1
ACTION_TYPE_SIGN        = 2
ACTION_TYPE_FLASH       = 3


; Action Format
;
; Command      - 1 byte       ( 0 reserved for no action )
; Next         - 1 byte
; Parameters   - up to 6 bytes
; Padding      - to make 8-byte aligned
;


; Offset Field
; 0:     Type
; 1:     Next
; 2-7:   Parameters+padding

; Dialog - display a word balloon
;------------------------------------------
; 0:     01
; 1:     Next
; 2:     StringPtr
; 4:     Width
; 5:     Height
; 6:     Padding
;
; Mode = refresh + wait

; Dialog - display a sign
;------------------------------------------
; 0:     01
; 1:     Next
; 2:     StringPtr
; 4:     Width
; 5:     Height
; 6:     Padding
;
; Mode = refresh + wait

; Flash - flash screen
;------------------------------------------
; 0:     02
; 1:     Next
; 2:     Padding
;
; Mode = refresh

; Alert - display message and play sound
;------------------------------------------
; 0:     03
; 1:     Next
; 2:     StringPtr   - Message to display, can be length 0
; 4:     SoundPtr    - Sound to play, can be length 0
; 6:     Padding
;
; Mode = none

; Logic - work with game state
;------------------------------------------
; 0:     04
; 1:     Next
; 2:     Command
; 3:     Source1
; 4:     Source2
; 5:     Destination
; 6:     Address
; 7:     Padding
;
; cmd: 0 = nop:      do nothing
;      1 = inc:      inc dest
;      2 = copy:     copy source to dest
;      3 = compare:  if source == dest jump to jump

.align 256



actionTable:



; 0
    .byte   ACTION_TYPE_DIALOG
    .byte   1                       ; Next dialog
    .word   dialogStringWelcome
    .byte   30,4                    ; width, height
    .byte   0,0                     ; Padding

; 1 - Sign
    .byte   ACTION_TYPE_SIGN
    .byte   0                       ; Next dialog
    .word   dialogStringSign
    .byte   36,5                    ; width, height
    .byte   0,0                     ; Padding

; 2
    .byte   ACTION_TYPE_DIALOG
    .byte   0                       ; Next dialog
    .word   dialogStringHello
    .byte   14,4                    ; width, height
    .byte   0,0                     ; Padding

; 3
    .byte   ACTION_TYPE_DIALOG
    .byte   0                       ; Next dialog
    .word   dialogStringOink
    .byte   14,3                    ; width, height
    .byte   0,0                     ; Padding

; 4
    .byte   ACTION_TYPE_FLASH
    .byte   0
    .byte   0,0,0,0,0,0

; width =  (1+maxcol)*2 (range = 14 - 26)
; height = 2 + rows

; max characters in a line: 16
; max rows = 10
;
;                0123456789abcdef
;                1              .
;                2              .
;                3              .
;                4              .
;                5              .
;                6              .
;                7              .
;                8              .
;                9...............

 dialogStringWelcome:
    StringCont  "WELCOME TO THE"   
    String      "GAME!"   
   
 dialogStringSign:
    StringCont  "  HOW TO PLAY:"
    StringCont  " ARROWS TO MOVE"
    String      "SPACE TO INTERACT"


 dialogStringHello:
    String     "HELLO."   

 dialogStringOink:
    String     " OINK!"   

