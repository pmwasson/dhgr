;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; Action
;
; Data for game actions
;-----------------------------------------------------------------------------

; Flags
ACTION_NONE             = $00

ACTION_FG_TILE          = $10
ACTION_SELECT           = $20
ACTION_PASSIVE          = $40
ACTION_FLIP_BG          = $80

FG_COIN                 = ACTION_FG_TILE + 0

actionState:
    .byte   ACTION_NONE                     ; 0 - NOP
    .byte   ACTION_SELECT                   ; 1 - Sign - Instructions
    .byte   ACTION_SELECT                   ; 2 - "Hello"
    .byte   ACTION_SELECT                   ; 3 - "Oink"
    .byte   ACTION_SELECT                   ; 4 - "Zap"
    .byte   ACTION_NONE                     ; 5 - NOP
    .byte   ACTION_NONE                     ; 6 - NOP
    .byte   ACTION_SELECT+ACTION_FLIP_BG    ; 7 - Door open
    .byte   ACTION_SELECT                   ; 8 - Door closed
    .byte   ACTION_PASSIVE+FG_COIN          ; 9 - Coin
    .byte   ACTION_PASSIVE+FG_COIN          ; a - Coin
    .byte   ACTION_PASSIVE+FG_COIN          ; b - Coin
    .byte   ACTION_PASSIVE+FG_COIN          ; c - Coin
    .byte   ACTION_NONE                     ; d
    .byte   ACTION_NONE                     ; e
    .byte   ACTION_NONE                     ; f
    .byte   ACTION_SELECT                   ; 10
    .byte   ACTION_SELECT                   ; 11
    .byte   ACTION_SELECT                   ; 12
    .byte   ACTION_SELECT                   ; 13
    .byte   ACTION_SELECT                   ; 14
    .byte   ACTION_SELECT                   ; 15

actionCommandTable:
    .byte   $00                             ; 0 - NOP
    .byte   $01                             ; 1 - Sign - Instructions
    .byte   $02                             ; 2 - "Hello"
    .byte   $03                             ; 3 - "Oink"
    .byte   $04                             ; 4 - "Zap"
    .byte   $00                             ; 5 - NOP
    .byte   $00                             ; 6 - NOP
    .byte   $07                             ; 7 - Door
    .byte   $07                             ; 8 - Door
    .byte   $08                             ; 9 - Coin
    .byte   $08                             ; a - Coin
    .byte   $08                             ; b - Coin
    .byte   $08                             ; c - Coin    
    .byte   $00                             ; d
    .byte   $00                             ; e
    .byte   $00                             ; f
    .byte   $07                             ; 10
    .byte   $07                             ; 11
    .byte   $07                             ; 12
    .byte   $07                             ; 13
    .byte   $07                             ; 14
    .byte   $07                             ; 15


ACTION_TYPE_NONE        = 0
ACTION_TYPE_DIALOG      = 1
ACTION_TYPE_SIGN        = 2
ACTION_TYPE_FLASH       = 3
ACTION_TYPE_DOOR        = 4
ACTION_TYPE_PICKUP      = 5

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

; Door - toggle bg_state
;------------------------------------------
; 0:     03
; 1:     Next
; 2:     State to toggle (0=mine)
; 3:     Padding
;
; Mode = refresh

; Alert - display message and play sound
;------------------------------------------
; 0:     04
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
    .byte   32,8                    ; width, height
    .byte   0,0                     ; Padding

; 2
    .byte   ACTION_TYPE_DIALOG
    .byte   0                       ; Next dialog
    .word   dialogStringHello
    .byte   14,3                    ; width, height
    .byte   0,0                     ; Padding

; 3
    .byte   ACTION_TYPE_DIALOG
    .byte   0                       ; Next dialog
    .word   dialogStringOink
    .byte   14,3                    ; width, height
    .byte   0,0                     ; Padding

; 4
    .byte   ACTION_TYPE_DIALOG
    .byte   5                       ; Next dialog
    .word   dialogStringZap
    .byte   14,3                    ; width, height
    .byte   0,0                     ; Padding

; 5
    .byte   ACTION_TYPE_FLASH
    .byte   6
    .byte   0,0,0,0,0,0

; 6
    .byte   ACTION_TYPE_DIALOG
    .byte   0                       ; Next dialog
    .word   dialogStringHaHa
    .byte   14,3                    ; width, height
    .byte   0,0                     ; Padding

; 7
    .byte   ACTION_TYPE_DOOR
    .byte   0
    .byte   0
    .byte   0,0,0,0,0

; 8
    .byte   ACTION_TYPE_PICKUP
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
    StringCont  "HOW TO PLAY:"
    StringCont  "@ARROWS TO MOVE"
    StringCont  "@SPACE TO"
    StringCont  " INTERACT"
    StringCont  "@STEP ON ITEMS"
    String      " TO PICK UP"


 dialogStringHello:
    String     "HELLO."   

 dialogStringOink:
    String     " OINK!"   

 dialogStringZap:
    String     "ZAP!!!"   

 dialogStringHaHa:
    String     "HA HA!"   


