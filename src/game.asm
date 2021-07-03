;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; DHGR Toolbox -- Game
;  Game engine for DHGR maps and tiles

; Proposed memory map (may change)
;------------------------------------------------
;
;               Main                Aux
;
;   0000-0BFF   [ System usage / text pages     ]
;
;   0C00-0DFF   [ ProDos buffer ][ Unused       ]
;
;   0E00-1FFF   [ Engine routines               ]
;
;   2000-3FFF   [ DGHR Page 1                   ]
;
;   4000-5FFF   [ DGHR Page 2                   ]
;               [ Read data     ]
;
;   6000-7FFF   [ Game program  ][ Map 64x64x2  ]
;   8000-8FFF   [ Game program  ][ Dialog       ]
;   
;   9000-9FFF   [ Background Tiles (64)         ]
;
;   A000-AFFF   [ Foreground Tiles + Masks (32) ]
;
;   B000-B7FF   [ Font Tiles (128)              ]
;

;------------------------------------------------
; Constants
;------------------------------------------------

.include "defines.asm"
.include "macros.asm"

;------------------------------------------------
; Constants
;------------------------------------------------

;------------------------------------------------

.segment "CODE"
.org    $6000

;=============================================================================
; Main program
;=============================================================================

; Main
;------------------------------------------------
.proc main

    jsr     init

    jsr    inline_print
    StringCR "Loading game assets..."


    ldx     #assetBG
    jsr     loadAsset
    ldx     #assetFG
    jsr     loadAsset
    ldx     #assetMap
    jsr     loadAsset


    ; Exit to monitor
    jsr    inline_print
    StringCR "Press any key to exit"


:
    lda     KBD
    bpl     :-
    sta     KBDSTRB

    jmp     monitor     ; no return!


.endproc


;-----------------------------------------------------------------------------
; Init
;-----------------------------------------------------------------------------
.proc init

    ; Set up text screen
    jsr     $c300       ; 80 column mode
    jsr     HOME        ; clear screen
    lda     #23         ; put cursor on last line
    sta     CV
    jsr     VTAB

    ; Set ctrl-y vector
    lda     #$4c        ; JMP
    sta     $3f8
    lda     #<quit
    sta     $3f9
    lda     #>quit
    sta     $3fa

    ; Clear errors
    lda     #0
    sta     fileError

    rts
.endproc

;-----------------------------------------------------------------------------
; Monitor
;
;  Exit to monitor
;-----------------------------------------------------------------------------
.proc monitor

    jsr    inline_print
    StringCR "Enter ctrl-y to quit to ProDos"

    bit     TXTSET
    jmp     MONZ        ; enter monitor

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


;-----------------------------------------------------------------------------
; Load Asset
;
;   Pass asset # * 16 in X
;-----------------------------------------------------------------------------

.proc loadAsset
    
    lda     fileDescription+0,x
    sta     stringPtr0
    lda     fileDescription+1,x
    sta     stringPtr1
    jsr     print

    lda     #':' + $80
    jsr     COUT

    ; set pathname
    lda     fileDescription+2,x
    sta     open_params+1
    lda     fileDescription+3,x
    sta     open_params+2

    ; set address
    lda     fileDescription+4,x
    sta     read_params+2
    lda     fileDescription+5,x
    sta     read_params+3

    ; set size
    lda     fileDescription+6,x
    sta     read_params+4
    lda     fileDescription+7,x
    sta     read_params+5


    jsr     loadData    ; link return

    rts

.endproc

;-----------------------------------------------------------------------------
; Load Data
;   Load data using ProDOS
;-----------------------------------------------------------------------------
.proc loadData

    jsr    inline_print
    String "Reading "

    lda     open_params+1
    sta     stringPtr0   
    lda     open_params+2
    sta     stringPtr1
    jsr     print_length

    lda     #13
    jsr     COUT

    ; open file
    jsr     MLI
    .byte   CMD_OPEN
    .word   open_params
    bcc     :+

    jsr    inline_print
    StringCR "File not found"
    inc     fileError
    rts
:

    ; set reference number 
    lda     open_params+5
    sta     read_params+1
    sta     close_params+1

    ; read data
    jsr    MLI
    .byte  CMD_READ
    .word  read_params
    bcc    :+

    jsr    inline_print
    StringCR "Read Error"
    inc     fileError
    rts
:

    jsr    MLI
    .byte  CMD_CLOSE
    .word  close_params
    bcc    :+

    jsr    inline_print
    StringCR "File close error"
    inc     fileError
:
    rts

.endproc

;-----------------------------------------------------------------------------
; Global ProDos parameters
;-----------------------------------------------------------------------------

fileError:  .byte   0

open_params:
    .byte   $3
    .word   $0                  ; *OVERWRITE* pathname     
    .word   FILEBUFFER
    .byte   $0                  ;             reference number

read_params:
    .byte   $4
    .byte   $0                  ;             reference number
    .word   $0                  ; *OVERWRITE* address of data buffer
    .word   $0                  ; *OVERWRITE* number of bytes to read
    .word   $0                  ;             number of bytes read

close_params:
    .byte   $1
    .byte   $0                  ;             reference number


fileTypeBG:     String "Background Tileset"
fileTypeFG:     String "Foreground Tileset"
fileTypeMap:    String "Map"

fileNameBG:     StringLen "/DHGR/TILESET0"
fileNameFG:     StringLen "/DHGR/TILESET1"
fileNameMap:    StringLen "/DHGR/MAP0"

fileDescription:    ; type, name, address, size
    .word   fileTypeBG,     fileNameBG,     READBUFFER,128*64     ; 0  = background
    .word   fileTypeFG,     fileNameFG,     READBUFFER,128*64     ; 8  = foreground
    .word   fileTypeMap,    fileNameMap,    READBUFFER,64*64      ; 16 = map

assetBG     =   8*0
assetFG     =   8*1
assetMap    =   8*2

;-----------------------------------------------------------------------------
; Utilies

.include "inline_print.asm"
.include "sounds.asm"
