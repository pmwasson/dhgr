;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; Example 7x8 DHGR Tile Sheet
;-----------------------------------------------------------------------------

.align 256

tileSheet_7x8:

    ; FAT FONT (Inverse)

    ; @ (dot)
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$6A,$55,$7F,$7F,$6A,$55,$7F           
    .byte $7F,$6A,$55,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
           

    ; A
    .byte $7F,$70,$07,$7F,$3F,$00,$00,$7E,$0F,$03,$60,$78,$0F,$03,$60,$78
    .byte $0F,$00,$00,$78,$0F,$03,$60,$78,$0F,$03,$60,$78,$7F,$7F,$7F,$7F

    ; B
    .byte $0F,$00,$00,$7E,$0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$00,$00,$7E
    .byte $0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$00,$00,$7E,$7F,$7F,$7F,$7F

    ; C
    .byte $3F,$00,$00,$7E,$0F,$03,$60,$78,$0F,$7F,$60,$7F,$0F,$7F,$60,$7F
    .byte $0F,$7F,$60,$7F,$0F,$03,$60,$78,$3F,$00,$00,$7E,$7F,$7F,$7F,$7F

    ; D
    .byte $0F,$00,$00,$7E,$0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$03,$60,$78
    .byte $0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$00,$00,$7E,$7F,$7F,$7F,$7F

    ; E
    .byte $0F,$00,$00,$78,$0F,$7F,$60,$7F,$0F,$7F,$60,$7F,$0F,$40,$00,$7F
    .byte $0F,$7F,$60,$7F,$0F,$7F,$60,$7F,$0F,$00,$00,$78,$7F,$7F,$7F,$7F

    ; F
    .byte $0F,$00,$00,$78,$0F,$7F,$60,$7F,$0F,$7F,$60,$7F,$0F,$40,$00,$7F
    .byte $0F,$7F,$60,$7F,$0F,$7F,$60,$7F,$0F,$7F,$60,$7F,$7F,$7F,$7F,$7F

    ; G
    .byte $3F,$00,$00,$7E,$0F,$3F,$60,$78,$0F,$7F,$60,$7F,$0F,$03,$60,$78
    .byte $0F,$3F,$60,$78,$0F,$3F,$60,$78,$3F,$00,$00,$7E,$7F,$7F,$7F,$7F

    ; H
    .byte $0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$00,$00,$78
    .byte $0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$03,$60,$78,$7F,$7F,$7F,$7F

    ; I
    .byte $3F,$00,$00,$7E,$7F,$70,$07,$7F,$7F,$70,$07,$7F,$7F,$70,$07,$7F
    .byte $7F,$70,$07,$7F,$7F,$70,$07,$7F,$3F,$00,$00,$7E,$7F,$7F,$7F,$7F

    ; J
    .byte $7F,$03,$7F,$78,$7F,$03,$7F,$78,$7F,$03,$7F,$78,$7F,$03,$7F,$78
    .byte $0F,$03,$60,$78,$0F,$00,$00,$78,$3F,$00,$00,$7E,$7F,$7F,$7F,$7F

    ; K
    .byte $0F,$0F,$60,$78,$0F,$03,$60,$78,$0F,$00,$00,$7E,$0F,$40,$00,$7F
    .byte $0F,$00,$00,$7E,$0F,$03,$60,$78,$0F,$0F,$60,$78,$7F,$7F,$7F,$7F

    ; L
    .byte $0F,$7F,$60,$7F,$0F,$7F,$60,$7F,$0F,$7F,$60,$7F,$0F,$7F,$60,$7F
    .byte $0F,$7F,$60,$7F,$0F,$7F,$60,$7F,$0F,$00,$00,$78,$7F,$7F,$7F,$7F

    ; M
    .byte $0F,$0F,$78,$78,$0F,$03,$60,$78,$0F,$00,$00,$78,$0F,$00,$00,$78
    .byte $0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$03,$60,$78,$7F,$7F,$7F,$7F

    ; N
    .byte $0F,$03,$78,$78,$0F,$03,$60,$78,$0F,$03,$00,$78,$0F,$00,$00,$78
    .byte $0F,$00,$60,$78,$0F,$03,$60,$78,$0F,$0F,$60,$78,$7F,$7F,$7F,$7F

    ; O
    .byte $3F,$00,$00,$7E,$0F,$03,$60,$78,$0F,$0F,$78,$78,$0F,$0F,$78,$78
    .byte $0F,$0F,$78,$78,$0F,$03,$60,$78,$3F,$00,$00,$7E,$7F,$7F,$7F,$7F

    ; P
    .byte $0F,$00,$00,$7E,$0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$00,$00,$7E
    .byte $0F,$7F,$60,$7F,$0F,$7F,$60,$7F,$0F,$7F,$60,$7F,$7F,$7F,$7F,$7F

    ; Q
    .byte $3F,$00,$00,$7E,$0F,$03,$60,$78,$0F,$0F,$78,$78,$0F,$0F,$78,$78
    .byte $0F,$0F,$78,$78,$0F,$43,$60,$7F,$3F,$3C,$00,$78,$7F,$7F,$7F,$7F

    ; R
    .byte $0F,$00,$00,$7E,$0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$00,$00,$7E
    .byte $0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$03,$60,$78,$7F,$7F,$7F,$7F

    ; S
    .byte $3F,$00,$00,$78,$0F,$7F,$60,$7F,$0F,$7F,$60,$7F,$3F,$00,$00,$7E
    .byte $7F,$03,$7F,$78,$7F,$03,$7F,$78,$0F,$00,$00,$7E,$7F,$7F,$7F,$7F

    ; T
    .byte $0F,$00,$00,$78,$7F,$70,$07,$7F,$7F,$70,$07,$7F,$7F,$70,$07,$7F
    .byte $7F,$70,$07,$7F,$7F,$70,$07,$7F,$7F,$70,$07,$7F,$7F,$7F,$7F,$7F

    ; U
    .byte $0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$03,$60,$78
    .byte $0F,$03,$60,$78,$0F,$00,$00,$78,$3F,$00,$00,$7E,$7F,$7F,$7F,$7F

    ; V
    .byte $0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$03,$60,$78
    .byte $3F,$00,$00,$7E,$7F,$70,$07,$7F,$7F,$7C,$1F,$7F,$7F,$7F,$7F,$7F

    ; W
    .byte $0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$00,$00,$78
    .byte $0F,$00,$00,$78,$0F,$03,$60,$78,$0F,$0F,$78,$78,$7F,$7F,$7F,$7F

    ; X
    .byte $0F,$03,$60,$78,$0F,$03,$60,$78,$3F,$03,$60,$7E,$7F,$70,$07,$7F
    .byte $3F,$03,$60,$7E,$0F,$03,$60,$78,$0F,$03,$60,$78,$7F,$7F,$7F,$7F

    ; Y
    .byte $0F,$03,$60,$78,$0F,$03,$60,$78,$3F,$03,$60,$7E,$3F,$00,$00,$7E
    .byte $7F,$70,$07,$7F,$7F,$70,$07,$7F,$7F,$70,$07,$7F,$7F,$7F,$7F,$7F

    ; Z
    .byte $0F,$00,$00,$78,$7F,$0F,$7F,$78,$7F,$00,$7F,$7E,$7F,$70,$07,$7F
    .byte $3F,$7F,$00,$7F,$0F,$7F,$78,$7F,$0F,$00,$00,$78,$7F,$7F,$7F,$7F

    ; [    
    .byte $0F,$40,$00,$7F,$0F,$7F,$60,$7F,$0F,$7F,$60,$7F,$0F,$7F,$60,$7F           
    .byte $0F,$7F,$60,$7F,$0F,$7F,$60,$7F,$0F,$40,$00,$7F,$7F,$7F,$7F,$7F           

    ; \
    .byte $0F,$7F,$60,$7F,$3F,$7F,$00,$7F,$7F,$7C,$01,$7F,$7F,$70,$07,$7F           
    .byte $7F,$40,$1F,$7F,$7F,$00,$7F,$7E,$7F,$03,$7F,$78,$7F,$7F,$7F,$7F           

    ; ]                    
    .byte $0F,$40,$00,$7F,$7F,$40,$1F,$7F,$7F,$40,$1F,$7F,$7F,$40,$1F,$7F           
    .byte $7F,$40,$1F,$7F,$7F,$40,$1F,$7F,$0F,$40,$00,$7F,$7F,$7F,$7F,$7F           

    ; ^
    .byte $7F,$7F,$7F,$7F,$7F,$7C,$1F,$7F,$7F,$70,$07,$7F,$7F,$43,$61,$7F           
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           

    ; _
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$0F,$00,$00,$78,$7F,$7F,$7F,$7F           

    ; sp
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           

    ; !
    .byte $7F,$70,$07,$7F,$7F,$70,$07,$7F,$7F,$70,$07,$7F,$7F,$70,$07,$7F           
    .byte $7F,$7F,$7F,$7F,$7F,$70,$07,$7F,$7F,$70,$07,$7F,$7F,$7F,$7F,$7F           

    ; "
    .byte $7F,$43,$61,$7F,$7F,$43,$61,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           

    ; #
    .byte $7F,$7F,$7F,$7F,$7F,$43,$61,$7F,$0F,$00,$00,$78,$7F,$43,$61,$7F           
    .byte $0F,$00,$00,$78,$7F,$43,$61,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           

    ; $ 
    .byte $7F,$7C,$1F,$7F,$3F,$00,$00,$7E,$0F,$7C,$1E,$7F,$3F,$00,$00,$7E           
    .byte $7F,$3C,$1F,$78,$3F,$00,$00,$7E,$7F,$7C,$1F,$7F,$7F,$7F,$7F,$7F           

    ; %
    .byte $0F,$03,$7E,$78,$0F,$00,$7E,$7E,$7F,$40,$1F,$7F,$7F,$70,$07,$7F           
    .byte $7F,$7C,$01,$7F,$3F,$3F,$00,$78,$0F,$3F,$60,$78,$7F,$7F,$7F,$7F           

    ; &
    .byte $7F,$7C,$1F,$7F,$7F,$43,$61,$7F,$7F,$43,$61,$7F,$0F,$7C,$1E,$7F           
    .byte $0F,$43,$7E,$7F,$0F,$03,$7E,$7E,$7F,$3C,$01,$78,$7F,$7F,$7F,$7F           

    ; '
    .byte $7F,$7F,$7F,$7F,$7F,$70,$07,$7F,$7F,$70,$07,$7F,$7F,$7F,$7F,$7F           
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           

    ; (
    .byte $7F,$70,$07,$7F,$7F,$7C,$01,$7F,$7F,$7C,$01,$7F,$7F,$7C,$01,$7F           
    .byte $7F,$7C,$01,$7F,$7F,$7C,$01,$7F,$7F,$70,$07,$7F,$7F,$7F,$7F,$7F           

    ; )
    .byte $7F,$70,$07,$7F,$7F,$40,$1F,$7F,$7F,$40,$1F,$7F,$7F,$40,$1F,$7F           
    .byte $7F,$40,$1F,$7F,$7F,$40,$1F,$7F,$7F,$70,$07,$7F,$7F,$7F,$7F,$7F           

    ; *
    .byte $7F,$7F,$7F,$7F,$0F,$3C,$1E,$78,$7F,$40,$01,$7F,$0F,$00,$00,$78           
    .byte $7F,$40,$01,$7F,$0F,$3C,$1E,$78,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           

    ; +
    .byte $7F,$7F,$7F,$7F,$7F,$7C,$1F,$7F,$7F,$7C,$1F,$7F,$3F,$00,$00,$7E           
    .byte $7F,$7C,$1F,$7F,$7F,$7C,$1F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           

    ; ,
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
    .byte $7F,$7F,$7F,$7F,$7F,$70,$1F,$7F,$7F,$7C,$07,$7F,$7F,$7F,$7F,$7F           

    ; -
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$3F,$00,$00,$7E           
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           

    ; .
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           
    .byte $7F,$7F,$7F,$7F,$7F,$70,$07,$7F,$7F,$70,$07,$7F,$7F,$7F,$7F,$7F           

    ; /
    .byte $7F,$03,$7F,$78,$7F,$00,$7F,$7E,$7F,$40,$1F,$7F,$7F,$70,$07,$7F           
    .byte $7F,$7C,$01,$7F,$3F,$7F,$00,$7F,$0F,$7F,$60,$7F,$7F,$7F,$7F,$7F           

    ; 0
    .byte $3F,$00,$00,$7E,$0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$03,$60,$78           
    .byte $0F,$03,$60,$78,$0F,$03,$60,$78,$3F,$00,$00,$7E,$7F,$7F,$7F,$7F           

    ; 1
    .byte $7F,$70,$07,$7F,$7F,$70,$01,$7F,$3F,$70,$00,$7F,$7F,$70,$07,$7F           
    .byte $7F,$70,$07,$7F,$7F,$70,$07,$7F,$3F,$00,$00,$7E,$7F,$7F,$7F,$7F           

    ; 2
    .byte $0F,$00,$00,$7E,$7F,$03,$7F,$78,$7F,$03,$7F,$78,$3F,$00,$00,$7E           
    .byte $0F,$7F,$60,$7F,$0F,$7F,$60,$7F,$0F,$00,$00,$78,$7F,$7F,$7F,$7F           

    ; 3
    .byte $0F,$00,$00,$7E,$7F,$03,$7F,$78,$7F,$03,$7F,$78,$7F,$00,$01,$7E           
    .byte $7F,$03,$7F,$78,$7F,$03,$7F,$78,$0F,$00,$00,$7E,$7F,$7F,$7F,$7F           

    ; 4
    .byte $0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$03,$60,$78,$0F,$00,$00,$78           
    .byte $7F,$03,$7F,$78,$7F,$03,$7F,$78,$7F,$03,$7F,$78,$7F,$7F,$7F,$7F           

    ; 5
    .byte $0F,$00,$00,$78,$0F,$7F,$60,$7F,$0F,$7F,$60,$7F,$0F,$00,$00,$7E           
    .byte $7F,$03,$7F,$78,$7F,$03,$7F,$78,$0F,$00,$00,$7E,$7F,$7F,$7F,$7F           

    ; 6
    .byte $3F,$00,$00,$7E,$0F,$7F,$60,$7F,$0F,$7F,$60,$7F,$0F,$00,$00,$7E           
    .byte $0F,$03,$60,$78,$0F,$03,$60,$78,$3F,$00,$00,$7E,$7F,$7F,$7F,$7F           

    ; 7
    .byte $0F,$00,$00,$78,$7F,$03,$7F,$78,$7F,$03,$7F,$78,$7F,$00,$7F,$7E           
    .byte $7F,$40,$1F,$7F,$7F,$70,$07,$7F,$7F,$70,$07,$7F,$7F,$7F,$7F,$7F           

    ; 8
    .byte $3F,$00,$00,$7E,$0F,$03,$60,$78,$0F,$03,$60,$78,$3F,$00,$00,$7E           
    .byte $0F,$03,$60,$78,$0F,$03,$60,$78,$3F,$00,$00,$7E,$7F,$7F,$7F,$7F           

    ; 9
    .byte $3F,$00,$00,$7E,$0F,$03,$60,$78,$0F,$03,$60,$78,$3F,$00,$00,$78           
    .byte $7F,$03,$7F,$78,$7F,$03,$7F,$78,$3F,$00,$00,$7E,$7F,$7F,$7F,$7F           

    ; :
    .byte $7F,$7F,$7F,$7F,$7F,$70,$07,$7F,$7F,$70,$07,$7F,$7F,$7F,$7F,$7F           
    .byte $7F,$70,$07,$7F,$7F,$70,$07,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           

    ; ;
    .byte $7F,$7F,$7F,$7F,$7F,$70,$07,$7F,$7F,$70,$07,$7F,$7F,$7F,$7F,$7F           
    .byte $7F,$70,$07,$7F,$7F,$70,$07,$7F,$7F,$7C,$01,$7F,$7F,$7F,$7F,$7F           

    ; <
    .byte $7F,$40,$1F,$7F,$7F,$70,$07,$7F,$7F,$7C,$01,$7F,$3F,$7F,$00,$7F           
    .byte $7F,$7C,$01,$7F,$7F,$70,$07,$7F,$7F,$40,$1F,$7F,$7F,$7F,$7F,$7F           

    ; =
    .byte $7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$40,$01,$7F,$7F,$7F,$7F,$7F           
    .byte $7F,$40,$01,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F           

    ; >
    .byte $7F,$7C,$01,$7F,$7F,$70,$07,$7F,$7F,$40,$1F,$7F,$7F,$00,$7F,$7E           
    .byte $7F,$40,$1F,$7F,$7F,$70,$07,$7F,$7F,$7C,$01,$7F,$7F,$7F,$7F,$7F           

    ; ?
    .byte $3F,$00,$00,$7E,$0F,$03,$60,$78,$7F,$00,$7F,$7E,$7F,$40,$1F,$7F           
    .byte $7F,$70,$07,$7F,$7F,$7F,$7F,$7F,$7F,$70,$07,$7F,$7F,$7F,$7F,$7F           
