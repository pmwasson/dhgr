;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; Paths
;
; Paths are all relative.
; 

; Segment (4-bytes)
;   x_speed         d,0.xxxxxxx     s:0=right,1=left
;   y_speed         d,0.yyyyyyy     s:0=up,1=down
;   count           0 = end (explode?), 1-255 duration count
;   next            nnnnnn00 

PATH_X      = 0     ; bit 7 = sign, bit 6:0 magnitude in the form 0.xxxxxxx
PATH_Y      = 1     ; bit 7 = sign, bit 6:0 magnitude in the form 0.yyyyyyy
PATH_COUNT  = 2     ; 1-255 (0=256)
PATH_NEXT   = 3     ; next path

.align 256

path:

path_0:
    ; Use a spread sheet to figure out paths
    ;       x       y       count   next    x1,y1 -> x2,y2  speed   dx  dy  distance
    .byte   14  ,   49  ,   29  ,   4   ;   2   -9  5   2   0.4     3   11  11.40
    .byte   0   ,   0   ,   153 ,   8   ;   5   2   5   2   0.4     0   0   0.00
    .byte   12  ,   24  ,   22  ,   12  ;   5   2   7   6   0.2     2   4   4.47
    .byte   148 ,   16  ,   32  ,   16  ;   7   6   2   10  0.2     -5  4   6.40
    .byte   0   ,   0   ,   204 ,   20  ;   2   10  2   10  0.2     0   0   0.00
    .byte   39  ,   52  ,   10  ,   24  ;   2   10  5   14  0.5     3   4   5.00
    .byte   0   ,   0   ,   25  ,   28  ;   5   14  5   14  0.9     0   0   0.00
    .byte   133 ,   25  ,   72  ,   32  ;   5   14  2   28  0.2     -3  14  14.32
    .byte   0   ,   153 ,   185 ,   36  ;   2   28  2   -9  0.2     0   -37 37.00