;-----------------------------------------------------------------------------
; Paul Wasson - 2021
;-----------------------------------------------------------------------------
; Sprites

.align 256

spriteSheet:

sprite_playerShip_0:
    StringHiBG  "./|\." , '.'
    StringHiBG  "</^\>" , '.'
    SpriteInfo  5,2             ; 5 by 2


sprite_playerShip_1:
    StringHiBG  "./^\." , '.'
    StringHiBG  "</^\>" , '.'
    SpriteInfo  5,2             ; 5 by 2

sprite_kiteM:
    StringHiBG  "O...O" , '.'
    StringHiBG  "|\^/|" , '.'
    StringHiBG  "|>-<|" , '.'
    StringHiBG  "|/^\|" , '.'
    StringHiBG  "O...O" , '.'
    SpriteInfo  5,5             ; 5 by 5

sprite_kiteL:
    StringHiBG  "..O"  , '.'
    StringHiBG  "o/|"  , '.'
    StringHiBG  "|<|"  , '.'
    StringHiBG  "o\|"  , '.'
    StringHiBG  "..O"  , '.'
    SpriteInfo  3,5             ; 3 by 5

sprite_kiteR:
    StringHiBG  "O.."  , '.'
    StringHiBG  "|\o"  , '.'
    StringHiBG  "|>|"  , '.'
    StringHiBG  "|/o"  , '.'
    StringHiBG  "O.."  , '.'
    SpriteInfo  3,5             ; 3 by 5

; Mock up
sprite_lives3:
    StringHi    "^^^"
    SpriteInfo  3,1             ; 3 by 1

sprite_bad0_0:
    StringHiBG  "<[]>" , '.'
    StringHiBG  "/><\" , '.'
    StringHiBG  "\../" , '.'
    SpriteInfo  4,3             ; 4 by 3

sprite_bad0_1:
    StringHiBG  ">[]<" , '.'
    StringHiBG  "\></" , '.'
    StringHiBG  "/..\" , '.'
    SpriteInfo  4,3             ; 4 by 3

sprite_bad1_0:
    StringHiBG  "/{}{}\" , '.'
    StringHiBG  "\(())/" , '.'
    StringHiBG  ".))((." , '.'
    SpriteInfo  6,3             ; 6 by 3

sprite_bad1_1:
    StringHiBG  "/{}{}\" , '.'
    StringHiBG  "\))((/" , '.'
    StringHiBG  ".(())." , '.'
    SpriteInfo  6,3             ; 6 by 3

sprite_bad2_0:
    StringHiBG  "./|\." , '.'
    StringHiBG  "<- ->" , '.'
    StringHiBG  ".\|/." , '.'
    SpriteInfo  5,3             ; 5 by 3

sprite_bad2_1:
    StringHiBG  "./|\." , '.'
    StringHiBG  "<-+->" , '.'
    StringHiBG  ".\|/." , '.'
    SpriteInfo  5,3             ; 5 by 3

