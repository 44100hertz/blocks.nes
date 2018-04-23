.segment "STARTUP"

.segment "HEADER"
.byte "NES", $1a, 2, 1, 0, 0

.segment "VECTORS"
.word vblank, reset, 0

.segment "ZEROPAGE"
col:     .res 1

.macro  ppu_write       addr, value
        ldx #>addr
        stx $2006
        ldx #<addr
        stx $2006
        ldx value
        stx $2007
.endmacro

.segment "STARTUP"
reset:
        sei
        cld
        ldx #$ff        ; fix stack
        txs
:       bit $2002       ; wait 2 frames
        bpl :-
:       bit $2002
        bpl :-

        ldx #0
        stx col
        ;     VPHBSINN
        ldx #%10001000  ; enable NMI, sprites on $1000
        stx $2000
        ;     BGRsbMmG
        ldx #%00011110  ; show sprites/bg
        stx $2001

.segment "CODE"
main:
:       jmp :-          ; spin until vblank

vblank:
        inc col
        ppu_write $3f00, col      ; set bg color
        jmp main

.segment "CHARS"
