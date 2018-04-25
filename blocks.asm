;; current concept:
;; store a list of block changes to apply in the PPU during draw

.segment "HEADER"
.byte "NES", $1a, 2, 1, 0, 0

.segment "VECTORS"
.word vblank, reset, 0

.segment "ZEROPAGE"
update_done: .res 1
scroll:      .res 1
init_board_timer: .res 1
board = $200

.macro  ppu_addr        addr
        ldx #>addr
        stx $2006
        ldx #<addr
        stx $2006
.endmacro

.macro  ppu_write       addr, value
        ppu_addr addr
        ldx value
        stx $2007
.endmacro

.segment "CODE"

set_scroll_and_flags:
        ldx scroll
        stx $2005
        ldx scroll
        stx $2005
set_ppu_flags:
        ;     VPHBSINN
        ldx #%10001000  ; enable NMI, sprites on $1000, nametable $2000
        stx $2000
        rts

reset:
        sei
        cld
        ldx #$ff        ; fix stack
        txs
:       bit $2002       ; wait 2 frames
        bpl :-
:       bit $2002
        bpl :-

        ppu_write $3f00, #$01   ; bg color
        ppu_write $3f01, #$34   ; block col 0
        ppu_write $3f02, #$24
        ppu_write $3f03, #$14

        ;     BGRsbMmG
        ldx #%01111110  ; show sprites/bg
        stx $2001
        jsr set_ppu_flags
        jsr init_board

main:
        ldx #0          ; "I'm not done yet"
        stx update_done

        ldx #1          ; "Ok we're done now"
        stx update_done
:       jmp :-          ; spin until vblank

vblank:
        pha             ; save a
        lda update_done ; check if done updating
        bne no_return
return:
        pla             ; not done with update
        rti
no_return:              ; done with update
        pla             ; nuke old a
        pla             ; nuke return data
        pla
        pla

        jsr update_tiles
        jsr set_scroll_and_flags
        jmp main

board_y = 5
board_x = 5
board_pos = $2000 + board_y*$20 + board_x

init_board:
        ldx #22
        stx init_board_timer
        rts

draw_top:
@pos = board_pos - $20
        lda #>@pos
        ldx #<@pos
        bne draw_border_row     ; hi pos always nonzero
draw_bottom:
@pos = board_pos + $20*20
        lda #>@pos
        ldx #<@pos

draw_border_row:
        ldy #2          ; border tile
        sty 0
        ldy #12         ; width
        jsr fill_row
go_back:
        rts

update_tiles:
        lda init_board_timer
        beq go_back
        dec init_board_timer
        cmp #22
        beq draw_bottom
        cmp #1
        beq draw_top
        
draw_border:
;; a is the timer, range 21..2
        clc
        adc #board_y-2  ; move timer into y position
        ldx #$1f        ; base address
        tay             ; save a
;; determine high address byte
        sec
@loop:  inx
        sbc #$8
        bpl @loop
        tya             ; restore a
;; shift a into lower Y position
        asl             ; turn a into column
        asl
        asl
        asl
        asl
;; draw tiles
        clc
        adc #board_x
        sta 0           ; swap bytes
        txa
        ldx 0
;; left edge
        ldy #$2         ; border tile
        sta $2006
        stx $2006
        sty $2007
;; middle (call fill_row)
        inx
        ldy #0          ; blank tile
        sty 0
        ldy #10         ; fill width
        jsr fill_row
;; right edge
        ldy #$2         ; border tile
        sta $2006
        stx $2006
        sty $2007
        rts

;; input: a - addr hi, x - addr lo, y - width, 0 - fill with
fill_row:
@loop:
        sta $2006
        stx $2006
        pha
        lda 0          ; border tile
        sta $2007
        pla
        inx
        dey
        bne @loop
        rts

.segment "CHARS"

.res  16, 0     ; bg

.byte %11111111 ; block p1
.byte %01111110
.byte %00111100
.byte %00111100
.byte %00111100
.byte %00111100
.byte %00000000
.byte %00000000
.byte %00000000 ; p2
.byte %10000001
.byte %11111111
.byte %11111111
.byte %11111111
.byte %11111111
.byte %10000001
.byte %00000000

.res 16,$ff     ; border
