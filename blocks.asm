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

        ppu_write $3f00, #$01   ; bg color
        ppu_write $3f01, #$34   ; block col 0
        ppu_write $3f02, #$24
        ppu_write $3f03, #$14

        ;     BGRsbMmG
        ldx #%01111110  ; show sprites/bg
        stx $2001
        jsr set_ppu_flags
        jsr init_board

.segment "CODE"
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

init_board:
        ldx #22
        stx init_board_timer
        rts

draw_top:
        lda #$20
        ldx #6 * 32 + 2
        jsr draw_border_row
        rts
draw_bottom:
        lda #$23
        ldx #3 * 32 + 2
        jsr draw_border_row
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
;; determine high address byte
        adc #4          ; y position
        ldx #$1f        ; base address
        tay             ; save a
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
;; draw tiles to GPU
        clc
        adc #$2         ; x position
        ldy #$1
        stx $2006       ; left edge
        sta $2006
        sty $2007

        adc #11         ; right edge 
        stx $2006
        sta $2006
        sty $2007
        rts

draw_border_row:
        ldy #12         ; loop counter
@loop:
        sta $2006
        stx $2006
        pha
        lda #$1
        sta $2007
        pla
        inx
        dey
        bne @loop
        rts

set_scroll_and_flags:
        ldx scroll      ; set scroll
        stx $2005
        ldx scroll
        stx $2005
set_ppu_flags:
        ;     VPHBSINN
        ldx #%10001000  ; enable NMI, sprites on $1000, nametable $2000
        stx $2000
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
