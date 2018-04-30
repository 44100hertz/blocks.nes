.setcpu "6502"

.segment "HEADER"
.byte "NES", $1a, 2, 1, 0, 0

.segment "VECTORS"
.word vblank, reset, 0

.segment "ZEROPAGE"
                        .res 8 ;; can be used for function calls
update_done:            .res 1
scroll:                 .res 1
init_board_timer:       .res 1
clear_screen_timer:     .res 1

.segment "RAM"
timer_len = 11
timer:                  .res timer_len

.segment "CODE"

data:
palettes:
.byte $01, $14, $25, $35
.byte $11, $2c, $3c
.byte $16, $27, $38
.byte $1d, $2d, $3d

t_blank  = 0
t_block  = 1
t_block2 = 2
t_block3 = 3
t_border = 4

board_y = 5
board_x = 5
board_pos = $2000 + board_y*$20 + board_x

timer_x = board_x
timer_y = board_y+21
timer_pos = $2000 + timer_y*$20 + timer_x

timer_start:  .byte "00:00:00:00"
timer_limits: .byte "::;6:;6:;6:"

;; uses: x
set_scroll_and_flags:
        ldx scroll
        stx $2005
        ldx scroll
        stx $2005
set_ppu_flags:
        ;     VPHBSINN
        ldx #%10000000  ; enable NMI, sprites/tiles both on $0, nametable $2000
        stx $2000
        rts

;; inputs: x - first output addr
;;         y - first input addr
;;         a - last input addr
;;         1 - output high byte (not updated)
;; uses:   0, a, x, y
ppu_copy:
        sta 0
@loop:
        lda 1
        sta $2006
        stx $2006
        lda data,y
        sta $2007
        iny
        inx
        cpy 0
        bne @loop
        rts

reset:
        sei
        cld
        ldx #$ff        ; fix stack
        txs
:       bit $2002       ; wait 2 frames
        bpl :-
        ldx #0
        lda #0
@loop:                  ; zero all the memory
        sta $000,x
        sta $100,x
        sta $200,x
        sta $300,x
        sta $400,x
        sta $500,x
        sta $600,x
        sta $700,x
        inx
        bne @loop
:       bit $2002
        bpl :-

        ldx #$3f        ; copy palette
        stx 1
        ldx #0
        lda #4
        ldy #palettes - data
        jsr ppu_copy

        ;     BGRsbMmG
        ldx #%01111110  ; show sprites/bg
        stx $2001

        jsr set_ppu_flags
        jsr init_board

main:
        ldx #0          ; "I'm not done yet"
        stx update_done
        jsr update_timer

        ldx #1          ; "Ok we're done now"
        stx update_done
:       jmp :-          ; spin until vblank

update_timer:
        ldx #timer_len
@loop:
        inc timer-1,x
        lda timer-1,x
        cmp timer_limits-1,x
        bne @done
        lda timer_start-1,x
        sta timer-1,x
        dex
        bne @loop
@done:
        rts

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

draw_timer:
        ldy #>timer_pos
        ldx #<timer_pos
@loop:
        sty $2006
        stx $2006
        lda timer - <timer_pos,x
        sta $2007
        inx
        cpx #<timer_pos+timer_len
        bne @loop
        rts

init_board:
        ldy #timer_len
@loop:
        lda timer_start-1,y
        sta timer-1,y
        dey
        bne @loop
        ldx #$1e
        stx clear_screen_timer
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
        ldy #t_border   ; border tile
        sty 0
        ldy #12         ; width
        jsr fill_row
        rts

clear_screen:
;; fill two rows with blank tile
        sec
        sbc #2
        jsr y_coord_to_addr
        ldy #$40
@loop:
        stx $2006
        sta $2006
        pha
        lda #t_blank
        sta $2007
        pla
        clc
        adc #$1
        dey
        bne @loop
        dec clear_screen_timer
        dec clear_screen_timer
        rts

normal_draw:
        jsr draw_timer
        rts

update_tiles:
        lda clear_screen_timer
        bne clear_screen
        lda init_board_timer
        beq normal_draw
        dec init_board_timer
        cmp #22
        beq draw_bottom
        cmp #1
        beq draw_top

draw_border:
;; a is the timer, range 21..2
        clc
        adc #board_y-2  ; move timer into y position
        jsr y_coord_to_addr
        clc
        adc #board_x
;; left edge
        ldy #t_border   ; border tile
        stx $2006
        sta $2006
        sty $2007
;; right edge
        adc #11
        stx $2006
        sta $2006
        sty $2007
        rts

y_coord_to_addr:
        ldx #$1f        ; base address - 1
        tay             ; save a
        sec
@loop:  inx             ; repeated subtract to find upper byte
        sbc #$8
        bpl @loop
        tya             ; restore a
        asl             ; move a into lower byte
        asl
        asl
        asl
        asl
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
.incbin "chr.bin"
