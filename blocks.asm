.setcpu "6502"

.segment "HEADER"
.byte "NES", $1a, 2, 1, 0, 0

.segment "VECTORS"
.word vblank, reset, 0

.segment "CHARS"
.incbin "chr.bin"

.segment "ZEROPAGE"
                        .res 8 ; can be used for function calls
update_done:            .res 1
scroll:                 .res 1
init_board_timer:       .res 1
clear_screen_timer:     .res 1
pause:                  .res 1
buttons:
btn_a:                  .res 1
btn_b:                  .res 1
btn_select:             .res 1
btn_start:              .res 1
btn_up:                 .res 1
btn_down:               .res 1
btn_left:               .res 1
btn_right:              .res 1

drop_timer:             .res 1 ; fractional part of drop
drop_rate:              .res 2
fast_drop_rate:         .res 2

enable_solidify:        .res 1

.segment "RAM"
oam:
oam_block:              .res 4*4
oam_text:               .res 4*5
oam_end:
oam_pad:                .res $100 + oam - oam_end

board:                  .res 128 ; bitpacked board data
tile_addrs:             .res 12  ; tile nametable addrs, used during nmi
tile_bits:              .res 12  ; addrs used for writing to board. high = offset, lo = bit
ttest_addrs:            .res 12  ; mirrors of above used to see if positions are valid
ttest_bits:             .res 12

timer_len = 11
timer:                  .res timer_len

;; macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.define POS(xx, yy) $2000 + (xx) + (yy) * $20

.macro  copy     out, in, len
        ldx #len+1
:
        lda in-1,x
        sta out-1,x
        dex
        bne :-
.endmacro

.macro  ppu_copy out, in, len
        ldx #len
        ldy #<out + len - 1
:
        lda #>out
        sta $2006
        sty $2006
        lda in-1,x
        sta $2007
        dey
        dex
        bne :-
.endmacro

;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

board_y = 5
board_x = 5
board_pos = POS board_x, board_y
timer_pos = POS board_x, board_y+21

t_blank  = 0
t_block  = 1
t_block2 = 2
t_block3 = 3
t_border = 4

;; data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.segment "CODE"

pal_0:
.byte $03, $14, $25, 0   ; tiles
.byte $0c, $1c, $2c, 0
.byte $07, $17, $27, 0
.byte $2d, $1d, $2d, $0d ; bg color
.byte $14, $25, $35, 0   ; sprites
.byte $11, $2c, $3c, 0
.byte $16, $27, $38, 0
.byte $1d, $2d, $3d

timer_start:  .byte "00:00:00:00"
timer_limits: .byte "::;6:;6:;6:"

text_x = (board_x+3) *8 + 4
text_y = (board_y+10)*8 - 5

spr_ready:
.byte text_y, 'R', 1, text_x
.byte text_y, 'E', 1, text_x+8
.byte text_y, 'A', 1, text_x+16
.byte text_y, 'D', 1, text_x+24
.byte text_y, 'Y', 1, text_x+32

spr_pause:
.byte text_y, 'P', 0, text_x
.byte text_y, 'A', 0, text_x+8
.byte text_y, 'U', 0, text_x+16
.byte text_y, 'S', 0, text_x+24
.byte text_y, 'E', 0, text_x+32

block_x = (board_x+5)*8
block_y = (board_y-2)*8-1

spr_testblock:
.byte block_y+0, 1, 0, block_x
.byte block_y+0, 1, 0, block_x+8
.byte block_y+8, 1, 0, block_x
.byte block_y+8, 1, 0, block_x+8

flag_bits: .byte $80, $40, $20, $10, $08, $04, $02, $01

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; startup                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

reset:
        sei
        cld
        ldx #$ff        ; fix stack
        txs
:       bit $2002       ; wait a frame
        bpl :-

        inx             ; x is now zero
        stx $2000       ; disable everything
        stx $2001       ; force blanking
        tax
@loop:                  ; zero all the memory
        sta $000,x
;        sta $100,x     ; no reason to zero stack
        lda #$ff        ; ff oam
        sta $200,x
        lda #0
        sta $300,x
        sta $400,x
        sta $500,x
        sta $600,x
        sta $700,x
        inx
        bne @loop

:       bit $2002       ; wait another frame
        bpl :-

        ;     BGRsbMmG
        ldx #%00011110  ; show sprites/bg
        stx $2001

        ppu_copy $3f01, pal_0, $1f

        jsr set_ppu_flags
        jsr init_board

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; logic                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

main:
        ldx #0          ; "I'm not done yet"
        stx update_done

read_controls:
        lda #1          ; prepare buttons for reading
        sta $4016
        lsr a           ; zero A
        sta $4016
        tax             ; zero X
@loop:
        lda $4016
        lsr a           ; get bit 0
        rol buttons,x   ; shift into button tracker
        inx
        cpx #8
        bne @loop

        lda btn_select  ; if paused and pressed select, reset game
        bit pause
        beq no_reset
        jsr init_game

no_reset:
        lda btn_start   ; test if start just pressed
        and #3
        cmp #1
        bne no_toggle_pause
toggle_pause:
        lda pause
        beq enable_pause
        bne disable_pause
enable_pause:
        sec             ; set pause to 1
        rol pause
        copy oam_text, spr_pause, 20
        jmp no_toggle_pause
disable_pause:
        lsr pause       ; clear pause
        lda #$ff        ; overwrite pause text with FF
        ldx #20+1
@loop:
        sta oam_text-1,x
        dex
        bne @loop

no_toggle_pause:
        lda pause
        beq no_jmp_finish_update ; long jump to end update
        jmp finish_update
no_jmp_finish_update:
        lda enable_solidify
        beq game_tick
        copy oam_block, spr_testblock, 20

;; store previously calculated tile offsets/bits to the board
write_to_board:
        ldx #0
@loop:
        ldy tile_bits,x    ; board offset
        lda tile_bits+1,x  ; one bit
        ora board,y        ; set bit
        sta board,y
        inx
        inx
        cpx #8
        bne @loop
        ;; TODO: logic to prepare next piece
        dec enable_solidify

game_tick:
        lda btn_down
        lsr a           ; if down currently pressed (low bit)
        lda #0
        rol a           ; ...turn into 2
        rol a
        tax             ; use as index
        lda drop_timer
        adc drop_rate+1,x ; to use faster drop rate
        sta drop_timer
        lda drop_rate,x
        adc #0
        sta 0

shift:
        lda #0
        sta 1
;; read left and right
        ldy #0  ; shift
        lda btn_right
        and #$3
        cmp #1
        bne no_right
        lda #8
        sta 1
no_right:
        lda btn_left
        and #$3
        cmp #1
        bne no_left
        lda #$f8
        sta 1
no_left:
        ldx #0
@loop:
        lda oam_block+3,x
        clc
        adc 1
        sta oam_block+3,x
        inx
        inx
        inx
        inx
        cpx #16
        bne @loop

drop:
        dec 0
        bmi drop_done
apply_drop:
        ldx #0
@loop:
        lda oam_block,x
        clc
        adc #8
        sta oam_block,x
        inx
        inx
        inx
        inx
        cpx #16
        bne @loop
        jmp drop
drop_done:

;; First, I convert the sprite addrs to nametable addrs.
;; Then, I test if they're ok by comparing to bitpacked board positions.

update_tile_addrs:
        ldx #0              ; tile_addr index
        ldy #0              ; oam_block index
@loop:
        lda oam_block,y     ; y position
        clc
        adc #1              ; add 1 (or 2) to get real y (last 8 rounded off)
        cmp #(board_y+20)*8 ; y bounds check (easier here than in nametable)
        bcs position_fail
        sta 0               ; save real y
        rol                 ; move top 2 bits to bottom for addr
        rol
        rol
        and #$03            ; ...and isolate them
        ora #$20            ; base nametable addr
        sta ttest_addrs,x      ; write high addr

        lda 0               ; get y again for lowish bits
        and #$f8            ; turn into block coord
        asl                 ; multiply 8 into 32
        asl
        sta 0               ; save it...

        lda oam_block+3,y   ; x position
        lsr                 ; divide by 8 to get block x
        lsr
        lsr
        clc
        adc 0               ; ...add back to X
        sta ttest_addrs+1,x    ; write lo addr
        inx                 ; next output index
        inx
        iny                 ; next input index
        iny
        iny
        iny
        cpy #16
        bne @loop

;; Convert and save above nametable indices to board bit indices.
;; Four bytes per row, 30 rows bitpacked. (yes, that's the whole screen :P)
;; This also tests if they're valid and fails if so.

test_board_pos:
        ldx #0 
@loop:
        lda ttest_addrs,x   ; get high byte
        and #$3             ; trim excess
        lsr                 ; rotate into upper index
        ror
        ror
        ror
        sta 0               ; save for later
        lda ttest_addrs+1,x ; get low byte
        and #$7
        tay                 ; turn low bits into bit index
        lda flag_bits,y
        sta ttest_bits+1,x  ; save for later in loop and possible later copying
        lda ttest_addrs+1,x ; get low byte again
        lsr                 ; divide by 8 (bits account for bottom 8)
        lsr
        lsr
        ora 0               ; finalize index
        sta ttest_bits,x    ; save alongside addr
        tay
        lda board,y         ; test relevant bit
        and ttest_bits+1,x
        bne position_fail   ; collision
        inx
        inx
        cpx #8
        bne @loop

position_success:
        copy tile_addrs, ttest_addrs, 16 ; copying all 16 copies bits as well
        jmp update_timer

position_fail:
;; clear current block sprite
        lda #$ff
        ldx #0
@loop:
        sta oam_block,x
        inx
        cpx #16
        bne @loop
        lda #1
        sta enable_solidify
        jmp update_timer

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

finish_update:
        ldx #1          ; "Ok we're done now"
        stx update_done
:       jmp :-          ; spin until vblank

;; subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

init_board:
        ldx #30
        stx clear_screen_timer
        ldx #22
        stx init_board_timer
        ldx #1
        stx pause
init_game:
        copy oam_block, spr_testblock, 16
        ldx #8
        stx drop_rate+1
        ldx #0
        stx drop_rate
        ldx #80
        stx fast_drop_rate+1
        ldx #0
        stx fast_drop_rate
        stx drop_timer
;; init timer
        ldy #timer_len
@loop:
        lda timer_start-1,y
        sta timer-1,y
        dey
        bne @loop
        copy oam_text, spr_ready, 20
        rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; graphics                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

        lda #0          ; oam DMA
        sta $4004
        lda #>oam
        sta $4014

        lda clear_screen_timer
        beq no_clear_screen
clear_screen:
        dec clear_screen_timer
        sbc #0          ; carry is clear; this subtracts 1
        jsr y_coord_to_addr
        sta 0
        ldy #$20        ; clear one screen line
@loop:
        stx $2006
        lda 0
        sta $2006
        lda #t_blank
        sta $2007
        clc
        inc 0
        dey
        bne @loop
        jmp draw_done

no_clear_screen:
        lda init_board_timer
        beq no_init_board
        dec init_board_timer
        cmp #1
        beq draw_top
        cmp #22
        beq draw_bottom

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
        jmp draw_done

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
        ldy #12         ; width
@loop:
        sta $2006       ; fill row
        stx $2006
        pha
        lda #t_border   ; border tile
        sta $2007
        pla
        inx
        dey
        bne @loop
        jmp draw_done

no_init_board:
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

;; turn a block sprite into block tiles.
;; the nametable addrs were found in the main update loop.

        lda enable_solidify
        beq no_solidify
solidify:
        ldx #0
        ldy #t_block    ; constant block fill value
@loop:
        lda tile_addrs,x        ; high byte
        sta $2006
        lda tile_addrs+1,x      ; lo byte
        sta $2006
        sty $2007      ; y has block value
        inx
        inx
        cpx #8
        bne @loop
no_solidify:

draw_done:
        jsr set_scroll_and_flags
        jmp main

;; subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

y_coord_to_addr:
        ldx #$1f        ; base address - 1
        sec
@loop:
        inx             ; repeated subtract to find upper byte
        sbc #$8
        bpl @loop
        asl             ; move a into lower byte
        asl
        asl
        asl
        asl
        rts
