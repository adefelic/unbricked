INCLUDE "hardware.inc"

DEF BRICK_LEFT EQU $05
DEF BRICK_RIGHT EQU $06
DEF BLANK_TILE EQU $08

SECTION "Header", ROM0[$100]

	jp EntryPoint

	ds $150 - @, 0 ; make room for the header

EntryPoint:
	; don't turn off the lcd outside of VBlank
WaitVBlank:
	ld a, [rLY]
	cp 144
	jp c, WaitVBlank

	; Turn the LCD off. the screen must be off to safely access VRAM and OAM
	ld a, 0
	ld [rLCDC], a

	; Copy BG tile data into VRAM
	ld de, Tiles            ; source in ROM
	ld hl, _VRAM9000        ; dest in VRAM
	ld bc, TilesEnd - Tiles ; # of bytes (pixel data) remaining
	call Memcopy

	; Copy BG tilemap into VRAM
	ld de, Tilemap              ; source in ROM
	ld hl, _SCRN0               ; dest in VRAM
	ld bc, TilemapEnd - Tilemap ; # of bytes (tile indices) remaining
	call Memcopy

	; copy Paddle object tile data into VRAM
	ld de, Paddle
	ld hl, _VRAM8000 ; vram start + 16 * oam id
	ld bc, PaddleEnd - Paddle
	call Memcopy

	; copy Ball object tile data into VRAM
	ld de, Ball
	ld hl, _VRAM8000 + $10 ; vram start + 16 * oam id
	ld bc, BallEnd - Ball
	call Memcopy

	ld a, 0        ; value to write to bytes
	ld b, 160      ; # of bytes to write
	ld hl, _OAMRAM ; dest
ClearOam:
	ld [hli], a
	dec b
	jp nz, ClearOam

	; load Paddle (Tile 0) into OAM
	ld hl, _OAMRAM ; dest.
	ld a, 128 + 16
	ld [hli], a    ; y coordinate
	ld a, 16 + 8
	ld [hli], a    ; x coordinate
	ld a, 0
	ld [hli], a    ; 0 tile id
	ld [hli], a    ; 0 attrs

	; load Ball (Tile 1) into OAM
	ld a, 100 + 16 ; y coordinate
	ld [hli], a
	ld a, 32 + 8   ; x coordinate
	ld [hli], a
	ld a, 1        ; 1 tile id
	ld [hli], a
	ld a, 0        ; 0 attrs
	ld [hl], a

	; init ball momentum (up and right)
	ld a, 1
	ld [wBallMomentumX], a
	ld a, -1
	ld [wBallMomentumY], a

	; init palette display registers
	ld a, %11100100
	ld [rBGP], a
	ld [rOBP0], a

	; init frame counter
	ld a, 0
	ld [wFrameCounter], a

	; turn the LCD on
	ld a, LCDCF_ON | LCDCF_BGON | LCDCF_OBJON
	ld [rLCDC], a
Main:
	ld a, [rLY]
	cp 144
	jp nc, Main
WaitVBlank2:
	ld a, [rLY]
	cp 144
	jp c, WaitVBlank2

BounceOnTop:
	; remember to offset the OAM position
	; (8, 16) in OAM pixel coordinates is (0, 0) on the screen
	ld a, [_OAMRAM + 4] ; ball y
	sub a, 16 + 1       ; check 1 pixel above the ball's TL corner
	ld c, a
	ld a, [_OAMRAM + 5] ; ball x
	sub a, 8
	ld b, a
	call GetTileByPixel ; returns tile address in hl
	ld a, [hl]          ; put tile id in a
	call IsWallTile
	jp z, BounceOnRight
	ld a, 1
	ld [wBallMomentumY], a
	call CheckAndHandleBrick
BounceOnRight:
	ld a, [_OAMRAM + 4] ; ball y
	sub a, 16
	ld c, a
	ld a, [_OAMRAM + 5] ; ball x
	sub a, 8 - 1 ; check 1 pixel right of the ball's TL corner
	ld b, a
	call GetTileByPixel
	ld a, [hl]
	call IsWallTile
	jp z, BounceOnLeft
	ld a, -1
	ld [wBallMomentumX], a
	call CheckAndHandleBrick
BounceOnLeft:
	ld a, [_OAMRAM + 4] ; ball y
	sub a, 16
	ld c, a
	ld a, [_OAMRAM + 5] ; ball x
	sub a, 8 + 1 ; check 1 pixel left of the ball's TL corner
	ld b, a
	call GetTileByPixel
	ld a, [hl]
	call IsWallTile
	jp z, BounceOnBottom
	ld a, 1
	ld [wBallMomentumX], a
	call CheckAndHandleBrick
BounceOnBottom:
	ld a, [_OAMRAM + 4] ; ball y
	sub a, 16 - 1 ; check 1 pixel below of the ball's TL corner
	ld c, a
	ld a, [_OAMRAM + 5] ; ball x
	sub a, 8
	ld b, a
	call GetTileByPixel
	ld a, [hl]
	call IsWallTile
	jp z, BounceDone
	ld a, -1
	ld [wBallMomentumY], a
	call CheckAndHandleBrick
BounceDone:
	; update ball's position
	; add the ball's momentum to its position in OAM
	ld a, [wBallMomentumX]
	ld b, a
	ld a, [_OAMRAM + 5]
	add a, b
	ld [_OAMRAM + 5], a

	ld a, [wBallMomentumY]
	ld b, a
	ld a, [_OAMRAM + 4]
	add a, b
	ld [_OAMRAM + 4], a

	; check if the ball is low enough to bounce off of the paddle
	ld a, [_OAMRAM]
	sub a, 4
	ld b, a
	ld a, [_OAMRAM + 4]
	cp a, b
	jp nz, PaddleBounceDone

	; compare X positions of the two objects
	ld a, [_OAMRAM + 5] ; ball X
	ld b, a
	ld a, [_OAMRAM + 1] ; paddle X
	sub a, 8
	cp a, b
	jp nc, PaddleBounceDone
	add a, 8 + 16
	cp a, b
	jp c, PaddleBounceDone

	ld a, -1
	ld [wBallMomentumY], a

PaddleBounceDone:

	; check current keys for left or right movement
	call UpdateKeys

	; check for left pressed
CheckLeft:
	ld a, [wCurKeys]
	and a, PADF_LEFT
	jp z, CheckRight
Left:
	; move the paddle one pixel to the left
	ld a, [_OAMRAM + 1] ; _OAMRAM + 1 is the x coord of the zeroth object
	dec a
	; if we've hit the edge of the playable area, dont move
	cp a, 15
	jp z, Main
	ld [_OAMRAM + 1], a
	jp Main

	; check for right pressed
CheckRight:
	ld a, [wCurKeys]
	and a, PADF_RIGHT
	jp z, Main
Right:
	; move the paddle one pixel to the right
	ld a, [_OAMRAM + 1] ; _OAMRAM + 1 is the x coord of the zeroth object
	inc a
	; if we've hit the edge of the playable area, dont move
	cp a, 105
	jp z, Main
	ld [_OAMRAM + 1], a
	jp Main

UpdateKeys:
	; poll controller buttons
	ld a, P1F_GET_BTN
	call .onenibble
	ld b, a ; b3-0 are button input, b7-4 are 1s

	; poll controller dpad
	ld a, P1F_GET_DPAD
	call .onenibble
	swap a ; a7-4 are dpad input, a3-0 are 1s

	; input is active low so we xor with 1s to check for input
	xor a, b ; a7-4 are dpad input, a3-0 are button input
	ld b, a

	; release the controller
	ld a, P1F_GET_NONE
	ldh [rP1], a

	; combine with previous wCurKeys to make wNewKeys
	ld a, [wCurKeys]
	xor a, b ; a = keys that changed state
	and a, b ; a = keys that changes state to pressed
	ld [wNewKeys], a
	ld a, b
	ld [wCurKeys], a
	ret
.onenibble ; load nibble into a
	ldh [rP1], a ; switch the key matrix. why hram?
	call .knownret ; burn 10 cycles
	ldh a, [rP1] ; ignore value while waiting for key matrix to settle
	ldh a, [rP1]
	ldh a, [rP1] ; this read counts
	or a, $F0 ; mask out top nibble
.knownret
	ret

; copy bytes from one area to another
; @param de: source
; @param hl: destination
; @param bc: length
Memcopy:
	ld a, [de]
	ld [hli], a
	inc de
	dec bc
	ld a, b
	or c
	jp nz, Memcopy
	ret

; Convert a pixel position to a tilemap address
; hl = $9800 + x + Y * 32
; @param b: X
; @param c: Y
; @return hl: tile address
GetTileByPixel:
	; first, we need to divide by 8 to convert a pixel position to a tile position
	; after this we want to multiply the y position by 32 (y * row length)
	; these operations effectively cancel out so we only need to mask the y value
	; (dividing by 8 would shift the number 3 times right, dropping the 3 LSBs)
	; (multiplying by 32 shifts 5 left)
	ld a, c ; bg y pixel coord between 00 and FF
	and a, %11111000 ; clamp to the top pixel y value of whichever tile the coord is on
	ld l, a
	ld h, 0
	; now in hl, coord * 8  %00000000 11111000
	add hl, hl ; coord * 16 %00000001 11110000
	add hl, hl ; coord * 32 %00000011 11100000
	; convert the bg x pixel coord. divide by 8 to find the tile #.
	ld a, b
	srl a ; x / 2 %11111111 -> %01111111
	srl a ; x / 4 %01111111 -> %00111111
	srl a ; x / 8 %00111111 -> %00011111
	; add the two offsets together
	add a, l ; add lsb of y offset to x offset. no problems here.
	ld l, a ; load into lsb of return value (tilemap addr).
	; i dont think this is necessary ...?
	;adc a, h ; add msb of y offset and add the carry. is adc necessary?
	;sub a, l ; subtract lsb of y offset? is the order here wrong?
	;ld h, a
	; add offset to base address
	ld bc, _SCRN0
	add hl, bc
	ret

; @param a: tile ID
; @return z: set if a is a wall
IsWallTile:
	cp a, $08
	ret
	;cp a, $00
	;ret z
	;cp a, $01
	;ret z
	;cp a, $02
	;ret z
	;cp a, $03
	;ret z
	;cp a, $04
	;ret z
	;cp a, $05
	;ret z
	;cp a, $06
	;ret z
	;cp a, $07
	;ret

; checks if a brick was colliided with and breaks it if possible
; @param hl: address of tile
CheckAndHandleBrick:
CheckAndHandleBrickLeft:
	ld a, [hl]
	cp a, BRICK_LEFT
	jr nz, CheckAndHandleBrickRight
	; break brick from left side
	ld [hl], BLANK_TILE
	inc hl
	ld [hl], BLANK_TILE
CheckAndHandleBrickRight:
	cp a, BRICK_RIGHT
	ret nz
	; break brick from right side
	ld [hl], BLANK_TILE
	dec hl
	ld [hl], BLANK_TILE
	ret

Tiles:
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33322222
	dw `33322222
	dw `33322222
	dw `33322211
	dw `33322211
	dw `33333333
	dw `33333333
	dw `33333333
	dw `22222222
	dw `22222222
	dw `22222222
	dw `11111111
	dw `11111111
	dw `33333333
	dw `33333333
	dw `33333333
	dw `22222333
	dw `22222333
	dw `22222333
	dw `11222333
	dw `11222333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33322211
	dw `33322211
	dw `33322211
	dw `33322211
	dw `33322211
	dw `33322211
	dw `33322211
	dw `33322211
	dw `22222222
	dw `20000000
	dw `20111111
	dw `20111111
	dw `20111111
	dw `20111111
	dw `22222222
	dw `33333333
	dw `22222223
	dw `00000023
	dw `11111123
	dw `11111123
	dw `11111123
	dw `11111123
	dw `22222223
	dw `33333333
	dw `11222333
	dw `11222333
	dw `11222333
	dw `11222333
	dw `11222333
	dw `11222333
	dw `11222333
	dw `11222333
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `11001100
	dw `11111111
	dw `11111111
	dw `21212121
	dw `22222222
	dw `22322232
	dw `23232323
	dw `33333333
	; Paste your logo here:
	DB $00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01
	DB $00,$00,$00,$00,$00,$00,$20,$20,$F8,$F8,$04,$04,$06,$06,$02,$02
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$C0,$C0,$B0,$B0
	DB $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00
	DB $02,$02,$02,$02,$02,$02,$04,$04,$04,$04,$84,$84,$FC,$FC,$00,$00
	DB $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00
	DB $18,$18,$0C,$0C,$04,$04,$04,$04,$04,$04,$84,$84,$CC,$CC,$78,$78
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FC,$FC
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$07
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$C0,$C0
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF
	DB $86,$86,$03,$03,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	DB $0C,$0C,$30,$30,$E0,$E0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	DB $71,$71,$1F,$1F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	DB $80,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
TilesEnd:

Tilemap:
	db $00, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $02, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $0A, $0B, $0C, $0D, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $0E, $0F, $10, $11, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $12, $13, $14, $15, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $16, $17, $18, $19, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
TilemapEnd:

Paddle:
    dw `13333331
    dw `30000003
    dw `13333331
    dw `00000000
    dw `00000000
    dw `00000000
    dw `00000000
    dw `00000000
PaddleEnd:

Ball:
    dw `00033000
    dw `00322300
    dw `03222230
    dw `03222230
    dw `00322300
    dw `00033000
    dw `00000000
    dw `00000000
BallEnd:


SECTION "Counter", WRAM0
wFrameCounter: db

SECTION "Input Variables", WRAM0
wCurKeys: db
wNewKeys: db

SECTION "Ball Data", WRAM0
wBallMomentumX: db
wBallMomentumY: db

