    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required files with VCS register memory mappings and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   include "vcs.h"
   include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare the variables starting from memory address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80

JetXPos         byte		; player0 x-position
JetYPos         byte		; player0 y-position
BomberXPos      byte	  	; player1 x-position	
BomberYPos      byte	  	; player1 y-position	
JetSpritePtr    word		; pointer to player0 sprite lookup table
JetColorPtr     word		; pointer to player0 color lookup table
BomberSpritePtr word		; pointer to player1 sprite lookup table
BomberColorPtr  word		; pointer to player1 color lookup table

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT = 9			; player0 sprite height (# of rows in lookup table)
BOMBER_HEIGHT = 9		; player0 sprite height (# of rows in lookup table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg Code
    org $F000

Reset:
    CLEAN_START			; call macro to reset memory and registers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #10
    sta JetYPos			; JetYPos = 10
    lda #60
    sta JetXPos			; JetXPos = 60
    lda #83
    sta BomberYPos
    lda #54
    sta BomberXPos

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the pointers to the correct lookup table addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #<JetSprite		
    sta JetSpritePtr		; lo-byte pointer for jet sprite lookup table
    lda #>JetSprite
    sta JetSpritePtr+1		; hi-byte pointer for jet sprite lookup table
    
    lda #<JetColor
    sta JetColorPtr		; lo-byte pointer for jet color lookup table
    lda #>JetColor
    sta JetColorPtr+1		; hi-byte pointer for jet color lookup table

    lda #<BomberSprite		
    sta BomberSpritePtr		; lo-byte pointer for bomber sprite lookup table
    lda #>BomberSprite
    sta BomberSpritePtr+1	; hi-byte pointer for bomber sprite lookup table

    lda #<BomberColor
    sta BomberColorPtr		; lo-byte pointer for bomber color lookup table
    lda #>BomberColor
    sta BomberColorPtr+1	; hi-byte pointer for bomber color lookup table

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK			; turn on VBLANK
    sta VSYNC			; turn on VSYNC
    REPEAT 3
       sta WSYNC		; display 3 recommended lines of VSYNC 
    REPEND
    lda #0
    sta VSYNC			; turn off VSYNC
    REPEAT 37
        sta WSYNC		; display the 37 recommended lines of VBLANK
    REPEND
    sta VBLANK			; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the 96 visible scanlines of our main game (96 because we are performing
;; two WSYNCS per loop and this rendering 2 scanlines per loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLine:
    lda #$84
    sta COLUBK			; set color background to blue

    lda #$C2
    sta COLUPF			; set playfield/grass color to green

    lda #%0000001		; enable playfield reflection
    sta CTRLPF

    lda #$F0
    sta PF0			; setting PF0 bit pattern

    lda #$FC			; setting PF1 bit pattern
    sta PF1
    
    lda #0			; setting PF2 bit pattern
    sta PF2

    ldx #96			; X counts the number of remaining scanlines
.GameLineLoop:
.AreWeInsideJetSprite:
    txa
    sec
    sbc JetYPos			; subtract sprite Y-coordinate
    cmp JET_HEIGHT		; are we inside the sprite height bounds?
    bcc .DrawSpriteP0		; if result < SpriteHeight, call the draw routine
    lda #0			; else, set lookup index to zero (draws nothing)

.DrawSpriteP0
    tay				; load Y so we can work with the pointer
    lda (JetSpritePtr),Y	; load player0 bitmap data from lookup table
    sta WSYNC
    sta GRP0			; set graphics for player0
    lda (JetColorPtr),Y		; load player color from lookup table
    sta COLUP0			; set color of player 0

.AreWeInsideBomberSprite:
    txa
    sec
    sbc BomberYPos		; subtract sprite Y-coordinate
    cmp BOMBER_HEIGHT		; are we inside the sprite height bounds?
    bcc .DrawSpriteP1		; if result < SpriteHeight, call the draw routine
    lda #0			; else, set lookup index to zero (draws nothing)

.DrawSpriteP1
    tay				; load Y so we can work with the pointer
    lda (BomberSpritePtr),Y	; load player1 bitmap data from lookup table
    sta WSYNC
    sta GRP0			; set graphics for player1
    lda (BomberColorPtr),Y	; load player color from lookup table
    sta COLUP0			; set color of player1 

    dex				; X--
    bne .GameLineLoop		; repeat next main game scanline until finished
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK			; turn VBLANK on
    REPEAT 30
        sta WSYNC		; display 30 recommended lines of VBLANK overscan
    REPEND
    lda #0
    sta VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jump to next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame		; continue to display the next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JetSprite:
    .byte #%00000000         ;
    .byte #%00010100         ;   # #
    .byte #%01111111         ; #######
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #

JetSpriteTurn:
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #

BomberSprite:
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00101010         ;  # # #
    .byte #%00111110         ;  #####
    .byte #%01111111         ; #######
    .byte #%00101010         ;  # # #
    .byte #%00001000         ;    #
    .byte #%00011100         ;   ###

JetColor:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$BA
    .byte #$0E
    .byte #$08

JetColorTurn:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$0E
    .byte #$0E
    .byte #$08

BomberColor:
    .byte #$00
    .byte #$32
    .byte #$32
    .byte #$0E
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size with exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC			; move to position @FFFC
    word Reset			; write 2 bytes with the program reset address
    word Reset			; write 2 bytes with the interruption vector
    



















