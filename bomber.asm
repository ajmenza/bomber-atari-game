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
Score		byte		; 2-digit score stored as BCD
Timer		byte		; 2-digit timer sored as BCD
Temp		byte		; auxiliary variable to store temp score values
OnesDigitOffset word		; lookup table offset for the score 1's digit
TensDigitOffset word		; lookup table offset for the score 10's digit	

JetSpritePtr    word		; pointer to player0 sprite lookup table
JetColorPtr     word		; pointer to player0 color lookup table
BomberSpritePtr word		; pointer to player1 sprite lookup table
BomberColorPtr  word		; pointer to player1 color lookup table
JetAnimOffset   byte		; player0 sprite frame offset for animation
Random		byte		; random number generated to set enemy position
ScoreSprite	byte		; store the sprite bit pattern for the score
TimerSprite	byte		; store the sprite bit pattern for the timer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT = 9			; player0 sprite height (# of rows in lookup table)
BOMBER_HEIGHT = 9		; player0 sprite height (# of rows in lookup table)
DIGITS_HEIGHT = 5		; scoreboard digit height (# of rows in lookup table)

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
    lda #68
    sta JetXPos              ; JetXPos = 68
    lda #10
    sta JetYPos              ; JetYPos = 10
    lda #62
    sta BomberXPos           ; BomberXPos = 62
    lda #83
    sta BomberYPos           ; BomberYPos = 83
    lda #%11010100
    sta Random               ; Random = $D4
    lda #0
    sta Score
    sta Timer

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
    REPEAT 33
        sta WSYNC		; display the 37 recommended lines of VBLANK
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed in the pre-VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda JetXPos
    ldy #0
    jsr SetObjectXPos		; set player0 horizontal position


    lda BomberXPos
    ldy #1
    jsr SetObjectXPos		; set player1 horizontal position

    jsr CalculateDigitOffset	; calculate the scoreboard digits lookup table offset

    sta WSYNC
    sta HMOVE			; apply the horizontal offsets previously set
    
    lda #0
    sta VBLANK			; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #0			; clear TIA registers before each new frame
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    lda #$1C			; set playfield/scoreboard color to white
    sta COLUPF
    lda #%00000000
    sta CTRLPF			; disable playfield reflection
   
    ldx #DIGITS_HEIGHT		; start X counter with 5 (height of digits)
.ScoreDigitLoop:
    ldy TensDigitOffset		; get the tens digit offset for the Score
    lda Digits,Y		; load the bit pattern from the lookup table
    and #$F0			; mask/remove the graphics for the ones digit
    sta ScoreSprite		; save the score tens digit pattern in a variable

    ldy OnesDigitOffset		; get the ones digit offset for the Score
    lda Digits,Y		; load the digit bit pattern from lookup table
    and #$0F			; mask/remove the graphics for the tens digit
    ora ScoreSprite		; merge it with the saved tens digit sprite
    sta ScoreSprite		; and save it
    sta WSYNC
    sta PF1			; update the playfield to display the Score sprite

    ldy TensDigitOffset+1	; get the left digit offset for the Timer
    lda Digits,Y		; load the digit pattern from lookup table
    and #$F0			; mask/remove the graphics for the ones digit
    sta TimerSprite		; save the timer tens digit pattern in a vairable

    ldy OnesDigitOffset+1	; get the ones digit offset for the Timer
    lda Digits,Y		; load digit pattern from the lookup table
    and #$0F			; mask/remove the graphics for the tens digit
    ora TimerSprite		; merge with the saved tens digit graphics
    sta TimerSprite		; and save it

    jsr Sleep12Cycles		; wastes some cycles
    
    sta PF1			; update the playfield for the Timer display

    ldy ScoreSprite		; preload for the next scanline
    sta WSYNC

    sty PF1			; update playfield for the score display
    inc TensDigitOffset
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1	; increment all digits for the next line of data

    jsr Sleep12Cycles		; waste some cycles

    dex				; X--
    sta PF1			; update the playfield for the Timer display
    bne .ScoreDigitLoop		; if dex != 0, then branch
    
    sta WSYNC

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

    ldx #84			; X counts the number of remaining scanlines
.GameLineLoop:
.AreWeInsideJetSprite:
    txa
    sec
    sbc JetYPos			; subtract sprite Y-coordinate
    cmp JET_HEIGHT		; are we inside the sprite height bounds?
    bcc .DrawSpriteP0		; if result < SpriteHeight, call the draw routine
    lda #0			; else, set lookup index to zero (draws nothing)

.DrawSpriteP0
    clc				; clear carry flag before addition
    adc JetAnimOffset		; jump to correct sprite frame address in memory
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
    
    lda %00000101
    sta NUSIZ1			; Stretch player1 sprite

    lda (BomberSpritePtr),Y	; load player1 bitmap data from lookup table
    sta WSYNC
    sta GRP1			; set graphics for player1
    lda (BomberColorPtr),Y	; load player color from lookup table
    sta COLUP1			; set color of player1 

    dex				; X--
    bne .GameLineLoop		; repeat next main game scanline until finished

    lda #0
    sta JetAnimOffset		; reset jet animation frame to zero each frame
    
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
;; Process joystick input for player 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000		; player0 joystick up
    bit SWCHA
    bne CheckP0Down		; if bit pattern doesn't match, bypass Up block
    inc JetYPos
    lda #0
    sta JetAnimOffset		; reset sprite animation to first frame

CheckP0Down:
    lda #%00100000		; player0 joystick down
    bit SWCHA
    bne CheckP0Left
    dec JetYPos
    lda #0
    sta JetAnimOffset		; reset sprite animation to first frame

CheckP0Left:
    lda #%01000000		; player0joystick left
    bit SWCHA			
    bne CheckP0Right
    dec JetXPos
    lda JET_HEIGHT		; 9
    sta JetAnimOffset		; set animation offset to the second frame

CheckP0Right:
    lda #%10000000		; player0joystick right
    bit SWCHA
    bne EndInputCheck
    inc JetXPos
    lda JET_HEIGHT		; 9
    sta JetAnimOffset		; set animation offset to the second frame

EndInputCheck:			; fallback when no input was performed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations to update position for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateBomberPosition:
    lda BomberYPos
    clc
    cmp #0			; compare bomber y-position with 0
    bmi .ResetBomberPosition	; if it is < 0, then reset y-position to the top
    dec BomberYPos		; else, decrement enemy y-position for next frame
    jmp EndPositionUpdate		
.ResetBomberPosition
    jsr GetRandomBomberPos      ; call subroutine for random x-position

EndPositionUpdate:		; fallback for the position update code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for object collision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0P1:
    lda #%10000000		; CXPPMM bit 7 detects P0 and P1
    bit CXPPMM			; check CXPPMM bit 7 with the above pattern
    bne .CollisionP0P1		; if collision between P0 and P1 happened
    jmp CheckCollisionP0PF	; else, skip to next check
.CollisionP0P1:
    jsr GameOver		; call GameOver subroutine

CheckCollisionP0PF:
    lda #%10000000		; CXP0FB bit 7 detects P0 and PF collision
    bit CXP0FB			; check CXP0FB bit 7 with the above pattern
    bne .CollisionP0PF		; if collision between P0 and P1 happened
    jmp EndCollisionCheck	; else, skip to the end check

.CollisionP0PF:
    jsr GameOver		; call GameOver subroutine

EndCollisionCheck:		; fallback
    sta CXCLR			; clear all collision flags before the next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jump to next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame		; continue to display the next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle object horizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A is the target x-coordinate position in pixels of our object
;; Y is the object type (0:player0, 1:player1, 2:missile0, 3: missile1, 4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
    sta WSYNC			; start a fresh new scanline
    sec				; make sure carry flag is set before substraction
.Div15Loop
    sbc #15			; subtract 15 from accumulator
    bcs .Div15Loop		; loop until carry-flag is clear
    eor #7			; handle offset range from -8 to 7
    asl
    asl
    asl
    asl
    sta HMP0,Y			; store the fine offset to the correct HMxx
    sta RESP0,Y			; fix object position in 15-step increment
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Over subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver subroutine
    lda #$30
    sta COLUBK
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to generate a Linear-Feedback Shift Register random number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate a LFSR random number
;; Divide the random value by 4 to limit the size of the result to match river.
;; Add 30 to compensate for the left green playfield
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomBomberPos subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random			; performs a series of shifts and bit operations

    lsr
    lsr				; divide the value by 4 with 2 right shifts
    sta BomberXPos		; save it to the variable BomberXPos
    lda #30
    adc BomberXPos		; adds 30 + BomberXPos to compensate for left FF
    sta BomberXPos		; and sets the new value to the bomber x-position

    lda #96
    sta BomberYPos		; set the y-position to the top of the screen

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle scoreboard digits to be displayed on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert the high and low nibbles of the variable Score and Timer into the offsets
;; of digits lookup table so the values can be displayed. Each digit has a height
;; of 5 bytes in the lookup table
;;
;; For the low nibble we need to multiply by 5
;;  - we can use left shifts to perform multiplication by 2
;;  - for any number N, the value of N*5 = (N*2*2*)+N
;; For the upper nibble, since its already times 16, we need to divide it and then
;; multiply by 5:
;;  - we can use right shifts to perform division by 2
;;  - for any number N, the value of (N/16)*5 = (N/2/2)+(N/2/2/2/2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateDigitOffset subroutine
    ldx #1			; X register to the loop counter
.PrepareScoreLoop		; this will loop twice, first X=1, and then X=0
    
    lda Score,X			; load accumulator with Timer (X=1) or Score (X=0)
    and #%00001111		; forces first 4 bits to 0 and keeps last 4 the same (removes tens digit)
    sta Temp			; save the value of A into Temp
    asl				; shift left (it is now N*2)
    asl				; shift left (it is now N*4)
    adc Temp			; add the value saved in Temp (+N)
    sta OnesDigitOffset,X	; save A in OnesDigitOffset+1 or OnesDigitOffset

    lda Score,X			; load A with Timer (X=1) or Score (X=0)
    and #%11110000		; remove the ones digit by masking 4 bits
    lsr				; shift right (it is now N/2)
    lsr				; shift right (it is now N/4)
    sta Temp			; save the value of A into Temp
    lsr				; shift right (it is now N/8)
    lsr				; shift right (it is now N/16)
    adc Temp			; add the value saved in Temp (N/16)+(N/4)
    sta TensDigitOffset,X	; store A in TensDigitOffset+1 or TensDigitOffset

    dex				; X--
    bpl .PrepareScoreLoop	; while X >= 0, loop to pass a second time

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to waste 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsr takes 6 cycles
;; rts takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #

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
    



















