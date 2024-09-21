    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include macros and definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set variables at address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80

JetXPos         byte    ; P0 X position
JetYPos         byte    ; P0 Y position
BomberXPos      byte    ; P1 X position
BomberYPos      byte    ; P1 Y position
MissileXPos     byte    ; M0 X postition
MissileYPos     byte    ; M0 Y postition
Random          byte    ; random random for bomber X position
Score           byte    ; two digit score stored as BCD 
Timer           byte    ; two digit timer stored as BCD
Temp            byte    ; variable to store temporary score values
OnesDigitOffset word    ; lookup table offset for the score's 1's digit
TensDigitOffset word    ; lookup table offset for the score's 10's digit
JetSpritePtr    word    ; Pointer to P0 sprite lookup table 
JetColorPtr     word    ; Pointer to P0 color lookup table 
BomberSpritePtr word    ; Pointer to P1 sprite lookup table 
BomberColorPtr  word    ; Pointer to P1 color lookup table 
JetAnimOffset   byte    ; P0 sprite frame animation offset
ScoreSprite     byte    ; store the sprite bit pattern for the score 
TimerSprite     byte    ; store the sprite bit pattern for the timer 
TerrainColor    byte    ; store the color of the terrain
RiverColor      byte    ; store the color of the river

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT = 9          ; p0 sprite height (# rows in lookup table)
BOMBER_HEIGHT = 9       ; p1 sprite height (# rows in lookup table)
DIGITS_HEIGHT = 5       ; # rows in each digit lookup table

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start ROM code at $F0000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg Code
    org $F000
Reset: 
    CLEAN_START         ; Clean memory and TIA

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM variables and any TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #20
    sta JetYPos         ; JetYPos = 20
    lda #100    
    sta JetXPos         ; JetXPos = 60
    lda #83
    sta BomberYPos      ; BomberYPos = 83
    lda #54
    sta BomberXPos      ; BomberXPos = 54

    lda #%11010100
    sta Random

    lda #0
    sta Score
    sta Timer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare a MACRO to check if we should display the missile 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    MAC DRAW_MISSILE
        lda #%00000000  
        cpx MissileYPos ; compare X (current scanline) with MissileYPos
        bne .SkipMissileDraw; if x != missile Y position, then skip drawing it
.DrawMissile:
        lda #%00000010  ; else: enable missile 0 display
        inc MissileYPos ; MissileYPos++
        inc MissileYPos ; MissileYPos++
.SkipMissileDraw:
        sta ENAM0       ; TIA register to set missile display
    ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize pointers to correct memory positions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #<JetSprite
    sta JetSpritePtr    ; lo-byte ptr for jet sprite lookup table
    lda #>JetSprite
    sta JetSpritePtr+1  ; hi-byte ptr for jet sprite lookup table   

    lda #<JetColor
    sta JetColorPtr    ; lo-byte ptr for jet color lookup table
    lda #>JetColor
    sta JetColorPtr+1  ; hi-byte ptr for jet color lookup table   

    lda #<BomberSprite
    sta BomberSpritePtr    ; lo-byte ptr for bomber sprite lookup table
    lda #>BomberSprite
    sta BomberSpritePtr+1  ; hi-byte ptr for bomber sprite lookup table   

    lda #<BomberColor
    sta BomberColorPtr    ; lo-byte ptr for bomber color lookup table
    lda #>BomberColor
    sta BomberColorPtr+1  ; hi-byte ptr for bomber color lookup table   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame: 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VBLANK and VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK          ; Turn on VBLANK
    sta VSYNC           ; Turn on VSYNC

    REPEAT 3
        sta WSYNC       ; 3 scanlines for VSYNC
    REPEND
    lda #0
    sta VSYNC           ; turn VSYNC OFF
    
    REPEAT 33
        sta WSYNC
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed in the VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda JetXPos
    ldy #0
    jsr SetObjectXPos   ; set player horizontal pos

    lda BomberXPos
    ldy #1
    jsr SetObjectXPos   ; set bomber horizontal pos

    lda MissileXPos
    ldy #2
    jsr SetObjectXPos   ; set missile horizontal pos

    jsr CalculateDigitOffset; calculate the scoreboard lookup table offset

    sta WSYNC
    sta HMOVE

    lda #0 
    sta VBLANK           ; turn VBLANK OFF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #0              ; clear TIA registers before each frame
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    sta CTRLPF
    sta COLUBK          ; set the background color

    lda #$1E
    sta COLUPF          ; set the number color 
    
    ldx #DIGITS_HEIGHT  ; start X counter with (height of digits)

.ScoreDigitLoop:
    ldy TensDigitOffset ; get the tens digit offset for the score
    lda Digits,Y        ; load the bit pattern from the lookup table
    and #$F0            ; mask/remove the graphics for the ones digit
    sta ScoreSprite     ; save the tens digit pattern in a variable

    ldy OnesDigitOffset ; get the ones digiti offset for the Score
    lda Digits,Y        ; load the digit bit pattern from the lookup table 
    and #$0F            ; mask/remove the graphics for the tens digit
    ora ScoreSprite     ; merge it with the saved tens digit sprite
    sta ScoreSprite     ; and save it
    sta WSYNC           ; wait for the end of the scanline
    sta PF1             ; update the playfield to display the ScoreSprite

    ldy TensDigitOffset+1; get the left (tens) digit offset for the timer
    lda Digits,Y        ; load the digit pattern from the lookup table
    and #$F0            ; mask/remove the graphics from the ones digit
    sta TimerSprite     ; save the timer tens digit pattern in a variable
     
    ldy OnesDigitOffset+1; get the ones digit offset for the Timer
    lda Digits,Y        ; load the digit pattern from the lookup table
    and #$0F            ; mask/remove the graphics for the tens digit
    ora TimerSprite     ; merge with the saved tens digit graphic
    sta TimerSprite     ; and save it

    jsr Sleep12Cycles   ; waste some cycles

    sta PF1             ; update the playfield for the Timer display

    ldy ScoreSprite     ; preload for the next scanline 
    sta WSYNC           ; wait for next scanline

    sty PF1             ; update playfield for the Score display
    inc TensDigitOffset 
    inc TensDigitOffset+1
    inc OnesDigitOffset 
    inc OnesDigitOffset+1; increment digits for the next line of data

    jsr Sleep12Cycles   ; waste some cycles

    dex                 ; X--
    sta PF1             ; update the playing field for the Timer display
    bne .ScoreDigitLoop ; if dex != 0,  branch to ScoreDigitLoop

    sta WSYNC

    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display 84 visible scanlines of our main game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLine:  
    lda TerrainColor
    sta COLUPF          ; set playfield color 

    lda RiverColor
    sta COLUBK          ; set the river background color

    lda #%00000001
    sta CTRLPF          ; set playing field to reflect
    lda #$F0
    sta PF0             ; setting PF0 bit pattern
    lda #$FC
    sta PF1             ; setting PF1 bit pattern
    lda #0
    sta PF2             ; setting PF2 bit pattern

    ldx #85             ; counter for remaining scalines 
.GameLineLoop:
    DRAW_MISSILE        ; macro to check if we should draw missile
.AreWeInsideJetSprite:
    txa                 ; transfer X to the accumulator
    sec                 ; set carry flag before substracting
    sbc JetYPos         ; subtract sprite Y-coordinate
    cmp #JET_HEIGHT      ; are we inside the sprite?
    bcc .DrawSpriteP0   ; if result < SpriteHeight, call draw routine
    lda #0              ; else, set lookup index to 0
.DrawSpriteP0
    clc                 ; clear flag before adition
    adc JetAnimOffset   ; jump to the correct sprite frame address 

    tay                 ; load Y so we can use the ptr
    lda (JetSpritePtr),Y; laod the player0 bitmap data from the lookup table
    sta WSYNC           ; wait for scanline
    sta GRP0            ; set graphics for P0
    lda (JetColorPtr),Y ; load the player0 bitmap color from the lookup table
    sta COLUP0          ; set color for P0

.AreWeInsideBomberSprite:
    txa                 ; transfer X to the accumulator
    sec                 ; set carry flag before substracting
    sbc BomberYPos      ; subtract sprite Y-coordinate
    cmp #BOMBER_HEIGHT   ; are we inside the sprite?
    bcc .DrawSpriteP1   ; if result < SpriteHeight, call draw routine
    lda #0              ; else, set lookup index to 0
.DrawSpriteP1
    tay                 ; load Y so we can use the ptr
    lda (BomberSpritePtr),Y; laod the player1 bitmap data from the lookup table
    sta WSYNC           ; wait for scanline
    sta GRP1            ; set graphics for P1
    lda (BomberColorPtr),Y; load the player1 bitmap color from the lookup table
    sta COLUP1          ; set color for P1

    dex                 ; X--
    bne .GameLineLoop   ; loop while X != 0

    lda #0
    sta JetAnimOffset   ; reset animation frame offset every frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK          ; turn VBLANK on again
    REPEAT 30
        sta WSYNC       ; 30 lines of VBLANK
    REPEND
    lda #0
    sta VBLANK          ; turn VBLANK off

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process input for P0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000      ; P0 joystick up
    bit SWCHA
    bne CheckP0Down     ; if bit pattern doesnt match, bypass Up block
    lda #0              
    sta JetAnimOffset   ; reset sprite frame to first
    lda JetYPos         ; load the JetYPos to the A register
    cmp #$4e            ; compare position to top limit 
.P0UpPressed:
    bpl NoInput         ; if it is > top limit, don't move
    inc JetYPos         ; else, decrement the Y position

CheckP0Down:
    lda #%00100000      ; P0 joystick down
    bit SWCHA
    bne CheckP0Left     ; if bit pattern doesnt match, bypass down block
    lda #0              
    sta JetAnimOffset   ; reset sprite frame to first
    lda JetYPos         ; load the JetYPos to the A register
    cmp #1              ; compare y position with 0 (bottom limit) 
.P0DownPressed:
    bmi NoInput         ; if it is < 0, don't move
    dec JetYPos         ; else, increment the Y position

CheckP0Left:
    lda #%01000000      ; P0 joystick left
    bit SWCHA
    bne CheckP0Right    ; if bit pattern doesnt match, bypass left block
    lda #JET_HEIGHT     ; 9
    sta JetAnimOffset   ; set animation offset to the second frame
    lda JetXPos         ; load the JetXPos to the A register
    cmp #$1e            ; compare position to right most limit
.P0LeftPressed:
    bmi NoInput         ; if it is < right most limit, don't move
    dec JetXPos         ; else, decrement the X positiion
    
CheckP0Right:
    lda #%10000000      ; P0 joystick right
    bit SWCHA
    bne CheckButtonPressed; if bit pattern doesnt match, bypass right block
    lda #JET_HEIGHT     ; 9
    sta JetAnimOffset   ; set animation offset to the second frame
    lda JetXPos         ; load the JetXPos to the A register
    cmp #$68            ; compare position to left most limit
.P0RightPressed:
    bpl NoInput         ; if it is > left most limit, don't move
    inc JetXPos         ; else, increment the X positiion

CheckButtonPressed:
    lda #%10000000      ; if button is pressed 
    bit INPT4
    bne NoInput         ; if bit pattern doesnt match, bypass right block
.ButtonPressed:
    lda JetXPos
    clc 
    adc #5 
    sta MissileXPos     ; set missile X position equal to P0
    lda JetYPos
    clc 
    adc #5 
    sta MissileYPos     ; set missile X position equal to P0
    

NoInput:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculate position updates for next frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateBomberPosition:
    lda BomberYPos
    clc
    cmp #0              ; compare y position with 0 
    bmi .ResetBomberPosition; if it is < 0 reset y position to the top
    dec BomberYPos      ; else, decrement enemy y position
    jmp EndPositionUpdate
.ResetBomberPosition
    jsr GetRandomBomberPos; call subroutine for random x position

.SetScoreValues:
    sed                 ; activating decimal mode for score and timer values

    lda Timer
    clc
    adc #1
    sta Timer           ; add 1 to the Timer (BCD does not work well with INC)

    cld                 ; clear the decimal mode

EndPositionUpdate:      ; Fallback for the position update code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for object collision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0P1:
    lda #%10000000      ; CXPPMM bit 7 detects P0 and P1 collision
    bit CXPPMM          ; check CXPPMM bit 7 
    bne .P0P1Collided   ; if collision P0 P1 happens, game over
    jsr SetTerrainRiverColor; else, set playfield color to green/blue
    jmp CheckCollisionM0P1; else, skip to next collision check
.P0P1Collided:    
    jsr GameOver

CheckCollisionM0P1:
    lda #%10000000      ; CXM0P bit 7 detects M0 and P1 collision
    bit CXM0P           ; check CXM0P bit 7 
    bne .M0P1Collided   ; if collision M0 P1 happens, game over
    jmp EndCollisionCheck; else, skip to next collision check
.M0P1Collided:    
    sed                 ; set decimal mode 
    lda Score           
    clc                 
    adc #1              
    sta Score           ; increment the score
    cld                 ; clear decimal mode
    lda #0
    sta MissileYPos     ; reset the missile 

    

EndCollisionCheck:      ; fallback
    sta CXCLR           ; clear collision flags before the next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jump back to a frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the color for the terrain and river to green and blue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetTerrainRiverColor subroutine
    lda #$C2
    sta TerrainColor    ; set terrain color to green
    lda #$84
    sta RiverColor      ; set the river color to blue
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle object horizontal position with fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A is the target X coordinate position in pixels
;; Y is the object type (0:player0, 1:player1, 2:missile1, 3:missile2, 4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPos subroutine
    sta WSYNC           ; start a fresh scanline
    sec                 ; set carry flag before subtracting
.Div15Loop
    sbc #15             ; subtract 15 from A register
    bcs .Div15Loop      ; loop while carry flag is still set
    eor #7              ; handle offset range from -8 to 7
    asl
    asl
    asl
    asl                 ; four leftshifts to get top 4 bits
    sta HMP0,Y          ; store the fine offset to the correct HMxx
    sta RESP0,Y         ; fix object position in 15-step increment
    rts                 ; return from subroutine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Over subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver subroutine
    lda #$30
    sta TerrainColor    ; set the terrain color to red
    sta RiverColor      ; set the river color to red

    lda #0
    sta Score           ; Score = 0
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to genrate a Linear-Feedback Shift Register ranwom generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Genrate LFSR random number
;; Divide the random number by 4 to match the river size
;; Add 30 to compensate river position (green playing field)c:w
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomBomberPos:
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl 
    asl 
    eor Random
    asl
    rol Random          ; performs a shifts and bit operations

    lsr 
    lsr                 ; divide the value by 4 by performing 2 right shifts
    sta BomberXPos      ; save it to the variable BomberXPos
    lda #30              
    adc BomberXPos      ; adds 30 + BomberXPos to compensate for playingfield
    sta BomberXPos      ; sets new value to BomberXPos variable

    lda #96
    sta BomberYPos      ; set the y position to the top of the screen
    rts                 ; return from subroutine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle scoreboard digits to be displayed on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert high and low nibbles of the variables Score and Timer into the
;; offset of digits lookup table so the values can be displayed.
;; (DIGIT_HEIGHT = 5)
;;
;; For the low nibble we need to multiply by 5
;;  - For any number N, the value of N*5 = N*2*2+N
;;
;; For the upper nibble, since its already times 16, we divide it and then 
;; multiply by 5:
;;  - for any number N, the value of (N/16)*5 = N/2/2 + N/2/2/2/2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CalculateDigitOffset subroutine
    ldx #1              ; X register is the loop counter
.PrepareScoreLoop       ; this will loop twice, first X=1 then X=0
    lda Score,X         ; load A with Timer (X=1) or Score (X=0)
    and #$0F            ; remove the tens digit by masking 4 bits with 00001111
    sta Temp            ; save value of A into Temp variable
    asl                 ; shift left (it is now N*2)
    asl                 ; shift left (it is now N*2*2)
    adc Temp            ; add the temp value (it is now N*2*2 + N)
    sta OnesDigitOffset,X; save A in OnesDigitOffset or OnesDigitOffset+1

    lda Score,X         ; load A with the Timer (X=1) or Score (X=0)
    and #$F0            ; remove the one's digit by masking 4 bits with  
    lsr                 ; shift right (it is now N/2)
    lsr                 ; shift right (it is now N/4)
    sta Temp            ; load A value into Temp variable 
    lsr                 ; shift right (it is now N/8)
    lsr                 ; shift right (it is now N/16)
    adc Temp            ; add the value in Temp (N/16+N/4)
    sta TensDigitOffset,X; store A in TensDigitOffset+1 or TensDigitOffset

    dex
    bpl .PrepareScoreLoop; while X>=0 loop 

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to waste 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsr takes 6 cycles
;; rts takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:

    .byte #%01110111 ; ### ###
    .byte #%01010101 ; # # # #
    .byte #%01010101 ; # # # #
    .byte #%01010101 ; # # # #
    .byte #%01110111 ; ### ###

    .byte #%00010001 ;    #   #
    .byte #%00010001 ;    #   #
    .byte #%00010001 ;    #   #
    .byte #%00010001 ;    #   #
    .byte #%00010001 ;    #   #

    .byte #%01110111 ;  ### ###
    .byte #%00010001 ;    #   #
    .byte #%01110111 ;  ### ###
    .byte #%01000100 ;  #   #  
    .byte #%01110111 ;  ### ###

    .byte #%01110111 ;  ### ###
    .byte #%00010001 ;    #   #
    .byte #%01110111 ;  ### ###
    .byte #%00010001 ;    #   #
    .byte #%01110111 ;  ### ###

    .byte #%01010101 ;  # # # #
    .byte #%01010101 ;  # # # #
    .byte #%01110111 ;  ### ###
    .byte #%00010001 ;    #   #
    .byte #%00010001 ;    #   #

    .byte #%01110111 ;  ### ###
    .byte #%01000100 ;  #   #  
    .byte #%01110111 ;  ### ###
    .byte #%00010001 ;    #   #
    .byte #%01110111 ;  ### ###

    .byte #%01110111 ;  ### ###
    .byte #%01000100 ;  #   #  
    .byte #%01110111 ;  ### ###
    .byte #%01010101 ;  # # # #
    .byte #%01110111 ;  ### ###

    .byte #%01110111 ;  ### ###
    .byte #%00010001 ;    #   #
    .byte #%00010001 ;    #   #
    .byte #%00010001 ;    #   #
    .byte #%00010001 ;    #   #

    .byte #%01110111 ;  ### ###
    .byte #%01010101 ;  # # # #
    .byte #%01110111 ;  ### ###
    .byte #%01010101 ;  # # # #
    .byte #%01110111 ;  ### ###

    .byte #%01110111 ;  ### ###
    .byte #%01010101 ;  # # # #
    .byte #%01110111 ;  ### ###
    .byte #%00010001 ;    #   #
    .byte #%00010001 ;    #   #

    .byte #%00100010 ;   #   # 
    .byte #%01010101 ;  # # # #
    .byte #%01110111 ;  ### ###
    .byte #%01010101 ;  # # # #
    .byte #%01010101 ;  # # # #

    .byte #%01110111 ;  ### ###
    .byte #%01010101 ;  # # # #
    .byte #%01100110 ;  ##  ## 
    .byte #%01010101 ;  # # # #
    .byte #%01110111 ;  ### ###

    .byte #%01110111 ;  ### ###
    .byte #%01000100 ;  #   #  
    .byte #%01000100 ;  #   #  
    .byte #%01000100 ;  #   #  
    .byte #%01110111 ;  ### ###
    
    .byte #%01100110 ;  ##  ## 
    .byte #%01010101 ;  # # # #
    .byte #%01010101 ;  # # # #
    .byte #%01010101 ;  # # # #
    .byte #%01100110 ;  ##  ## 

    .byte #%01110111 ;  ### ###
    .byte #%01000100 ;  #   #  
    .byte #%01110111 ;  ### ###
    .byte #%01000100 ;  #   #  
    .byte #%01110111 ;  ### ###

    .byte #%01110111 ;  ### ###
    .byte #%01000100 ;  #   #  
    .byte #%01110111 ;  ### ###
    .byte #%01000100 ;  #   #  
    .byte #%01000100 ;  #   #  

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size with exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC                ; move to position $FFFC
    word Reset               ; write 2 bytes with the program reset address
    word Reset               ; write 2 bytes with the interruption vector
