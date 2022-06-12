.const debug = false

#import "constants.asm"

//.import source "macros.asm"

.const KOALA_TEMPLATE = "C64FILE, Bitmap=$0000, ScreenRam=$1f40, ColorRam=$2328, BackgroundColor = $2710"

.var picture = LoadBinary("cbm.kla", KOALA_TEMPLATE)
.var bounced = LoadBinary("bounce.bin")

// [EQUATES] -------------------------------------------------------------------

  .var sin_counter0  = $a90            // sinetable index for each sprite
  .var sin_counter1  = $a91     
  .var sin_counter2  = $a92

  .var hsin_counter3  = $a93           // half sinetable index for each sprite
  .var hsin_counter4  = $a94
  .var hsin_counter5  = $a95
  .var hsin_counter6  = $a96
  .var hsin_counter7  = $a97

  .var bsin_counter   = $a98

.const starScreenChar  = $0400         // Screen address
.const StarScreenCols  = $d800         // Character attribute address

.const charBase        = $3000         // Address of our character set

.var  star1Init       = charBase+$1d0 // Init address for each star
.var  star2Init       = charBase+$298
.var  star3Init       = charBase+$240
.var  star4Init       = charBase+$2e0

.var  star1Limit      = charBase+$298 // Limit for each star
.var  star2Limit      = charBase+$360 // Once limit is reached, they are reset
.var  star3Limit      = charBase+$298
.var  star4Limit      = charBase+$360

.var  star1Reset      = charBase+$1d0 // Reset address for each star
.var  star2Reset      = charBase+$298
.var  star3Reset      = charBase+$1d0
.var  star4Reset      = charBase+$298

.var  staticStar1     = charBase+$250 // 2 Locations for blinking static stars
.var  staticStar2     = charBase+$1e0

.const starColourLimit = 20            // use values 1 to 20
                                // Galencia uses these values
                                // 1     = mono
                                // 2     = duo
                                // 20    = full colour



// [ZERO PAGE VARIABLES] -------------------------------------------------------

.var starfieldPtr    = $f0           // 4 x pointers for moving stars
.var starfieldPtr2   = $f2
.var starfieldPtr3   = $f4
.var starfieldPtr4   = $f6

.var zeroPointer     = $f8           // General purpose pointer

.var rasterCount     = $fa           // Counter that increments each frame

.var zeroPointer2    = $fb

.var plasmaCnt =        $02
.var add =              $04
.var yPos =             $05
.var screen =           $4c08

.var colorTable =       $7000
.var bitmap =           $6000

.const e               = 40*24*8



  *=$9000
sine64:
.for (var i=0; i<$200; i++)
        .by 32 + 32 * sin(i/[$100/2/PI])
sine128:
.for (var i=0; i<$200; i++)
        .by 64 + 64 * sin(i/[$100/2/PI])

  *=$0801
  BasicUpstart($0810)
  *=$0810
  jmp code

  .import source "macros.asm"

  .const X_POS = $75
  .const Y_POS = $6f
  .const Y_HALF = $bc
  .const X_HALF = $64

.var width = 23
.var height = 2
.var sineSpreadX =      $07
.var sineSpreadY =      $04
.var colorSpreadX =     $01
.var colorSpreadY =     $02
.var realtimeSpread0 =  $06
.var realtimeSpread1 =  $07

sineSpeeds:     .byte $03,$fe
addSpeed:       .byte $fe
colors:         .byte $00,$02,$9b,$2b,$24,$48,$2a,$4a,$ca,$aa,$af,$a7,$f7,$77,$71,$11


code:
    sei
    lda #BLACK
    sta BORDER_COLOR                    // black background
    sta BACKGROUND_COLOR
    lda #$93
    jsr $ffd2                           // clear screen

    lda #$00
    jsr $1000                           // init sid

    lda #$ff
    sta SPRITE_ENABLE                   // enable some multicolor sprites
    sta SPRITE_MULTICOLOR
    lda #$07
    sta SPRITE_EXPAND_X

    lda #LIGHT_BLUE                     // with these colors
    sta SPRITE_MULTICOLOR_0
    lda #WHITE
    sta SPRITE_MULTICOLOR_1
    lda #$00
    sta SPRITE_MSB_X                    // not scroll more

    lda #$2c ; sta SPRITE_0_POINTER     // $0b00
    lda #$2d ; sta SPRITE_1_POINTER     // $0b40
    lda #$2e ; sta SPRITE_2_POINTER     // $0b80
    lda #$2f ; sta SPRITE_3_POINTER     // $0bc0
    lda #$30 ; sta SPRITE_4_POINTER
    lda #$31 ; sta SPRITE_5_POINTER
    lda #$32 ; sta SPRITE_6_POINTER
    lda #$33 ; sta SPRITE_7_POINTER

    lda #X_POS; sta SPRITE_0_X          // store x positions for each sprite
    lda #X_POS+44; sta SPRITE_1_X       // offsets too.
    lda #X_POS+88; sta SPRITE_2_X
    lda #X_POS+11; sta SPRITE_3_X
    lda #X_POS+33; sta SPRITE_4_X
    lda #X_POS+55; sta SPRITE_5_X
    lda #X_POS+77; sta SPRITE_6_X
    lda #X_POS+99; sta SPRITE_7_X

    lda #Y_POS
    sta SPRITE_0_Y
    sta SPRITE_1_Y                      // store y position
    sta SPRITE_2_Y

    lda #Y_HALF
    sta SPRITE_3_Y
    sta SPRITE_4_Y
    sta SPRITE_5_Y
    sta SPRITE_6_Y
    sta SPRITE_7_Y

    lda #BLUE
    sta SPRITE_0_COLOR
    sta SPRITE_1_COLOR
    sta SPRITE_2_COLOR                  // color 1
    sta SPRITE_3_COLOR
    sta SPRITE_4_COLOR
    sta SPRITE_5_COLOR
    sta SPRITE_6_COLOR
    sta SPRITE_7_COLOR

    lda #$00 ; sta sin_counter0
    lda #$30 ; sta sin_counter1         // also offset
    lda #$50 ; sta sin_counter2

    lda #$40 ; sta hsin_counter3
    lda #$30 ; sta hsin_counter4
    lda #$20 ; sta hsin_counter5
    lda #$10 ; sta hsin_counter6
    lda #$00 ; sta hsin_counter7
                                        // now run starfield
    sta bsin_counter

    // fill bitmap...
        ldx #0
        lda #%10101010
!:      sta bitmap+64,x
        eor #%11111111
        txa
        sec
        sbc #183
        beq endbit
        lda bitmap+64,x
        inx
        bne !-
endbit:
    // fill bitmap...
        ldx #0
        lda #%10101010
!:      sta bitmap+64+64*5,x
        eor #%11111111
        txa
        sec
        sbc #183
        beq endbit2
        lda bitmap+64,x
        inx
        bne !-
endbit2:

    jsr initPlasma

    jmp star

initPlasma:
    ldx #0
!loop:
    txa
    asl
    asl
    asl
    bcc !+
    eor #$ff
!:  lsr
    lsr
    lsr 
    lsr
    tay
    lda colors,y
    sta colorTable,x
    sta colorTable+$100,x
    inx
    bne !loop-

wopee:

  inc sin_counter0
  inc sin_counter1
  inc sin_counter2

  inc sin_counter0
  inc sin_counter1      // inc 3 times
  inc sin_counter2

  inc sin_counter0
  inc sin_counter1
  inc sin_counter2


  ldx sin_counter0      // load index
  lda sinetable,x       // load the table,index
  adc #Y_POS            // add it to native y position
  sta SPRITE_0_Y        // store it at the sprite y

  // do this for all sprites
  ldx sin_counter1
  lda sinetable,x
  adc #Y_POS
  sta SPRITE_1_Y

  ldx sin_counter2
  lda sinetable,x
  adc #Y_POS
  sta SPRITE_2_Y

  rts             // this is called from the starfield, so return

#import "sine256.txt"

*=$0b00 "Sprites"
#import "c.txt"

do:
  
  inc hsin_counter3
  inc hsin_counter4
  inc hsin_counter5
  inc hsin_counter6
  inc hsin_counter7

  ldx hsin_counter3
  lda #Y_HALF
  sbc bounce,x
  sta SPRITE_3_Y

  ldx hsin_counter4
  lda #Y_HALF
  sbc bounce,x
  sta SPRITE_4_Y

  ldx hsin_counter5
  lda #Y_HALF
  sbc bounce,x
  sta SPRITE_5_Y

  ldx hsin_counter6
  lda #Y_HALF
  sbc bounce,x
  sta SPRITE_6_Y

  ldx hsin_counter7
  lda #Y_HALF
  sbc bounce,x
  sta SPRITE_7_Y

  inc bsin_counter
  inc bsin_counter

  ldx bsin_counter
  lda bigsine,x

  sec
  
  adc #X_HALF

  sta SPRITE_3_X
  adc #$16
  sta SPRITE_4_X
  adc #$18
  sta SPRITE_5_X
  adc #$16
  sta SPRITE_6_X
  adc #$16
  sta SPRITE_7_X

  rts

star:
        sei                             // Disable all IRQ
        lda #<charBase                  // Clear charset data
        sta zeroPointer
        lda #>charBase
        sta zeroPointer+1

        ldx #8-1
        ldy #0
        tya
rclrChars:
        sta (zeroPointer),y
        iny
        bne rclrChars
        inc zeroPointer+1
        dex
        bne rclrChars

        sta $d020                       // Border and screen colour to 0 (black)
        sta $d021

        lda #28                         // Characters at $3000
        sta $d018
                
        jsr initStarfield               // Reset all pointers
        jsr CreateStarScreen            // Initialise Starfield
        lda #0
        sta rasterCount

        jsr copyimage

        lda #0
        ldx #0
!:
        sta bitmap+40*24*8-64,x
        sta bitmap+40*23*8,x
        inx 
        bne !-

        lda #0
        ldx #$40
!:      sta $d800 + 40*23,x
        dex
        bne !-
    
        lda #0
        sta charpos
        sta charpos+1
        sta framecount

        lda $DD00
        and #%11111100
        ora #%00000010 // VIC-II bank 1
        sta $dd00

        lda #$38
        sta $d018

        jsr copy_inverted_system_font

        ldx #$00
clearlastrow:
        lda #BLACK
        sta $d800+24*40, x
        lda #' '
        sta ($4400)+24*40, x
        sta ($4400)+23*40, x
        inx
        cpx #40
        bne clearlastrow

            // IRQ setup
        sei
        lda #$35        // Bank out kernal and basic
        sta $01
        // Setup raster IRQ
        SetupIRQ(irq0, 16, false)

        lda #0
        sta framecount

        cli

{
infloop:
        jmp infloop
}

irq0: {
        irq_start(end)

        lda #0
        sta $d021

        inc framecount

        lda #$3b
        sta $d011
        lda #$d8
        sta $d016
        lda #$38
        sta $d018

        jsr scroller_update_char_row    // update the scroller text

        irq_end(irq1, $67)
end:    rti
}

irq1: {
        irq_start(end)
        sec
        sec                     //delay 28 cycles
        lda #$0a //hides 'ASL A'
        bpl *-1

        DebugRaster(LIGHT_BLUE)

        lda $DD00 
        and #%11111100
        ora #%00000011 // VIC-II bank 0
        sta $dd00

        lda #$1b
        sta $d011
        lda #28                         // Characters at $3000
        sta $d018

        inc rasterCount                 // Increment our 8 bit counter
        jsr doStarfield                 // erase, move and redraw the star
        jsr wopee                       // also draw "C64"
        jsr do                          // and "ROCKS"
        jsr updatePlasma                // update plasma
        jsr $1003                       // also update music

        DebugRaster(0)

        irq_end(irq2, $d2)
end:    rti 
}

irq2: {
        irq_start(end)

        DebugRaster(GREEN)

        lda #$3b
        sta $d011
        lda #$38
        sta $d018
        lda #$d8
        sta $d016

        lda $DD00
        and #%11111100
        ora #%00000010 // VIC-II bank 1
        sta $dd00

        DebugRaster(0)

        irq_end(irq3,50+200-12)
end:    rti 
}

irq3: {
        irq_start(end)

        DebugRaster(WHITE)

        lda #$1b        // screen on, text mode
        sta $d011

        lda framecount
        and #7
        eor #7 // xor bits 0-2 and leave bit 3 zero for 38 column mode
        sta $d016

        lda #$10 // bank + $0400
        sta $d018

        DebugRaster(0)

        irq_end(irq4,50+200-9)
end:    rti 
}

irq4: {

        double_irq(end, irq5)
irq5:


    // Wait exactly 9 * (2+3) cycles so that the raster line is in the border
    ldx #$09
    dex
    bne *-1

    // First line is a bad line (so we have only 23 cycles!)
    lda colors1+0           // 4 cycles
    sta $d021               // 4 cycles
    .for (var i = 0; i < 6; i++) {
        nop
    }
    bit $fe

    // Next 7 lines are normal lines, so 63 cycles per line
    ldx #$01
!:
    lda colors1,x           // 4 cycles
    sta $d021               // 4 cycles
    .for (var i = 0; i < (63-15)/2; i++) {
        nop
    }
    inx                     // 2
    cpx #colorend-colors1   // 2
    bne !-                  // 3

    lda #0
    sta $d021

        irq_end(irq0, 16)
end:    rti
}

        // some c code (he a)
copyimage:
        ldx #$00
!loop:
    .for (var i = 0; i < 4; i++) {
        lda colorRam+i*256, x
        sta $d800+i*256, x
    }
        inx
        bne !loop-
        rts

*=$1000 "Music"
#import "hey.asm"


// VIC-II BANK 1
.const VIC_BASE = $4000

// Copy ROM font from $D000 ROM to $4000
// Code adapted from: https://dustlayer.com/vic-ii/2013/4/23/vic-ii-for-beginners-part-2-to-have-or-to-not-have-character
copy_inverted_system_font: {
    sei         // disable interrupts while we copy
    ldx #$08    // we loop 8 times (8x255 = 2Kb)
    lda #$33    // make the CPU see the Character Generator ROM...
    sta $01     // ...at $D000 by storing %00110011 into location $01
    lda #$d0    // load high byte of $D000
    sta $fc     // store it in a free location we use as vector
    ldy #$00    // init counter with 0
    sty $fb     // store it as low byte in the $FB/$FC vector

    lda #$40    // load high byte of $4000
    sta $f1     // store it in a free location we use as vector
    ldy #$00    // init counter with 0
    sty $f0     // store it as low byte in the $FB/$FC vector

loop:
    lda ($fb),y // read byte from vector stored in $fb/$fc
    eor #255
    sta ($f0),y // write to the RAM under ROM at same position
    iny         // do this 255 times...
    bne loop    // ..for low byte $00 to $FF
    inc $fc     // when we passed $FF increase high byte...
    inc $f1     // when we passed $FF increase high byte...
    dex         // ... and decrease X by one before restart
    bne loop    // We repeat this until X becomes Zero
    lda #$37    // switch in I/O mapped registers again...
    sta $01     // ... with %00110111 so CPU can see them
    cli         // turn off interrupt disable flag
    rts         // return from subroutine
}

framecount: .byte 0
charpos:    .byte 0, 0

//----------------------------------------------------------
scroller_update_char_row: {
    pha
    txa
    pha
    tya
    pha
    DebugRaster(GREEN)
    lda framecount
    and #7
    bne noscroll
    ldx #$00
moveline:
    lda (VIC_BASE+$0400)+24*40+1, x
    sta (VIC_BASE+$0400)+24*40, x
    inx
    cpx #39
    bne moveline

    clc
    lda charpos
    adc #<scrolltext
    sta $20
    lda charpos+1
    adc #>scrolltext
    sta $21

    ldy #0
    lda ($20),y
    sta (VIC_BASE+$0400)+24*40 + 39

    add16_imm8(charpos, 1)

    // wrap around for scroll char pos
    lda charpos+0
    cmp #<(scrolltextend-scrolltext)
    bne noscroll
    lda charpos+1
    cmp #>(scrolltextend-scrolltext)
    bne noscroll
    lda #0
    sta charpos
    sta charpos+1
noscroll:
    DebugRaster(0)
    pla 
    tay
    pla
    tax
    pla
    rts
}

.align 64
colors1:
                .text "abmnmaj"
colorend:

scrolltext:
    .text "  guess what? this is my first program i ever wrote for the commodore 64...         1090 lines of code!      --in 5 days--        "
    .text "        pretty impressive, eh?"
    .text "                   wave on da keyz here"
    .text "          xd           "
    //.text "brace yourselves for a glitch..            this program will self destruct in --42.3845 seconds--"
    .text "                   credits time ig?"
    .text "                   "
    .text " music - 'and i say hey no no hey no' by linus"
    .text "                   "
    //.text " bruh        more glitches                                                         "
    .text " code  - waverider     but also some code 'borrowed' from various sources"
    .text "    (don't worry, i understand the code that i take and i edit them *very* thoroughly.        "
    .text " gfx   - lol i just took stock images of a monitor and c64 and pasted them together, and then used gimp 2.10.30 to edit them together"  //   (of course now they are gone...)     "
    .text " tools - ca65 and kick assembler/sublime text for programming,  project one, gimp, microsoft paint, and scratch.mit.edu for gfx, and spritemate.com for sprite graphix           "
    .text " the movement of 'rocks' is created using an fds wavetable instrument in famitracker, saved the instrument, then i used hexed.it to remove the header for the .fti file.       how peculiar...         "
    .text "           ***restarting scroller***          "
scrolltextend:

// [Do Starfield] --------------------------------------------------------------

// This routine does 3 things:

// 1) Erases stars
// 2) Moves stars
// 3) Draws stars in new position


doStarfield:

// Erase stars

        lda #0                                  // Erase 4 stars
        tay
        sta (starfieldPtr),y
        sta (starfieldPtr2),y
        sta (starfieldPtr3),y
        sta (starfieldPtr4),y

// Move star 1

        lda rasterCount                         // Test bit 0 of counter
        and #1                                  // move 1 pixel every
        beq rstar1Done                          // other frame, to simulate
        inc starfieldPtr                        // 1/2 pixel movement
        bne rok
        inc starfieldPtr+1
rok:
        lda starfieldPtr
        cmp #<star1Limit
        bne rstar1Done
        lda starfieldPtr+1
        cmp #>star1Limit
        bne rstar1Done
        lda #<star1Reset                        // Reset 1
        sta starfieldPtr
        lda #>star1Reset
        sta starfieldPtr+1
rstar1Done:

// Move star 2

        inc starfieldPtr2                       // 1 pixel per frame
        bne rok2                                
        inc starfieldPtr2+1
rok2:
        lda starfieldPtr2
        cmp #<star2Limit
        bne rstar2Done
        lda starfieldPtr2+1
        cmp #>star2Limit
        bne rstar2Done
        lda #<star2Reset                        // Reset 2
        sta starfieldPtr2
        lda #>star2Reset
        sta starfieldPtr2+1
rstar2Done:

// Move star 3

        lda rasterCount                         // half pixel per frame
        and #1
        beq rstar3done
        inc starfieldPtr3
        bne rok3
        inc starfieldPtr3+1
rok3:
        lda starfieldPtr3
        cmp #<star3Limit
        bne rstar3done
        lda starfieldPtr3+1
        cmp #>star3Limit
        bne rstar3done
        lda #<star3Reset                        // Reset 3
        sta starfieldPtr3
        lda #>star3Reset
        sta starfieldPtr3+1
rstar3done:

// Move star 4

        lda starfieldPtr4                       // 2 pixels per frame
        clc
        adc #2
        sta starfieldPtr4
        bcc rok4
        inc starfieldPtr4+1
rok4:
        lda starfieldPtr4+1
        cmp #>star4Limit
        bne rstar4done
        lda starfieldPtr4
        cmp #<star4Limit
        bcc rstar4done
        lda #<star4Reset                       // Reset 4
        sta starfieldPtr4
        lda #>star4Reset
        sta starfieldPtr4+1
rstar4done:

 // 2 static stars that flicker

        lda #192                       
        ldy rasterCount
        cpy #230
        bcc rshow
        lda #0
rshow:   
        sta staticStar1

        tya
        eor #$80
        tay
        lda #192
        cpy #230
        bcc rshow2
        lda #0
rshow2:  
        sta staticStar2

// Plot new stars
                
        ldy #0
        lda (starfieldPtr),y            // Moving stars dont overlap other stars
        ora #3                          // as they use non conflicting bit
        sta (starfieldPtr),y            // combinations

        lda (starfieldPtr2),y
        ora #3
        sta (starfieldPtr2),y

        lda (starfieldPtr3),y
        ora #12
        sta (starfieldPtr3),y

        lda (starfieldPtr4),y
        ora #48
        sta (starfieldPtr4),y

        rts


// [Initialise Starfield Pointers] ---------------------------------------------

// Initialise all pointers, note these INIT values maybe different to RESET
// values, this is give a non-uniform appearance to the stars and reduce
// obvious 'patterning' - I tried to give a fairly natural appearance.

initStarfield:
        lda #<star1Init 
        sta starfieldPtr
        lda #>star1Init
        sta starfieldPtr+1

        lda #<star2Init 
        sta starfieldPtr2
        lda #>star2Init
        sta starfieldPtr2+1

        lda #<star3Init 
        sta starfieldPtr3
        lda #>star3Init
        sta starfieldPtr3+1

        lda #<star4Init  
        sta starfieldPtr4
        lda #>star4Init
        sta starfieldPtr4+1

        rts


// [Create Star Screen] --------------------------------------------------------

// Creates the starfield charmap and colour charmap

// This routine paints vertical stripes of colour into the colourmap
// so the stars are different colours

// It also plots the correct characters to the screen, wrapping them around
// at the correct char count to give7 to the starfield effect.


CreateStarScreen:
        ldx #40-1                       // Create starfield of chars
rlp:     
        txa
        pha
        tay
        lda starfieldRow,x

        sta rsmc1+1
        ldx #58+25
        cmp #58+25
        bcc rlow
        ldx #58+50
rlow:   
        stx rsmc3+1
        txa
        sec
        sbc #25
        sta rsmc2+1
        lda #<starScreenChar
        sta zeroPointer
        lda #>starScreenChar
        sta zeroPointer+1 
        ldx #25-1
rsmc1:   
        lda #3
        sta (zeroPointer),y
        lda zeroPointer
        clc
        adc #40
        sta zeroPointer
        bcc rclr
        inc zeroPointer+1
rclr:    
        inc rsmc1+1
        lda rsmc1+1
rsmc3:   
        cmp #0
        bne ronscreen
rsmc2:   
        lda #0
        sta rsmc1+1
ronscreen:        
        dex
        bpl rsmc1

        pla
        tax
        dex
        bpl rlp

        lda #<StarScreenCols           // Fill colour map with vertical stripes of colour for starfield
        sta zeroPointer
        lda #>StarScreenCols
        sta zeroPointer+1
        ldx #25-1
rlp1:    
        stx rsmcx+1
        ldx #0
        ldy #40-1
rlp2:
        lda starfieldCols,x
        sta (zeroPointer),y
        inx
        cpx #starColourLimit
        bne rcol
        ldx #0
rcol:
        dey
        bpl rlp2
        lda zeroPointer
        clc
        adc #40
        sta zeroPointer
        bcc rhiOk
        inc zeroPointer+1
        inc zeroPointer2+1
rhiOk:
rsmcx:
        ldx #0
        dex
        bpl rlp1
        rts

// plasma

updatePlasma:
        lda plasmaCnt+0
        clc
        adc sineSpeeds+0
        sta plasmaCnt+0
        lda plasmaCnt+1
        clc
        adc sineSpeeds+1

        sta plasmaCnt+1
        lda add
        clc
        adc addSpeed
        anc #$3f
        sta add

        lda #<screen
        sta store+1
        lda #>screen
        sta store+2
        
        lda #0
        sta sine0+1
        sta sine1+1
        sta rtSine+1
        sta color+1

        lda #height-1
        sta yPos
yLoop:
        ldx plasmaCnt + 0
        ldy plasmaCnt + 1
        clc
sine0:  lda sine128,x
sine1:  adc sine64,y
        sta lineOffset
        
        lda sine0+1
        clc
        adc #realtimeSpread0
        sta sine0+1

        lda sine1+1
        clc
        adc #realtimeSpread1
        sta sine1+1

        ldx #width-1
xLoop:
        lda sineOffsets,x
        clc
        adc lineOffset
        tay
        lda colorOffsets,x
        clc
rtSine: adc sine64,y
        adc add
        tay
color:  lda colorTable,y
store:  sta screen,x
        dex
        bpl xLoop

        lda rtSine+1
        clc
        adc #sineSpreadY
        sta rtSine+1

        lda color+1
        clc
        adc #colorSpreadY
        sta color+1

        lda store+1
        clc
        adc #40
        sta store+1
        bcc !+
        inc store+2
!:      
        dec yPos        
        bpl yLoop
        rts

sineOffsets:    
        .fill 40, i*sineSpreadX
colorOffsets:   
        .fill 40, i*colorSpreadX
lineOffset:     
        .by 0

// [DATA] ----------------------------------------------------------------------

// Dark starfield so it doesnt distract from bullets and text
starfieldCols:

        .byte 14,10,12,15,14,13,12,11,10,14
        .byte 14,10,14,15,14,13,12,11,10,12

// Star positions, 40 X positions, range 58-107
starfieldRow:
        .byte 058,092,073,064,091,062,093,081,066,094
        .byte 086,059,079,087,080,071,076,067,082,095
        .byte 100,078,099,060,075,063,084,065,083,096
        .byte 068,088,074,061,090,098,085,101,097,077

bigsine:
  .byte $21,$21,$22,$23,$24,$24,$25,$26,$27,$28,$28,$29,$2a,$2b,$2b,$2c
  .byte $2d,$2e,$2e,$2f,$30,$30,$31,$32,$32,$33,$34,$34,$35,$35,$36,$37
  .byte $37,$38,$38,$39,$39,$3a,$3a,$3b,$3b,$3c,$3c,$3c,$3d,$3d,$3d,$3e
  .byte $3e,$3e,$3f,$3f,$3f,$3f,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40
  .byte $40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$3f,$3f,$3f,$3f,$3f,$3e
  .byte $3e,$3e,$3d,$3d,$3d,$3c,$3c,$3b,$3b,$3b,$3a,$3a,$39,$39,$38,$37
  .byte $37,$36,$36,$35,$35,$34,$33,$33,$32,$31,$31,$30,$2f,$2f,$2e,$2d
  .byte $2c,$2c,$2b,$2a,$29,$29,$28,$27,$26,$26,$25,$24,$23,$22,$22,$21
  .byte $20,$1f,$1f,$1e,$1d,$1c,$1b,$1b,$1a,$19,$18,$18,$17,$16,$15,$15
  .byte $14,$13,$12,$12,$11,$10,$10,$0f,$0e,$0e,$0d,$0c,$0c,$0b,$0b,$0a
  .byte $0a,$09,$08,$08,$07,$07,$06,$06,$06,$05,$05,$04,$04,$04,$03,$03
  .byte $03,$02,$02,$02,$02,$02,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$02,$02,$02,$02,$03,$03
  .byte $03,$04,$04,$04,$05,$05,$05,$06,$06,$07,$07,$08,$08,$09,$09,$0a
  .byte $0a,$0b,$0c,$0c,$0d,$0d,$0e,$0f,$0f,$10,$11,$11,$12,$13,$13,$14
  .byte $15,$16,$16,$17,$18,$19,$19,$1a,$1b,$1c,$1d,$1d,$1e,$1f,$20,$20

bounce: .fill bounced.getSize(), bounced.get(i)

*=$4c00;            .fill picture.getScreenRamSize(), picture.getScreenRam(i)
*=$5c00; colorRam:  .fill picture.getColorRamSize(), picture.getColorRam(i)
*=$6000;            .fill picture.getBitmapSize(), picture.getBitmap(i)