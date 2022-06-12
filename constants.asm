//////////////////////////////////////////////////////////////////////////////////////
// CityXen - https://linktr.ee/cityxen
//////////////////////////////////////////////////////////////////////////////////////
// Deadline's C64 Assembly Language Library: Constants
//////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////////
// Various Memory Constants
.const SECONDARY_ADDRESS    = $B9
.const DEVICE_NUMBER        = $BA
.const PNTR                 = $D3
.const CURSOR_X_POS         = $D3
.const TBLX                 = $D6
.const CURSOR_Y_POS         = $D6
.const CURSOR_COLOR         = $286
.const VICSCN               = $400
.const SCREEN_RAM           = $400
.const COLOR_RAM            = $D800
// Safe zero page locations $57-$70, $92-$96, $A3-$B1, $F7-$FE
.const TEMP_1               = $FB
.const TEMP_2               = $FC
.const zp_tmp               = $FB
.const zp_tmp_lo            = $FB
.const zp_tmp_hi            = $FC
.const TEMP_3               = $FD
.const TEMP_4               = $FE
.const TEMP_5               = $02
.const TEMP_6               = $c003
.const TEMP_7               = $04
.const TEMP_8               = $05
.const zp_ptr_screen        = $60
.const zp_ptr_screen_lo     = $60
.const zp_ptr_screen_hi     = $61
.const zp_ptr_color         = $62
.const zp_ptr_color_lo      = $62
.const zp_ptr_color_hi      = $63
.const zp_point_tmp         = $59
.const zp_point_tmp_lo      = $59
.const zp_point_tmp_hi      = $5a
.const zp_ptr_2             = $64
.const zp_ptr_2_lo          = $64
.const zp_ptr_2_hi          = $65
.const zp_temp              = $a3
.const zp_temp2             = $a4
.const zp_temp3             = $a5
.const JOYPORT_TIMER        = $05
//////////////////////////////////////////////////////////////////////////////////////
// VARIOUS STUFF
.const ZP_DATA_DIRECTION    = $00
.const ZP_IO_REGISTER       = $01
.const KERNAL_STOP_VECTOR   = $0328
//////////////////////////////////////////////////////////////////////////////////////
// SPRITE POINTERS
.const SPRITE_POINTERS      = $7F8
.const SPRITE_0_POINTER     = $7F8
.const SPRITE_1_POINTER     = $7F9
.const SPRITE_2_POINTER     = $7FA
.const SPRITE_3_POINTER     = $7FB
.const SPRITE_4_POINTER     = $7FC
.const SPRITE_5_POINTER     = $7FD
.const SPRITE_6_POINTER     = $7FE
.const SPRITE_7_POINTER     = $7FF
//////////////////////////////////////////////////////////////////////////////////////
// VIC CONSTANTS
.const SPRITE_LOCATIONS     = $D000
.const SPRITE_0_X           = $D000 // 53248 SP0X Sprite 0 Horizontal Position
.const SPRITE_0_Y           = $D001 // 53249 SP0Y Sprite 0 Vertical Position
.const SPRITE_1_X           = $D002 // 53250 SP1X Sprite 1 Horizontal Position
.const SPRITE_1_Y           = $D003 // 53251 SP1Y Sprite 1 Vertical Position
.const SPRITE_2_X           = $D004 // 53252 SP2X Sprite 2 Horizontal Position
.const SPRITE_2_Y           = $D005 // 53253 SP2Y Sprite 2 Vertical Position
.const SPRITE_3_X           = $D006 // 53254 SP3X Sprite 3 Horizontal Position
.const SPRITE_3_Y           = $D007 // 53255 SP3Y Sprite 3 Vertical Position
.const SPRITE_4_X           = $D008 // 53256 SP4X Sprite 4 Horizontal Position
.const SPRITE_4_Y           = $D009 // 53257 SP4Y Sprite 4 Vertical Position
.const SPRITE_5_X           = $D00A // 53258 SP5X Sprite 5 Horizontal Position
.const SPRITE_5_Y           = $D00B // 53259 SP5Y Sprite 5 Vertical Position
.const SPRITE_6_X           = $D00C // 53260 SP6X Sprite 6 Horizontal Position
.const SPRITE_6_Y           = $D00D // 53261 SP6Y Sprite 6 Vertical Position
.const SPRITE_7_X           = $D00E // 53262 SP7X Sprite 7 Horizontal Position
.const SPRITE_7_Y           = $D00F // 53263 SP7Y Sprite 7 Vertical Position
.const SPRITE_LOCATIONS_MSB = $D010 // 53264 Most Significant Bits of Sprites 0-7 Horizontal Position
.const SPRITE_MSB_X         = $D010 // 53264 Most Significant Bits of Sprites 0-7 Horizontal Position
.const VIC_CONTROL_REG_1    = $D011 // 53265 RST8 ECM- BMM- DEN- RSEL [   YSCROLL   ]
.const VIC_RASTER_COUNTER   = $D012 // 53266
.const VIC_LIGHT_PEN_X      = $D013 // 53267
.const VIC_LIGHT_PEN_Y      = $D014 // 53268
.const SPRITE_ENABLE        = $D015 // 53269
.const VIC_CONTROL_REG_2    = $D016 // 53270 ---- ---- RES- MCM- CSEL [   XSCROLL   ]
.const SPRITE_EXPAND_Y      = $D017 // 53271
.const VIC_MEM_POINTERS     = $D018 // 53272 VM13 VM12 VM11 VM10 CB13 CB12 CB11 ----
.const VIC_INTERRUPT_REG    = $D019 // 53273 IRQ- ---- ---- ---- ILP- IMMC IMBC IRST
.const VIC_INTERRUPT_ENABLE = $D01A // 53274 ---- ---- ---- ---- ELP- EMMC EMBC ERST
.const SPRITE_PRIORITY      = $D01B // 53275
.const SPRITE_MULTICOLOR    = $D01C // 53276
.const SPRITE_EXPAND_X      = $D01D // 53277
.const SPRITE_COLLISION_SPR = $D01E // 53278
.const SPRITE_COLLISION_DATA= $D01F // 53279
.const BORDER_COLOR         = $D020 // 53280
.const BACKGROUND_COLOR     = $D021 // 53281
.const BACKGROUND_COLOR_1   = $D022 // 53282
.const BACKGROUND_COLOR_2   = $D023 // 53283
.const BACKGROUND_COLOR_3   = $D024 // 53284
.const SPRITE_MULTICOLOR_0  = $D025
.const SPRITE_MULTICOLOR_1  = $D026
.const SPRITE_COLORS        = $D027
.const SPRITE_0_COLOR       = $D027
.const SPRITE_1_COLOR       = $D028
.const SPRITE_2_COLOR       = $D029
.const SPRITE_3_COLOR       = $D02A
.const SPRITE_4_COLOR       = $D02B
.const SPRITE_5_COLOR       = $D02C
.const SPRITE_6_COLOR       = $D02D
.const SPRITE_7_COLOR       = $D02E
//////////////////////////////////////////////////////////////////////////////////////
// IO CONSTANTS
.const JOYSTICK_PORT_2      = $DC00
.const JOYSTICK_PORT_1      = $DC01
//////////////////////////////////////////////////////////////////////////////////////