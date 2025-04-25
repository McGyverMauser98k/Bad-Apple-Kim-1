;	6502 Animation Player for K-1008 Visable Memory by Michal Starega 2025

; Load data from $4000 (first will be full frame after RLE encoding and than frames as differences from previous frame.
; Could be modified to use any data (full frame, differences) in certain spots to maintain lowest input data possible
; If You use instead delay rutine for rendering full frame in other spot partially, that could speed up playing time just copying that data later. About 20% of full image could be rendered that way,
; without loosing to much FPS overall, but about 2000 bytes in memory for temp could be needed.

.org = $0200  				;PLAYER PROGRAM MEMORY LOCATION AND START POINT

;	---- DATA CONVERSION TABLE FOR VISABLE MEMORY ----
ldx #$0F
convtab:
  lda data,x
  sta $10,x
  dex
  bpl convtab
  jmp notend

;convert for proper streched bytes from single data byte.
 data: .byte $00,$03,$0C,$0F,$30,$33,$3C,$3F,$C0,$C3,$CC,$CF,$F0,$F3,$FC,$FF
	
;START FULL FRAME RENDERING
notend:
	lda #$00					;reset input data pointer
	sta $00
	lda #$40					;reset input data page
	sta $01

startover:						;In case using full frame without reseting data pointer
	ldy #$00					;RESET INDEX Y
	sty $02						;visable memory data pointer
	lda #$20					;visable page data pointer
	sta $03						;visable memory first line page data pointer
	sta $05						;visable memory second line page data pointer
	lda #$28					;visable memory second line data pointer
	sta $04
	sta $06						;line count 40 bytes
	lda #$64					;iterations to end 100 DOUBLE LINES FOR ONE SCREEN
	sta $07

	
;	----	FULL FRAME PLAYER START ---- 

load:							;LOAD INPUT DATA
	ldx #$00					;RESET INDEX X
	lda ($00,x)					;LOAD ITERATIONS COUNTER
	tax							;COPY OF INPUT ENCODED BYTE
	and #$0F					;TAKE OUT ITERATIONS FROM ENCODED BYTE
	sta $09						;ITERATIONS FOR INPUT DATA byte
	txa							;RESTORE ENCODED BYTE			
	lsr A			
	lsr A			
	lsr A			
	lsr A			
	tax							;CONVERT READED BYTE TO SUITABLE FOR VISABLE MEMORY DISPLAING
	lda $10,x					;LOAD PROPER BYTE FROM DATA CONVERSION TABLE
	tax							;SAVE CONVERTED INPUT DATA FOR VISABLE MEMORY
	inc $00						;increment data pointer
	bne loop					;check for overflow
	inc $01						;increment data page after overflow

loop:							;DRAWING LINE
		txa						;LOAD BYTE TO STORE IN VISABLE MEMORY	
		sta ($02),y				;STORE BYTE IN FIRST LINE
		sta ($04),y				;STORE BYTE IN SECOND LINE
		iny						;NEXT Y
		bne go					;CHECK FOR OVERFLOW
			inc $03				;INCREMENT FIRST LINE PAGE AFTER OVERFLOW
			inc $05				;INCREMENT SECOND LINE PAGE AFTER OVERFLOW
		go:
		cpy $06					;CHECK END OF LINE - 40 BYTES PER LINE ON SCREEN
		beq end					;END LINE		
		back:
		lda $09					;LOAD ITERATION COUNTER AND CHECK IS IT ON ZERO
		beq load
	dec $09						;DECREMENT ITERATIONS
jmp loop						;GO FOR NEXT SAME OUTPUT BYTE OR SAVE LAST ONE FROM ITERATIONS
 
end:							;END OF TWO LINES
	tya							;TAKE OUT NEXT LINE POINTER
	clc
	adc #$28					;UPDATE FOR SECOND LINE START +40 BYTES
	bcc go2						;CHECK IS THE SAME PAGE IF NOT UPDATE PAGES
	inc $03						;INCREMENT FIRST LINE PAGE
	inc $05						;INCREMENT SECOND LINE PAGE
	go2:
	tay							;STORE LINE POINTER
	clc
	adc #$28					;ADD 40 BYTES FROM LINE START TO KNOW WHERE TO GO FOR NEXT LINE
	sta $06						;STORE END OF LINE POINTER
	dec $07						;DECREMENT LINE COUNTER TO DETERMINE END OF SCREEN
	bne back					;GO FOR NEXT LINE OR STOP SCREEN RENDERING
jmp diffplay						;END OF SCREEN RENDERING -> FOR VIDEO PLAYING IT WILL GO BACK TO START WITH NEXT FRAME DATA

;	----	END OF FULL FRAME PLAYER ---- 

middle:							;go back to start of player - too far for just one jump
lda #$00
sta $0D
lda #$20
sta $0E
ldy #$00
clear:
lda #$FF
STA ($0D),y
iny
bne clear
inc $0E
lda $0E
cmp #$40
bne clear
jmp notend

;	----	DIFFERENCE BETWEEN FRAMES PLAYER START ---- 

diffplay:
	lda #$20					;visable memory page data pointer
	sta $03						;visable memory first line page data pointer
	sta $05						;data page for new data input
	ldx #$00
	stx $04						;data pointer for new data input
	stx $06						;temporary data page
	stx $07						;temporary data to save on screen
	stx $08						;pointer for page change
	stx $02						;visable memory data pointer
	ldy #$00
	lda #$E1					;frames x2-1 with differences for loop purposes
	sta $09
	lda #$02					;loop to slow down frame rate after each frame
	sta $0A

start:
lda ($00),y						;load first nibble - data page/data
and #$F0						;take out data page nibble
lsr
lsr
lsr
lsr
;-------- changeing data output page from 20 to 30 depending of input byte.
sta $06							;save in temporary data page low nibble - 00 do 0F
cmp $08
beq not
cmp #$00
bne not
lda $03
eor #$10						;change page 20<->30 after F-->0
sta $03
;-------- Loop to slow down FPS to about 30 from 45
dec $0A							;decrement pages count - triggers delay after complete frame
bne nodelay						;no delay in middle of frame
lda #$07						;first outer loop initialization
sta $0B
outloop:						;outer loop initialization				
lda #$F9		
sta $0C
inloop:
dec $0C							;decrement outer loop
bne inloop
dec $0B							;decrement inner loop
bne outloop
lda #$02						;reset after frame delay trigger
sta $0A
nodelay:
;------- End of delay loop
dec $09							;frame count to reset player
beq middle						;jump to begining
not:
lda $06							;make copy of actual data page low nibble for comparing page change
sta $08							;low nibble of change page (0-F) for second pass
;------- Visable memory page decoding END.

;------- SAVE DATA TO VISABLE MEMORY -------
clc
adc $03 
sta $05							;actual change page + pointer combined in one byte (20-3F)
lda ($00),y						;on screen data which will be displayed on screen after decoding
and #$0F
tax
lda $10,x						;load proper data for visable memory
sta $07		 					;save decoded data in temp
ldx #$00
iny								;next input data - change to data pointer byte
bne pom
inc $01
pom:
lda ($00),y						;data pointer
sta $04
iny
bne pom2
inc $01
pom2:
lda $07							;load data which will be displayed in firs line
sta ($04,x)						;save change on screen memory in first line
clc
lda $04
adc #$28						;update pointer for second line +40 bytes per line
sta $04
lda $05							;update pointer page if needed
adc #$00
sta $05
lda $07							;load data which will be displayed in second line
sta ($04,x)						;save change on screen memory in second line
jmp start

;	----	END OF DIFFERENCE BETWEEN FRAMES PLAYER ---- 