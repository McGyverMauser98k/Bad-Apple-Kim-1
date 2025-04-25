;	6502 Video Player V2.0 by Michal Starega 2025

;It decodes RLE string of data from encoded file, as continous RLE encoded frames. It converts input from $4000 to Visable memory with shifting proper bytes to make it seen on screen as it should.

.org = $0200  				;PLAYER PROGRAM MEMORY LOCATION

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
	
;START FRAME RENDERING
notend:
	lda #$00					;reset input data pointer
	sta $01
	lda #$40					;reset input data page
	sta $02
	lda #$58					;reset frame counts
	sta $00

startover:
	dec $00						;DECREMENT FRAME COUNTER USEFULL FOR LOOP TESTS
	beq notend					;loops around avaliable frames for testing purposes
	lda #$00					;visable memory data pointer
	sta $03
	lda #$20					;visable page data pointer
	sta $04						;visable memory first line page data pointer
	sta $06						;visable memory second line page data pointer
	lda #$28					;visable memory second line data pointer
	sta $05
	sta $08						;line count 40 bytes
	lda #$64					;iterations to end 100 DOUBLE LINES FOR ONE SCREEN
	sta $07
	ldy #$00					;RESET INDEX Y
	
;	----	PLAYER START ---- 

load:							;LOAD INPUT DATA
	ldx #$00
	lda ($01,x)					;LOAD ITERATIONS COUNTER
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
	inc $01						;increment data pointer
	bne loop					;check for overflow
	inc $02						;increment data page after overflow

loop:							;DRAWING LINE
		txa						;LOAD BYTE TO STORE IN VISABLE MEMORY	
		sta ($03),y				;STORE BYTE IN FIRST LINE
		sta ($05),y				;STORE BYTE IN SECOND LINE
		iny						;NEXT Y
		bne go					;CHECK FOR OVERFLOW
			inc $04				;INCREMENT FIRST LINE PAGE AFTER OVERFLOW
			inc $06				;INCREMENT SECOND LINE PAGE AFTER OVERFLOW
		go:
		cpy $08					;CHECK END OF LINE - 40 BYTES PER LINE ON SCREEN
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
	inc $04						;INCREMENT FIRST LINE PAGE
	inc $06						;INCREMENT SECOND LINE PAGE
	go2:
	tay							;STORE LINE POINTER
	clc
	adc #$28					;ADD 40 BYTES FROM LINE START TO KNOW WHERE TO GO FOR NEXT LINE
	sta $08						;STORE END OF LINE POINTER
	dec $07						;DECREMENT LINE COUNTER TO DETERMINE END OF SCREEN
	bne back					;GO FOR NEXT LINE OR STOP SCREEN RENDERING
jmp startover					;END OF SCREEN RENDERING -> FOR VIDEO PLAYING IT WILL GO BACK TO START WITH NEXT FRAME DATA