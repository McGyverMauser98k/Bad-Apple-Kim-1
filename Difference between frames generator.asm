;	6502 Difference finder V2.0 by Michal Starega 2025

; Load first image after RLE compression to $6000
; Load next image after RLE compression to $7000
; Save output as difference file from $8000

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
	lda #$60					;reset input data page
	sta $02
	lda #$02
	sta $05

startover:
	lda #$00					;visable memory data pointer
	sta $03
	lda #$20					;visable page data pointer
	sta $04						;visable memory first line page data pointer
	lda #$28
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
;		sta ($05),y				;STORE BYTE IN SECOND LINE
		iny						;NEXT Y
		bne go					;CHECK FOR OVERFLOW
			inc $04				;INCREMENT FIRST LINE PAGE AFTER OVERFLOW
;			inc $06				;INCREMENT SECOND LINE PAGE AFTER OVERFLOW
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
;	inc $06						;INCREMENT SECOND LINE PAGE
	go2:
	tay							;STORE LINE POINTER
	clc
	adc #$28					;ADD 40 BYTES FROM LINE START TO KNOW WHERE TO GO FOR NEXT LINE
	sta $08						;STORE END OF LINE POINTER
	dec $07						;DECREMENT LINE COUNTER TO DETERMINE END OF SCREEN
	bne back					;GO FOR NEXT LINE OR STOP SCREEN RENDERING
jmp secondpass					;END OF SCREEN RENDERING -> FOR VIDEO PLAYING IT WILL GO BACK TO START WITH NEXT FRAME DATA

secondpass:
	dec $05
	beq compare2
	lda #$00					;reset input data pointer
	sta $01
	lda #$70					;reset input data page
	sta $02
	lda #$00					;visable memory data pointer
	sta $03
	lda #$40					;visable page data pointer
	sta $04						;visable memory first line page data pointer
	lda #$28
	sta $08						;line count 40 bytes
	lda #$64					;iterations to end 100 DOUBLE LINES FOR ONE SCREEN
	sta $07
	ldy #$00					;RESET INDEX Y
jmp load

compare2:
	lda #$00					;reset input data pointer
	sta $01
	lda #$20					;reset input data page
	sta $02
	lda #$00					;visable memory data pointer
	sta $03
	lda #$40					;visable page data pointer
	sta $04						;visable memory first line page data pointer
	lda #$00
	sta $05
	lda #$80
	sta $06
	ldx #$00
	ldy #$00					;RESET INDEX Y

compare:
lda ($03),y			;first image - $2000
cmp ($01),y			;second image - $4000
bne zapisz
iny
bne page
inc $02
inc $04
lda $04
cmp #$60
beq koniec
page:
jmp compare


zapisz:
lda ($03),y
jsr exchange
sta $07
lda $02
and #$0F
asl
asl
asl
asl
clc
adc $07
sta ($05,x)			;save whats data diffrent in that place
inc $05
bne page4
inc $06
page4:
tya
sta ($05,x)			;save pointer for difference
inc $05
bne page3
inc $06
page3:
iny
bne page5
inc $02
inc $04
lda $04
cmp #$60
beq koniec
page5:
jmp compare

koniec:
jmp koniec

exchange:
cmp #$00
bne save1
lda #$00
rts
save1: 
cmp #$03
bne save2
lda #$01
rts
save2:
cmp #$0C
bne save3
lda #$02
rts
save3:
cmp #$0F
bne save4
lda #$03
rts
save4:
cmp #$30
bne save5
lda #$04
rts
save5:
cmp #$33
bne save6
lda #$05
rts
save6:
cmp #$3C
bne save7
lda #$06
rts
save7:
cmp #$3F
bne save8
lda #$07
rts
save8:
cmp #$C0
bne save9
lda #$08
rts
save9:
cmp #$C3
bne save10
lda #$09
rts
save10:
cmp #$CC
bne save11
lda #$0A
rts
save11:
cmp #$CF
bne save12
lda #$0B
rts
save12:
cmp #$F0
bne save13
lda #$0C
rts
save13:
cmp #$F3
bne save14
lda #$0D
rts
save14:
cmp #$FC
bne save15
lda #$0E
rts
save15:
cmp #$FF
bne save16
lda #$0F
rts
save16:
wyjdz:
rts