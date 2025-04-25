; 160x100 Bitmap encoder - LRE version 2.0 Michal Starega 2025

; This encoder generates RLE with input data on $4000 and outputs data on $6000 which is RLE compression using input data nibbles, which gives a little more compression ratio. Output file could be used directly with PlayerV2.



.ORG = $0200

;	----	ZERO PAGE DATA ----
	lda #$00					;get ready
	sta $00						;first data byte left side
	sta $01						;first data byte right side
	sta $02						;second data byte left side
	sta $03						;iterations +1
	sta $05						;input data pointer
	sta $07						;output data pointer
	sta $09						;rotate data pointer
	sta $0A						;rotate data page = always 00
	sta $0B						;data copy
	sta $0C						;second data byte right side
	lda #$03					;iteration for input data
	sta $04
	lda #$40					;input data page
	sta $06
	lda #$60					;output data page
	sta $08
	ldx #$F0					;stack initialization
	txs
	ldx #$00					;reset register x
	ldy #$00					;reset register y


;	----	INPUT DATA	----

jsr input						;get first input data

;	----	ENCODER	----

count:
		dec $04					;decrement rotation counter
		beq new					;get new data to compare
		lda ($09,x)				;load first byte of data
		sta $0B					;save copy for later saving output data
		inc $09					;increse data pointer
		cmp ($09,x)				;compare data byte with next one
	bne save					;save encoded data if there's difference
		inc $03					;increse iterations
		lda $03			
		cmp #$10				;check if it's max 16 iterations "F" - one iteration is indicated by 0
	beq save2					;if there's max iterations save current state
jmp count

save:							;save output data
		lda $0B					;load previously saved data to save
		clc						;clear any messing carry bits
		adc $03					;add iterations to readed byte
		sta ($07),y				;store output data byte (first half is data, second half is iterations)
		jsr nexty				;increse y for next pointer
		lda #$00				;reset iterations counter
		sta $03
jmp count

save2:							;save output data

		lda $0B					;load previously saved data to save
		clc						;clear any messing carry bits
		dec $03
		adc $03					;add iterations to readed byte
		sta ($07),y				;store output data byte (first half is data, second half is iterations)
		jsr nexty				;increse y for next pointer
		clc
		lda #$00
		sta $03
jmp count

;	----	NEXT DATA	----
new:
		jsr input				;get next data
		lda #$00				;reset data pointer for new one
		sta $09
		lda #$03				;reset pointer for data rotation
		sta $04
jmp count

;	----	INPUT DATA	----
input:
	clc
	lda ($05,x)					;first input data byte
	and #$F0
	sta $00						;first data byte for comparsion
	lda ($05,x)					;first input data byte
	and #$0F
	rol
	rol
	rol
	rol
	sta $01						;second data byte for comparsion
	jsr nextx					;next byte
	lda ($05,x)					;second input data byte
	and #$F0
	sta $02						;third data byte for comparsion
	lda ($05,x)					;first input data byte
	and #$0F
	rol
	rol
	rol
	rol
	sta $0C	
rts

nexty:							;output data pointer and page
	iny
	bne return
	inc $08						;increase page for output data
rts

nextx:							;input data range checking
	inc $05
	bne return
	inc $06
	lda $06
	cmp #$48					;end of input data 2048kb
bne return
	
;	----	DONE ENCODING INDICATOR	(NOT NEEDED IF SAVING MEMORY IS PRIORITY)----
loop:							;shows OK on visable memory when done
lda #$FE
sta $2000
sta $2118
lda #$82
sta $2028
sta $2050
sta $2078
sta $20A0
sta $20C8
sta $20F0

lda #$42
sta $2001
sta $2119
lda #$44
sta $2029
sta $20F1
lda #$48
sta $2051
sta $20C9
lda #$70
sta $2079
sta $20A1
jmp loop

return:							;go back
rts