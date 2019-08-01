; ----------------- MMC/SD routines ------------------

mmcByteNoSend:
	ldi temp,0xff
mmcByte:

.if MMC_DEBUG
	printstring "MMC: <--"
	rcall printhex
.endif
	
	out SPDR,temp
mmcWrByteW:
	in temp,SPSR
	sbrs temp,7
	 rjmp mmcWrByteW
	in temp,SPDR

.if MMC_DEBUG
	printstring ", -->"
	rcall printhex
	printnewline
.endif
	ret


;Wait till the mmc answers with the response in temp2, or till a timeout happens.
mmcWaitResp:
	ldiw z,0
mmcWaitResploop:
	rcall mmcByteNoSend
	cpi temp,0xff
	brne mmcWaitResploopEnd
	adiw zl,1
	cpi zh,255
	breq mmcWaitErr
	rjmp mmcWaitResploop
mmcWaitResploopEnd:
	ret


mmcWaitErr:
	mov temp,temp2
	rcall printhex
	printstring ": Error: MMC resp timeout!"
	printnewline
	rjmp resetAVR

mmcInit:
	ldi temp,0x53
	out SPCR,temp
	
	;Init start: send 80 clocks with cs disabled
	sbi P_MMC_CS,mmc_cs

;	ldi temp2,20
	ldi temp2,10     ; exactly 80 clocks
mmcInitLoop:
	mov temp,temp2
	rcall mmcByte
	dec temp2
	brne mmcInitLoop

	cbi P_MMC_CS,mmc_cs
	rcall mmcByteNoSend
	rcall mmcByteNoSend
	rcall mmcByteNoSend
	rcall mmcByteNoSend
	rcall mmcByteNoSend
	rcall mmcByteNoSend
	sbi P_MMC_CS,mmc_cs
	rcall mmcByteNoSend
	rcall mmcByteNoSend
	rcall mmcByteNoSend
	rcall mmcByteNoSend

	;Send init command
	cbi P_MMC_CS,mmc_cs
	ldi temp,0xff	;dummy
	rcall mmcByte
	ldi temp,0xff	;dummy
	rcall mmcByte
	ldi temp,0x40	;cmd
	rcall mmcByte
	ldi temp,0	;pxh
	rcall mmcByte
	ldi temp,0	;pxl
	rcall mmcByte
	ldi temp,0	;pyh
	rcall mmcByte
	ldi temp,0	;pyl
	rcall mmcByte
	ldi temp,0x95	;crc
	rcall mmcByte
	ldi temp,0xff	;return byte
	rcall mmcByte

	ldi temp2,0 			;Error Code 0
	rcall mmcWaitResp  		;Test on CMD0 is OK

	sbi P_MMC_CS,mmc_cs		;disable /CS
	rcall mmcByteNoSend


;Read OCR till card is ready
	ldi temp2,100			;repeat counter
mmcInitOcrLoop:	
	push temp2

	cbi P_MMC_CS,mmc_cs		;enable /CS
	ldi temp,0xff	;dummy
	rcall mmcByte
	ldi temp,0x41	;cmd
	rcall mmcByte
	ldi temp,0	;pxh
	rcall mmcByte
	ldi temp,0	;pxl
	rcall mmcByte
	ldi temp,0	;pyh
	rcall mmcByte
	ldi temp,0	;pyl
	rcall mmcByte
;	ldi temp,0x95			;crc
	ldi temp,0x01			;crc
	rcall mmcByte
	rcall mmcByteNoSend

	ldi temp2,1
	rcall mmcWaitResp		;wait until mmc-card send a byte <> 0xFF
					;the first answer must be 0x01 (Idle-Mode)
	cpi temp,0
	breq mmcInitOcrLoopDone 	;second answer is 0x00 (Idle-Mode leave) CMD1 is OK

	sbi P_MMC_CS,mmc_cs		;disable /CS

	rcall mmcByteNoSend

	ldi	temp,10
	rcall	delay_ms
	
	pop temp2
	dec temp2
	brne mmcInitOcrLoop		;repeat 

	ldi temp2,4  
	rjmp mmcWaitErr

mmcInitOcrLoopDone:
	pop temp2
	sbi P_MMC_CS,mmc_cs  		;disable /CS
	rcall mmcByteNoSend

	out SPCR,_0
	ret


;Call this with yh:yl:xh:xl = sector number
;
mmcReadSect:
	ldi temp,0x50
	out SPCR,temp

	cbi P_MMC_CS,mmc_cs
	rcall mmcByteNoSend
	ldi temp,0x51	;cmd (read sector)
	rcall mmcByte
	lsl	xl			;convert to byte address (*512)
	rol	xh
	rol 	yl
	mov	temp,yl
	rcall mmcByte
	mov temp,xh ;pxl
	rcall mmcByte
	mov temp,xl ;pyh
	rcall mmcByte
	ldi temp,0  ;pyl
	rcall mmcByte
	ldi temp,0x95	;crc
	rcall mmcByte
	ldi temp,0xff	;return byte
	rcall mmcByte

	;resp
	ldi temp2,2
	rcall mmcWaitResp

	;data token
	ldi temp2,3
	rcall mmcWaitResp

	;Read sector to AVR RAM
	ldiw	z,hostbuf
mmcreadloop:
	rcall mmcByteNoSend
	st z+,temp
	cpi zl,low(hostbuf+512)
	brne mmcreadloop
	cpi zh,high(hostbuf+512)
	brne mmcreadloop

	;CRC
	rcall mmcByteNoSend
	rcall mmcByteNoSend

	sbi P_MMC_CS,mmc_cs
	rcall mmcByteNoSend

	out SPCR,_0
	ret


;Call this with yh:yl:xh:xl = sector number
;
mmcWriteSect:
	ldi temp,0x50
	out SPCR,temp

	cbi P_MMC_CS,mmc_cs
	rcall mmcByteNoSend

	ldi temp,0x58	;cmd (write sector)
	rcall mmcByte
	lsl	xl			;convert to byte address (*512)
	rol	xh
	rol 	yl
	mov	temp,yl
	rcall mmcByte
	mov temp,xh ;pxl
	rcall mmcByte
	mov temp,xl ;pyh
	rcall mmcByte
	ldi temp,0  ;pyl
	rcall mmcByte
	ldi temp,0x95	;crc
	rcall mmcByte
	ldi temp,0xff	;return byte
	rcall mmcByte

	;resp
	ldi temp2,1
	rcall mmcWaitResp

	;Send data token
	ldi temp,0xfe
	rcall mmcByte

	;Write sector from AVR RAM
	ldiw	z,hostbuf
mmcwriteloop:
	ld temp,z+
	rcall mmcByte
	cpi zl,low(hostbuf+512)
	brne mmcwriteloop
	cpi zh,high(hostbuf+512)
	brne mmcwriteloop

	;CRC
	rcall mmcByteNoSend
	rcall mmcByteNoSend

	;Status. Ignored for now.
	rcall mmcByteNoSend

;Wait till the mmc has written everything
mmcwaitwritten:
	rcall mmcByteNoSend
	cpi temp,0xff
	brne mmcwaitwritten

	sbi P_MMC_CS,mmc_cs
	rcall mmcByteNoSend

	out SPCR,_0
	ret


;Set up wdt to time out after 1 sec.
resetAVR:
	lds	temp,txcount	;Wait, till tx buffer is empty
	tst	temp
	brne	resetAVR
	
	cli
	ldi temp,(1<<WDCE)
	outm8 WDTCSR,temp
	ldi temp,(1<<WDCE) | (1<<WDE) | (110<<WDP0)
	outm8 WDTCSR,temp
resetwait:
	rjmp resetwait


