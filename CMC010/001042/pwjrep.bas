10	!---------------------------------------------------------------&
	!	SYSTEM NAME		-Sub System			&
	!	Program Description Name				&
	!								&
	!	PWJREP.B2S	V1.0	February 1987			&
	!								&
	! Author - F.Starman,   Computer Management Center, Inc.	&
	!								&
	! Files-PRNTJJ.TMP	-ASCII	Printing Info			&
	!	PWJH.DAT	-ISAM	Header File 			&
	!	PWJL.DAT	-ISAM	Line File 			&
	!	CUSTOM.DAT	-ISAM	Customer File 			&
	!---------------------------------------------------------------&
	!		      Copyright (c) 1984 by 			&
	!	  Computer Management Center, Idaho Falls, Idaho	&
	! 								&
	! This software is furnished under a license and may be used	&
	! and copied only in accordance with the terms of such license 	&
	! and with the inclusion of the above copyright notice.  This 	&
	! software or any other copies therof may not be provided or 	&
	! otherwise made available to any other person.  No title to 	&
	! and ownership of the software is hereby transferred.		&
	!      The information in this software is subject to change	&
	! without notice and should not be construed as a committment 	&
	! by Computer Management Center. 				&
	!      CMC assumes no responsibility for the use or reliability &
	! of its software on equipment which is not supported by CMC. 	&
	!---------------------------------------------------------------&

30	ON ERROR GOTO 19000 &
\	JUNK$	= SYS(CHR$(6%)+CHR$(-7%)) 		!^C ERROR TRAPPING &
	! &
\	TEMP$	= SYS(CHR$(12%)) 			!LAST FILE OPENED &
\	PRJPRG$	= MID(TEMP$,23%,2%)+NUM1$(ASCII(MID(TEMP$,25%,1%))) + ':'+   &
		'['+NUM1$(ASCII(MID(TEMP$,6%,1%)))+','+NUM1$(ASCII(MID(TEMP$,&
		5%,1%)))+']'+RAD$(ASCII(MID(TEMP$,7%,1%))+SWAP%(ASCII(MID(   &
		TEMP$,8%,1%))))+RAD$(ASCII(MID(TEMP$,9%,1%))+SWAP%(ASCII(MID(&
		TEMP$,10%,1%))))   &
	! &
\	TEMP$	= SYS(CHR$(6%)+CHR$(9%)) 		!GET JOB NUMBER &
\	JJ%	= ASCII(LEFT(TEMP$,1%))/2% &
\	JJ$	= RIGHT(NUM1$(JJ%+100%),2%) &

60	CH%,OUTPUT.CH% = 1%			! Keybrd,Output channel &
\	OPEN 'KB:' AS FILE 1%, MODE 8%+256% 	! Echo,Disbl Hibrn,%^C 	&

70	ESC$		= CHR$(155%)		! Escape code for VT100 &
\	COLM.ON$	= ESC$+'[?3h'		! 132 Column mode 	&
\	COLM.OFF$	= ESC$+'[?3l'		! 80 Column mode 	&
\	R.ON$		= ESC$+'[7m'      	! Reverse video 	&
\	G.OFF$		= ESC$+'[m'		! Select graphic off 	&
\	CLRLIN$		= ESC$+'[2K'		! Erase entire line 	&
\	CLSCN$		= ESC$+'[H'+ESC$+'[J'	! Clear entire screen 	&
\	ENTER.COPY$	= ESC$+'[5i'		! Enter media copy 	&
\	EXIT.COPY$	= ESC$+'[4i'		! Exit media copy 	&
\	HID.CURSOR$	= ESC$+"[?25l"+ESC$+"[24;1h"	! Hide cursor	&
\	CUR.ON$		= ESC$+"[?25h"		! Cursor on		&

80	!COM(THIS) DROP.DEAD.DATE$ = 8, VERSION.NO$ = 6, SERIAL.NO$ = 10 &
	DROP.DEAD.DATE$	= '        ' &
\	VERSION.NO$	= 'V1.0' &
\	DATE.TIME$	= SYS(CHR$(6%)+CHR$(20%)+CHR$(0%)+CHR$(0%)+CHR$(1%)+ &
				CHR$(SWAP%(1%))+STRING$(11%,0%)+ &
				CHR$(SWAP%(0%))+CHR$(1%)+CHR$(SWAP%(1%))) &
\	IF 	DROP.DEAD.DATE$<>'' &
	THEN	IF 	DROP.DEAD.DATE$<MID(DATE.TIME$,7%,8%) &
		THEN	MESG$	= MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(47%)),3%,30%) &
\			PRINT #CH%, MESG$;SYS(CHR$(9%)) &

100	DEVICE.CH%,PRNT.TMP%	= 02% &
\	PWJH.CH%		= 02% &
\	PWJL.CH%		= 04% &
\	CUSTOM.CH%		= 06% &
\	VENDES.CH%		= 08% &
\	MENU.CH%		= 12% &
\	PRINT.WIDTH%		= 132% &
\	WIDTH%			= PRINT.WIDTH%-32% &

120	!---------------------------------------OPEN THE ASCII PRINT FILE &
	OPEN 'PRNT'+JJ$+'.TMP' FOR INPUT AS FILE PRNT.TMP% &
\	NEXT.REPORT%	= 0% &
\	CHAIN.LINE%	= 0% &
\	CHAIN.PROGRAM$	= '' &

130	INPUT LINE #PRNT.TMP%, TEMP$ &
\	PR$	= LEFT(TEMP$,2%) &
\	CMD$	= CVT$$(RIGHT(TEMP$,4%),140%) &
\	IF	PR$='RN'	THEN	GOTO 160 IF NEXT.REPORT% &
\					NEXT.REPORT% = -1% &

140	LIN.PER.PAGE%	= VAL(CMD$)	IF PR$ = 'LP' &
\	SPAGE%		= VAL(CMD$)	IF PR$ = 'SP' &
\	EPAGE%		= VAL(CMD$)	IF PR$ = 'EP' &
\	COPIES%		= VAL(CMD$)	IF PR$ = 'CP' &
\	REPDATE$	= CMD$		IF PR$ = 'RD' &
\	DISPLAY$	= CMD$		IF PR$ = 'DP' &
\	AUTOSCROLL$	= CMD$		IF PR$ = 'AS' &
\	SPOOLR$		= CMD$		IF PR$ = 'SL' &
\	OUTDEV$		= CMD$		IF PR$ = 'OD' &
\	MS$		= CMD$		IF PR$ = 'MS' &
\	UD1$		= CMD$		IF PR$ = 'U1' &
\	UD2$		= CMD$		IF PR$ = 'U2' &
\	UD3$		= CMD$		IF PR$ = 'U3' &
\	UD4$		= CMD$		IF PR$ = 'U4' &
\	UD5$		= CMD$		IF PR$ = 'U5' &
\	IF	PR$ = 'PC' &
	THEN	TEMP%	= 1% &
\		TEMP%	= 2% IF LEFT(CMD$,1%) = '\' &
\		PC$	= PC$ + CHR$(VAL(MID(CMD$,TEMP%+(LOOP%-1%),3%))) &
				FOR LOOP%=1% TO LEN(CMD$) STEP 4% &

150	GOTO 130 &

160	GOTO 190 IF CHAIN.FLAG%=0% AND SRTKEY$='S' &
\	KILL 'PRNT'+JJ$+'.TMP' &
\	GOTO 190 IF PR$<>'RN' &

170	OPEN 'PRNT'+JJ$+'.TMP' FOR OUTPUT AS FILE PRNT.TMP% + 1% &
\	CHAIN.LINE%	= 31000% &

180	PRINT #PRNT.TMP% + 1%, TEMP$ &
\	INPUT LINE #PRNT.TMP%, TEMP$ &
\	CHAIN.PROGRAM$	= CVT$$(RIGHT(TEMP$,4%),140%) IF LEFT(TEMP$,2%)='PG' &
		AND CHAIN.PROGRAM$='' &
\	GOTO 180 &

190	FROM.ITEM$	= CVT$$(UD1$,132%) &
\	TO.ITEM$	= CVT$$(UD2$,132%) &
\	BATCH.NUM$	= UD3$ &
\	CLOSE PRNT.TMP% &

200	IF 	FNO%(DEVICE.CH%,'DEVICE.DAT','/RO','')=0% &
	THEN	PWJH.DEVICE$	= MID(FNL$,7%,20%)+'' &
			IF FNG%(DEVICE.CH%,'PWJH  ')=0% &
\		PWJL.DEVICE$	= MID(FNL$,7%,20%)+'' &
			IF FNG%(DEVICE.CH%,'PWJL  ')=0% &
\		CUSTOM.DEVICE$	= MID(FNL$,7%,20%)+'' &
			IF FNG%(DEVICE.CH%,'CUSTOM')=0% &
\		VENDES.DEVICE$	= MID(FNL$,7%,20%)+'' &
			IF FNG%(DEVICE.CH%,'VENDES')=0% &
\		V%	= FNC%(DEVICE.CH%) &

300	IF FNO%(PWJH.CH%,PWJH.DEVICE$+'PWJH'+BATCH.NUM$+'.DAT','/RO','') &
	THEN	V$=FNMESS$(CH%,FNS%,'PWJH'+BATCH.NUM$+'.DAT',0%,-1%) &
\		GOTO 17540 &

310	IF FNO%(PWJL.CH%,PWJL.DEVICE$+'PWJL'+BATCH.NUM$+'.DAT','/RO','') &
	THEN	V$=FNMESS$(CH%,FNS%,'PWJL'+BATCH.NUM$+'.DAT',0%,-1%) &
\		GOTO 17540 &

320	V%=FNO%(CUSTOM.CH%,CUSTOM.DEVICE$+'CUSTOM.DAT','/RO','')&
\	V%=FNO%(VENDES.CH%,VENDES.DEVICE$+'VENDES.DAT','/RO','')&

400	OPEN 'MENU.FIL/RO' FOR INPUT AS FILE MENU.CH% &
\	DIM #12%, A0$(1%)=64% &
\	COMPANY$	= CVT$$(RIGHT(A0$(1%),2%),128%) &
\	CLOSE MENU.CH% &
	
510	COLM.ON$	= '' IF PRINT.WIDTH% = 80%			&
\	TITLE$		= 'INVOICE JOURNAL - BATCH NO. '+BATCH.NUM$	&
\	TITLE$		= LEFT(SPACE$((WIDTH%-LEN(TITLE$))/2%)+		&
				TITLE$+SPACE$(WIDTH%),WIDTH%) 		&
\	COMPANY$	= CVT$$(LEFT(COMPANY$,WIDTH%),128%)		&
\	COMPANY$	= LEFT(SPACE$((WIDTH%-LEN(COMPANY$))/2%)+	&
				COMPANY$+SPACE$(WIDTH%),WIDTH%) 	&
\	PAGE.TOP$	= STRING$(2%,10%)+CHR$(13%)+'Date: '+DATE$(0%)+	&
				' '+COMPANY$+'        Page <<###>>'+	&
				CHR$(10%)+CHR$(13%)+'Time: '+TIME$(0%)+	&
				'  '+TITLE$+'        '+VERSION.NO$+	&
				CHR$(10%)+CHR$(13%)+			&
				SPACE$((PRINT.WIDTH%-LEN(REPDATE$))/2%)+&
				REPDATE$+STRING$(2%,10%)+CHR$(13%)+    	&
				STRING$(PRINT.WIDTH%,61%)+		&
				CHR$(10%)+CHR$(13%)	      		&
\	PAGE.TOP1$	='ORDNUM  INVNUM   INVDATE   ORDDATE   CUSTOMER '+ &
		SPACE$(19%)+'PO #     SOLD BY  CARRIER         TERMS     FOB'+&
			CHR$(10%)+CHR$(13%) &
\	PAG.A$		=STRING$(PRINT.WIDTH%,61%)+CHR$(10%)+CHR$(13%)	&
\	USE1$		='\    \  \      \ \      \  \      \  \    \ \'+ &
		SPACE$(18%)+'\ \      \ \      \ \             \ \       \  !' &
\	USE2$		=SPACE$(16%)+'###  \             \  \'+SPACE$(27%)+ &
		'\  \ \  \    \ \      \ ##,###.## ##,###.### !  ###,### '+ &
		'###,###.##' &
\	BOT.MARGIN%	= 6% &
\	PAGE.BOT$	= STRING$(BOT.MARGIN%,10%)+CHR$(13%) &
\	PAGE.BREAK%	= LIN.PER.PAGE% - BOT.MARGIN% &
\	MORE.LINES%	= 2% &
\	DISPLAY.CONTROL%= 15% &

10000	IF 	DISPLAY$<>'Y' &
	THEN	CLOSE CH% &
\		RESET.CH%	= 1% &
\		TEMP.CH%	= 12% &
\		OPEN OUTDEV$ AS FILE OUTPUT.CH%, MODE 2% &
\		IF 	SPAGE%>1%	THEN	OPEN 'NL:' AS FILE TEMP.CH% &
\						OUTPUT.CH%=TEMP.CH% &

17000	!---------------------------------------OUTPUT REPORT &
	V%	= FNG%(PWJH.CH%,FROM.ITEM$) &
\	IF 	DISPLAY$='Y' &
	THEN	PRINT #OUTPUT.CH%, CLSCN$;R.ON$;COLM.ON$;PAGE.TOP1$;G.OFF$; &
			FNSR$('2;19');FNP$('19;01'); &
	ELSE	PRINT #OUTPUT.CH%, ENTER.COPY$;	&
			FNPAGE$(LIN.PER.PAGE%,0%,1%,PAGE.TOP$+PAGE.TOP1$+ &
			PAG.A$,''); &
\		LINE.COUNT%=FNLINE% &

17030	FIELD #PWJH.CH%+1%,FNL% AS JUNK$,	&
			06% AS PWJH.ORDNUM$,		! Order # &
			06% AS PWJH.SOLDTO$,		! Sold to &
			06% AS PWJH.SHIPTO$,		! Ship to &
			08% AS PWJH.INVNUM$,		! Invoice # &
			02% AS PWJH.INVDAT$,		! Invoice Date &
			02% AS PWJH.SHPDAT$,		! Ship Date &
			08% AS PWJH.CUSPO$,		! Customer PO &
			08% AS PWJH.SOLDBY$,		! Sold by &
			08% AS PWJH.TERMS$,		! Terms &
			15% AS PWJH.CARNAM$,		! Carrier name &
			01% AS PWJH.FOBFLG$,		! FOB &
			02% AS PWJH.LINE$(1%),		! Line Count &
			02% AS PWJH.LINE$(2%)		! Line Count &
\	GOTO 17400 IF TO.ITEM$<PWJH.ORDNUM$ AND TO.ITEM$<>'' &
\	CUST.NAME$='' &
\	CUST.NAME$=MID(FNL$,7%,29%) IF FNG%(CUSTOM.CH%,PWJH.SOLDTO$)=0% &
\	PRINT #OUTPUT.CH%, &
\	PRINT #OUTPUT.CH% USING USE1$, 	&
		PWJH.ORDNUM$, &
		PWJH.INVNUM$, &
		FND6$(CVT$%(PWJH.INVDAT$)), &
		FND6$(CVT$%(PWJH.SHPDAT$)), &
	        PWJH.SOLDTO$, &
		CUST.NAME$, &
		PWJH.CUSPO$, &
		PWJH.SOLDBY$, &
		PWJH.CARNAM$, &
		PWJH.TERMS$, &
		PWJH.FOBFLG$ &
\	PRINT #OUTPUT.CH%, &
\	LINE.COUNT%	= LINE.COUNT% + 3% &
\	GOSUB 17600 &
		
17040	GOTO 17350 IF FNG%(PWJL.CH%,PWJH.ORDNUM$) &

17045	FIELD #PWJL.CH%+1%,FNL% AS JUNK$,	&
			06% AS PWJL.ORDNUM$,		! Order # &
			01% AS PWJL.WINDOW$,		! Window Flag &
			02% AS PWJL.LINE$,		! Line # &
			15% AS PWJL.PRONUM$,		! Pack &
			03% AS PWJL.STONUM$,		! Store # &
			06% AS PWJL.LOTNUM$,		! LOT # &
			26% AS PWJL.DESC$,		! Description &
			08% AS PWJL.ACCNUM$,		! Account # &
			08% AS PWJL.QTY$,		! Quanity &
			08% AS PWJL.PRICE$,		! Price &
			08% AS PWJL.POUNDS$,		! Weight &
			08% AS PWJL.EXT$,		! Extension &
			01% AS PWJL.PRTYPE$		! Price Flag &
\	GOTO 17350 IF PWJL.ORDNUM$<>PWJH.ORDNUM$ &

17070	IF PWJL.WINDOW$='1' &
	THEN	GOSUB 18000 &
	ELSE	GOSUB 18100 &

17300	WINDOW$=PWJL.WINDOW$+'' &
\	ORDNUM$=PWJL.ORDNUM$+'' &
\	GOTO 17045 IF FNN%(PWJL.CH%)=0% &

17350	GOTO 17400 IF END.FLAG% &	
\	GOTO 17030 IF FNN%(PWJH.CH%)=0% &

17400   !&

17500	IF 	DISPLAY$<>'Y' &
	THEN	PRINT #OUTPUT.CH%, FNPAGE$(LIN.PER.PAGE%,LINE.COUNT%,FNPAGE%, &
			'',PAGE.BOT$); &
	ELSE	V$	= FNMESS$(OUTPUT.CH%,0%,'End of report',0%,-1%) &

17540	!KILL 'TEMPORARY.FILES' &

17550	PRINT #OUTPUT.CH%, EXIT.COPY$; IF DISPLAY$<>'Y' &
\	PRINT #OUTPUT.CH%, FNSR$('1;24');COLM.OFF$; IF DISPLAY$='Y' &
\	CLOSE OUTPUT.CH% &
\	V%	= FNX%(CHAIN.PROGRAM$,CHAIN.LINE%,'') &

17600	!---------------------------------------PAGINATION SUBROUTINE &
	IF 	DISPLAY$<>'Y' AND LINE.COUNT%+MORE.LINES%>PAGE.BREAK% &
	THEN	END.FLAG% = -1% IF PAGE%>=EPAGE% AND EPAGE%<>0% &
\		RETURN IF END.FLAG% &
\		PRINT #OUTPUT.CH%, FNPAGE$(LIN.PER.PAGE%,LINE.COUNT%, &
			FNPAGE%,PAGE.TOP$+PAGE.TOP1$+PAG.A$,PAGE.BOT$); &
\		LINE.COUNT%	= FNLINE% &
\		IF 	SPAGE%>1% &
		THEN	IF 	SPAGE%<=PAGE% &
			THEN	OUTPUT.CH% = RESET.CH% &
\				PRINT #OUTPUT.CH%, ENTER.COPY$; &
					FNPAGE$(LIN.PER.PAGE%,0%, &
					PAGE%,PAGE.TOP$+PAGE.TOP1$+PAG.A$,''); &
\				LINE.COUNT% = FNLINE% &
\				SPAGE% = 0% &

17650	IF 	DISPLAY$='Y' AND LINE.COUNT%>DISPLAY.CONTROL% &
	THEN	V$	= FNMESS$(OUTPUT.CH%,0%,'',-1%,-1%) &
\		LINE.COUNT%	= 0% &
\		PRINT #OUTPUT.CH%, FNP$('24;1');CLRLIN$;FNP$('18;132') &

17670	RETURN &

18000	IF PWJL.ORDNUM$+PWJL.WINDOW$<>ORDNUM$+WINDOW$ &
	THEN	PRINT #OUTPUT.CH%,'     *PAYABLES* TYPE  VENDOR  NAME     '+ &
		'                  ITM       AMOUNT' &
\		LINE.COUNT%	= LINE.COUNT% + 1% &

18010	VENNAM$='' &
\	VENNAM$=MID(FNL$,7%,29%) IF FNG%(VENDES.CH%,PWJL.LOTNUM$)=0% &
\	PRINT #OUTPUT.CH% USING SPACE$(16%)+'\ \   \    \  \'+SPACE$(20%)+ &	
					'\     ###',&
		PWJL.STONUM$,   &
		PWJL.LOTNUM$,   &
		VENNAM$, &
		CVT$F(PWJL.POUNDS$), &
\	IF CVT$F(PWJL.EXT$)=0. &
	THEN	PRINT #OUTPUT.CH%, &
	ELSE	PRINT #OUTPUT.CH% USING '   ###,###.##' ,	&
		CVT$F(PWJL.EXT$) &

18020	LINE.COUNT%	= LINE.COUNT% + 1% &
\	GOSUB 17600 &
\	RETURN &

18100	RETURN IF CVT$F(PWJL.QTY$)=0. &

18110	IF PWJL.ORDNUM$+PWJL.WINDOW$<>ORDNUM$+WINDOW$ &
	THEN	PRINT #OUTPUT.CH%,'     *SALES*    LIN  PRONUM           DESCRIPTION'+SPACE$(20%)+&
		'STO  LOTNUM  ACCOUNT  QUANTITY      PRICE PT   POUND     AMOUNT' &
\		LINE.COUNT%	= LINE.COUNT% + 1% &

18120	PRINT #OUTPUT.CH% USING USE2$,	&
		CVT$%(PWJL.LINE$),&
		PWJL.PRONUM$,	&
		PWJL.DESC$,	&
		PWJL.STONUM$,   &
		PWJL.LOTNUM$,   &
		PWJL.ACCNUM$,   &
		CVT$F(PWJL.QTY$), &
		CVT$F(PWJL.PRICE$), &
		PWJL.PRTYPE$,   &
		CVT$F(PWJL.POUNDS$), &
		CVT$F(PWJL.EXT$) &
\	LINE.COUNT%	= LINE.COUNT% + 1% &
\	TOTAL.EXT=TOTAL.EXT+CVT$F(PWJL.EXT$) &
\	GOSUB 17600 &
\	RETURN &

18900	! SEND MESSAGE IF DETACHED KEYBOARD &

19000	!---------------------------------------ERROR TRAPPING 		&
	RESUME 17500	IF ERR=11% AND ERL=17030%			&
\	RESUME 510	IF ERL=400% 					&
\	RESUME 17540	IF ERR=54% 					&
\	RESUME 17550	IF ERL=17540 					&
\	RESUME 190	IF ERL=130% OR ERL=160% OR ERL=180%		&
\	IF 	ERR=28%	THEN	JUNK$ = SYS(CHR$(6%) + CHR$(-7%)) 	&
\				RESUME 17500	IF OUTPUT.CH%=CH% 	&

19010	IF 	ERR=27%	THEN	RESUME 70	IF ERL=60% 		&
\				RESUME 18900 				&

19020	IF 	ERL=120%						&
	THEN	V$=FNMESS$(CH%,0%,'Missing print control file.  '+	&
			'Aborting',0%,-1%)				&
\		RESUME 17500						&

19900	ON ERROR GOTO 0 						&

30000	!---------------------------------------TERMINAL INPUT 		&
	DEF FNINP$(CHN%,KYP%,FILLCHAR$,INPUTLEN%,TO.ERR%)		&
\		PRINT #CHN%, STRING$(INPUTLEN%,ASCII(FILLCHAR$)); 	&
			  STRING$(INPUTLEN%,8%); 			&
\		PRINT #CHN%, RECORD 256%,CHR$(KYP%+INPUTLEN%)+FILLCHAR$;&
\		GET #CHN% 						&
\		FIELD #CHN%, RECOUNT AS BUFFER$ 			&
\		BUFFER$	= '%^C' IF INSTR(1%,BUFFER$,CHR$(3%)) 		&
\		FNINP$	= CVT$$(BUFFER$,4%) 				&
\		V=SQR(-1) IF BUFFER$='%^C' AND TO.ERR% !^C Trappping	&
\	FNEND 								&
						
30200	DEF*FNP$(ROWCOL$)=ESC$+'['+ROWCOL$+'H'	!DIRECT CURSOR ADDRESS	&

30250	!---------------------------------------MESSAGE HANDLER		&
	DEF*FNMESS$(CHN%,ERRNUM%,DESC$,TO.ERR%,PRINT.TEST%)		&
\		MESG$	= MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERRNUM%)),	&
			3%,30%)+DESC$					&
\		MESG$	= DESC$+'' IF ERRNUM%<1%			&
\		IF 	PRINT.TEST%					&
		THEN	PRINT #CHN%, FNP$('24;1');CLRLIN$;MESG$;	&
				FNP$('24;105');'Hit any key to continue';&
\			NW$=FNINP$(CHN%,128%,' ',1%,TO.ERR%)		&

30260	FNMESS$=MESG$							&
\	FNEND 								&
						
30280	DEF*FNSR$(BEGEND$)=ESC$+'['+BEGEND$+'r'	!SCROLL CONTROL		&

30420	DEF FND6$(D9%)=RIGHT(NUM1$((D9% AND 15%*32%)/32%+100%),2%)	&
		+ '/'+RIGHT(NUM1$((D9% AND 31%)+100%),2%)		&
		+ '/'+RIGHT(NUM1$(((SWAP%(D9%) AND 254%)/2%)+100%),2%)	&

30500	DEF FNZ(Y,N%)=SGN(Y)*10.^(-N%)*INT(ABS(Y)*10.^N%+.5001)  	&

30600	DEF FNPAGE$(Y0%,Y1%,Y2%,Y0$,Y1$) 	!PAGING FUNCTION	&
\		Y2$	= '' 						&
\		Y2$	= STRING$(Y0%-(Y1%+LEN(XLATE(Y1$,STRING$(10%,0%)+   &
					CHR$(10%)))),10%) IF Y1$<>'' 	&
\		PAGE.LINE%	= LEN(XLATE(Y0$,STRING$(10%,0%)+CHR$(10%))) &
\		Y%	= INSTR(1%,Y1$+Y0$,'<<#') 			&
\		Y3%	= INSTR(1%,Y1$+Y0$,'#>>') 			&
\		Y$	= RIGHT(NUM1$(100000+Y2%),8%-(Y3%-Y%)) 		&
\		Y3%	= -3% IF Y%=0% 					&
\		PRINT #OUTPUT.CH%, Y2$;LEFT(Y1$+Y0$,Y%-1%);Y$; 		&
			RIGHT(Y1$+Y0$,Y3%+3%); 				&
\		PAGE%	= Y2% 						&
\	FNEND 								&

30650	DEF FNPAGE%	= PAGE% + 1% 					&

30660	DEF FNLINE%	= PAGE.LINE% 					&

30800	!---------------------------------------COMPARING FUNCTION	&
	DEF FNCOMP%(Y$,Y2$) 						&
\	Y9%=0% 								&
\	Y9%=-1% IF Y2$='*' 						&
\	Y2$=Y2$+',' 							&

30820	IF 	Y9%=0% 							&
	THEN	Y1$=LEFT(Y2$,INSTR(1%,Y2$,',')-1%) 			&
\		Y2$=RIGHT(Y2$,LEN(Y1$)+2%) 				&
\		Y1%=INSTR(1%,Y1$,'/') 					&
\		Y2%=LEN(Y1$)-Y1%					&
\		IF 	Y1%+INSTR(1%,Y1$,'?')=0% 			&
		THEN	Y9%=Y$=Y1$ 					&
		ELSE 	IF 	Y1% 					&
			THEN	Y9%=LEFT(Y1$,Y1%-1%)<=LEFT(Y$,Y1%-1%)	&
				AND LEFT(Y$,Y2%)<=RIGHT(Y1$,Y1%+1%)	&
			ELSE	CHANGE CVT$$(LEFT(Y$,30%),-1%) TO Y%	&
\				CHANGE CVT$$(LEFT(Y1$,30%),-1%) TO Y1%	&
\				GOTO 30830 IF (Y%(Y3%)<>Y1%(Y3%))-	&
					(Y1%(Y3%)=63%) 			&
						FOR Y3%=1% TO Y1%(0%)	&
\				Y9%=-1% 				&

30830	GOTO 30820 IF Y2$<>'' AND Y9%=0% 				&
\	FNCOMP%=Y9% 							&
\	FNEND 								&
						
30900	CHAIN.FLAG%=-1% 			!CHAIN ENTER LINE	&

31000	GOTO 30					!REPORT ENTER LINE	&

32767	END &

