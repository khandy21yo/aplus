10	!---------------------------------------------------------------&
	!	SYSTEM NAME		-Sub System			&
	!	Program Description Name				&
	!								&
	!	PWJSRP.B2S	V1.0	February 1987			&
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

100	DIM AMOUNT(50%,5%),TYPE.NUM$(50%,5%),BROKER.AMT(50,5%),STORE$(20%), &
		STORE.ACCOUNT$(20%),STORE.AMT(20%),STORE.POUND(20%) &
\	DIM SJ.ACCOUNT$(50%),DEBIT(50%),CREDIT(50%),STORE.OP$(50%) &
\	DEVICE.CH%,PRNT.TMP%	= 02% &
\	PWJH.CH%		= 02% &
\	PWJL.CH%		= 04% &
\	CUSTOM.CH%		= 06% &
\	CHART.CH%		= 06% &
\	VENDES.CH%		= 08% &
\	APCCTL.CH%		= 10% &
\	PWJACC.CH%		= 11% &
\	MENU.CH%		= 12% &
\	PRINT.WIDTH%		= 132% &
\	WIDTH%			= PRINT.WIDTH%-32% &
\	COMMAND$(1%)		= 'OUTSIDE PURCHASE' &
\	COMMAND$(2%)		= 'BROKERAGE' &
\	COMMAND$(3%)		= 'BROKERAGE CREDIT' &
\	COMMAND$(4%)		= 'FREIGHT ' &
\	COMMAND$(5%)		= 'FREIGHT CREDIT' &

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
\		APCCTL.DEVICE$=MID(FNL$,7%,20%)+'' &
			IF FNG%(DEVICE.CH%,'APCCTL')=0% &
\		PWJACC.DEVICE$=MID(FNL$,7%,20%)+'' &
			IF FNG%(DEVICE.CH%,'PWJACC')=0% &
\		V%	= FNC%(DEVICE.CH%) &

300	AP.ACCT$,AR.ACCT$='XXXXXXXX' &
\	IF FNO%(APCCTL.CH%,APCCTL.DEVICE$+'APCCTL.DAS',"/RO/SF/NS","") &
	THEN	V$=FNMESS$(CH%,FNS%,'APCCTL.DAS',0%,-1%) &
\		GOTO 17540 &

310	IF FNG%(APCCTL.CH%,'ARL')=0% &
	THEN	TEMP%=FNL% + 64% &
\		FIELD #APCCTL.CH%, TEMP%+30%*(LOOP%-1%) AS TEMP$, &
				8% AS APCCTL.ACC$(LOOP%) &
					FOR LOOP%=1% TO 8% &
\		AR.ACCT$=APCCTL.ACC$(1%)+'' &

320	IF FNG%(APCCTL.CH%,'APL')=0% &
	THEN	TEMP%=FNL% + 64% &
\		FIELD #APCCTL.CH%, TEMP%+30%*(LOOP%-1%) AS TEMP$, &
			8% AS APCCTL.ACC$(LOOP%) &
				FOR LOOP%=1% TO 8% &
\		AP.ACCT$=APCCTL.ACC$(1%)+'' &

330	IF FNO%(PWJH.CH%,PWJH.DEVICE$+'PWJH'+BATCH.NUM$+'.DAT','/RO','') &
	THEN	V$=FNMESS$(CH%,FNS%,'PWJH'+BATCH.NUM$+'.DAT',0%,-1%) &
\		GOTO 17540 &

340	IF FNO%(PWJL.CH%,PWJL.DEVICE$+'PWJL'+BATCH.NUM$+'.DAT','/RO','') &
	THEN	V$=FNMESS$(CH%,FNS%,'PWJL'+BATCH.NUM$+'.DAT',0%,-1%) &
\		GOTO 17540 &

350	V%=FNO%(CUSTOM.CH%,CUSTOM.DEVICE$+'CUSTOM.DAT','/RO','')&
\	V%=FNO%(VENDES.CH%,VENDES.DEVICE$+'VENDES.DAT','/RO','')&
\	V%=FNO%(PWJACC.CH%,PWJACC.DEVICE$+'PWJACC.DAS','/RO/SF','')&

400	OPEN 'MENU.FIL/RO' FOR INPUT AS FILE MENU.CH% &
\	DIM #12%, A0$(1%)=64% &
\	COMPANY$	= CVT$$(RIGHT(A0$(1%),2%),128%) &
\	CLOSE MENU.CH% &
	
510	COLM.ON$	= '' IF PRINT.WIDTH% = 80%			&
\	TITLE$		= 'SALES REPORT - BATCH NO. '+BATCH.NUM$	&
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
\	PAGE.TOP1$	='ORDNUM    INVNUM INVDATE'+SPACE$(16%)+'NUMBER CUSTOMER/'+ &
		'VENDOR                 ACCOUNT       AMOUNT'+ &
		SPACE$(16%)+CHR$(10%)+CHR$(13%) &
\	PAG.A$		=STRING$(PRINT.WIDTH%,61%)+CHR$(10%)+CHR$(13%)	&
\	USE1$		='\    \  \      \ \      \  ' &
\	USE2$		=SPACE$(40%)+'\    \ \'+SPACE$(27%)+'\  \      \ '+ &
			'#,###,###.## ' &
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
\	CUST.NAME$=MID(FNL$,7%,25%) IF FNG%(CUSTOM.CH%,PWJH.SOLDTO$)=0% &
\	PRINT #OUTPUT.CH%, &
\	PRINT #OUTPUT.CH% USING USE1$, 	&
		PWJH.ORDNUM$, &
		PWJH.INVNUM$, &
		FND6$(CVT$%(PWJH.INVDAT$)); &
\	PRINT #OUTPUT.CH%,CHR$(13%); &
\	LINE.COUNT%=LINE.COUNT%+1% &
\	IF DISPLAY$='Y' &
	THEN	PRINT #OUTPUT.CH%, &
\		LINE.COUNT%=LINE.COUNT%+1% &

17035	STORE.LOOP%=0% &
\	TOTAL.STORE,TOTAL.POUND,BALANCE=0. &
\	AMOUNT(I%,J%),BROKER.AMT(I%,J%)=0.    &
		FOR J%=1% TO 5% FOR I%=0% TO CVT$%(PWJH.LINE$(2%)) &
\	TYPE.NUM$(I%,J%)='' FOR J%=1% TO 5% FOR I%=0% TO CVT$%(PWJH.LINE$(2%)) &

17040	GOTO 17075 IF FNG%(PWJL.CH%,PWJH.ORDNUM$) &

17045	FIELD #PWJL.CH%+1%,FNL% AS JUNK$,	&
			06% AS PWJL.ORDNUM$,		! Order # &
			01% AS PWJL.WINDOW$,		! Window Flag &
			02% AS PWJL.LINE$,		! Line # &
			15% AS PWJL.PRONUM$,		! Pack &
			03% AS PWJL.STONUM$,		! Store # &
			06% AS PWJL.LOTNUM$,		! LOT # &
			26% AS PWJL.DESC$,		! Description &
			08% AS PWJL.ACCNUM$,		! Account # &
			08% AS PWJL.QTY$,		! Quantity &
			08% AS PWJL.PRICE$,		! Price &
			08% AS PWJL.POUNDS$,		! Weight &
			08% AS PWJL.EXT$,		! Extension &
			01% AS PWJL.PRTYPE$		! Price Flag &
\	GOTO 17080 IF PWJL.ORDNUM$<>PWJH.ORDNUM$ &

17070	ON VAL(PWJL.WINDOW$) GOSUB 18000, 18100 &

17075	GOTO 17045 IF FNN%(PWJL.CH%)=0% &

17080	GOSUB 18200 &
\	GOSUB 18300 &
\	PRINT #OUTPUT.CH% USING USE2$, &
		'', &
		'B A L A N C E', &
		'', &
		BALANCE &
\	LINE.COUNT%=LINE.COUNT%+1% &
 
17100	GOTO 17400 IF END.FLAG% &	
\	IF FNN%(PWJH.CH%)=0% &
	THEN	GOSUB 17600 &
\		GOTO 17030 &

17400   GOSUB 18600 &

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

18000	TYP$   =CVT$$(PWJL.STONUM$,-1%) &
\	IND%    =CVT$F(PWJL.POUNDS$) &
\	LOOP%   =(INSTR(1%,' OP BR BC FR FC ',TYP$)+1%)/3% &
\	AMOUNT(IND%,LOOP%)   =AMOUNT(IND%,LOOP%)+FNZ(CVT$F(PWJL.EXT$),2%) &
\	TYPE.NUM$(IND%,LOOP%)=TYP$+PWJL.LOTNUM$+'' &
\	RETURN &

18100	RETURN IF CVT$F(PWJL.QTY$)=0. &
\	STORE.OP$(CVT$%(PWJL.LINE$))=PWJL.STONUM$+'' &
\	FOR I%=1% TO STORE.LOOP% &
\		IF STORE$(I%)=PWJL.STONUM$ &
		THEN	GOTO 18110 &

18105	NEXT I% &
\	STORE.LOOP%,I%=STORE.LOOP%+1% &
\	STORE$(STORE.LOOP%)=PWJL.STONUM$+'' &
\	STORE.ACCOUNT$(I%)=PWJL.ACCNUM$+'' &
\	STORE.AMT(I%),STORE.POUND(I%)=0. &

18110	STORE.AMT(I%)=STORE.AMT(I%)+FNZ(CVT$F(PWJL.EXT$),2%) &
\	STORE.POUND(I%)=STORE.POUND(I%)+CVT$F(PWJL.POUNDS$) &
\	BROKER.AMT(I%,1%)=BROKER.AMT(I%,1%)+FNZ(AMOUNT(CVT$%(PWJL.LINE$),1%),2%) &
\	FOR J%=2% TO 5% &
\		AMOUNT(CVT$%(PWJL.LINE$),J%)=AMOUNT(CVT$%(PWJL.LINE$)-1%,J%) &
			IF CVT$%(PWJL.LINE$)>1% AND AMOUNT(CVT$%(PWJL.LINE$),J%)=0. &
\		BROKER.AMT(I%,J%)=BROKER.AMT(I%,J%)+ &
			FNZ(AMOUNT(CVT$%(PWJL.LINE$),J%)*CVT$F(PWJL.POUNDS$)*0.01,2%) &
\	NEXT J% & 
\	TOTAL.STORE=TOTAL.STORE+FNZ(CVT$F(PWJL.EXT$),2%) &
\	TOTAL.POUND=TOTAL.POUND+CVT$F(PWJL.POUNDS$) &
\	RETURN &

18200	FOR LOOP%=1% TO 5% &
\	 FOR IND%=0% TO (CVT$%(PWJH.LINE$(2%))-1%)*(1%-SGN(LOOP%-1%))+1% &
\		TOTAL.BROKER=0. &
\	        TOTAL.BROKER=TOTAL.BROKER+BROKER.AMT(J%,LOOP%) &
			FOR J%=1% TO STORE.LOOP% &
\		TOTAL.BROKER=AMOUNT(IND%,LOOP%) IF LOOP%=1% OR IND%=0% &
\		TYPE.NUM$(IND%,LOOP%)=TYPE.NUM$(IND%-1%,LOOP%) &
			IF IND%>0% AND TYPE.NUM$(IND%,LOOP%)='' &
\		GOTO 18206 IF TOTAL.BROKER=0. OR TYPE.NUM$(IND%,LOOP%)='' &
\		VEN.NAM$='' &
\		VEN.NAM$=MID(FNL$,7%,29%)  &
			IF FNG%(VENDES.CH%,RIGHT(TYPE.NUM$(IND%,LOOP%),3%))=0% &
\		ACCOUNT$=AP.ACCT$ &
\		ACCOUNT$=AR.ACCT$ IF LOOP%=5% &
\		TRAN.AMOUNT=-TOTAL.BROKER &
\		PRINT #OUTPUT.CH%  USING USE2$+' \                   \', &
			RIGHT(TYPE.NUM$(IND%,LOOP%),3%), &
			VEN.NAM$, &
			ACCOUNT$, &
			TRAN.AMOUNT, &
			COMMAND$(LOOP%) &
\		LINE.COUNT%	= LINE.COUNT% + 1% &
\		GOSUB 18500 &
\		GOSUB 17600 &

18205		ON LOOP% GOSUB 18210, 18220, 18220, 18220, 18220 &

18206	 NEXT IND% &
\	NEXT LOOP% &
\	RETURN &

18210	ACCOUNT$='XXXXXXXX' &
\	ACCOUNT$=MID(FNL$,6%,8%) &
		IF FNG%(PWJACC.CH%,STORE.OP$(IND%)+'OP')=0% &
\	TRAN.AMOUNT=AMOUNT(IND%,LOOP%) & 
\	PRINT #OUTPUT.CH% USING USE2$, &
		'', &
		'', &
		ACCOUNT$, &
		TRAN.AMOUNT &
\	LINE.COUNT%	= LINE.COUNT% + 1% &
\	GOSUB 18500 &
\	GOSUB 17600 &
\	RETURN &

18220	TOTAL.TRAN.AMOUNT=0. &
\	FOR I%=1% TO STORE.LOOP% &
\		ACCOUNT$='XXXXXXXX' &
\		ACCOUNT$=MID(FNL$,6%,8%) &
			IF FNG%(PWJACC.CH%,STORE$(I%)+ &
				LEFT(TYPE.NUM$(IND%,LOOP%),2%))=0% &
\		TRAN.AMOUNT=BROKER.AMT(I%,LOOP%) & 
\		TRAN.AMOUNT=FNZ(STORE.POUND(I%)/TOTAL.POUND*AMOUNT(IND%,LOOP%),2%) &
				IF TOTAL.POUND<>0. AND IND%=0% &
\		TRAN.AMOUNT=TOTAL.BROKER-TOTAL.TRAN.AMOUNT IF I%=STORE.LOOP% &
\		GOTO 18225 IF TRAN.AMOUNT=0. &
\		PRINT #OUTPUT.CH% USING USE2$, &
			'', &
			'', &
			ACCOUNT$, &
			TRAN.AMOUNT &
\		TOTAL.TRAN.AMOUNT=TOTAL.TRAN.AMOUNT+TRAN.AMOUNT &
\		LINE.COUNT%	= LINE.COUNT% + 1% &
\		GOSUB 18500 &
\		GOSUB 17600 &

18225	NEXT I% &		
\	RETURN IF LOOP%<>3% &

18230	ACCOUNT$=AP.ACCT$ &
\	TRAN.AMOUNT=TOTAL.BROKER &
\	PRINT #OUTPUT.CH%  USING USE2$, &
		'', &
		'', &
		ACCOUNT$, &
		TRAN.AMOUNT &
\	LINE.COUNT%	= LINE.COUNT% + 1% &
\	GOSUB 18500 &
\	GOSUB 17600 &

18240	ACCOUNT$=AR.ACCT$ &
\	TRAN.AMOUNT=-TOTAL.BROKER &
\	PRINT #OUTPUT.CH%  USING USE2$, &
		'', &
		'', &
		ACCOUNT$, &
		TRAN.AMOUNT &
\	LINE.COUNT%	= LINE.COUNT% + 1% &
\	GOSUB 18500 &
\	GOSUB 17600 &
\	RETURN &

18250	FOR I%=1% TO STORE.LOOP% &
\		ACCOUNT$='XXXXXXXX' &
\		ACCOUNT$=MID(FNL$,6%,8%) &
			IF FNG%(PWJACC.CH%,STORE$(I%)+ &
				LEFT(TYPE.NUM$(IND%,LOOP%),2%))=0% &
\		TRAN.AMOUNT=BROKER.AMT(I%,LOOP%) & 
\		TRAN.AMOUNT=FNZ(STORE.POUND(I%)/TOTAL.POUND*AMOUNT(IND%,LOOP%),2%) &
				IF TOTAL.POUND<>0. AND IND%=0% &
\		STOP &
\		GOTO 18255 IF TRAN.AMOUNT=0. &
\		PRINT #OUTPUT.CH% USING USE2$, &
			'', &
			'', &
			ACCOUNT$, &
			TRAN.AMOUNT &
\		LINE.COUNT%	= LINE.COUNT% + 1% &
\		GOSUB 18500 &
\		GOSUB 17600 &

18255	NEXT I% &
\	RETURN &

18300	!-------------------------------------PRINT SALES WINDOW &
	ACCOUNT$=AR.ACCT$ &
\	TRAN.AMOUNT=TOTAL.STORE &
\	PRINT #OUTPUT.CH% USING USE2$+' \                   \', &
			PWJH.SOLDTO$, &
			CUST.NAME$, &
			ACCOUNT$, &
			TRAN.AMOUNT, &
			'SALES' &
\	LINE.COUNT%	= LINE.COUNT% + 1% &
\	GOSUB 18500 &
\	FOR I%=1% TO STORE.LOOP% &
\		ACCOUNT$=STORE.ACCOUNT$(I%) & 
\		TRAN.AMOUNT=-STORE.AMT(I%) & 
\		PRINT #OUTPUT.CH% USING USE2$, &
			'', &
			'', &
			ACCOUNT$, &
			TRAN.AMOUNT &
\		LINE.COUNT%	= LINE.COUNT% + 1% &
\		GOSUB 18500 &
\		GOSUB 17600 &
\	NEXT I% &
\	RETURN &

18500	!--------------------------------------ACCUMULATE THE TRANSMITTAL &
	GOTO 18510 IF SJ.ACCOUNT$(LOOPP%)=ACCOUNT$ &
						FOR LOOPP%=1% TO SJ.LOOP% &
\	SJ.LOOP%,LOOPP%=SJ.LOOP%+1% &
\	DEBIT(LOOPP%),CREDIT(LOOPP%)=0. &
\	SJ.ACCOUNT$(LOOPP%)=ACCOUNT$ &

18510	IF 	TRAN.AMOUNT>=0. &
	THEN	DEBIT(LOOPP%) = DEBIT(LOOPP%) + TRAN.AMOUNT &
	ELSE	CREDIT(LOOPP%) = CREDIT(LOOPP%) - TRAN.AMOUNT &

18520	BALANCE=BALANCE+TRAN.AMOUNT &
\	RETURN &

18600	!--------------------------------------PRINT TRANSMITTAL TOTALS &

18610	V%=FNC%(CHART.CH%) &
\	V%=FNO%(CHART.CH%,CHART.DEVICE$+'CHART.DAT','/RO','') &
\	PAGE.TOP1$='     Account #'+SPACE$(32%)+'Debit         Credit'+ &
		'        Balance   S U M M A R Y'+CHR$(10%)+CHR$(13%) &
\	MORE.LINES%=100% &
\	GOSUB 17600 &
\	MORE.LINES%=2% &

18620	DEBIT.TOTAL,CREDIT.TOTAL,TOTAL=0. &
\	FOR I%=1% TO SJ.LOOP% &
\		FOR J%=1% TO SJ.LOOP%-1% &
\			IF 	SJ.ACCOUNT$(J%) > SJ.ACCOUNT$(J%+1%) &
			THEN	ACCOUNT$ = SJ.ACCOUNT$(J%) &
\				SJ.ACCOUNT$(J%) = SJ.ACCOUNT$(J%+1%) &
\				SJ.ACCOUNT$(J%+1%) = ACCOUNT$ &
\				DEBIT = DEBIT(J%) &
\				DEBIT(J%) = DEBIT(J%+1%) &
\				DEBIT(J%+1%) = DEBIT &
\				CREDIT = CREDIT(J%) &
\				CREDIT(J%) = CREDIT(J%+1%) &
\				CREDIT(J%+1%) = CREDIT &

18630		NEXT J% &
\	NEXT I% &

18650	FOR LOOP%=1% TO SJ.LOOP% &
\		TEMP$=SJ.ACCOUNT$(LOOP%) &
\		TEMP$=SPACE$(8%-LEN(TEMP$))+TEMP$ &
\		CHART.DESC$='' &
\		V%=FNG%(CHART.CH%,TEMP$) &
\		CHART.DESC$='Undefined' IF FNS% &
\		CHART.DESC$=MID(FNL$,9%,20%) IF FNS%=0% &
\		GOSUB 17600 &
\		PRINT #OUTPUT.CH% USING '     \      \  \                    \', &
			SJ.ACCOUNT$(LOOP%),CHART.DESC$; &
\		PRINT #OUTPUT.CH%, '               '; IF DEBIT(LOOP%)=0. &
\		PRINT #OUTPUT.CH% USING '###,###,###.## ', &
			DEBIT(LOOP%); IF DEBIT(LOOP%)<>0. &
\		PRINT #OUTPUT.CH%, '               '; IF CREDIT(LOOP%)=0. &
\		PRINT #OUTPUT.CH% USING '###,###,###.## ', &
			CREDIT(LOOP%); IF CREDIT(LOOP%)<>0. &
\		PRINT #OUTPUT.CH% USING '###,###,###.## ', &
			DEBIT(LOOP%)-CREDIT(LOOP%); &
\		PRINT #OUTPUT.CH% &
\		DEBIT.TOTAL  = DEBIT.TOTAL  + DEBIT(LOOP%) &
\		CREDIT.TOTAL = CREDIT.TOTAL + CREDIT(LOOP%) &
\		TOTAL=TOTAL+DEBIT(LOOP%)-CREDIT(LOOP%) &
\		LINE.COUNT%=LINE.COUNT%+1% &
\	NEXT LOOP% &
\	PRINT #OUTPUT.CH% &
\	PRINT #OUTPUT.CH% USING '\      \  '+SPACE$(27%)+ &
		'###,###,###.## ###,###,###.##   #,###,###.##',&
			'TOTAL', &
			DEBIT.TOTAL,&
			CREDIT.TOTAL , &
			TOTAL &
\	PRINT #OUTPUT.CH%,'POSTING ACCOUNT SUMMARY MAY BE DIFFERENT BECAUSE '+&
		'OF FREIGHT CREDIT' &
\	LINE.COUNT%=LINE.COUNT%+3% &
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

30900	CHAIN.FLAG%=-1% 			!CHAIN ENTER LINE	&

31000	GOTO 30					!REPORT ENTER LINE	&

32767	END &

