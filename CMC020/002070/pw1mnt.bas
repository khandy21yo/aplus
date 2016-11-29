10	!---------------------------------------------------------------&
	!	SALES JOURNAL MAINTENANCE 	- 			&
	! 								&
	!	PWJMNT.B2S	V1.0	January 1986 			&
	! 								&
	! Author - F.Starman,	 Computer Management Center, Inc. 	&
	! 								&
	! Files-CUSTOM.DAT	-ISAM	Customer Description File 	&
	! Files-CHART.DAT	-ISAM	Chart of accounts File 		&
	! Files-PWJH.DAT	-ISAM	Header file 			&
	! Files-PWJL.DAT	-ISAM 	Line item file 			&
	! 								&
	!---------------------------------------------------------------&
	! 								&
	! 								&
	!		      Copyright (c) 1984 by 			&
	!	  Computer Management Center, Idaho Falls, Idaho 	&
	! 								&
	! This software is furnished under a license and may be used 	&
	! and copied only in accordance with the terms of such license 	&
	! and with the inclusion of the above copyright notice.  This 	&
	! software or any other copies therof may not be provided or 	&
	! otherwise made available to any other person.  No title to 	&
	! and ownership of the software is hereby transferred. 		&
	! 								&
	! The information in this software is subject to change without &
	! notice and should not be construed as a committment by 	&
	! Computer Management Center. 					&
	! 								&
	! CMC assumes no responsibility for the use or reliability of 	&
	! its software on equipment which is not supported by CMC. 	&
	! 								&
	!---------------------------------------------------------------&

50	ON ERROR GOTO 19000 &
\	DIM DEFAULT$(35%),UNIT$(50%),WEIGHT(50%) &

60	OPEN 'KB:' AS FILE #1%, MODE 8%+256% &
					!    8% - Echo Control &
					!   16% - Disable hibernation, Cntl-C &
\	CH%=1%				! Keyboard channel &
\	ESC$=CHR$(155%)			! Escape code for VT100 control &
\	CLSCN$=ESC$+'[H'+ESC$+'[J'	! Clear screen &
\	CLRLIN$=ESC$+'[2K'		! Erase entire line &
\	INSERT.LIN$=ESC$+'[1L'		! Insert a line &
\	G.OFF$=ESC$+'[m'		! Select graphic off &
\	B.ON$=ESC$+'[1m'		! Bold face on &
\	BLINK.ON$=ESC$+'[5m'		! Blinking &
\	R.ON$=ESC$+'[7m'		! Reverse video &
\	COLM.ON$=ESC$+'[?3h'		! 132 Column mode &
\	COLM.OFF$=ESC$+'[?3l'		! 80 Column mode &
\	CLRBOT$=ESC$+'[21;1H'+ESC$+'[J'	! Erace cursor to end of screen &
\	LDS.ON$=ESC$+'(0'		! Line drawing set &
\	USASCII$=ESC$+'(B'		! United States ASCII &

80	!COM(THIS) DROP.DEAD.DATE$ = 8, VERSION.NO$ = 6, SERIAL.NO$ = 10 &
	DROP.DEAD.DATE$='        ' &
\	VERSION.NO$='V1.0' &
\	IF DROP.DEAD.DATE$<>'' &
	THEN	IF DROP.DEAD.DATE$<MID(SYS(CHR$(6%)+CHR$(20%)+CHR$(0%)+CHR$(0%)+ &
				CHR$(1%)+CHR$(SWAP%(1%))),7%,8%) &
		THEN	MESG$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(47%)), &
				3%,30%) &
\			PRINT #CH%, MESG$; &
\			V$=SYS(CHR$(9%)) &

100	DEVICE.CH%      = 02% &
\	PWJH.CH%	= 02% &
\	PWJL.CH%	= 04% &
\	CUSTOM.CH%	= 06% &
\	INVDES.CH%	= 08% &
\	PCKAGE.CH%	= 08% &
\	CHART.CH%	= 10% &
\	MENU.CH%   	= 12% &
\	LOOP.DATA%	= 24% &
\	CUST1%		= 3%  &
\	CUST2%		= 11% &
\	TP$='X' &

200	IF FNO%(DEVICE.CH%,'DEVICE.DAT','/RO','')=0% &
	THEN	PWJH.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) &
			IF FNG%(DEVICE.CH%,'PWJH')=0% &
\		PWJL.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) &
			IF FNG%(DEVICE.CH%,'PWJL')=0% &
\		CUSTOM.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) &
			IF FNG%(DEVICE.CH%,'CUSTOM')=0% &
\		INVDES.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) &
			IF FNG%(DEVICE.CH%,'INVDES')=0% &
\		PCKAGE.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) &
			IF FNG%(DEVICE.CH%,'PCKAGE')=0% &
\		CHART.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) &
			IF FNG%(DEVICE.CH%,'CHART')=0% &
\		V%=FNC%(DEVICE.CH%) &

300	PRINT #CH%, COLM.OFF$; &
		FNP$('4;28');'WAREHOUSE INVOICE JOURNAL '; &
		FNP$('6;28');'Batch Number <01> '; &
\	JUNK$ = FNINP$(CH%, 128%, '_', 2%,0%) &
\	GOTO 1045 IF JUNK$='%^C' &
\	JUNK$ = "01" IF JUNK$ = "" &
\	IF LEN(JUNK$) = 1% &
	THEN	JUNK$ = '0' + JUNK$ &

305	PRINT #CH%, CLSCN$+FNP$('1;75');B.ON$;BLINK.ON$;R.ON$;'WAIT'; &
			G.OFF$;FNP$('24;1'); &
\	BATCH.NUM$ = JUNK$ &

310	V%=FNO%(PWJH.CH%,PWJH.DEVICE$+'PWJH'+BATCH.NUM$+'.DAT','/RW','') &
\	V%=FNO%(PWJH.CH%,PWJH.DEVICE$+'PWJH'+BATCH.NUM$+'.DAT','/CR:8,256','') &
								IF FNS% = 5% &
\	IF FNS% &
	THEN	V$=FNMESS$(CH%,FNS%,'PWJH'+BATCH.NUM$+'.DAT',0%,-1%) &
\		GOTO 1045 &

320	V%=FNO%(PWJL.CH%,PWJL.DEVICE$+'PWJL'+BATCH.NUM$+'.DAT','/RW','') &
\	V%=FNO%(PWJL.CH%,PWJL.DEVICE$+'PWJL'+BATCH.NUM$+'.DAT','/CR:10,128','') &
								IF FNS% = 5% &
\	IF FNS% &
	THEN	V$=FNMESS$(CH%,FNS%,'PWJL'+BATCH.NUM$+'.DAT',0%,-1%) &
\		GOTO 1045 &

330	V%=FNO%(CUSTOM.CH%,CUSTOM.DEVICE$+'CUSTOM.DAT','/RW','') &
\	V%=FNO%(CUSTOM.CH%,CUSTOM.DEVICE$+'CUSTOM.DAT','/CR:8,256','') &
								IF FNS% = 5% &
\	IF FNS% &
	THEN	V$=FNMESS$(CH%,FNS%,'CUSTOM.DAT',0%,-1%) &
\		GOTO 1045 &

340	GOTO 390 IF FNO%(PCKAGE.CH%,PCKAGE.DEVICE$+'PCKAGE.DAT','/RO','') &
\	V%=FNG%(PCKAGE.CH%,'') &
\       PACK.INDEX%=0% &
\	WHILE FNS%=0% &
\		PACK.INDEX%=PACK.INDEX%+1% &
\		UNIT$(PACK.INDEX%)=LEFT(FNL$,4%) &
\		WEIGHT(PACK.INDEX%)=CVT$F(MID(FNL$,37%,8%)) &
\		V%=FNN%(PCKAGE.CH%) &
\	NEXT &
\	V%=FNC%(PCKAGE.CH%) &

390	INVDES.FLAG%=FNO%(INVDES.CH%,INVDES.DEVICE$+'INVDES.DAT','/RO','') &
\	CHART.FLAG%=FNO%(CHART.CH%,CHART.DEVICE$+'CHART.DAT','/RO','') &

400	OPEN 'MENU.FIL/RO' FOR INPUT AS FILE MENU.CH% &
\	DIM #12%, A0$(1%)=64% &
\	COMPANY$=CVT$$(RIGHT(A0$(1%),2%),128%) &
\	CLOSE MENU.CH% &

450	OPEN 'NL:' AS FILE 12%, RECORDSIZE 256%+256%+128% &
\	FIELD #12%,	06% AS CUSTOM.NUM$,		! Number &
			25% AS CUSTOM.NAME$,		! Name &
			25% AS CUSTOM.ADD1$,		! Address 1 &
			21% AS CUSTOM.ADD2$,		! Address 2 &
			15% AS CUSTOM.CITY$,		! City &
			02% AS CUSTOM.STATE$,		! State (Post office code) &
			05% AS CUSTOM.COUNTRY$,		! Country &
			10% AS CUSTOM.ZIP$,		! Zip code &
			10% AS CUSTOM.PHONE$,		! Phone Number (XXX-XXX-XXXX) &
			02% AS CUSTOM.SLSMAN$,		! Salesman &
			02% AS CUSTOM.TERR$,		! Territory &
			02% AS CUSTOM.LOC$,		! Inventory location &
			02% AS CUSTOM.TYPE$,		! Customer type &
			02% AS CUSTOM.CODE$,		! Customer code 2 &
			01% AS CUSTOM.TAXFLG$,		! Tax Flag &
			10% AS CUSTOM.STAXNUM$,		! Customer Sales tax ex number &
			02% AS CUSTOM.STAXLOC$,		! Sales tax location id &
			01% AS CUSTOM.METHOD$,		! Balance method &
							!  "O" = open item &
							!  "B" = balance foreward &
			01% AS CUSTOM.STMFLG$,		! Statement Flag &
							!  "1" = Gets a statement &
							!  "2" = Doesn't get statement &
			15% AS CUSTOM.ALPSRT$		! Alpha sort field &
\	FIELD #12%,	256% AS TEMP$, &
			06% AS PWJH.ORDER$,		! Order # &
			06% AS PWJH.SOLDTO$,		! Sold to &
			06% AS PWJH.SHIPTO$,		! Ship to &
			08% AS PWJH.INVNUM$,		! Invoice # &
			02% AS PWJH.INVDAT$,		! Invoice Date &
			02% AS PWJH.SHPDAT$,		! Ship Date &
			08% AS PWJH.SHPLOC$,		! Ship Location &
			06% AS PWJH.CARNUM$,		! Carrier # &
			06% AS PWJH.DEPOSIT$,		! DEPOSIT # &
			06% AS PWJH.BRONUM$,		! Broker # &
			06% AS PWJH.LABNUM$,		! Label # &
			08% AS PWJH.SOLDBY$,		! Sold by &
			08% AS PWJH.CUSPO$,		! Customer PO &
			08% AS PWJH.TERMS$,		! Terms &
			08% AS PWJH.BROACC$,		! Broker Account # &
			02% AS PWJH.LINE$,		! Line Count &
			06% AS PWJH.CARVOU$,		! Carrier voucher # &
			06% AS PWJH.BROVOU$,		! Broker voucher # &
			06% AS PWJH.LABVOU$,		! Label voucher # &
			08% AS PWJH.FRIGHT$,		! Frieght &
			08% AS PWJH.BROAMT$,		! Broker Amount &
			08% AS PWJH.CARACC$,		! Carrier Acct # &
			01% AS PWJH.FOBFLG$,		! FOB &
			01% AS PWJH.UNTFLG$		! Unit/Dollars &
\	FIELD #12%,	256% + 256% AS TEMP$, &
			06% AS PWJL.ORDER$,		! Order # &
			02% AS PWJL.LINE$,		! Line # &
			15% AS PWJL.PACK$,		! Pack &
			26% AS PWJL.DESC$,		! Description &
			01% AS PWJL.PRTFLG$,		! Print Flag (Y/N) &
			01% AS PWJL.TYPE$,		! Type &
			08% AS PWJL.ACCNUM$,		! Account # &
			08% AS PWJL.QTY$,		! Quanity &
			08% AS PWJL.PRICE$,		! Price &
			08% AS PWJL.EXT$,		! Extension &
			08% AS PWJL.BROKER$,		! Broker &
			08% AS PWJL.POUNDS$		! Weight &
\	FIELD #12%, 256% AS CUSTOM.BUF$, 256% AS PWJH.BUF$,128% AS PWJL.BUF$ &

500	FRAME$ = FRAME$ + FNP$(NUM1$(I%)+';1')+' '+FNP$(NUM1$(I%)+';80')+' ' &
							FOR I%=2% TO 19% &
\	COMMAND$  =' ' &
\	COMMAND$  = COMMAND$+RIGHT(NUM1$(I%+100%),2%)+' ' &
		FOR I%=1% TO LOOP.DATA% &

1000	!--------------------------------------------PROGRAM RESTART POINT &
	V%=FNG%(PWJH.CH%,'') &
\	KEY.POINTER%=FNR(PWJH.CH%) &
\	DATA.POINTER%=FNR(PWJH.CH%+1%) &
\	IF FNS%=0% &
	THEN	GOTO 1010 &
		ELSE	GOSUB 1060 &
\			GOTO 1030 &

1005	IF FNN%(PWJH.CH%) &
	THEN	GOTO 18910 &
		ELSE	KEY.POINTER%=FNR(PWJH.CH%) &
\			DATA.POINTER%=FNR(PWJH.CH%+1%) &
\			GOTO 1015 &

1010	GOSUB 1060 &

1015	GOSUB 6000 &

1030	CHNG.FLAG%, ADD.LINE%=0% &
\	PRINT #CH%, CLRBOT$;G.OFF$; &
		'COMMAND: Add Erase Change Blank Default '; &
		'Find Next Restore Quit '; &
\	OPT$=CVT$$(FNINP$(CH%,128%,' ',1%,1%),32%) &
\	OPT$='A' IF OPT$='' &
\	GOTO 1030 IF KEY.POINTER%=0% AND INSTR(1%,'AQD',OPT$)=0% &

1040	GOTO 1050 IF OPT$<>'Q' &

1045	PRINT #CH%, CLSCN$;FNSR$('1;24');FNP$('1;75');B.ON$;BLINK.ON$; &
		R.ON$;'WAIT';G.OFF$;FNP$('24;1');FNX%('',0%,''); &

1050	GOTO 1000 	IF OPT$='R' 		! Restore &
\	GOTO 1005 	IF OPT$='N' 		! Next &
\	GOTO 2000 	IF OPT$='A' OR OPT$='F' ! Add, Find &
\	GOTO 2200 	IF OPT$='C' OR OPT$='B' ! Change, Blank &
\	GOTO 2400 	IF OPT$='E' 		! Erase &
\	GOTO 2500 	IF OPT$='D' 		! Default &
\	GOTO 1030 &

1060	TEMP$     = 'Warehouse Invoice Journal' &
\	PRINT #CH%, COLM.OFF$;G.OFF$;R.ON$;LEFT(TEMP$,39%);SPACE$(40%-LEN(TEMP$)); &
		SPACE$(40%-LEN(COMPANY$));COMPANY$;FRAME$;FNP$('14;2'); &
		'       A        B          C         D         E        '; &
		' F        G       H    ';FNP$('15;2');LDS.ON$; &
		'ITMxPROD #xDESCRIPTION xACCT #  x     QTYx    PRICEx    '; &
		'  EXTx BROKER%x POUNDS';USASCII$; &
		FNP$('20;1');' Ending Item'+SPACE$(18%)+'Broker Comm'+ &
		SPACE$(11%)+'Frg'+SPACE$(11%)+'Bal'+SPACE$(11%);G.OFF$; &
\	PRINT #CH%,FNP$('02;02');'(01)*Order #';&
			FNP$('03;02');'(02) Invoice #'; &
			FNP$('04;02');'(03) Sold to:';	&
		 	FNP$('05;02');'(04) Name';	&
	 		FNP$('06;02');'(05) Add1'; 	&
			FNP$('07;02');'(06) Add2';	&
	 		FNP$('08;02');'(07) City'; 	&
			FNP$('09;02');'(09) St';	&
			FNP$('09;16');'(09) Zip'; 	&
			FNP$('10;02');'(10) Country';	&
			FNP$('11;02');'(11) Inv Date';	&
			FNP$('12;02');'(12) Shp Date';	&
			FNP$('13;02');'(13) Shp Loc';	&
\	PRINT #CH%,	FNP$('02;41');'(14) Broker';	&
			FNP$('03;41');'(15) Amount';&
			FNP$('04;41');'(16) Acct #';&
			FNP$('05;41');'(17) Carrier';	&
			FNP$('06;41');'(18) FOB';&
			FNP$('07;41');'(19) Unit/$'; &
			FNP$('08;41');'(20) Amt/100Wgt';&
			FNP$('09;41');'(21) Acct #';&
			FNP$('10;41');'(22) Sold by';	&
			FNP$('11;41');'(23) Cust PO';	&
			FNP$('12;41');'(24) Terms'; 	&
			FNSR$('16;19'); &
\	RETURN &

2000	!-------------------------------------------------SEARCH FOR KEYS &
	OPTION$='ADD ' &
\	OPTION$='FIND ' IF OPT$='F' &
\	INP$='' &
\	COMMIS,SUM.POUNDS,FRIG,TOT,PWJL.COUNT%,CUR.LINE%=0. &
\	PRINT #CH%,	G.OFF$;R.ON$; &
\	PRINT #CH%,	FNP$(NUM1$(LOOP%)+';1');CLRLIN$;' '; &
			FNP$(NUM1$(LOOP%)+';80');' '; &
				FOR LOOP%=16% TO 19% &
\	PRINT #CH% USING FNP$('20;14')+R.ON$+'###'+G.OFF$,LOOP.DATA%; &
\	PRINT #CH% USING FNP$('20;43')+R.ON$+'######.##'+FNP$('20;57')+ &
		'######.##'+FNP$('20;71')+'######.##'+G.OFF$+FNP$('21;1'), &
		COMMIS,FRIG,TOT; &
\	FOR LOOP%=1% TO LOOP.DATA% &
\		INP$=DEFAULT$(LOOP%) IF OPT$='A' &
\		GOSUB 6030 &
\	NEXT LOOP% &
\	GOSUB 6210 &
\	IF OPT$='F' OR FNG%(PWJH.CH%,PWJH.ORDER$)=0% &
	THEN	KEY.POINTER%=FNR(PWJH.CH%) &
\		DATA.POINTER%=FNR(PWJH.CH%+1%) &
\		GOTO 1015 &

2100	!-------------------------------------------ADD IN HEADER FILE  &
	GOSUB 6750 &
\	GOSUB 6010 FOR LOOP%=CUST1% TO CUST1% &
\	GOSUB 6010 FOR LOOP%=CUST1%+1%+7%*(1%-SGN(CUST.N%(0%))) TO CUST1%+7% &
\	GOSUB 2185 IF CUST.N%(0%)<>0%  &
\	CUSTOM.KEY%(0%)=FNR(CUSTOM.CH%) &
\	CUSTOM.DATA%(0%)=FNR(CUSTOM.CH%+1%) &
\	GOSUB 6010 FOR LOOP%=CUST1%+8% TO LOOP.DATA% &

2120	!----------------------------------------------ADD IN LINE FILE &
	LINE.NO%=16% &
\	TOP.LINE%=1% &
\	TEMP.COMMAND$=' ' &

2130	LINE.NO$=NUM1$(LINE.NO%) &
\	PRINT #CH%, FNPRT$(LINE.NO$); &
\	PRINT #CH% USING FNP$(LINE.NO$+';2')+'###',PWJL.COUNT%+LOOP.DATA%+1%; &
\	FOR LOOP%=LOOP.DATA%+1% TO LOOP.DATA%+8% &
\		GOSUB 6010 &
\		GOTO 2150 IF INP$='%END' &
\	NEXT LOOP% &
\	PWJL.COUNT%,CUR.LINE%=PWJL.COUNT%+1% &
\	LSET PWJL.ORDER$=PWJH.ORDER$+'' &
\	LSET PWJL.LINE$=CVT%$(PWJL.COUNT%) &
\	LSET PWJL.PRTFLG$='' &
\	LSET PWJL.TYPE$='' &
\	IF FNA%(PWJL.CH%,PWJL.BUF$) &
	THEN	V$=FNMESS$(CH%,FNS%,'PWJL.DAT',0%,-1%) &
\		GOTO 1000 &

2135	TEMP.COMMAND$=TEMP.COMMAND$+NUM1$(PWJL.COUNT%+LOOP.DATA%)+' ' &
\	GOSUB 6920 &

2140	PRINT #CH%, FNP$(NUM1$(LINE.NO%)+';1') &
\	TOP.LINE%=TOP.LINE%+1%-SGN(19%-LINE.NO%) &
\	LINE.NO%=LINE.NO%+SGN(19%-LINE.NO%) &
\	GOTO 2130 &

2150	PRINT #CH%, FNP$(LINE.NO$+';2')+SPACE$(78%); &
\	LSET PWJH.LINE$=CVT%$(PWJL.COUNT%) &
\	GOTO 2195 IF ADD.LINE%<>0% &
\	LSET PWJH.LABVOU$='' &
\	IF FNA%(PWJH.CH%,PWJH.BUF$) &
	THEN	V$=FNMESS$(CH%,FNS%,'PWJH.DAT',0%,-1%) &
\		GOTO 1000 &

2160	KEY.POINTER%=FNR(PWJH.CH%) &
\	DATA.POINTER%=FNR(PWJH.CH%+1%) &
\	GOTO 1030 &

2185	!------------------------------------CUSTOMER RECORD (SOLD TO) &
	LSET CUSTOM.NUM$=PWJH.SOLDTO$+'' &
\	IF FNA%(CUSTOM.CH%,CUSTOM.BUF$)=0% &
	THEN	RETURN &
	ELSE	V$=FNMESS$(CH%,FNS%,'CUSTOM.DAT',0%,-1%) &
\		RETURN &

2190	!------------------------------------CUSTOMER RECORD (SHIP TO) &
	LSET CUSTOM.NUM$=PWJH.SHIPTO$+'' &
\	IF FNA%(CUSTOM.CH%,CUSTOM.BUF$)=0% &
	THEN	RETURN &
	ELSE	V$=FNMESS$(CH%,FNS%,'CUSTOM.DAT',0%,-1%) &
\		RETURN &

2195	LOOP%=LOOP.DATA% &
\	GOSUB 2900 &
\	OPT$='C' &

2200	!----------------------------------------CHANGE OR BLANK RECORD &
	IF OPT$='B' &
	THEN	OPTION$ = 'BLANK ' &
	ELSE	OPTION$ = 'CHANGE ' &
\		CHNG.FLAG% = -1% &

2210	PRINT #CH%, CLRBOT$; OPTION$+':'; &
\	INP$=CVT$$(FNINP$(CH%,128%,' ',2%,1%),32%) &
\	GOTO 1030 IF INP$='' &
\	TEMP$=NUM1$(PWJL.COUNT%+LOOP.DATA%+1%)+' ' ! FOR ADD NEW RECORD &
\	LP%,LOOP%=(INSTR(1%,COMMAND$+TEMP.COMMAND$+TEMP$, &
		' '+INP$+' ')+2%)/3% &
\	GOTO 2210 IF VAL(INP$)=0. &
\	GOTO 2210 IF LOOP%<1% AND OPT$='C'&
\	FRIGHT=CVT$F(PWJH.FRIGHT$) &
\	GOTO 2210 IF (LOOP%<2% OR LOOP%=CUST1%) AND OPT$='B'&
\	GOTO 2230 IF LOOP%>LOOP.DATA% &
\	IF CUST1%<=LOOP% AND LOOP%<=CUST1%+7% &
		THEN	INDE%=0% &
\			V%=FNG%(-CUSTOM.CH%,NUM1$(CUSTOM.KEY%(INDE%)))+ &
				FNG%(-CUSTOM.CH%-1%,NUM1$(CUSTOM.DATA%(INDE%))) &
\			IF FNS%=0% &
			THEN	LSET CUSTOM.BUF$=FNL$+'' &
			ELSE	GOTO 2220 IF LOOP%=CUST1% &
\				PRINT #CH%, FNP$('24;1');CLRLIN$;B.ON$; &
			'Undefined Customer number.';FNP$('24;55'); &
			'Hit any key to continue';G.OFF$; &
\			NW$=FNINP$(CH%,128%,' ',1%,1%) &
\			GOTO 2210  &

2220	GOSUB 6010 IF OPT$='C' &
\	INP$='' IF OPT$='B' &
\	GOSUB 6030 IF OPT$='B' &
\	IF LOOP%=15% AND CVT$F(PWJH.BROAMT$)<>0.  &
	THEN	COMMIS=CVT$F(PWJH.BROAMT$) &
\		GOSUB 6935 &

2221	IF LOOP%=15% AND CVT$F(PWJH.BROAMT$)=0.  &
	THEN	V%=FNG%(PWJL.CH%,PWJH.ORDER$) &
\		COMMIS=0. &
\		WHILE FNS%=0% AND LEFT(FNL$,6%)=PWJH.ORDER$ &
\			LSET PWJL.BUF$=FNL$ &
\			COMMIS=COMMIS+0.01*CVT$F(PWJL.EXT$)* &
				CVT$F(PWJL.BROKER$) &
\			V%=FNN%(PWJL.CH%) &
\		NEXT &
\		GOSUB 6935 &

2222	IF LOOP%=20% &
	THEN	FRIG=FRIG+0.01*SUM.POUNDS*(CVT$F(PWJH.FRIGHT$)-FRIGHT) &
\		GOSUB 6935 &

2225	GOTO 2210 IF 	 CUST1%<LP% AND LP%<CUST2% AND CUST.N%(0%)<>0% &
\	GOTO 2290 &

2230	!--------------------CHANGE ON THE LINE &
	LOOP.CHNG%=LOOP%-LOOP.DATA% &
\	IF LOOP%>PWJL.COUNT%+LOOP.DATA%    		! ADD NEW RECORD &
	THEN	LINE.NO% = 16% + PWJL.COUNT% - TOP.LINE% &
\		LINE.NO% = 19% IF LINE.NO%>19% &
\		CUR.LINE% = TOP.LINE% + 3% &
\		CUR.LINE% = PWJL.COUNT% IF CUR.LINE%>PWJL.COUNT% &
\		LINE.NO% = 15% IF CUR.LINE%<PWJL.COUNT%-3% &
\		TOP.LINE% = PWJL.COUNT%-2% IF CUR.LINE%<PWJL.COUNT%-3% &
\		CUR.LINE% = PWJL.COUNT%-3% IF CUR.LINE%<PWJL.COUNT%-3% &
\		FOR CUR.LINE%=CUR.LINE%+1% TO PWJL.COUNT% &
\			LINE.NO%=LINE.NO%+1% &
\			GOSUB 6040 &
\			GOSUB 6050 &
\		NEXT CUR.LINE% &
\		CHNG.FLAG%=0% &
\		ADD.LINE%=1% &
\		OPTION$='ADD' &
\		GOTO 2140 &

2240	IF TOP.LINE%<=LOOP.CHNG% AND &
		TOP.LINE%+3%>=LOOP.CHNG%    		! ON THE SCREEN &
	THEN	CUR.LINE%=LOOP.CHNG% &
\		LINE.NO%=16%+CUR.LINE%-TOP.LINE% &
\		GOSUB 6040 &
\		GOTO 2260 &

2250	IF LOOP.CHNG%<TOP.LINE%-3% &
		OR LOOP.CHNG%>TOP.LINE%+6% 		! JUMP &
	THEN	STEP.TEMP%=1% &
\		LINE.NO%=15% &
\		CUR.LINE%,TOP.LINE%=LOOP.CHNG%-3% &
\		STEP.TEMP%=-1% IF LOOP.CHNG%<=4% &
\		LINE.NO%=20% IF LOOP.CHNG%<=4% &
\		CUR.LINE%=LOOP.CHNG%+3% IF LOOP.CHNG%<=4% &
\		TOP.LINE%=LOOP.CHNG% IF LOOP.CHNG%<=4% &
\		GOTO 2257 &

2255	STEP.TEMP%=1% &
\	LINE.NO%=16% &
\	STEP.TEMP%=-1% IF TOP.LINE%>LOOP.CHNG% &
\	LINE.NO%=19% IF STEP.TEMP%=1% &
\	CUR.LINE%=TOP.LINE%+4% &
\	CUR.LINE%=TOP.LINE%-1% IF STEP.TEMP%<1% &

2257	FOR CUR.LINE% = CUR.LINE% TO LOOP.CHNG% STEP STEP.TEMP% &
\		LINE.NO%=LINE.NO%+STEP.TEMP% &
\		GOSUB 6040 &
\		GOSUB 6050 &
\	NEXT CUR.LINE% &

2260	PRINT #CH%, CLRBOT$; 'Item: A B C D E F G H '; &
\	INP$=CVT$$(FNINP$(CH%,128%,' ',1%,1%),32%) &
\	GOTO 2200 IF INP$='' &
\	LOOP%=(INSTR(1%,' A B C D E F G H ',' '+INP$+' ')+1%)/2% &
\	GOTO 2260 IF LOOP%=0% &
\	LOOP%=LOOP%+LOOP.DATA% &
\	EXT=CVT$F(PWJL.EXT$) &
\	BROK=CVT$F(PWJL.BROKER$) &
\	POND=CVT$F(PWJL.POUNDS$) &
\	GOSUB 6010 IF OPT$='C' &
\	IF OPT$='B' &
		THEN	INP$='' &
\			GOSUB 6030 &

2290	GOSUB  2900 &
\	GOTO 2295 IF FNS%<>0% &
\	GOTO 2210 IF LOOP%<23% &
\	IF LOOP%<25%  &
	THEN	GOSUB 6740 &
\		GOSUB 6660 &

2292	GOSUB 6930 &
\	GOTO 2210 &

2295	V$=FNMESS$(CH%,FNS%,'PWJH.DAT',0%,-1%) &
\	GOTO 1010 &

2400	!-------------------------------------------------DELETE RECORD &
	PRINT #CH%, CLRBOT$;"Confirm deletion (Yes/No) "; &
\	INP$=CVT$$(FNINP$(CH%,128%," ",1%,1%),32%) &
\	GOTO 1030 IF INP$<>'Y' &
\	PWJL.COUNT%=CVT$%(PWJH.LINE$) &

2420	IF FND%(PWJH.CH%,' ') &
	THEN	V$=FNMESS$(CH%,FNS%,'PWJH.DAT',0%,-1%) &

2430	FOR  PWJL.LOOP%=1% TO PWJL.COUNT% &
\		IF FNG%(PWJL.CH%,PWJH.ORDER$+CVT%$(PWJL.LOOP%+LOOP.DATA%))=0% &
		THEN	IF FND%(PWJL.CH%,' ') &
			THEN	V$=FNMESS$(CH%,FNS%,'PWJL.DAT',0%,-1%) &

2440	NEXT PWJL.LOOP% &
\	GOTO 1005 &

2500	!-----------------------------------------------SET DEFAULT VALUES &
	FOR LOOP%=1% TO LOOP.DATA% &
\		INP$=DEFAULT$(LOOP%) &
\		GOSUB 6030 &
\	NEXT LOOP% &
\	PRINT #CH%,	FNP$(NUM1$(LOOP%)+';1');CLRLIN$;R.ON$;' '; &
			FNP$(NUM1$(LOOP%)+';80');' ';G.OFF$; &
				FOR LOOP%=16% TO 19% &

2510	OPTION$='DEFAULT ' &
\	PRINT #CH%, CLRBOT$;'Default:'; &
\	INP$=CVT$$(FNINP$(CH%,128%,' ',2%,1%),32%) &
\	GOTO 1010 IF INP$='' &
\	LOOP%=(INSTR(1%,' '+COMMAND$,INP$)+2%)/3% &
\	GOTO 2510 IF LOOP%<=1% &
\	GOSUB 6010 &
\	DEFAULT$(LOOP%)=INP$ &
\	GOTO 2510 &

2900	!-------------------------------------------------UPDATE A RECORD &
	IF CUST1%<LOOP% AND LOOP%<CUST2% AND LP%<>CUST1% &
	THEN	INDE%=INT(LOOP%/CUST2%) &
\		V%=FNG%(-CUSTOM.CH%,NUM1$(CUSTOM.KEY%(INDE%)))+ &
			FNG%(-CUSTOM.CH%-1%,NUM1$(CUSTOM.DATA%(INDE%))) &
\		V%=FNU%(CUSTOM.CH%,CUSTOM.BUF$) IF FNS%=0% &
\		RETURN &

2910	IF LOOP%<=LOOP.DATA% &
	THEN	V%=FNG%(-PWJH.CH%,NUM1$(KEY.POINTER%))+ &
			FNG%(-PWJH.CH%-1%,NUM1$(DATA.POINTER%)) &
\		V%=FNU%(PWJH.CH%,PWJH.BUF$) IF FNS%=0% AND LOOP%>1% &
\		V%=FNU%(-PWJH.CH%,PWJH.BUF$) IF FNS%=0% AND LOOP%=1% &
\		RETURN &

2920	V%=FNG%(PWJL.CH%,PWJH.ORDER$+CVT%$(CUR.LINE%)) &
\	V%=FNU%(PWJL.CH%,PWJL.BUF$) IF FNS%=0% &
\	RETURN &

6000	!------------------------------------------------DATA ENTRY SECTION &
	RETURN IF KEY.POINTER%=0% OR FNG%(-PWJH.CH%,NUM1$(KEY.POINTER%))+ &
			FNG%(-PWJH.CH%-1%,NUM1$(DATA.POINTER%)) &
\	LSET PWJH.BUF$=FNL$+'' &
\	PWJL.COUNT%=CVT$%(PWJH.LINE$) &
\	CUSTOM.KEY%(0%),CUSTOM.KEY%(1%),CUSTOM.DATA%(0%),CUSTOM.DATA%(1%)=0% &
\	LSET CUSTOM.BUF$='' &
\	LSET CUSTOM.BUF$=FNL$+'' IF FNG%(CUSTOM.CH%,PWJH.SOLDTO$)=0% &
\	CUSTOM.KEY%(0%)=FNR(CUSTOM.CH%) IF FNS%=0% &
\	CUSTOM.DATA%(0%)=FNR(CUSTOM.CH%+1%) IF FNS%=0% &
\	FOR LOOP%=1% TO LOOP.DATA% &
\		GOSUB 6009 &
\	NEXT LOOP% &

6002	!-------------------------------------LINE FILE &
	TEMP.COMMAND$=' ' &
\	LINE.NO%=16% &
\	COMMIS,SUM.POUNDS,FRIG,TOT,CUR.LINE%=0% &
\	TOP.LINE%=1% &
\	PRINT #CH% USING FNP$('20;14')+R.ON$+'###'+G.OFF$,LOOP.DATA%; &
\	PRINT #CH% USING FNP$('20;43')+R.ON$+'######.##'+FNP$('20;57')+ &
		'######.##'+FNP$('20;71')+'######.##'+G.OFF$+FNP$('21;1'), &
		0.,0.,0.; &
\	PRINT #CH%,	FNP$(NUM1$(LOOP%)+';2');SPACE$(78%); &
				FOR LOOP%=16% TO 19% &
\	FOR  PWJL.LOOP%=1% TO PWJL.COUNT% &
\		IF FNG%(PWJL.CH%,PWJH.ORDER$+CVT%$(PWJL.LOOP%))=0% &
		THEN	LSET PWJL.BUF$=FNL$+'' &
\			TEMP.COMMAND$=TEMP.COMMAND$+NUM1$(LOOP.DATA%+ &
				PWJL.LOOP%)+' ' &
\			GOSUB 6920 &
\			IF LINE.NO%<20% &
			THEN	CUR.LINE%=PWJL.LOOP% &
\				GOSUB 6050             ! PRINT LINE RECORD &
\				LINE.NO%=LINE.NO%+1% &

6005	NEXT PWJL.LOOP% &
\	RETURN &

6009	ON LOOP% GOSUB	6220, 6760, 6240, 6260, 6280, 6300, 6320, 6340, 6360, &
			6380, 6420, 6440, 6460, &
			6500, 6780, 6800, 6480, 6820, 6840, 6520, 6860, &
			6540, 6560, 6580, &
			6600, 6620, 6680, 6700, 6720, 6740, 6640, 6660  &
\	RETURN &

6010	ON LOOP% GOSUB	6210, 6750, 6230, 6250, 6270, 6290, 6310, 6330, 6350, &
			6370, 6410, 6430, 6450, &
			6490, 6770, 6790, 6470, 6810, 6830, 6510, 6850, &
			6530, 6550, 6570, &
			6590, 6610, 6670, 6690, 6710, 6730, 6630, 6650  &
\	IF LOOP%=CUST1% &
	THEN	INDE%=INT(LOOP%/CUST2%) &
\		CUST.N%(0%)=FNG%(CUSTOM.CH%,PWJH.SOLDTO$) IF LOOP%=CUST1% &
\		LSET CUSTOM.BUF$=FNL$+'' IF CUST.N%(INDE%)=0% &
\		LSET CUSTOM.BUF$=''      IF CUST.N%(INDE%)<>0% &
\		IF CUST.N%(INDE%)=0% &
		THEN	GOSUB 6060 &
		ELSE	PRINT #CH%, FNP$('24;1');CLRLIN$;B.ON$; &
			'Undefined Customer number.';FNP$('24;55'); &
			'Re-enter (y/N) ';G.OFF$; &
\			GOTO 6010 IF FNINP$(CH%,128%,' ',1%,1%)='Y' &
\			IF OPT$='C' &
			THEN	INDE%=INT(LOOP%/CUST2%) &
\				CUSTOM.KEY%(INDE%)=0% &
\				CUSTOM.DATA%(INDE%)=0% &
\				GOSUB 6009 FOR LOOP%=LOOP%+1% TO LOOP%+7% &

6015	IF LOOP%=29% AND OPT$='A' &
		THEN	DEFAULT$(30%),INP$=NUM1$(FNZ(CVT$F(PWJL.QTY$)* &
				CVT$F(PWJL.PRICE$))) &
\			GOSUB 6737 &

6019	RETURN &

6030	ON LOOP% GOSUB	6217, 6757, 6237, 6257, 6277, 6297, 6317, 6337, 6357, &
			6377, 6417, 6437, 6457, &
			6497, 6777, 6797, 6477, 6817, 6837, 6517, 6857, &
			6537, 6557, 6577, &
			6597, 6617, 6677, 6697, 6717, 6737, 6637, 6657  &
\	RETURN &

6040	V%=FNG%(PWJL.CH%,PWJH.ORDER$+CVT%$(CUR.LINE%)) &
\	LSET PWJL.BUF$=FNL$+'' &
\	RETURN &

6050	TOP.LINE%=1% IF TOP.LINE%=0% &
\	PRINT #CH%, FNP$('16;1'); INSERT.LIN$; IF LINE.NO%<16% &
\	PRINT #CH%, FNP$('19;1') IF LINE.NO%>19% &
\	TOP.LINE%=TOP.LINE%+1% IF LINE.NO%>19% &
\	LINE.NO%=19% IF LINE.NO%>19% &
\	TOP.LINE%=TOP.LINE%-1% IF LINE.NO%<16% &
\	LINE.NO%=16% IF LINE.NO%<16% &
\	LINE.NO$=NUM1$(LINE.NO%) &
\	PRINT #CH%,FNPRT$(LINE.NO$); & 
\	PRINT #CH% USING FNP$(LINE.NO$+';2')+'###',CUR.LINE%+LOOP.DATA%; &
\	GOSUB 6009 FOR LOOP%=LOOP.DATA%+1% TO LOOP.DATA%+8% &
\	RETURN &

6060	GOSUB 6009 FOR LOOP%=LOOP%+1% TO LOOP%+7% &
\	IF OPT$='C' &
	THEN	INDE%=INT(LOOP%/CUST2%) &
\		CUSTOM.KEY%(INDE%)=FNR(CUSTOM.CH%) &
\		CUSTOM.DATA%(INDE%)=FNR(CUSTOM.CH%+1%) &

6061	RETURN &

6200	!-----------------------------------------------DATA ENTRY SECTION &

6210	INP$=FNSIO$('','2;17',PWJH.ORDER$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.ORDER$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	GOTO 6210 IF INP$='' AND OPT$='A' &
\	IF OPT$='C' AND INP$<>'' &
	THEN	IF FNG%(PWJH.CH%,INP$+SPACE$(6%-LEN(INP$)))=0% &
		THEN	PRINT #CH%, CLRBOT$;FNP$('24;1');INP$;' is a duplicate key'; &
				FNP$('21;1');'COMMAND: Reenter Exit '; &
\			INP$=FNINP$(CH%,128%,' ',1%,0%) &
\			GOTO 6210 IF INP$='R' &
\			RETURN &

6217	LSET PWJH.ORDER$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6220	PRINT #CH%, FNP$('2;17');B.ON$;PWJH.ORDER$;G.OFF$; &
\	RETURN &

6230	INP$=FNSIO$('','4;17',PWJH.SOLDTO$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.SOLDTO$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	GOTO 6230 IF INP$='' AND OPT$='A' &

6237	LSET PWJH.SOLDTO$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6240	PRINT #CH%, FNP$('4;17');B.ON$;PWJH.SOLDTO$;G.OFF$; &
\	RETURN &

6250	INP$=FNSIO$('','5;'+NUM1$((LOOP%-1%)*4%+7%*SGN(LOOP%-4%)),CUSTOM.NAME$, &
		DEFAULT$(LOOP%),CHR$(CH%)+STRING$(2%,LEN(CUSTOM.NAME$))+ &
		CHR$(1%)+CHR$(0%)+OPTION$+' ALPHA') &

6257	LSET CUSTOM.NAME$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6260	PRINT #CH%, FNP$('5;'+NUM1$((LOOP%-1%)*4%+7%*SGN(LOOP%-4%))); &
		B.ON$;CUSTOM.NAME$;G.OFF$; &
\	RETURN &

6270	INP$=FNSIO$('','6;'+NUM1$((LOOP%-2%)*4%+7%*SGN(LOOP%-5%)),CUSTOM.ADD1$, &
		DEFAULT$(LOOP%),CHR$(CH%)+STRING$(2%,LEN(CUSTOM.ADD1$))+ &
		CHR$(1%)+CHR$(0%)+OPTION$+' ALPHA') &

6277	LSET CUSTOM.ADD1$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6280	PRINT #CH%, FNP$('6;'+NUM1$((LOOP%-2%)*4%+7%*SGN(LOOP%-5%)));B.ON$; &
		CUSTOM.ADD1$;G.OFF$; &
\	RETURN &

6290	INP$=FNSIO$('','7;'+NUM1$((LOOP%-3%)*4%+7%*SGN(LOOP%-6%)),CUSTOM.ADD2$, &
		DEFAULT$(LOOP%),CHR$(CH%)+STRING$(2%,LEN(CUSTOM.ADD2$))+ &
		CHR$(1%)+CHR$(0%)+OPTION$+' ALPHA') &

6297	LSET CUSTOM.ADD2$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6300	PRINT #CH%, FNP$('7;'+NUM1$((LOOP%-3%)*4%+7%*SGN(LOOP%-6%)));B.ON$; &
		CUSTOM.ADD2$;G.OFF$; &
\	RETURN &

6310	INP$=FNSIO$('','8;'+NUM1$((LOOP%-4%)*4%+7%*SGN(LOOP%-7%)),CUSTOM.CITY$,&
		DEFAULT$(LOOP%),CHR$(CH%)+STRING$(2%,LEN(CUSTOM.CITY$))+ &
		CHR$(1%)+CHR$(0%)+OPTION$+' ALPHA') &

6317	LSET CUSTOM.CITY$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6320	PRINT #CH%, FNP$('8;'+NUM1$((LOOP%-4%)*4%+7%*SGN(LOOP%-7%))); &
		B.ON$;CUSTOM.CITY$;G.OFF$; &
\	RETURN &

6330	INP$=FNSIO$('','9;'+NUM1$((LOOP%-5%)*4%+7%*SGN(LOOP%-8%)),CUSTOM.STATE$,&
		DEFAULT$(LOOP%),CHR$(CH%)+STRING$(2%,LEN(CUSTOM.STATE$))+ &
		CHR$(1%)+CHR$(0%)+OPTION$+' ALPHA') &

6337	LSET CUSTOM.STATE$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6340	PRINT #CH%, FNP$('9;'+NUM1$((LOOP%-5%)*4%+7%*SGN(LOOP%-8%))); &
		B.ON$;CUSTOM.STATE$;G.OFF$; &
\	RETURN &

6350	INP$=FNSIO$('','9;'+NUM1$((LOOP%-6%)*4%+7%*SGN(LOOP%-9%)+14%), &
		CUSTOM.ZIP$,DEFAULT$(LOOP%),CHR$(CH%)+ &
		STRING$(2%,LEN(CUSTOM.ZIP$))+CHR$(1%)+CHR$(0%)+OPTION$+' ALPHA') &

6357	LSET CUSTOM.ZIP$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6360	PRINT #CH%, FNP$('9;'+NUM1$((LOOP%-6%)*4%+7%*SGN(LOOP%-9%)+14%)); &
		B.ON$;CUSTOM.ZIP$;G.OFF$; &
\	RETURN &

6370	INP$=FNSIO$('','10;'+NUM1$((LOOP%-7%)*4%+7%*SGN(LOOP%-10%)+3%), &
		CUSTOM.COUNTRY$,DEFAULT$(LOOP%),CHR$(CH%)+ &
		STRING$(2%,LEN(CUSTOM.COUNTRY$))+CHR$(1%)+CHR$(0%)+ &
		OPTION$+' ALPHA') &

6377	LSET CUSTOM.COUNTRY$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6380	PRINT #CH%, FNP$('10;'+NUM1$((LOOP%-7%)*4%+7%*SGN(LOOP%-10%)+3%)); &
		B.ON$;CUSTOM.COUNTRY$;G.OFF$; &
\	RETURN &

6410	INP$=FNSIO$('','11;17',PWJH.INVDAT$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			CHR$(8%)+CHR$(6%)+CHR$(4%)+CHR$(0%)+OPTION$+' DATE') &
\	TEMP=VAL(INP$) &
\	GOTO 6410 IF FNDREAL%(INP$)=0% &

6417	LSET PWJH.INVDAT$=CVT%$(FND6%(INP$)) IF INP$<>'' OR CHNG.FLAG%=0% &

6420	PRINT #CH%, FNP$('11;17');B.ON$;FND6$(CVT$%(PWJH.INVDAT$));G.OFF$; &
\	RETURN &

6430	INP$=FNSIO$('','12;17',PWJH.SHPDAT$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			CHR$(8%)+CHR$(6%)+CHR$(4%)+CHR$(0%)+OPTION$+' DATE') &
\	TEMP=VAL(INP$) &
\	GOTO 6430 IF FNDREAL%(INP$)=0% &

6437	LSET PWJH.SHPDAT$=CVT%$(FND6%(INP$)) IF INP$<>'' OR CHNG.FLAG%=0% &

6440	PRINT #CH%, FNP$('12;17');B.ON$;FND6$(CVT$%(PWJH.SHPDAT$));G.OFF$; &
\	RETURN &

6450	INP$=FNSIO$('','13;17',PWJH.SHPLOC$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.SHPLOC$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &

6457	LSET PWJH.SHPLOC$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6460	PRINT #CH%, FNP$('13;17');B.ON$;PWJH.SHPLOC$;G.OFF$; &
\	RETURN &

6470	INP$=FNSIO$('','05;56',PWJH.CARNUM$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.CARNUM$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &

6477	LSET PWJH.CARNUM$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6480	PRINT #CH%, FNP$('05;56');B.ON$;PWJH.CARNUM$;G.OFF$; &
\	RETURN &

6490	INP$=FNSIO$('','02;56',PWJH.BRONUM$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.BRONUM$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &

6497	LSET PWJH.BRONUM$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6500	PRINT #CH%, FNP$('02;56');B.ON$;PWJH.BRONUM$;G.OFF$; &
\	RETURN &

6510	INP$=FNSIO$('#####.##','08;57',PWJH.FRIGHT$,DEFAULT$(LOOP%), &
			CHR$(CH%)+CHR$(8%)+CHR$(7%)+CHR$(5%)+CHR$(2%)+ &
			OPTION$+' VALUE') &

6517	LSET PWJH.FRIGHT$=CVTF$(VAL(INP$)) IF INP$<>'' OR CHNG.FLAG%=0% &

6520	PRINT #CH% USING FNP$('08;57')+B.ON$+'#####.##'+G.OFF$, &
		CVT$F(PWJH.FRIGHT$); &
\	RETURN &

6530	INP$=FNSIO$('','10;56',PWJH.SOLDBY$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.SOLDBY$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &

6537	LSET PWJH.SOLDBY$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6540	PRINT #CH%, FNP$('10;56');B.ON$;PWJH.SOLDBY$;G.OFF$; &
\	RETURN &

6550	INP$=FNSIO$('','11;56',PWJH.CUSPO$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.CUSPO$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &

6557	LSET PWJH.CUSPO$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6560	PRINT #CH%, FNP$('11;56');B.ON$;PWJH.CUSPO$;G.OFF$; &
\	RETURN &

6570	INP$=FNSIO$('','12;56',PWJH.TERMS$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.TERMS$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &

6577	LSET PWJH.TERMS$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6580	PRINT #CH%, FNP$('12;56');B.ON$;PWJH.TERMS$;G.OFF$; &
\	RETURN &

6590	INP$=FNSIO$('',NUM1$(LINE.NO%)+';6',PWJL.PACK$,DEFAULT$(LOOP%), &
		CHR$(CH%)+CHR$(6%)+CHR$(15%)+CHR$(1%)+CHR$(0%)+ &
		OPTION$+' ALPHA') &

6597	LSET PWJL.PACK$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6600	PRINT #CH%, FNP$(NUM1$(LINE.NO%)+';6');B.ON$;LEFT(PWJL.PACK$,6%);G.OFF$; &
\	RETURN &

6610	INVDES.FLAG%=FNG%(INVDES.CH%,PWJL.PACK$) &
\	IF INVDES.FLAG%	&
	THEN	INP$=FNSIO$('',NUM1$(LINE.NO%)+';13',PWJL.DESC$,DEFAULT$(LOOP%), &
			CHR$(CH%)+CHR$(12%)+CHR$(LEN(PWJL.DESC$))+CHR$(1%)+ &
			CHR$(0%)+OPTION$+' ALPHA') &
	ELSE	INP$=MID(FNL$,16%,26%) &

6611	GOTO 6620 IF INP$='%END' &

6617	LSET PWJL.DESC$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6620	PRINT #CH%, FNP$(NUM1$(LINE.NO%)+';13');B.ON$;LEFT(PWJL.DESC$,12%);G.OFF$; &
\	RETURN &

6630	INP$='' &
\	IF CVT$F(PWJH.BROAMT$)=0. &
	THEN	INP$=FNSIO$('###.##%',NUM1$(LINE.NO%)+';65',PWJL.BROKER$,DEFAULT$(LOOP%), &
			CHR$(CH%)+CHR$(6%)+CHR$(5%)+CHR$(5%)+CHR$(2%)+ &
			OPTION$+' VALUE') &
\		RETURN IF INP$='%END' &

6637	LSET PWJL.BROKER$=CVTF$(VAL(INP$)) IF INP$<>'' OR CHNG.FLAG%=0% &

6640	PRINT #CH% USING FNP$(NUM1$(LINE.NO%)+';65')+B.ON$+'###.##%'+G.OFF$, &
		CVT$F(PWJL.BROKER$); &
\	RETURN &

6650	GOTO 6660 IF PWJH.UNTFLG$<>'U' &
\	INP$=FNSIO$('######',NUM1$(LINE.NO%)+';74',PWJL.POUNDS$,DEFAULT$(LOOP%), &
			CHR$(CH%)+CHR$(6%)+CHR$(6%)+CHR$(5%)+CHR$(0%)+ &
			OPTION$+' VALUE') &
\	RETURN IF INP$='%END' &

6657	LSET PWJL.POUNDS$=CVTF$(VAL(INP$)) IF INP$<>'' OR CHNG.FLAG%=0% &

6660	PRINT #CH% USING FNP$(NUM1$(LINE.NO%)+';74')+B.ON$+'######'+G.OFF$, &
		CVT$F(PWJL.POUNDS$); &
\	RETURN &

6670	INP$=FNSIO$('',NUM1$(LINE.NO%)+';26',PWJL.ACCNUM$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJL.ACCNUM$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	IF INP$<>'' AND CHART.FLAG%=0% &
	THEN	IF FNG%(CHART.CH%,SPACE$(8%-LEN(INP$))+INP$) &
		THEN	PRINT #CH%, FNP$('24;1');CLRLIN$;B.ON$; &
			'Undefined account number.';FNP$('24;55'); &
			'Re-enter(y/N) ';G.OFF$; &
\			GOTO 6670 IF FNINP$(CH%,128%,' ',1%,1%)='Y' &

6677	RSET PWJL.ACCNUM$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6680	PRINT #CH%, FNP$(NUM1$(LINE.NO%)+';26');B.ON$;PWJL.ACCNUM$;G.OFF$; &
\	RETURN &

6690	INP$=FNSIO$('#####.##',NUM1$(LINE.NO%)+';35',PWJL.QTY$,DEFAULT$(LOOP%), &
		CHR$(CH%)+CHR$(8%)+CHR$(7%)+CHR$(5%)+CHR$(2%)+ &
			OPTION$+' VALUE') &
\	RETURN IF INP$='%END' &
\	INVDES.FLAG%=FNG%(INVDES.CH%,PWJL.PACK$) &
\	DEFAULT$(32%)='' &
\	WEIG=0. &
\	IF INVDES.FLAG%=0% AND PWJH.UNTFLG$='U' &
	THEN	PACK$=MID(FNL$,46%,4%) &
\		FOR I%=1% TO PACK.INDEX% &
\			IF UNIT$(I%)=PACK$ &	
		THEN	DEFAULT$(32%)=NUM1$(VAL(INP$)*WEIGHT(I%)) &
\			WEIG=WEIGHT(I%) &
\			GOTO 6697 &

6691		NEXT I% &

6697	IF INP$<>'' OR CHNG.FLAG%=0% &
	THEN	LSET PWJL.QTY$=CVTF$(VAL(INP$)) &
\		LSET PWJL.POUNDS$=CVTF$(VAL(INP$)*WEIG) &
\		LSET PWJL.EXT$=CVTF$(VAL(INP$)*CVT$F(PWJL.PRICE$)) &

6700	PRINT #CH% USING FNP$(NUM1$(LINE.NO%)+';35')+B.ON$+'#####.##'+G.OFF$, &
		CVT$F(PWJL.QTY$); &
\	RETURN &

6710	INP$=FNSIO$('######.##',NUM1$(LINE.NO%)+';44',PWJL.PRICE$,DEFAULT$(LOOP%), &
			CHR$(CH%)+CHR$(9%)+CHR$(8%)+CHR$(5%)+CHR$(2%)+ &
			OPTION$+' VALUE') &
\	RETURN IF INP$='%END' &

6717	IF INP$<>'' OR CHNG.FLAG%=0% &
	THEN	LSET PWJL.PRICE$=CVTF$(VAL(INP$)) &
\		LSET PWJL.EXT$=CVTF$(VAL(INP$)*CVT$F(PWJL.QTY$)) &

6720	PRINT #CH% USING FNP$(NUM1$(LINE.NO%)+';44')+B.ON$+'######.##'+G.OFF$, &
		CVT$F(PWJL.PRICE$); &
\	RETURN &

6730	INP$=FNSIO$('######.##',NUM1$(LINE.NO%)+';54',PWJL.EXT$,DEFAULT$(LOOP%), &
			CHR$(CH%)+CHR$(9%)+CHR$(8%)+CHR$(5%)+CHR$(2%)+ &
			OPTION$+' VALUE') &
\	RETURN IF INP$='%END' &

6737	LSET PWJL.EXT$=CVTF$(VAL(INP$)) IF INP$<>'' OR CHNG.FLAG%=0% &

6740	PRINT #CH% USING FNP$(NUM1$(LINE.NO%)+';54')+B.ON$+'######.##'+G.OFF$, &
		CVT$F(PWJL.EXT$); &
\	RETURN &

6750	INP$=FNSIO$('','3;17',PWJH.INVNUM$, &
		DEFAULT$(LOOP%),CHR$(CH%)+STRING$(2%,LEN(PWJH.INVNUM$))+ &
		CHR$(1%)+CHR$(0%)+OPTION$+' ALPHA') &

6757	RSET PWJH.INVNUM$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6760	PRINT #CH%, FNP$('3;17');B.ON$;PWJH.INVNUM$;G.OFF$; &
\	RETURN &

6770	INP$=FNSIO$('######.##','3;56',PWJH.BROAMT$,DEFAULT$(LOOP%), &
			CHR$(CH%)+CHR$(9%)+CHR$(8%)+CHR$(5%)+CHR$(2%)+ &
			OPTION$+' VALUE') &

6777	LSET PWJH.BROAMT$=CVTF$(VAL(INP$)) IF INP$<>'' OR CHNG.FLAG%=0% &

6780	PRINT #CH% USING FNP$('3;56')+B.ON$+'######.##'+G.OFF$, &
		CVT$F(PWJH.BROAMT$); &
\	RETURN &

6790	INP$=FNSIO$('','4;56',PWJH.BROACC$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.BROACC$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	IF INP$<>'' AND CHART.FLAG%=0% &
	THEN	IF FNG%(CHART.CH%,SPACE$(8%-LEN(INP$))+INP$) &
		THEN	PRINT #CH%, FNP$('24;1');CLRLIN$;B.ON$; &
			'Undefined account number.';FNP$('24;55'); &
			'Re-enter(y/N) ';G.OFF$; &
\			GOTO 6790 IF FNINP$(CH%,128%,' ',1%,1%)='Y' &

6797	RSET PWJH.BROACC$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6800	PRINT #CH%, FNP$('4;56');B.ON$;PWJH.BROACC$;G.OFF$; &
\	RETURN &

6810	INP$=FNSIO$('','6;56',PWJH.FOBFLG$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.FOBFLG$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	IF INSTR(1%,' D S ',' '+INP$+' ')=0% &
	THEN	PRINT #CH%,CLRBOT$;FNP$('24;1');"'D' - Distination "+ &
		"'S' - Shipping Point";FNP$('21;1'); &
		'COMMAND: Re-enter ';FNP$('24;55');'Hit any key to continue '; &
\		AA$=FNINP$(CH%,128%,' ',1%,0%) &
\		GOTO 6810 &

6817	LSET PWJH.FOBFLG$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6820	PRINT #CH%, FNP$('6;56');B.ON$;PWJH.FOBFLG$;G.OFF$; &
\	RETURN &

6830	DEFAULT$(LOOP%)='U' &
\	INP$=FNSIO$('','7;56',PWJH.UNTFLG$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.UNTFLG$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	IF INSTR(1%,' U $ ',' '+INP$+' ')=0% &
	THEN	PRINT #CH%,CLRBOT$;FNP$('24;1');"'U' - Unit '$' - Dollars";FNP$('21;1'); &
		'COMMAND: Re-enter ';FNP$('24;55');'Hit any key to continue '; &
\		AA$=FNINP$(CH%,128%,' ',1%,0%) &
\		GOTO 6830 &

6837	LSET PWJH.UNTFLG$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6840	PRINT #CH%, FNP$('7;56');B.ON$;PWJH.UNTFLG$;G.OFF$; &
\	RETURN &

6850	INP$=FNSIO$('','9;56',PWJH.CARACC$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.CARACC$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	IF INP$<>'' AND CHART.FLAG%=0% &
	THEN	IF FNG%(CHART.CH%,SPACE$(8%-LEN(INP$))+INP$) &
		THEN	PRINT #CH%, FNP$('24;1');CLRLIN$;B.ON$; &
			'Undefined account number.';FNP$('24;55'); &
			'Re-enter(y/N) ';G.OFF$; &
\			GOTO 6850 IF FNINP$(CH%,128%,' ',1%,1%)='Y' &

6857	RSET PWJH.CARACC$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6860	PRINT #CH%, FNP$('9;56');B.ON$;PWJH.CARACC$;G.OFF$; &
\	RETURN &

6920	!------------------------------------------NUMBER OF RECORD AND TOTAL &
	PRINT #CH% USING FNP$('20;14')+R.ON$+'###'+G.OFF$, &
		PWJL.COUNT%+LOOP.DATA%; &

6930	TOT=TOT+CVT$F(PWJL.EXT$)-EXT &
\	SUM.POUNDS=SUM.POUNDS+CVT$F(PWJL.POUNDS$)-POND &
\	FRIG=0.01*SUM.POUNDS*CVT$F(PWJH.FRIGHT$) &
\	IF CVT$F(PWJH.BROAMT$)=0. &
	THEN	COMMIS=COMMIS+0.01*(CVT$F(PWJL.EXT$)*CVT$F(PWJL.BROKER$)-EXT*BROK) &
	ELSE	COMMIS=CVT$F(PWJH.BROAMT$) &

6935	PRINT #CH% USING FNP$('20;43')+R.ON$+'######.##'+FNP$('20;57')+ &
		'######.##'+FNP$('20;71')+'######.##'+G.OFF$+FNP$('21;1'), &
		COMMIS,FRIG,TOT; &
\	FRIGHT,POND,BROK,EXT=0. &
\	RETURN &

18910	!----------------------------------------------END OF FILE &
	PRINT #CH%, CLRBOT$;FNP$('24;1');'End of file has been reached.'; &
		FNP$('24;55');'Hit any key to continue '; &
\	INP$=FNINP$(CH%,128%,' ',1%,0%) &
\	GOTO 1000 &

19000	!------------------------------------------------ERROR TRAPPING &
	RESUME IF ERR=52% OR ERR=51% &
\	RESUME 450 IF ERL=400 &
\	INP$='%^C' IF ERR=54% &
\	RESUME 1010 IF ERR=54% &

19999	ON ERROR GOTO 0 &

30000	!----------------------PROGRAM FUNCTIONS------------------------ &
	!------------------------------------------------TERMINAL INPUT &
	DEF FNINP$(CHN%,KYP%,FILLCHAR$,INPUTLEN%,TO.ERR%) &
\		PRINT #CHN%, STRING$(INPUTLEN%,ASCII(FILLCHAR$)); &
			  STRING$(INPUTLEN%,8%); &
\		PRINT #CHN%, RECORD 256%,CHR$(KYP%+INPUTLEN%)+FILLCHAR$; &
\		GET #CHN% &
\		FIELD #CHN%, RECOUNT AS BUFFER$ &
\		IF ASCII(BUFFER$)=128% OR ASCII(BUFFER$)=8% &
			OR ASCII(BUFFER$)=23% &
		THEN	BUFFER$="%END" IF MID(BUFFER$,2%,4%)="[21~" &
				OR MID(BUFFER$,2%,2%)="OP" &
\			BUFFER$="%ABORT" IF MID(BUFFER$,2%,4%)="[19~" &
				OR MID(BUFFER$,2%,2%)="OQ" &

30010		BUFFER$='%^C' IF INSTR(1%,BUFFER$,CHR$(3%)) &
\		FNINP$=CVT$$(BUFFER$,4%) &
\		V=SQR(-1) IF BUFFER$='%^C' AND TO.ERR% !^C Trappping &
\	FNEND &

30200	DEF*FNP$(ROWCOL$)=ESC$+'['+ROWCOL$+'H' 	! Direct Cursor Address &

30250	!-----------------------------------------------MESSAGE HANDLER &
	DEF*FNMESS$(CHN%,ERRNUM%,DESC$,TO.ERR%,PRINT.TEST%) &
\		MESG$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERRNUM%)),3%,30%)+ &
			DESC$ &
\		IF PRINT.TEST% &
		THEN	PRINT #CHN%, FNP$('24;1');CLRLIN$;MESG$; &
				FNP$('24;55'); &
				'Hit any key to continue.'; &
\			NW$=FNINP$(CHN%,128%,' ',1%,TO.ERR%) &

30260		FNMESS$=MESG$ &
\	FNEND &

30300	DEF*FNSR$(BEGEND$)=ESC$+'['+BEGEND$+'r'	! Scroll control &

30350	DEF FNFONE$(PHN$)='('+LEFT(PHN$,3%)+')'+MID(PHN$,4%,3%)+'-' &
		+RIGHT(PHN$,7%) &

30400	!-------------------------------------------------DATE HANDLERS &
	DEF FND8%(D8)=D8 						&

30410	DEF FND6%(D9$)=VAL(MID(D9$,3%,2%))+VAL(LEFT(D9$,2%))*32% 	&
		+ FND8%(VAL(RIGHT(D9$,5%)))*512% 			&

30420	DEF FND6$(D9%)=RIGHT(NUM1$((D9% AND 15%*32%)/32%+100%),2%) 	&
		+ '/'+RIGHT(NUM1$((D9% AND 31%)+100%),2%) 		&
		+ '/'+RIGHT(NUM1$(((SWAP%(D9%) AND 254%)/2%)+100%),2%) 	&

30450	DEF FNDREAL%(DATUM$) 			!CHECK FOR ACTUAL DATES	&
\	  	DAY%=VAL(MID(DATUM$,3%,2%)) 				&
\	  	YIR%=VAL(RIGHT(DATUM$,5%)) 				&
\	  	FNDREAL%,P%=0% &
\	  	FNDREAL%,P%=31% IF INSTR(1%,' 01 03 05 07 08 10 12 ',' '&
			+LEFT(DATUM$,2%)+' ') 				&
\	  	FNDREAL%,P%=30% IF INSTR(1%,' 04 06 09 11 ',' '+ 	&
			LEFT(DATUM$,2%)+' ') 				&
\	  	FNDREAL%,P%=28%+(1%-SGN(YIR%-4%*INT(YIR%/4%)))-		&
			(1%-SGN(YIR%)) 					&
				IF LEFT(DATUM$,2%)='02'			&
\	  	FNDREAL%=0% IF P%<DAY% OR DAY%=0% 			&
\	  	FNDREAL%=1% IF DATUM$='' 				&
\	FNEND 								&

30500	DEF*FNZ(Z)=INT(ABS(Z)*100.+.50001)/100.*SGN(Z) &

30600	DEF FNPRT$(LINE.NO$)=LDS.ON$+R.ON$+FNP$(LINE.NO$+';1')+' '+G.OFF$+ &
		SPACE$(03%)+CHR$(120%)+SPACE$(06%)+CHR$(120%)+ &
		SPACE$(12%)+CHR$(120%)+SPACE$(08%)+CHR$(120%)+ &
		SPACE$(08%)+CHR$(120%)+SPACE$(09%)+CHR$(120%)+ &
		SPACE$(09%)+CHR$(120%)+SPACE$(08%)+CHR$(120%)+ &
		SPACE$(07%)+R.ON$+' '+G.OFF$+USASCII$ &

30700	!========================================= DATA INPUT FUNCTIONS &
	DEF*FNSIO$(FRMAT$,ROWCOL$,VARABLE$,DEFAL$,ITEMS$) &
\		CHN%=ASCII(MID(ITEMS$,1%,1%)) &
\		KIND%=ASCII(MID(ITEMS$,4%,1%)) &
\		DECI=ASCII(MID(ITEMS$,5%,1%)) &
\		OPT$=MID(ITEMS$,6%,1%) &

30705		PRINT #CHN%, FNP$(ROWCOL$);R.ON$;B.ON$; &
\		IF OPT$='C' OR DEFAL$<>'' &
		THEN	ON KIND% GOTO 30710, 30715, 30720, 30725, 30730 &
		ELSE	PRINT #CHN%, SPACE$(ASCII(MID(ITEMS$,2%,1%))); &
\			GOTO 30735 &

30710		PRINT #CHN%, LEFT(VARABLE$,ASCII(MID(ITEMS$,2%,1%)));! ALPHA &
\		GOTO 30735 &

30715		PRINT #CHN% USING FRMAT$,ASCII(VARABLE$);  ! ASCII &
\		GOTO 30735 &

30720		PRINT #CHN% USING FRMAT$, CVT$%(VARABLE$)/(10.**DECI); &
\		GOTO 30735				   ! INTEGER &

30725		PRINT #CHN%, FND6$(CVT$%(VARABLE$));	   ! DATE &
\		GOTO 30735 &

30730		PRINT #CHN% USING FRMAT$,CVT$F(VARABLE$);  ! FLOAT &

30735		PRINT #CHN%,G.OFF$;CLRBOT$;RIGHT(ITEMS$,6%)+':  ';B.ON$; &
\		FIN$ = FNINP$(CHN%,0%,'_',ASCII(MID(ITEMS$,3%,1%)),1%) &
\		GOTO 30745 IF FIN$='%END' OR FIN$='%ABORT' &
\		V% = VAL(FIN$) IF KIND%=3% &
\		V  = VAL(FIN$) IF KIND%>3% &
\		GOTO 30705 IF KIND%=4% AND LEN(FIN$)<>6% AND FIN$<>'' &
\		IF FIN$='' &
		THEN	FIN$ = DEFAL$ IF INSTR(1%,'CF',OPT$)=0% &
\			GOTO 30745 &

30740		IF KIND%=3% OR KIND%=5% &
		THEN	TEMP = 1. &
\			TEMP = 10.**DECI IF INSTR(1%,FIN$,'.')=0% &
\			FIN$ = NUM1$(VAL(FIN$)/TEMP) &
\			FIN$ = FIN$ + '.' IF INSTR(1%,FIN$,'.')=0% &

30745		FNSIO$ = FIN$+'' &
\	FNEND &

32767	END &

