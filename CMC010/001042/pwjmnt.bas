10	!---------------------------------------------------------------&
	!	SALES JOURNAL MAINTENANCE 	- 			&
	! 								&
	!	PWJMNT.B2S	V1.0	January 1986 			&
	! 								&
	! Author - F.Starman,	 Computer Management Center, Inc. 	&
	! 								&
	! Files-CUSTOM.DAT	-ISAM	Customer Description File 	&
	! Files-CHART.DAT	-ISAM	Chart of accounts File 		&
	! Files-PWJHxx.DAT	-ISAM	Header file 			&
	! Files-PWJLxx.DAT	-ISAM 	Line item file 			&
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

90	OPEN 'SAMPLE' AS FILE 11%					&
\	DIM #11%, KEY.POINTER%(2%,200%),DATA.POINTER%(2%,200%),		&
		UNIT$(50%),WEIGHT(50%)	&	
\	KILL 'SAMPLE' 							&
\	DIM DEFAULT$(27%) &

100	DEVICE.CH%      = 02% &
\	PWJH.CH%,REG.CH%(0%)	= 02% &
\	PWJL.CH%,REG.CH%(1%),REG.CH%(2%)	= 04% &
\	CUSTOM.CH%		= 06% &
\	INVDES.CH%		= 08% &
\	PCKAGE.CH%		= 08% &
\	PWJACC.CH%		= 10% &
\	MENU.CH%   		= 12% &

200	IF FNO%(DEVICE.CH%,'DEVICE.DAT','/RO','')=0% &
	THEN	PWJH.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) &
			IF FNG%(DEVICE.CH%,'PWJH  ')=0% &
\		PWJL.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) &
			IF FNG%(DEVICE.CH%,'PWJL  ')=0% &
\		CUSTOM.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) &
			IF FNG%(DEVICE.CH%,'CUSTOM')=0% &
\		INVDES.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) &
			IF FNG%(DEVICE.CH%,'INVDES')=0% &
\		PCKAGE.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) &
			IF FNG%(DEVICE.CH%,'PCKAGE')=0% &
\		PWJACC.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) &
			IF FNG%(DEVICE.CH%,'PWJACC')=0% &
\		V%=FNC%(DEVICE.CH%) &

300	PRINT #CH%, COLM.OFF$; &
		FNP$('4;28');'WAREHOUSE INVOICE JOURNAL '; &
		FNP$('6;28');'Batch Number <01> '; &
\	JUNK$ = FNINP$(CH%, 128%, '_', 2%,0%) &
\	GOTO 1040 IF JUNK$='%^C' &
\	JUNK$ = "01" IF JUNK$ = "" &
\	IF LEN(JUNK$) = 1% &
	THEN	JUNK$ = '0' + JUNK$ &

305	PRINT #CH%, CLSCN$+FNP$('1;75');B.ON$;BLINK.ON$;R.ON$;'WAIT'; &
			G.OFF$;FNP$('24;1'); &
\	BATCH.NUM$ = JUNK$ &

310	V%=FNO%(PWJH.CH%,PWJH.DEVICE$+'PWJH'+BATCH.NUM$+'.DAT','/RW','') &
\	V%=FNO%(PWJH.CH%,PWJH.DEVICE$+'PWJH'+BATCH.NUM$+'.DAT','/CR:8,128','') &
								IF FNS% = 5% &
\	IF FNS% &
	THEN	V$=FNMESS$(CH%,FNS%,'PWJH'+BATCH.NUM$+'.DAT',0%,-1%) &
\		GOTO 1040 &

320	V%=FNO%(PWJL.CH%,PWJL.DEVICE$+'PWJL'+BATCH.NUM$+'.DAT','/RW','') &
\	V%=FNO%(PWJL.CH%,PWJL.DEVICE$+'PWJL'+BATCH.NUM$+'.DAT','/CR:11,128','') &
								IF FNS% = 5% &
\	IF FNS% &
	THEN	V$=FNMESS$(CH%,FNS%,'PWJL'+BATCH.NUM$+'.DAT',0%,-1%) &
\		GOTO 1040 &

330	V%=FNO%(CUSTOM.CH%,CUSTOM.DEVICE$+'CUSTOM.DAT','/RW','') &
\	IF FNS% &
	THEN	V$=FNMESS$(CH%,FNS%,'CUSTOM.DAT',0%,-1%) &
\		GOTO 1040 &

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
\	V%=FNO%(PWJACC.CH%,PWJACC.DEVICE$+'PWJACC.DAS','/RO/SF','')&

400	OPEN 'MENU.FIL/RO' FOR INPUT AS FILE MENU.CH% &
\	DIM #12%, A0$(1%)=64% &
\	COMPANY$=CVT$$(RIGHT(A0$(1%),2%),128%) &
\	CLOSE MENU.CH% &

450	OPEN 'NL:' AS FILE 12%, RECORDSIZE 128%+128% &
\	FIELD #12%,	06% AS PWJH.ORDNUM$,		! Order # &
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
\	FIELD #12%,	128% AS TEMP$, &
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
\	FIELD #12%, 	128% AS REG.BUF$(0%), &
			128% AS REG.BUF$(1%) &

500	!-----------------------------------------AREA OF MAIN DEFINITIONS &
	F.T.REGION$='01-24(11);11-13(05);16-19(11)'		!*******&
\	LINE.POS$(1%) = '001,005,010,017,021,030,080'			&
\	LINE.POS$(2%) = '001,005,012,023,027,034,043,052,061,063,069,080'   &
\	KEY.REG.0%=1%						!*******&	
\	SUM.REG% = (LEN(F.T.REGION$)-9%)/10% 				&
\	FOR SW%=0% TO SUM.REG%	 					&
\		BASE.TOP%(SW%)=VAL(MID(F.T.REGION$,1%+10*SW%,2%)) 	&	
\		BASE.BOT%(SW%)=VAL(MID(F.T.REGION$,4%+10*SW%,2%)) 	&
\		LOOP.REG%(SW%)=VAL(MID(F.T.REGION$,7%+10*SW%,2%)) 	&
\		WINDOW%(SW%)  =BASE.BOT%(SW%)-BASE.TOP%(SW%)+1% 	&
\	NEXT SW% 							&
\	LOOP.DATA%(0%)  = LOOP.REG%(0%) 				&
\	FOR SW%=1% TO SUM.REG%	 					&
\		LOOP.DATA%(SW%)    = LOOP.DATA%(SW%-1%)+LOOP.REG%(SW%) 	&
\		LAST.LINE%(SW%)    = 0% 				&
\		TOP.LINE%(SW%)	   = 0% 				&	
\	NEXT SW% 							&
\	FRAME$ = FRAME$ + FNP$(NUM1$(I%)+';1')+' '+FNP$(NUM1$(I%)+';80')+' ' &
						FOR I%=2% TO 19% 	&
\	FRM$(0%) = '' 							&
\	FRM$(1%) =FNP$('9;2')+'     A     B     C      D  '+SPACE$(51%) &
\	FRM$(2%) =FNP$('14;6')+'  A       B        C     D       E'+ &
		'        F         G   H   I             '  &
\	COMMAND$(0%)  = '          04 05 06 07 08 09 10 11 ' &
\	COMMAND$(1%)=' A  B  C  D  '				!*******&
\	COMMAND$(2%)=' A  B  C  D  E  F  G  H  I '		!*******&
\	TITLE$     = 'INVOICE JOURNAL - BATCH NO. '+BATCH.NUM$		&
\	SW%=0% 								&

1000	!----------------------------------------------------RESTART POINT &
	IF FNG%(REG.CH%(SW%),START.KEY$(SW%))=0%			&
	THEN	SEARCH%=1%						&
\		KEY.POINTER%(SW%,SEARCH%)=FNR(REG.CH%(SW%)) 		&
\		DATA.POINTER%(SW%,SEARCH%)=FNR(REG.CH%(SW%)+1%)		&
\		GOTO 1010 						&

1002	PRINT #CH%, G.OFF$; 						&
\	GOSUB 1060 							&
\	GOTO 1030 							&

1005	IF SW%=0% 							&
	THEN	IF 	FNN%(REG.CH%(SW%)) 				&
		THEN	GOTO 18910 					&
		ELSE	KEY.POINTER%(SW%,1%)=FNR(REG.CH%(SW%)) 		&
\			DATA.POINTER%(SW%,1%)=FNR(REG.CH%(SW%)+1%) 	&
\		GOTO 1015 						&

1006	IF RECORD.END%(SW%)=1% 						& 
	THEN	GOTO 18910 						&
	ELSE	SEARCH%=TOP.LINE%(SW%)+WINDOW%(SW%) 			&
\		GOTO 1015 						&

1010	GOSUB 1060 							&

1015	GOSUB 6000 							&

1030	CHNG.FLAG%=0% 							&
\	PRINT #CH%, CLRBOT$;G.OFF$;'COMMAND: Add Change Blank Erase Find '+  &
		'Next Restore Window Quit ';		&
\	OPT$=CVT$$(FNINP$(CH%,128%,' ',1%,1%),32%) 			&
\	OPT$='A' IF OPT$='' 						&
\	GOTO 1030 IF KEY.POINTER%(0%,1%)=0% AND INSTR(1%,'AQ',OPT$)=0% &
\	GOTO 1050 IF OPT$<>'Q' 						&

1040	PRINT #CH%, CLSCN$;FNSR$('1;24');FNP$('1;75');B.ON$;BLINK.ON$; 	&
		R.ON$;'WAIT';G.OFF$;FNP$('24;1');FNX%('',0%,''); 	&

1050	GOTO 1000 	IF OPT$='R' 		! Restore		&
\	GOTO 1005 	IF OPT$='N' 		! Next			&
\	GOTO 2000 	IF OPT$='A' 		! Add			&
\	GOTO 2000 	IF OPT$='F' 		! Find			&
\	GOTO 2400 	IF OPT$='E' 		! Erase 		&
\	GOSUB 1055 	IF OPT$='W'		! Window		&
\	GOTO 2200 	IF OPT$='C' OR OPT$='B' ! Change, Blank		&
\	GOTO 1030							&

1055	PSW%=SW% 							&
\	SW% = SW%+1%-INT((SW%+1%)/(SUM.REG%+1%))*(SUM.REG%+1%)  	&
\	PRINT #CH%,R.ON$+FRM$(PSW%)+B.ON$+FRM$(SW%)+G.OFF$;  		&
\	PRINT #CH%, FNSR$(NUM1$(BASE.TOP%(SW%))+';'+			&
		NUM1$(BASE.BOT%(SW%))); 				&
\	RETURN								&

1060	PRINT #CH%,G.OFF$;						&
\	RETURN IF SW%>0% 						&
\	PRINT #CH%, COLM.OFF$;R.ON$;LEFT(TITLE$,39%); 			&
		SPACE$(40%-LEN(TITLE$));SPACE$(40%-LEN(COMPANY$)); 	&
		COMPANY$;FRAME$;FRM$(1%);FNP$('10;2');LDS.ON$; &
		'LINxTYPExVENNUMxITMx  AMOUNTx'+SPACE$(50%); &
		FNP$('14;2');'    ';FRM$(2%);FNP$('15;2');'ITMxPROD #xDESCRIPT  xSTOx'+ &
		'LOT # xACCT #  x     QTYx   PRICExTxPOUNDx       EXT';USASCII$; &
		FNP$('20;1');'     Ending Item'+SPACE$(44%)+'BALANCE'+ &
		SPACE$(13%);G.OFF$; &
\	PRINT #CH%,FNP$('02;02');'(01)*Order #';&
			FNP$('03;02');'(02) INVOICE #'; &
			FNP$('03;41');'(03) Inv Date';	&
			FNP$('04;02');'(04) SOLD TO';	&
			FNP$('05;02');'(05) SHIP TO';	&
			FNP$('06;02');'(06) Cust PO';	&
			FNP$('07;02');'(07) Sold by';	&
			FNP$('08;02');'(08) CARRIER';&
			FNP$('06;41');'(09) Shp Date';	&
			FNP$('07;41');'(10) Terms'; 	&
			FNP$('08;41');'(11) FOB';&
\	RETURN &

2000	!-------------------------------------------------------KEY SEARCH &
	ORIG.SW%=SW% 							&
\	IF	OPT$='A' 	THEN	OPTION$='ADD '  		&
				ELSE	OPTION$='FIND ' 		&

2005	GOTO 2100 IF SW%>0% 						&

2010	GOTO 2015 IF OPT$='F' 						&
\	EXT,TOT=0. 							&
\	PRINT #CH%,R.ON$;						&
\	FOR J%=1% TO SUM.REG%	 					&
\		TOP.LINE%(J%),LAST.LINE%(J%)=0% 			&
\		LINE.NO%=BASE.TOP%(J%)	 				&
\		PRINT #CH% USING FNP$(NUM1$(BASE.BOT%(J%)+1%)+';2')+'###',0% &
\		PRINT #CH% USING FNP$(NUM1$(BASE.BOT%(J%)+1%)+';71')+ 	&
			'######.##',0.; IF J%=2%  			&
\		PRINT #CH%,FNP$(NUM1$(LINE.NO%+II%)+';1')+CLRLIN$+ 	&
			FNP$(NUM1$(LINE.NO%+II%)+';1')+' '+		&
			FNP$(NUM1$(LINE.NO%+II%)+';80')+' '; 		&
					FOR II%=0% TO WINDOW%(J%)-1% 	&
\	NEXT J% 							&
\	PRINT #CH%,G.OFF$;						&

2015	INP$=''								&
\	FOR LOOP%=LOOP.DATA%(SW%)-LOOP.REG%(SW%)+1% TO LOOP.DATA%(SW%) 	&
\		INP$=DEFAULT$(LOOP%) IF OPT$='A' 			&
\		GOSUB 6030 						&
\	NEXT LOOP% 							&

2020	GOSUB 6010 FOR LOOP% = 1% TO KEY.REG.0%				&
\	SEARCH.KEY$=PWJH.ORDNUM$+''					&

2030	IF 	OPT$='F' OR FNG%(PWJH.CH%,SEARCH.KEY$)=0% 		&
	THEN	KEY.POINTER%(SW%,1%)=FNR(REG.CH%(SW%)) 			&
\		DATA.POINTER%(SW%,1%)=FNR(REG.CH%(SW%)+1%) 		&
\		GOTO 1015 						&

2050  	GOSUB 6010 FOR LOOP%=KEY.REG.0%+1% TO LOOP.DATA%(SW%)		&
\	LSET PWJH.LINE$(1%)=CVT%$(0%)					&
\	LSET PWJH.LINE$(2%)=CVT%$(0%)					&
\	IF 	FNA%(REG.CH%(SW%),REG.BUF$(SW%)) 			&
	THEN	ON SW%+1% GOSUB 4000, 4001, 4001			&
\		GOTO 1000 						&

2090	KEY.POINTER%(0%,1%)=FNR(REG.CH%(0%)) 				&
\	DATA.POINTER%(0%,1%)=FNR(REG.CH%(0%)+1%) 			&

2099	GOSUB 1055							&

2100	!----------------------------------------------ADD INTO THE WINDOWS &
	IF OPT$='A' 							&
		THEN	SEARCH%,ORIG.LIN%=LAST.LINE%(SW%) 		&
		ELSE	PRINT #CH%,CLRBOT$;OPTION$;' Line No. '; 	&
\			SEARCH%=VAL(FNINP$(CH%,128%,' ',3%,1%)) 	&
\			GOTO 1030 IF SEARCH%=0% 			&
\			GOTO 2100 IF SEARCH%<1% OR SEARCH%>LAST.LINE%(SW%) &
\			GOSUB 18500 					&
\			GOTO 2100 					&

2120	GOSUB 18500 							&
\	LIN%=LAST.LINE%(SW%) 						&
\	LINE.NO%=LINE.NO%+SGN(BASE.BOT%(SW%)-LINE.NO%) IF LAST.LINE%(SW%)<>0% &
\	DEFAULT$(LOOP%)=''						&
		FOR LOOP%=LOOP.DATA%(SW%)-LOOP.REG%(SW%)+1% TO LOOP.DATA%(SW%)&
	
2130	LINE.NO$=NUM1$(LINE.NO%) 					&
\	PRINT #CH%,FNLIN$(SW%,LINE.NO$);				&

2140	FOR LOOP%=LOOP.DATA%(SW%)-LOOP.REG%(SW%)+1% TO LOOP.DATA%(SW%) 	&
\		INP$=DEFAULT$(LOOP%) 					&
\		GOSUB 6030 						&
\	NEXT LOOP% 							&

2145	IF SW%=1% 							&
	THEN	LSET PWJL.PRONUM$=''					&
\		LSET PWJL.DESC$=''					&
\		RSET PWJL.ACCNUM$=''					&
\		LSET PWJL.QTY$=CVTF$(0.)				&
\		LSET PWJL.PRICE$=CVT$(0.)				&
\		LSET PWJL.PRTYPE$=''					&

2150	FOR LOOP%=LOOP.DATA%(SW%-1%)+1% TO LOOP.DATA%(SW%) 		&
\		GOSUB 6010 						&
\		GOTO 2170 IF INP$='~~PF1' 				&
\	NEXT LOOP% 							&
	
2155    GOSUB 2190							& 

2160	PRINT #CH%,FNP$(LINE.NO$+';1');CHR$(10%); 			&
\	KEY.POINTER%(SW%,LIN%)=FNR(REG.CH%(SW%)) 			&
\	DATA.POINTER%(SW%,LIN%)=FNR(REG.CH%(SW%)+1%) 			&
\	LAST.LINE%(SW%)=LAST.LINE%(SW%)+1% 				&
\	LINE.NO%=LINE.NO%+SGN(BASE.BOT%(SW%)-LINE.NO%) 			&
\	PRINT #CH% USING FNP$(NUM1$(BASE.BOT%(SW%)+1%)+';2')+R.ON$+	&
		'###'+G.OFF$, LIN%;					&
\	GOSUB 6920 IF SW%=2%						&
\	GOTO 2130 							&

2170	PRINT #CH%,G.OFF$+FNP$(LINE.NO$+';2');SPACE$(78%);		&
\	LIN%=LIN%-1% 							&
\	TOP.LINE%(SW%)=LAST.LINE%(SW%)-WINDOW%(SW%)+2% 			&
\	TOP.LINE%(SW%)=SGN(LAST.LINE%(SW%)) IF TOP.LINE%(SW%)<=0% 	&
\	RECORD.END%(SW%)=1% 						&

2175	V%=FNG%(-REG.CH%(0%),NUM1$(KEY.POINTER%(0%,1%)))+ 		&
		FNG%(-REG.CH%(0%)-1%,NUM1$(DATA.POINTER%(0%,1%))) 	&
\	LSET REG.BUF$(0%)=FNL$ 						&
\	START.KEY$(SW%)=PWJH.ORDNUM$+NUM1$(SW%)+''			&
\	LSET PWJH.LINE$(SW%)=CVT%$(LAST.LINE%(SW%)) 		!*******&
\	IF FNU%(REG.CH%(0%),REG.BUF$(0%)) 				&
			THEN	ON SW%+1% GOSUB 4000, 4001, 4001	&
\				GOTO 1000 				&

2180 	GOTO 2099 IF SW%+1%<=SUM.REG% AND ORIG.SW%=0% 			&
\	GOSUB 1055 IF ORIG.SW%=0%					&
\	GOTO 1030 							&

2190	!----------------------------------Add Record to all Windows    &
	LSET PWJL.ORDNUM$=PWJH.ORDNUM$					&
\	LSET PWJL.WINDOW$=NUM1$(SW%)					&
\	ON SW%+1% GOSUB 4000, 4001, 4001 IF FNA%(REG.CH%(SW%),REG.BUF$(1%)) &
\	RETURN 								&

2200	!------------------------------------------CHANGE AND BLANK RECORD &
	GOSUB 2910 							&
\	IF 	INP$='' 	THEN	1030	ELSE	1010 		&

2400	!-----------------------------------------------------ERASE RECORD &
	GOSUB 2920 							&
\	IF 	INP$='Y' AND SW%=0%   THEN	1005	ELSE	1030 	&

2900	!-------------------------------------------------UPDATE A RECORD &
	GOTO 2905 IF SW%>0%						&
\	V%=FNG%(-REG.CH%(0%),NUM1$(KEY.POINTER%(0%,1%)))+ 		&
		FNG%(-REG.CH%(0%)-1%,NUM1$(DATA.POINTER%(0%,1%))) 	&
\	V%=FNU%(-REG.CH%(0%),REG.BUF$(0%)) IF LOOP%<KEY.REG.0%+1% AND FNS%=0% &
\	V%=FNU%(REG.CH%(0%),REG.BUF$(0%))  IF LOOP%>KEY.REG.0% AND FNS%=0% &
\	RETURN IF LOOP%>KEY.REG.0%					&
\	START.KEY$(SW%)=PWJH.ORDNUM$+NUM1$(SW%)+''			&

2902	FOR I%=1% TO SUM.REG%						&
\		GOSUB 2906						&
\	NEXT I%								&
\	RETURN								&
	
2905	V%=FNG%(-REG.CH%(SW%),NUM1$(KEY.POINTER%(SW%,SEARCH%)))+ 	&
		FNG%(-REG.CH%(SW%)-1%,NUM1$(DATA.POINTER%(SW%,SEARCH%)))&
\	V%=FNU%(REG.CH%(SW%),REG.BUF$(1%)) IF FNS%=0%			&
\	RETURN								&

2906	!--------------------------------------Update to the 1st Window &
	FOR SEARCH%=1% TO LAST.LINE%(I%)				&
\		V%=FNG%(-REG.CH%(I%),NUM1$(KEY.POINTER%(I%,SEARCH%)))+	&
		  FNG%(-REG.CH%(I%)-1%,NUM1$(DATA.POINTER%(I%,SEARCH%)))&
\		LSET REG.BUF$(1%)=FNL$					&
\		LSET PWJL.ORDNUM$=PWJH.ORDNUM$+''			&
\		V%=FNU%(-REG.CH%(I%),REG.BUF$(1%)) IF FNS%=0%		&
\	NEXT SEARCH%							&
\	RETURN 								&

2910	!---------------------------------------CHANGE AND BLANK SUBROUTINE &	
	IF 	OPT$='B' 						&
	THEN	OPTION$ = 'BLANK ' 					&
	ELSE	OPTION$ = 'CHANGE ' 					&
\		CHNG.FLAG% = -1% 					&

2911	GOTO 2915 IF SW%=0% 						&
\	PRINT #CH%, CLRBOT$;OPTION$+' Item Nr. '; 			&
\	INP$=CVT$$(FNINP$(CH%,128%,' ',3%,1%),32%) 			&
\	GOTO 2919 IF INP$='' 						&
\	SEARCH%=VAL(INP$)						&
\	GOTO 2911 IF SEARCH%<1% OR SEARCH%>LAST.LINE%(SW%) 		&
\	GOSUB 18500 							&

2914	EXT=FNZ(CVT$F(PWJL.EXT$),2%) IF SW%=2%				&
	
2915	PRINT #CH%, CLRBOT$;'Item '+COMMAND$(SW%)+':'; 			&
\	INP$=CVT$$(FNINP$(CH%,128%,' ',2%-SGN(SW%),1%),32%) 		&
\	GOTO 2911 IF SW%>0% 	IF INP$=''				&
\	GOTO 2919 IF SW%=0% 	IF INP$=''				&
\	LOOP%=(INSTR(1%,COMMAND$(SW%),' '+INP$+' ')+2%)/3% 		&
\	GOTO 2915 IF LOOP%=0%						&
\	LOOP%=LOOP.DATA%(SW%)-LOOP.REG%(SW%)+LOOP%+SGN(SW%)		&
\	GOTO 2915 IF LOOP%<1% OR LOOP%>LOOP.DATA%(SW%)			&
\	IF 	OPT$='C' 						&
	THEN	GOSUB 6010 						&
	ELSE	INP$='' 						&
\		GOSUB 6030 						&

2917	IF SW%=2% &
	THEN	GOSUB 6950 &
\		LSET PWJL.EXT$ = &	
			CVTF$(FNZ(CVT$F(PWJL.PRICE$)*CVT$F(PWJL.QTY$),2%)) &
\		LSET PWJL.EXT$ = &
			CVTF$(FNZ(0.01*CVT$F(PWJL.PRICE$)*CVT$F(PWJL.POUNDS$),2%)) &
				IF PWJL.PRTYPE$='W' &

2918	GOSUB 2900 							&
\	IF FNS% &
	THEN	ON SW%+1% GOSUB 4000, 4001, 4001			&
	ELSE 	GOTO 2914 IF SW%<2% 					&
\		GOSUB 6009 FOR LOOP%=LOOP.DATA%(SW%-1%)+2% TO LOOP.DATA%(SW%) &
\		GOSUB 6920 						&
\		GOTO 2914 						&

2919	EXT=0. &
\	RETURN 								&

2920	!--------------------------------------------------ERASE SUBROUTINE &
	GOSUB 1055 IF SW%>0%						&
\	GOSUB 1055 IF SW%>0%						&
\	PRINT #CH%, CLRBOT$;'Confirm deletion (Yes/No) '; 		&
\	INP$=CVT$$(FNINP$(CH%,128%,' ',1%,1%),32%) 			&
\	GOTO 2925 IF INP$<>'Y' 						&

2922	FOR I%=1% TO SUM.REG%						&
\		GOSUB 2926						&
\	NEXT I%								&

2924	V%=FNG%(-REG.CH%(0%),NUM1$(KEY.POINTER%(0%,1%)))+ 		&
		FNG%(-REG.CH%(0%)-1%,NUM1$(DATA.POINTER%(0%,1%))) 	&
\	IF	FND%(REG.CH%(0%),'') 					&
	THEN	ON SW%+1% GOSUB 4000, 4001, 4001			&
	ELSE	PRINT #CH%,FNP$('24;1');CLRLIN$;'Record has been erased.'; &
			FNP$('24;55');'Hit any key to continue '; 	&
\		NW$=FNINP$(CH%,128%,' ',1%,1%) 				&
\		KEY.POINTER%(0%,1%)=0%					&
\		DATA.POINTER%(0%,1%)=0%					&

2925	RETURN 								&

2926	!-----------------------------Erase Records from the 1st Window &
	FOR SEARCH%=1% TO LAST.LINE%(I%)				&
\		V%=FNG%(-REG.CH%(I%),NUM1$(KEY.POINTER%(I%,SEARCH%)))+	&
		  FNG%(-REG.CH%(I%)-1%,NUM1$(DATA.POINTER%(I%,SEARCH%)))&
\		ON I%+1% GOSUB 4000, 4001, 4001 IF FND%(REG.CH%(I%),'') &
\		KEY.POINTER%(I%,SEARCH%)=0%				&
\		DATA.POINTER%(I%,SEARCH%)=0%				&
\	NEXT SEARCH%							&
\	RETURN 								&

4000	!---------------------------------------------------MESSAGE OUTPUT &
	V$=FNMESS$(CH%,FNS%,' PWJH.DAT ',0%,-1%) 		!*******&
\	RETURN								&

4001	V$=FNMESS$(CH%,FNS%,' PWJL.DAT ',0%,-1%) 		!*******&
\	RETURN								&

6000	!------------------------------------DATA MAINTENANCE GOSUB SECTION &
	SEARCH%	   = 1%	IF SW%=0%					&
\	RETURN IF DATA.POINTER%(SW%,SEARCH%)=0% OR  			&
		FNG%(-REG.CH%(SW%),NUM1$(KEY.POINTER%(SW%,SEARCH%)))+	&
                 FNG%(-REG.CH%(SW%)-1%,NUM1$(DATA.POINTER%(SW%,SEARCH%))) &
\	LSET REG.BUF$(SGN(SW%))=FNL$+'' 				&
\	ORIG.SW%=SW% 							&
\	PRINT.LOOP%=0% 							&

6003	IF SW%=0% 							&
	THEN	GOSUB 6009 FOR LOOP%=1% TO LOOP.DATA%(SW%) 		&

6005	GOSUB 6050 							&
\	GOSUB 1055 IF ORIG.SW%=0%					&
\	RETURN 								&

6009	ON LOOP% GOSUB	6220, 6760, 6420, 6240, 6320, 6560, 6540, 6480, 6440,&
			6580, 6820, &
			6880, 6280, 6300, 6500, 6520, &
			6880, 6600, 6620, 6260, 6460, 6680, 6700, 6720, 6900, &
			6660, 6740 &
\	RETURN &

6010	ON LOOP% GOSUB	6210, 6760, 6420, 6230, 6310, 6550, 6530, 6470, 6430,&
			6570, 6810, &
			6870, 6270, 6290, 6490, 6510, &
			6870, 6590, 6610, 6250, 6450, 6670, 6690, 6710, 6890, &
			6650, 6730 &
\	RETURN &

6030	ON LOOP% GOSUB	6217, 6757, 6417, 6237, 6317, 6557, 6537, 6477, 6437, &
			6577, 6817, &
			6877, 6277, 6297, 6497, 6517, &
			6877, 6597, 6617, 6257, 6457, 6677, 6697, 6717, 6897, &
			6657, 6737 &
\	RETURN &

6050	!------------------------------------------------PRINT LINE RECORDS &
	IF ORIG.SW%>0% 							&
	THEN	LIN%=SEARCH%-1% IF START.KEY$(SW%)=LEFT(FNL$,LEN(START.KEY$(SW%)))&
	ELSE	GOSUB 1055 						&
\		TOT,EXT=0.		 				&
\		START.KEY$(SW%)=PWJH.ORDNUM$+NUM1$(SW%)+''		&
\		LIN%,TOP.LINE%(SW%)=0% 					&
\		PRINT.LOOP%,LAST.LINE%(SW%)=CVT$%(PWJH.LINE$(SW%)) 	&
\		PRINT #CH% USING FNP$(NUM1$(BASE.BOT%(SW%)+1%)+';2')+R.ON$+ &
			'###'+G.OFF$, LAST.LINE%(SW%);			&
\		V%=FNG%(REG.CH%(SW%),START.KEY$(SW%)) 			&

6054	PRINT.LOOP%=WINDOW%(SW%) IF PRINT.LOOP%<WINDOW%(SW%) 		&
\	PRINT.LOOP%=MOVE.LOOP%   IF MOVE.LOOP%<>0% 			&

6055	DIFF%,RECORD.END%(SW%)=0% 					&
\	LINE.NO$=NUM1$(BASE.BOT%(SW%)) 					&
\	COLUMN$=FNLIN$(SW%,LINE.NO$)					&

6060	FOR LOP%=1% TO PRINT.LOOP% 					&
\		V%=FNG%(REG.CH%(SW%),START.KEY$(SW%)+CVT%$(LIN%+1%)) 	&
			IF START.KEY$(SW%)<>LEFT(FNL$,LEN(START.KEY$(SW%)))&
\		IF FNS% 						&
		THEN	RECORD.END%(SW%)=1% 				&
\			PRINT #CH%, R.ON$;				&
!\			PRINT #CH%, FNP$(LINE.NO$+';2');NUM1$(FNS%);'*';&
!		START.KEY$(SW%);'*';LEFT(FNL$,LEN(START.KEY$(SW%)));'*';&	
!		NUM1$(LEN(START.KEY$(SW%)));				&
\			PRINT #CH%, FNP$(LINE.NO$+';1')+CHR$(10%)+      &
				CLRLINE$+FNP$(LINE.NO$+';1')+' '+	&
				FNP$(LINE.NO$+';80')+' ';		&
				FOR II%=1% TO WINDOW%(SW%)+1%-LOP% 	&
\			PRINT #CH%, G.OFF$;				&
\			GOTO 6090 					&

6065		PRINT #CH%, FNP$(LINE.NO$+';1')+CHR$(10%); 		&
			IF DIFF%<WINDOW%(SW%)-1% 			&
		
6070		LSET REG.BUF$(1%)=FNL$ 				&
\		GOTO 6075 IF DIFF%>WINDOW%(SW%)-2% 			&
\		PRINT #CH%,COLUMN$;					&
\		GOSUB 6009 FOR LOOP%=LOOP.DATA%(SW%-1%)+1% TO LOOP.DATA%(SW%) &

6075		IF ORIG.SW%=0% 						&
		THEN	KEY.POINTER%(SW%,LOP%)=FNR(REG.CH%(SW%)) 	&
\			DATA.POINTER%(SW%,LOP%)=FNR(REG.CH%(SW%)+1%) 	&
\			GOSUB 6920					&

6080		LIN%=LIN%+1% 						&
\		TOP.LINE%(SW%)=LIN% IF LOP%=1% 				&
\		DIFF%=LIN%-TOP.LINE%(SW%) 				&
\		V%=FNN%(REG.CH%(SW%)) 					&
\	NEXT LOP% 							&

6085	TOP.LINE%(SW%)=TOP.LINE%(SW%)-(WINDOW%(SW%)-PRINT.LOOP%) 	&
			IF MOVE.LOOP%<>0%				&
\	RECORD.END%(SW%)=1% IF TOP.LINE%(SW%)+WINDOW%(SW%)>LAST.LINE%(SW%) &

6090	IF ORIG.SW%=0% &
	THEN	GOTO 6050 IF SW%+1%<=SUM.REG% AND ORIG.SW%=0% 		&

6095	RETURN 								&

6200	!-----------------------------------------------DATA ENTRY SECTION &

6210	INP$=FNSIO$('','2;17',PWJH.ORDNUM$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.ORDNUM$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	GOTO 6220 IF LEFT(INP$,2%)='~~' &
\	GOTO 6210 IF INP$='' AND OPT$='A' &
\	IF OPT$='C' AND INP$<>'' &
	THEN	IF FNG%(PWJH.CH%,INP$+SPACE$(6%-LEN(INP$)))=0% &
		THEN	PRINT #CH%, CLRBOT$;FNP$('24;1');INP$;' is a duplicate key'; &
				FNP$('21;1');'COMMAND: Reenter (Y/n)'; &
\			INP$=FNINP$(CH%,128%,' ',1%,0%) &
\			GOTO 6210 IF INP$='Y' &

6217	LSET PWJH.ORDNUM$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6220	PRINT #CH%, FNP$('2;17');B.ON$;PWJH.ORDNUM$;G.OFF$; &
\	RETURN &

6230	INP$=FNSIO$('','4;17',PWJH.SOLDTO$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.SOLDTO$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	GOTO 6240 IF LEFT(INP$,2%)='~~' &
\	GOTO 6230 IF INP$='' AND OPT$='A' &
\	IF INP$<>'' &
	THEN	IF FNG%(CUSTOM.CH%,INP$+SPACE$(6%-LEN(INP$))) &
		THEN	PRINT #CH%, CLRBOT$;FNP$('24;1');INP$;' Undefined '; &
			'Customer number ';FNP$('21;1');'COMMAND: Reenter (Y/n)'; &
\			AA$=FNINP$(CH%,128%,' ',1%,0%) &
\			GOTO 6230 IF AA$='Y' &

6237	LSET PWJH.SOLDTO$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6240	CUST.NAM$=SPACE$(25%) &
\	CUST.NAM$=MID(FNL$,7%,25%) IF FNG%(CUSTOM.CH%,PWJH.SOLDTO$)=0% AND &
				CVT$$(PWJH.SOLDTO$,-1%)<>'' &
\	PRINT #CH%, FNP$('4;17');B.ON$;PWJH.SOLDTO$; &
\	PRINT #CH%, FNP$('4;25');CUST.NAM$;G.OFF$; &
\	RETURN &

6250	INP$=FNSIO$('',LINE.NO$+';24',PWJL.STONUM$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJL.STONUM$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	GOTO 6260 IF LEFT(INP$,2%)='~~' &
\	IF OPT$='A' &
	THEN	DEFAULT$(LOOP%)=INP$ &
\		IF FNG%(PWJACC.CH%,INP$+SPACE$(3%-LEN(INP$))+'  ')=0% &
		THEN	RSET PWJL.ACCNUM$=MID(FNL$,6%,8%) &
\			GOSUB 6680 &

6257	LSET PWJL.STONUM$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6260	PRINT #CH%,FNP$(LINE.NO$+';24')+B.ON$+PWJL.STONUM$+G.OFF$; &
\	RETURN &

6270	INP$=FNSIO$('',LINE.NO$+';7',PWJL.STONUM$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJL.STONUM$)-1%)+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	GOTO 6280 IF LEFT(INP$,2%)='~~' &
\	IF INSTR(1%,' BR FR OP BC FC ',' '+INP$+' ')=0% &
	THEN	PRINT #CH%,CLRBOT$;FNP$('23;1');"'BR' - Broker  'FR' - Freight"+ &
		" 'OP' - Outside Purchase";FNP$('24;1');"'BC' - Broker credit"+&
		" 'FR' - Freight credit "+ FNP$('21;1')+&
		'COMMAND: Re-enter ';FNP$('24;55');'Hit any key to continue '; &
\		AA$=FNINP$(CH%,128%,' ',1%,0%) &
\		GOTO 6270 &

6277	LSET PWJL.STONUM$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6280	PRINT #CH%, FNP$(LINE.NO$+';7');B.ON$;LEFT(PWJL.STONUM$,2%);G.OFF$; &
\	RETURN &

6290	INP$=FNSIO$('',LINE.NO$+';11',PWJL.LOTNUM$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJL.LOTNUM$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	GOTO 6300 IF LEFT(INP$,2%)='~~' &

6297	LSET PWJL.LOTNUM$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6300	PRINT #CH%, FNP$(LINE.NO$+';11');B.ON$;PWJL.LOTNUM$;G.OFF$; &
\	RETURN &

6310	INP$=FNSIO$('','5;17',PWJH.SHIPTO$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.SHIPTO$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	GOTO 6320 IF LEFT(INP$,2%)='~~' &

6317	LSET PWJH.SHIPTO$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6320	CUST.NAM$=SPACE$(25%) &
\	CUST.NAM$=MID(FNL$,7%,25%) IF FNG%(CUSTOM.CH%,PWJH.SHIPTO$)=0% AND &
				CVT$$(PWJH.SHIPTO$,-1%)<>'' &
\	PRINT #CH%, FNP$('5;17');B.ON$;PWJH.SHIPTO$; &
\	PRINT #CH%, FNP$('5;25');CUST.NAM$;G.OFF$; &
\	RETURN &

6410	INP$=FNSIO$('','3;59',PWJH.INVDAT$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			CHR$(8%)+CHR$(6%)+CHR$(4%)+CHR$(0%)+OPTION$+' DATE') &
\	GOTO 6420 IF LEFT(INP$,2%)='~~' &

6417	LSET PWJH.INVDAT$=CVT%$(FND6%(INP$)) IF INP$<>'' OR CHNG.FLAG%=0% &

6420	PRINT #CH%, FNP$('3;59');B.ON$;FND6$(CVT$%(PWJH.INVDAT$));G.OFF$; &
\	RETURN &

6430	INP$=FNSIO$('','6;59',PWJH.SHPDAT$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			CHR$(8%)+CHR$(6%)+CHR$(4%)+CHR$(0%)+OPTION$+' DATE') &
\	GOTO 6440 IF LEFT(INP$,2%)='~~' &
\	DEFAULT$(LOOP%)=INP$ &

6437	LSET PWJH.SHPDAT$=CVT%$(FND6%(INP$)) IF INP$<>'' OR CHNG.FLAG%=0% &

6440	PRINT #CH%, FNP$('6;59');B.ON$;FND6$(CVT$%(PWJH.SHPDAT$));G.OFF$; &
\	RETURN &

6450	INP$=FNSIO$('',LINE.NO$+';28',PWJL.LOTNUM$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJL.LOTNUM$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	GOTO 6460 IF LEFT(INP$,2%)='~~' &

6457	LSET PWJL.LOTNUM$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6460	PRINT #CH%, FNP$(LINE.NO$+';28');B.ON$;PWJL.LOTNUM$;G.OFF$; &
\	RETURN &

6470	INP$=FNSIO$('','08;17',PWJH.CARNAM$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.CARNAM$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	GOTO 6480 IF LEFT(INP$,2%)='~~' &

6477	LSET PWJH.CARNAM$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6480	PRINT #CH%, FNP$('08;17');B.ON$;PWJH.CARNAM$;G.OFF$; &
\	RETURN &

6490	INP$=FNSIO$('###',LINE.NO$+';18',PWJL.POUNDS$,DEFAULT$(LOOP%), &
			CHR$(CH%)+CHR$(3%)+CHR$(3%)+CHR$(3%)+CHR$(0%)+ &
			OPTION$+' VALUE') &
\	GOTO 6500 IF LEFT(INP$,2%)='~~' &
\	DEFAULT$(LOOP%)='' &

6497	LSET PWJL.POUNDS$=CVTF$(VAL(INP$)) IF INP$<>'' OR CHNG.FLAG%=0% &

6500	PRINT #CH% USING FNP$(LINE.NO$+';18')+B.ON$+'###'+G.OFF$, &
		CVT$F(PWJL.POUNDS$); &
\	RETURN &

6510	INP$=FNSIO$('#####.##',LINE.NO$+';22',PWJL.EXT$,DEFAULT$(LOOP%), &
			CHR$(CH%)+CHR$(8%)+CHR$(7%)+CHR$(5%)+CHR$(2%)+ &
			OPTION$+' VALUE') &
\	GOTO 6520 IF LEFT(INP$,2%)='~~' &

6517	LSET PWJL.EXT$=CVTF$(FNZ(VAL(INP$),2%)) IF INP$<>'' OR CHNG.FLAG%=0% &

6520	PRINT #CH% USING FNP$(LINE.NO$+';22')+B.ON$+'#####.##'+G.OFF$, &
		CVT$F(PWJL.EXT$); &
\	RETURN &

6530	INP$=FNSIO$('','7;17',PWJH.SOLDBY$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.SOLDBY$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	GOTO 6540 IF LEFT(INP$,2%)='~~' &

6537	LSET PWJH.SOLDBY$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6540	PRINT #CH%, FNP$('7;17');B.ON$;PWJH.SOLDBY$;G.OFF$; &
\	RETURN &

6550	INP$=FNSIO$('','06;17',PWJH.CUSPO$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.CUSPO$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	GOTO 6560 IF LEFT(INP$,2%)='~~' &

6557	LSET PWJH.CUSPO$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6560	PRINT #CH%, FNP$('06;17');B.ON$;PWJH.CUSPO$;G.OFF$; &
\	RETURN &

6570	INP$=FNSIO$('','7;59',PWJH.TERMS$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.TERMS$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	GOTO 6580 IF LEFT(INP$,2%)='~~' &
\	DEFAULT$(LOOP%)=INP$ IF OPT$='A' &

6577	LSET PWJH.TERMS$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6580	PRINT #CH%, FNP$('7;59');B.ON$;PWJH.TERMS$;G.OFF$; &
\	RETURN &

6590	INP$=FNSIO$('',LINE.NO$+';6',PWJL.PRONUM$,DEFAULT$(LOOP%), &
		CHR$(CH%)+CHR$(6%)+CHR$(15%)+CHR$(1%)+CHR$(0%)+ &
		OPTION$+' ALPHA') &
\	GOTO 6600 IF LEFT(INP$,2%)='~~' &

6597	LSET PWJL.PRONUM$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6600	PRINT #CH%, FNP$(LINE.NO$+';6');B.ON$;LEFT(PWJL.PRONUM$,6%);G.OFF$; &
\	RETURN &

6610	IF FNG%(INVDES.CH%,PWJL.PRONUM$) &
	THEN	INP$=FNSIO$('',LINE.NO$+';13',PWJL.DESC$,DEFAULT$(LOOP%), &
			CHR$(CH%)+CHR$(10%)+CHR$(LEN(PWJL.DESC$))+CHR$(1%)+ &
			CHR$(0%)+OPTION$+' ALPHA') &
	ELSE	INP$=MID(FNL$,16%,26%) &

6611	GOTO 6620 IF LEFT(INP$,2%)='~~' &

6617	LSET PWJL.DESC$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6620	PRINT #CH%, FNP$(LINE.NO$+';13');B.ON$;LEFT(PWJL.DESC$,10%);G.OFF$; &
\	RETURN &

6650	GOSUB 6950 IF OPT$='A' &
\	INP$=FNSIO$('#####',LINE.NO$+';64',PWJL.POUNDS$,DEFAULT$(LOOP%), &
			CHR$(CH%)+CHR$(5%)+CHR$(5%)+CHR$(5%)+CHR$(0%)+ &
			OPTION$+' VALUE') &
\	GOTO 6660 IF LEFT(INP$,2%)='~~' &
\	DEFAULT$(LOOP%)='' &

6657	LSET PWJL.POUNDS$=CVTF$(VAL(INP$)) IF INP$<>'' OR CHNG.FLAG%=0% &

6660	PRINT #CH% USING FNP$(LINE.NO$+';64')+B.ON$+'#####'+G.OFF$, &
		CVT$F(PWJL.POUNDS$); &
\	RETURN &

6670	DEFAULT$(LOOP%)=PWJL.ACCNUM$+'' IF OPT$='A' &
\	INP$=FNSIO$('',LINE.NO$+';35',PWJL.ACCNUM$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJL.ACCNUM$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	GOTO 6680 IF LEFT(INP$,2%)='~~' &
\	DEFAULT$(LOOP%)='' &

6677	RSET PWJL.ACCNUM$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6680	PRINT #CH%, FNP$(LINE.NO$+';35');B.ON$;PWJL.ACCNUM$;G.OFF$; &
\	RETURN &

6690	INP$=FNSIO$('#####.##',LINE.NO$+';44',PWJL.QTY$,DEFAULT$(LOOP%), &
		CHR$(CH%)+CHR$(8%)+CHR$(7%)+CHR$(5%)+CHR$(2%)+ &
			OPTION$+' VALUE') &
\	GOTO 6700 IF LEFT(INP$,2%)='~~' &

6697	IF INP$<>'' OR CHNG.FLAG%=0% &
	THEN	LSET PWJL.QTY$=CVTF$(VAL(INP$)) &

6700	PRINT #CH% USING FNP$(LINE.NO$+';44')+B.ON$+'#####.##'+G.OFF$, &
		CVT$F(PWJL.QTY$); &
\	RETURN &

6710	INP$=FNSIO$('####.###',LINE.NO$+';53',PWJL.PRICE$,DEFAULT$(LOOP%), &
			CHR$(CH%)+CHR$(8%)+CHR$(7%)+CHR$(5%)+CHR$(3%)+ &
			OPTION$+' VALUE') &
\	GOTO 6720 IF LEFT(INP$,2%)='~~' &

6717	IF INP$<>'' OR CHNG.FLAG%=0% &
	THEN	LSET PWJL.PRICE$=CVTF$(VAL(INP$)) &

6720	PRINT #CH% USING FNP$(LINE.NO$+';53')+B.ON$+'####.###'+G.OFF$, &
		CVT$F(PWJL.PRICE$); &
\	RETURN &

6730	IF OPT$='A' &
	THEN	DEFAULT$(LOOP%)=NUM1$(CVT$F(PWJL.PRICE$)*CVT$F(PWJL.QTY$))  &
\		LSET PWJL.EXT$ =CVTF$(FNZ(CVT$F(PWJL.PRICE$)*CVT$F(PWJL.QTY$),2%)) &
\		IF PWJL.PRTYPE$='W' &
		THEN	DEFAULT$(LOOP%)=NUM1$(0.01*CVT$F(PWJL.PRICE$)*CVT$F(PWJL.POUNDS$)) &
\			LSET PWJL.EXT$ = &
			 CVTF$(FNZ(0.01*CVT$F(PWJL.PRICE$)*CVT$F(PWJL.POUNDS$),2%)) &

6731	INP$=FNSIO$('#######.##',LINE.NO$+';70',PWJL.EXT$,DEFAULT$(LOOP%), &
			CHR$(CH%)+CHR$(10%)+CHR$(9%)+CHR$(5%)+CHR$(2%)+ &
			OPTION$+' VALUE') &
\	GOTO 6740 IF LEFT(INP$,2%)='~~' &
\	DEFAULT$(LOOP%)='' &

6737	LSET PWJL.EXT$=CVTF$(FNZ(VAL(INP$),2%)) IF INP$<>'' OR CHNG.FLAG%=0% &

6740	PRINT #CH% USING FNP$(LINE.NO$+';70')+B.ON$+'#######.##'+G.OFF$, &
		CVT$F(PWJL.EXT$); &
\	RETURN &

6750	INP$=FNSIO$('','3;17',PWJH.INVNUM$, &
		DEFAULT$(LOOP%),CHR$(CH%)+STRING$(2%,LEN(PWJH.INVNUM$))+ &
		CHR$(1%)+CHR$(0%)+OPTION$+' ALPHA') &
\	GOTO 6760 IF LEFT(INP$,2%)='~~' &

6757	RSET PWJH.INVNUM$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6760	PRINT #CH%, FNP$('3;17');B.ON$;PWJH.INVNUM$;G.OFF$; &
\	RETURN &

6810	INP$=FNSIO$('','8;59',PWJH.FOBFLG$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJH.FOBFLG$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	GOTO 6820 IF LEFT(INP$,2%)='~~' &
\	IF INSTR(1%,' D S ',' '+INP$+' ')=0% &
	THEN	PRINT #CH%,CLRBOT$;FNP$('24;1');"'D' - Destination "+ &
		"'S' - Shipping Point";FNP$('21;1'); &
		'COMMAND: Re-enter ';FNP$('24;55');'Hit any key to continue '; &
\		AA$=FNINP$(CH%,128%,' ',1%,0%) &
\		GOTO 6810 &

6817	LSET PWJH.FOBFLG$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6820	PRINT #CH%, FNP$('8;59');B.ON$;PWJH.FOBFLG$;G.OFF$; &
\	RETURN &

6870	LIN%=LIN%+1% &
\	INP$=NUM1$(LIN%) &

6877	LSET PWJL.LINE$=CVT%$(VAL(INP$)) IF INP$<>'' OR CHNG.FLAG%=0% &

6880	PRINT #CH% USING FNP$(LINE.NO$+';2')+B.ON$+'###'+G.OFF$, &
		CVT$%(PWJL.LINE$); &
\	RETURN &

6890	INP$=FNSIO$('',LINE.NO$+';62',PWJL.PRTYPE$,DEFAULT$(LOOP%), &
		CHR$(CH%)+CHR$(1%)+CHR$(1%)+CHR$(1%)+CHR$(0%)+ &
		OPTION$+' ALPHA') &
\	GOTO 6900 IF LEFT(INP$,2%)='~~' &
\	DEFAULT$(LOOP%)=INP$ IF OPT$='A' &
\	IF INSTR(1%,' U W ',' '+INP$+' ')=0% &
	THEN	PRINT #CH%,CLRBOT$;FNP$('24;1');"'U' - Price/Unit 'W' - "+ &
		"Price/100Wgt";FNP$('21;1'); &
		'COMMAND: Re-enter ';FNP$('24;55');'Hit any key to continue '; &
\		AA$=FNINP$(CH%,128%,' ',1%,0%) &
\		GOTO 6890 &

6897	LSET PWJL.PRTYPE$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6900	PRINT #CH%, FNP$(LINE.NO$+';62');B.ON$;PWJL.PRTYPE$;G.OFF$; &
\	RETURN &

6920	!------------------------------------------NUMBER OF RECORD AND TOTAL &

6930	TOT=TOT+FNZ(CVT$F(PWJL.EXT$),2%)-EXT &

6940	PRINT #CH% USING FNP$('20;71')+R.ON$+'######.##'+G.OFF$+FNP$('21;1'), &
					TOT; &
\	EXT=0. &
\	RETURN &

6950	DEFAULT$(LOOP%)='' &
\	RETURN IF FNG%(INVDES.CH%,PWJL.PRONUM$) &
\	LSET PWJL.DESC$=MID(FNL$,16%,26%) &
\	PACK$          =MID(FNL$,46%,4%) &
\	FOR I%=1% TO PACK.INDEX% &
\		IF UNIT$(I%)=PACK$ &	
		THEN	DEFAULT$(LOOP%)=NUM1$(CVT$F(PWJL.QTY$)*WEIGHT(I%)) &
\			LSET PWJL.POUNDS$=CVTF$(CVT$F(PWJL.QTY$)*WEIGHT(I%)) &
\			GOTO 6952 &

6951	NEXT I% &

6952	RETURN &

18500	!---------------------------------------------------LINE SUBROUTINE &

18505	IF SEARCH%<TOP.LINE%(SW%)-WINDOW%(SW%)		! JUMP BACKWARD	&
	THEN	GOSUB 6000 						&
\		LINE.NO%=BASE.TOP%(SW%)					&
\		GOTO 18550						&

18510	IF SEARCH%>=TOP.LINE%(SW%)+2%*WINDOW%(SW%)	! JUMP FOREWARD	&
	THEN	SEARCH%=SEARCH%-WINDOW%(SW%)+1%				&
\		GOSUB 6000 						&
\		SEARCH%=LIN%						&
\		LINE.NO%=BASE.BOT%(SW%)					&
\		GOTO 18550						&

18515	IF SEARCH%<TOP.LINE%(SW%)			! MOVE BACKWARD &
	THEN	PRINT.LOOP%=TOP.LINE%(SW%)-SEARCH%			&
\		LINE.NO%=BASE.TOP%(SW%)					&
\		LINE.NO$=NUM1$(LINE.NO%) 				&
\		COLUMN$=FNLIN$(SW%,LINE.NO$)				&
\		LIN%=TOP.LINE%(SW%)					&
\		FOR LOP%=1% TO PRINT.LOOP%				&
\			V%=FNG%(-REG.CH%(SW%),NUM1$(KEY.POINTER%(SW%,LIN%-1%)))+&
        		 FNG%(-REG.CH%(SW%)-1%,NUM1$(DATA.POINTER%(SW%,LIN%-1%))) &
\			PRINT #CH%,FNP$(LINE.NO$+';1')+INSERT.LIN$;	&
\			PRINT #CH%,COLUMN$;				&
\			LSET REG.BUF$(1%)=FNL$ 			&
\			GOSUB 6009 FOR LOOP%=LOOP.DATA%(SW%-1%)+1% 	&
					TO LOOP.DATA%(SW%) &
\			LIN%=LIN%-1%					&
\		NEXT LOP%						&
\		TOP.LINE%(SW%)=SEARCH%					&
\		RECORD.END%(SW%)=0% IF TOP.LINE%(SW%)+WINDOW%(SW%)<=LAST.LINE%(SW%) &
	
18520	IF SEARCH%>=TOP.LINE%(SW%)+WINDOW%(SW%)		! MOVE FOREWARD &
	THEN	MOVE.LOOP%=SEARCH%-(TOP.LINE%(SW%)+WINDOW%(SW%)-1%)	&
\		SEARCH%=TOP.LINE%(SW%)+WINDOW%(SW%)			&
\		GOSUB 6000 						&
\		SEARCH%=LIN%						&
\		MOVE.LOOP%=0%						&
\		LINE.NO%=BASE.BOT%(SW%)					&

18525	IF TOP.LINE%(SW%)<=SEARCH% AND 					&
		SEARCH%<TOP.LINE%(SW%)+WINDOW%(SW%)	       ! SCREEN &
	THEN	LINE.NO%=BASE.TOP%(SW%)+(SEARCH%-TOP.LINE%(SW%))	&

18550	V%=FNG%(-REG.CH%(SW%),NUM1$(KEY.POINTER%(SW%,SEARCH%)))+	&
        	FNG%(-REG.CH%(SW%)-1%,NUM1$(DATA.POINTER%(SW%,SEARCH%)))&
\	LSET REG.BUF$(1%)=FNL$ IF FNS%=0%				&
\	PRINT #CH%,FNP$(NUM1$(LINE.NO%)+';1');CHR$(10%); 		&
				IF OPT$='A' AND LINE.NO%=BASE.BOT%(SW%) &
\	LINE.NO$=NUM1$(LINE.NO%) 					&
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

30000	!---------------------------------------TERMINAL INPUT 		&
	DEF FNINP$(CHN%,KYP%,FILLCHAR$,INPUTLEN%,TO.ERR%)		&
\		PRINT #CHN%, STRING$(INPUTLEN%,ASCII(FILLCHAR$)); 	&
			  STRING$(INPUTLEN%,8%); 			&
\		PRINT #CHN%, RECORD 256%,CHR$(KYP%+INPUTLEN%)+FILLCHAR$;&
\		GET #CHN% 						&
\		FIELD #CHN%, RECOUNT AS BUFFER$ 			&
\		IF ASCII(BUFFER$)=128% OR ASCII(BUFFER$)=8% OR 		&
			ASCII(BUFFER$)=23% 				&
		THEN	BUFFER$='~~PF1' IF MID(BUFFER$,2%,4%)='[21~' OR  &
				MID(BUFFER$,2%,2%)='OP'			&
\			BUFFER$='~~PF2' IF MID(BUFFER$,2%,4%)='[19~'   &
				OR MID(BUFFER$,2%,2%)='OQ' 		&
\			BUFFER$='~~PF3' IF MID(BUFFER$,2%,2%)='OR'	&
\			BUFFER$='~~PF4' IF MID(BUFFER$,2%,2%)='OS'	&

30010		BUFFER$='%^C' IF INSTR(1%,BUFFER$,CHR$(3%)) 		&
\		FNINP$=CVT$$(BUFFER$,4%) 				&
\		V=SQR(-1) IF BUFFER$='%^C' AND TO.ERR% !^C Trappping	&
\	FNEND 								&

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

30400	!-------------------------------------------------DATE HANDLERS &
	DEF FND8%(D8)=D8 						&

30410	DEF FND6%(D9$)=VAL(MID(D9$,3%,2%))+VAL(LEFT(D9$,2%))*32% 	&
		+ FND8%(VAL(RIGHT(D9$,5%)))*512% 			&

30420	DEF FND6$(D9%)=RIGHT(NUM1$((D9% AND 15%*32%)/32%+100%),2%) 	&
		+ '/'+RIGHT(NUM1$((D9% AND 31%)+100%),2%) 		&
		+ '/'+RIGHT(NUM1$(((SWAP%(D9%) AND 254%)/2%)+100%),2%) 	&

30500	DEF FNZ(Y,N%)=SGN(Y)*10.^(-N%)*INT(ABS(Y)*10.^N%+.5001)  	&

30550	!------------------------------PRINT LINE IN THE WINDOW	&
	DEF FNLIN$(SW%,LINE.NO$)					&
\		COLUMN$=''						&
\		COLUMN$=COLUMN$+FNP$(LINE.NO$+';'+MID(LINE.POS$(SW%),CC%,3%))+&
			CHR$(120%) 					&
			FOR CC%=5% TO LEN(LINE.POS$(SW%))-4% STEP 4% 	&
\		FNLIN$= FNP$(LINE.NO$+';1')+R.ON$+' '+G.OFF$+LDS.ON$+	&
			COLUMN$+R.ON$+FNP$(LINE.NO$+';80')+' '+G.OFF$+  &
			USASCII$&
\	FNEND								&

30700	!---------------------------------------DATA MAINT FUNCTION	&
	DEF*FNSIO$(FRMAT$,ROWCOL$,VARABLE$,DEFAL$,ITEMS$)		&
\		CHN%=ASCII(MID(ITEMS$,1%,1%))				&
\		KIND%=ASCII(MID(ITEMS$,4%,1%))				&
\		DECI=ASCII(MID(ITEMS$,5%,1%))				&
\		OPT$=MID(ITEMS$,6%,1%)					&

30705		PRINT #CHN%, FNP$(ROWCOL$);R.ON$;B.ON$;			&
\		IF 	OPT$='C' OR DEFAL$<>''				&
		THEN	ON KIND% GOTO 30710, 30715, 30720, 30725, 30730	&
		ELSE	PRINT #CHN%, SPACE$(ASCII(MID(ITEMS$,2%,1%)));	&
\			GOTO 30735					&

30710		PRINT #CHN%, LEFT(VARABLE$,ASCII(MID(ITEMS$,2%,1%)));	&
\		GOTO 30735						&

30715		PRINT #CHN% USING FRMAT$,VARABLE$;  			&
\		GOTO 30735						&

30720		PRINT #CHN% USING FRMAT$, CVT$%(VARABLE$)/(10.**DECI);	&
\		GOTO 30735				   ! INTEGER	&

30725		PRINT #CHN%, FND6$(CVT$%(VARABLE$));	   ! DATE	&
\		GOTO 30735						&

30730		PRINT #CHN% USING FRMAT$,CVT$F(VARABLE$);  ! FLOAT	&

30735		PRINT #CHN%,G.OFF$;CLRBOT$;RIGHT(ITEMS$,6%)+':  ';B.ON$;&
\		FIN$ = FNINP$(CHN%,0%,'_',ASCII(MID(ITEMS$,3%,1%)),1%)	&
\		GOTO 30745 IF LEFT(FIN$,2%)='~~' &
\		V% = VAL(FIN$) IF KIND%=3%				&
\		V  = VAL(FIN$) IF KIND%>3%				&
\		IF 	FIN$=''						&
		THEN	FIN$ = DEFAL$ IF INSTR(1%,'CF',OPT$)=0%		&
\			GOTO 30745					&

30740		IF 	KIND%=3% OR KIND%=5%				&
		THEN	TEMP = 1.					&
\			TEMP = 10.**DECI IF INSTR(1%,FIN$,'.')=0%	&
\			FIN$ = NUM1$(VAL(FIN$)/TEMP)			&
\			FIN$ = FIN$ + '.' IF INSTR(1%,FIN$,'.')=0%	&

30745		FNSIO$ = FIN$+''					&
\	FNEND								&

32767	END &

