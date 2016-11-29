10	!---------------------------------------------------------------&
	! SALES REPORT FROM THE G/L					&
	!								&
	!								&
	! Author - Randall Beard, Computer Management Center, Inc.	&
	!								&
	! Files-GL(MMM).DAS and GL(MMM).(YY)S				&
	!		-ISAM	General Ledger File			&
	!								&
	!---------------------------------------------------------------&
	! 								&
	! 								&
	!		      Copyright (c) 1984 by 			&
	!	  Computer Management Center, Idaho Falls, Idaho	&
	! 								&
	! This software is furnished under a license and may be used	&
	! and copied only in accordance with the terms of such license 	&
	! and with the inclusion of the above copyright notice.  This 	&
	! software or any other copies therof may not be provided or 	&
	! otherwise made available to any other person.  No title to 	&
	! and ownership of the software is hereby transferred.		&
	! 								&
	! The information in this software is subject to change without &
	! notice and should not be construed as a committment by 	&
	! Computer Management Center. 					&
	! 								&
	! CMC assumes no responsibility for the use or reliability of 	&
	! its software on equipment which is not supported by CMC. 	&
	! 								&
	!---------------------------------------------------------------&

30	ON ERROR GOTO 19000 &
\	JUNK$ = SYS(CHR$(6%) + CHR$(-7%)) &
	! &
\	TEMP$	= SYS(CHR$(12%)) 			!LAST FILE OPENED &
\	PRJPRG$	= MID(TEMP$,23%,2%)+NUM1$(ASCII(MID(TEMP$,25%,1%))) + ':'+   &
		'['+NUM1$(ASCII(MID(TEMP$,6%,1%)))+','+NUM1$(ASCII(MID(TEMP$,&
		5%,1%)))+']'+RAD$(ASCII(MID(TEMP$,7%,1%))+SWAP%(ASCII(MID(   &
		TEMP$,8%,1%))))+RAD$(ASCII(MID(TEMP$,9%,1%))+SWAP%(ASCII(MID(&
		TEMP$,10%,1%))))   &
	! &
\	JUNK$ = SYS(CHR$(6%) + CHR$(9%))	! SYS CALL FOR JOB &
\	JJ%  = ASCII(LEFT(JUNK$,1%))/2%		! GET THE JOB NUMBER &
\	JJ$  = RIGHT(NUM1$(JJ%+100%),2%)	! SET THE JOB INTO A STRING &

60	CH%,OUTPUT.CH%=1%		! Keyboard channel &
\	OPEN 'KB:' AS FILE 1%, MODE 8%+256% &
					!    8% - Echo Control &
					!   16% - Disable hibernation, Cntl-C &

70	ESC$=CHR$(155%)			! Escape code for VT100 control &
\	CLSCN$=ESC$+'[H'+ESC$+'[J'	! Clear screen &
\	COLM.ON$=ESC$+'[?3h'		! 132 Column mode &
\	COLM.OFF$=ESC$+'[?3l'		! 80 Column mode &
\	CLRLIN$=ESC$+'[2K'		! Erase entire line &
\	ENTER.COPY$=ESC$+'[5i'		! Enter media copy &
\	EXIT.COPY$=ESC$+'[4i'		! Exit media copy &

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

100	DEVICE.CH%,PRNT.TMP%	= 02% &
\	GLMMM.CH%		= 02% &
\	TEMPJJ.CH%		= 04% &
\	MENU.CH%		= 12% &

120	!============================================================== &
	! OPEN THE PRINT CONTROL FILE					&
	!============================================================== &
	OPEN 'PRNT'+JJ$+'.TMP' FOR INPUT AS FILE PRNT.TMP% &
\	NEXT.REPORT%=0% &
\	CHAIN.PROGRAM$='' &
\	CHAIN.LINE%=0% &

130	INPUT LINE #PRNT.TMP%, A$ &
\	PR$=LEFT(A$,2%) &
\	CMD$=CVT$$(RIGHT(A$,4%),140%) &
\	IF PR$='RN' &
	THEN	IF NEXT.REPORT% &
		THEN	160 &
			ELSE	NEXT.REPORT%=-1% &

140	LIN.PER.PAGE%	= VAL(CMD$) IF PR$='LP' ! LINES PER PAGE &
\	SPAGE%		= VAL(CMD$) IF PR$='SP' ! START PAGE &
\	EPAGE%		= VAL(CMD$) IF PR$='EP'	! END PAGE &
\	COPIES%		= VAL(CMD$) IF PR$='CP'	! NUMBER OF COPIES &
\	REPDATE$	= CMD$ IF PR$='RD'	! REPORT DATE &
\	DISPLAY$	= CMD$ IF PR$='DP'	! DISPLAY (Y/N) &
\	AUTOSCROLL$	= CMD$ IF PR$='AS'	! AUTOSCROLL (Y/N) &
\	SPOOLR$		= CMD$ IF PR$='SL'	! SPOOLER NAME &
\	OUTDEV$		= CMD$ IF PR$='OD'	! OUTPUT DEVICE &
\	MS$		= CMD$ IF PR$='MS'	! MESSAGE &
\	U1$		= CMD$ IF PR$='U1'	! UDF 1 &
\	U2$		= CMD$ IF PR$='U2'	! UDF 2 &
\	U3$		= CMD$ IF PR$='U3'	! UDF 3 &
\	U4$		= CMD$ IF PR$='U4'	! UDF 4 &
\	U5$		= CMD$ IF PR$='U5'	! UDF 5 &
		! IF A FILE IS TO BE CREATED THEN THE &
		! VARIABLE SRTKEY MUST BE SET TO 'S' &
		! \	SRTKEY$='S' IF U?$='????' &
\	IF PR$='PC' &
	THEN	TEMP%=1% &
\		TEMP%=2% IF LEFT(CMD$,1%)='\' &
\		PC$=PC$+CHR$(VAL(MID(CMD$,TEMP%+(LOOP%-1%)*4%,3%))) &
			FOR LOOP%=1% TO LEN(CMD$)/4% &
		! PRINTER CONTROL ESCAPE SEQUENCE &
		! FORMAT '\###\###\###\###' &
		! '\' IS THE DELIMITER &
		! '###' IS THE ASCII VALUE TO PRINT &

150	GOTO 130 &

160	GOTO 190 IF CHAIN.FLAG%=0% &
		! IF FILE IS TO SORT SKIP KILL FILE &
\	KILL 'PRNT'+JJ$+'.TMP' &
		! KILL THE PRNTJJ.TMP FILE &
\	GOTO 190 IF PR$<>'RN' &
		! SKIP CREATING PRINT WORK FILE IF THERE ISN'T &
		! ANOTHER REPORT TO PRINT &

170	OPEN 'PRNT'+JJ$+'.TMP' FOR OUTPUT AS FILE PRNT.TMP%+1% &
		! OPEN A NEW PRINT WORK FILE FOR THE NEXT &
		! SERIES OF REPORTS TO BE PRINTED &
\	CHAIN.LINE%=31000% &

180	PRINT #PRNT.TMP%+1%, A$; &
\	INPUT LINE #PRNT.TMP%, A$ &
\	CHAIN.PROGRAM$=CVT$$(RIGHT(A$,4%),140%) IF LEFT(A$,2%)='PG' &
		AND CHAIN.PROGRAM$='' &
\	GOTO 180 &

190	! SET USER DEFINED FIELDS &
	START.ACCT$=U1$ &
\	END.ACCT$=U2$ &
\	MONTH$=LEFT(U3$,3%) &
\	YEAR$=RIGHT(U3$,4%) &

200	IF FNO%(DEVICE.CH%,'DEVICE.DAT','/RO','')=0% &
	THEN	GLMMM.DEVICE$=MID(FNL$,7%,20%)+'' &
			IF FNG%(DEVICE.CH%,'GLMMM')=0% &
\		V%=FNC%(DEVICE.CH%) &

210	GOTO 300 IF CHAIN.FLAG% &
\	V%=FNO%(GLMMM.CH%,GLMMM.DEVICE$+'GL'+MONTH$+'.'+YEAR$+'S', &
		'/RO/SF/NS','') &
\	V%=FNO%(GLMMM.CH%,GLMMM.DEVICE$+'GL'+MONTH$+'.DAS','/RO/SF/NS','') &
		IF FNS%=5% &
\	IF FNS% &
	THEN	V$=FNMESS$(CH%,FNS%,'GL'+MONTH$+'.DAS'+FNP$('24;55'),0%,-1%) &
\		GOTO 17540 &

220	IF FNO%(TEMPJJ.CH%,GLMMM.DEVICE$+'TEMP'+JJ$+'.TMP','/SF/CR:128','') &
	THEN	V$=FNMESS$(CH%,FNS%,'TEMP'+JJ$+'.TMP'+FNP$('24;55'),0%,-1%) &
\		GOTO 17540 &

230	GOTO 290 IF FNG%(GLMMM.CH%,'') &

240	FIELD #GLMMM.CH%, FNL% AS TEMP$, &
			08% AS GL.ACCNO$, &
			02% AS GL.SOURCE$, &
			16% AS GL.REFNO$, &
			02% AS GL.TRANDAT$, &
			26% AS GL.DESC$, &
			08% AS GL.AMOUNT$, &
			06% AS GL.XREFNO$, &
			02% AS GL.POSTIM$, &
			02% AS GL.POSDAT$, &
			06% AS GL.BNKCDE$, &
			08% AS GL.CKNO$, &
			06% AS GL.VCHRNO$, &
			06% AS GL.SUBACC$, &
			06% AS GL.PHASE$, &
			08% AS GL.REGQTY$, &
			08% AS GL.PREQTY$, &
			02% AS GL.UPDATE$, &
			04% AS TEMP$, &
			02% AS GL.POINTER$ &
\	FIELD #GLMMM.CH%, FNL% AS TEMP$, &
			128% AS GL.BUF$ &
\	IF CVT$%(GL.POINTER$)>0% &
	THEN	IF START.ACCT$<=CVT$$(GL.ACCNO$,-1%) AND &
				CVT$$(GL.ACCNO$,-1%)<=END.ACCT$ &
		THEN	V%=FNA%(TEMPJJ.CH%,GL.BUF$) &

250	GOTO 240 IF FNN%(GLMMM.CH%)=0% &
\	V%=FNX%(PRJPRG$,30999%,'') &

300	IF FNO%(TEMPJJ.CH%,GLMMM.DEVICE$+'TEMP'+JJ$+'.TMP','/SF','') &
	THEN	V$=FNMESS$(CH%,FNS%,'TEMP'+JJ$+'.TMP'+FNP$('24;55'),0%,-1%) &
\		GOTO 17540 &

400	OPEN 'MENU.FIL/RO' FOR INPUT AS FILE MENU.CH% &
\	DIM #12%, A0$(1%)=64% &
\	COMPANY$=CVT$$(RIGHT(A0$(1%),2%),128%) &
\	CLOSE MENU.CH% &

450	! RESUME IF ERROR &

510	TITLE$='SALES REPORT FROM G/L' &
\	REPDATE$='FOR '+MONTH$+', '+YEAR$ &
\	PAGE.TOP$=STRING$(3%,10%)+CHR$(13%)+ &
		'Date: '+DATE$(0%)+SPACE$(50-LEN(COMPANY$)/2%)+ &
		COMPANY$+SPACE$(57%-LEN(COMPANY$)/2%)+'Page <<###>>'+ &
		CHR$(10%)+CHR$(13%)+ &
		'Time: '+TIME$(0%)+SPACE$(51%-LEN(TITLE$)/2%)+ &
		TITLE$+SPACE$(57%-LEN(TITLE$)/2%)+'V1.0'+ &
		CHR$(10%)+CHR$(13%)+SPACE$(65%-LEN(REPDATE$)/2%)+REPDATE$+ &
		STRING$(2%,10%)+CHR$(13%)+ &
		STRING$(131%,61%)+CHR$(10%)+CHR$(13%) &
\	PAGE.TOP1$=	'ACCT #    '+ &
			'TRANDATE  '+ &
			'SC  '+ &
			'REFERENCE   '+ &
			'DESCRIPTION           '+ &
			'XREF #  '+ &
			'SUBCD   '+ &
			'       QTY'+ &
			'        AMOUNT' &
\	USE.1$=		'\      \  '+ &
			'\      \  '+ &
			'\\  '+ &
			'\        \  '+ &
			'\                  \  '+ &
			'\    \  '+ &
			'\    \  '+ &
			'###,###.##'+ &
			' ##,###,###.##' &
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

17000	!-------------------------------------------------OUTPUT REPORT &
	GOTO 17500 IF FNG%(TEMPJJ.CH%,'') &
\	IF 	DISPLAY$='Y' &
	THEN	PRINT #OUTPUT.CH%, CLSCN$;R.ON$;COLM.ON$;PAGE.TOP1$;G.OFF$; &
			FNSR$('2;19');FNP$('19;01'); &
	ELSE	PRINT #OUTPUT.CH%, ENTER.COPY$;PC$;	&
			FNPAGE$(LIN.PER.PAGE%,0%,1%,PAGE.TOP$+PAGE.TOP1$,''); &
\		LINE.COUNT%=FNLINE% &

17030	FIELD #TEMPJJ.CH%, FNL% AS TEMP$, &
			08% AS TEMPJJ.ACCNO$, &
			02% AS TEMPJJ.SOURCE$, &
			16% AS TEMPJJ.REFNO$, &
			02% AS TEMPJJ.TRANDAT$, &
			26% AS TEMPJJ.DESC$, &
			08% AS TEMPJJ.AMOUNT$, &
			06% AS TEMPJJ.XREFNO$, &
			02% AS TEMPJJ.POSTIM$, &
			02% AS TEMPJJ.POSDAT$, &
			06% AS TEMPJJ.BNKCDE$, &
			08% AS TEMPJJ.CKNO$, &
			06% AS TEMPJJ.VCHRNO$, &
			06% AS TEMPJJ.SUBACC$, &
			06% AS TEMPJJ.PHASE$, &
			08% AS TEMPJJ.REGQTY$, &
			08% AS TEMPJJ.PREQTY$, &
			02% AS TEMPJJ.UPDATE$, &
			04% AS TEMP$, &
			02% AS TEMPJJ.POINTER$ &
\	QTY=CVT$F(TEMPJJ.REGQTY$)+CVT$F(TEMPJJ.PREQTY$) &
\	AMT=CVT$F(TEMPJJ.AMOUNT$) &
\	GOSUB 18000 IF TEST.ACCNO$<>TEMPJJ.ACCNO$ AND TEST.ACCNO$<>'' &
\	TEST.ACCNO$=TEMPJJ.ACCNO$+'' &
\	PRINT #OUTPUT.CH% USING USE.1$,	&
				CVT$$(TEMPJJ.ACCNO$,140%), &
				FND6$(CVT$%(TEMPJJ.TRANDAT$)), &
				CVT$$(TEMPJJ.SOURCE$,132%), &
				CVT$$(TEMPJJ.REFNO$,140%), &
				CVT$$(TEMPJJ.DESC$,132%), &
				CVT$$(TEMPJJ.XREFNO$,140%), &
				CVT$$(TEMPJJ.SUBACC$,140%), &
				-QTY, &
				-AMT &
\	ACCT.QTY.TOTAL=ACCT.QTY.TOTAL-QTY &
\	ACCT.AMT.TOTAL=ACCT.AMT.TOTAL-AMT &
\	GRND.QTY.TOTAL=GRND.QTY.TOTAL-QTY &
\	GRND.AMT.TOTAL=GRND.AMT.TOTAL-AMT &
\	LINE.COUNT%=LINE.COUNT%+1% &
\	GOSUB 17600	! Check the pagination &
\	GOTO 17400 IF END.FLAG% &

17350	IF FNN%(TEMPJJ.CH%)=0% &
 	THEN	GOTO 17030 &

17400	IF END.FLAG%=0% &
	THEN	GOSUB 18000 IF ACCT.QTY.TOTAL<>0.0 OR ACCT.AMT.TOTAL<>0.0 &
\		PRINT #OUTPUT.CH% USING USE.1$,	&
				'', &
				'', &
				'', &
				'', &
				'GRAND TOTAL', &
				'', &
				'', &
				GRND.QTY.TOTAL, &
				GRND.AMT.TOTAL &
\		LINE.COUNT%=LINE.COUNT%+1% &

17500	IF 	DISPLAY$<>'Y' &
	THEN	PRINT #OUTPUT.CH%, FNPAGE$(LIN.PER.PAGE%,LINE.COUNT%,FNPAGE%, &
			'',PAGE.BOT$); &
	ELSE	V$	= FNMESS$(OUTPUT.CH%,0%,'End of report'+ &
				FNP$('24;105'),0%,-1%) &

17540	KILL GLMMM.DEVICE$+'TEMP'+JJ$+'TMP' &

17550	PRINT #OUTPUT.CH%, EXIT.COPY$; IF DISPLAY$<>'Y' &
\	PRINT #OUTPUT.CH%, FNSR$('1;24');COLM.OFF$; IF DISPLAY$='Y' &
\	CLOSE OUTPUT.CH% &
\	V%	= FNX%(CHAIN.PROGRAM$,CHAIN.LINE%,'') &

17600	!---------------------------------------PAGINATION SUBROUTINE &
	IF 	DISPLAY$<>'Y' AND LINE.COUNT%+MORE.LINES%>PAGE.BREAK% &
	THEN	END.FLAG% = -1% IF PAGE%>=EPAGE% AND EPAGE%<>0% &
\		RETURN IF END.FLAG% &
\		PRINT #OUTPUT.CH%, FNPAGE$(LIN.PER.PAGE%,LINE.COUNT%, &
			FNPAGE%,PAGE.TOP$+PAGE.TOP1$,PAGE.BOT$); &
\		LINE.COUNT%	= FNLINE% &
\		IF 	SPAGE%>1% &
		THEN	IF 	SPAGE%<=PAGE% &
			THEN	OUTPUT.CH% = RESET.CH% &
\				PRINT #OUTPUT.CH%, ENTER.COPY$;PC$; &
					FNPAGE$(LIN.PER.PAGE%,0%, &
					PAGE%,PAGE.TOP$+PAGE.TOP1$,''); &
\				LINE.COUNT% = FNLINE% &
\				SPAGE% = 0% &

17650	IF 	DISPLAY$='Y' AND LINE.COUNT%>DISPLAY.CONTROL% &
	THEN	V$	= FNMESS$(OUTPUT.CH%,0%,FNP$('24;105'),-1%,-1%) &
\		LINE.COUNT%	= 0% &
\		PRINT #OUTPUT.CH%, FNP$('24;1');CLRLIN$;FNP$('18;132') &

17670	RETURN &

18000	!=============================================================== &
	! PRINT ACCOUNT TOTAL &
	!=============================================================== &
	PRINT #OUTPUT.CH% USING USE.1$,	&
				'', &
				'', &
				'', &
				'', &
				'ACCOUNT TOTAL', &
				'', &
				'', &
				ACCT.QTY.TOTAL, &
				ACCT.AMT.TOTAL &
\	ACCT.QTY.TOTAL,ACCT.AMT.TOTAL=0.0 &
\	PRINT #OUTPUT.CH% &
\	LINE.COUNT%=LINE.COUNT%+2% &
\	GOSUB 17600 &
\	RETURN &

19000	!------------------------------------------------ERROR TRAPPING &
	RESUME 160 IF ERL=130% &
\	RESUME 190 IF ERL=170% OR ERL=180% &
\	RESUME 450 IF ERL=400% 						&
\	RESUME 70 IF ERL=60% AND ERR=27% 				&
\	RESUME 17550 IF ERL=17540%					&
\	RESUME 17540 IF ERR=54%						&

19010	IF ERR=28% 							&
	THEN	JUNK$ = SYS(CHR$(6%) + CHR$(-7%)) 			&
\		RESUME 17540 						&

19100	IF ERL=120% &
	THEN	PRINT #CH%, FNP$('24;1');CLRLIN$; &
			'Unable to find print control file.  Aborting'; &
			FNP$('24;55');'Hit any key to continue'; &
\		INP$=FNINP$(CH%,128%,' ',1%,0%) &
\		RESUME 17500 &

19900	ON ERROR GOTO 0 						&

30000	!----------------------PROGRAM FUNCTIONS------------------------&
	!------------------------------------------------TERMINAL INPUT &
	DEF FNINP$(CHN%,KYP%,FILLCHAR$,INPUTLEN%,TO.ERR%)		&
\		PRINT #CHN%, STRING$(INPUTLEN%,ASCII(FILLCHAR$)); 	&
			  STRING$(INPUTLEN%,8%); 			&
\		PRINT #CHN%, RECORD 256%,CHR$(KYP%+INPUTLEN%)+FILLCHAR$;&
\		GET #CHN% 						&
\		FIELD #CHN%, RECOUNT AS BUFFER$ 			&
\		BUFFER$='%^C' IF INSTR(1%,BUFFER$,CHR$(3%)) 		&
\		FNINP$=CVT$$(BUFFER$,4%) 				&
\		V=SQR(-1) IF BUFFER$='%^C' AND TO.ERR% !^C Trappping	&
\	FNEND 								&

30200	DEF*FNP$(ROWCOL$)=ESC$+'['+ROWCOL$+'H'	! Direct Cursor Address &

30250	!-----------------------------------------------SYSTEM MESSAGES	&
	DEF*FNMESS$(CHN%,ERRNUM%,DESC$,TO.ERR%,PRINT.TEST%)		&
\		MESG$=DESC$						&
\		MESG$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERRNUM%)),3%,30%)+	&
			' '+MESG$ IF ERRNUM%				&
\		IF PRINT.TEST%						&
		THEN	PRINT #CHN%, FNP$('24;1');CLRLIN$;MESG$;	&
				'Hit any key to continue.';	&
\			NW$=FNINP$(CHN%,128%,' ',1%,TO.ERR%)		&

30260		FNMESS$=MESG$						&
\	FNEND 								&

30300	DEF*FNSR$(BEGEND$)=ESC$+'['+BEGEND$+'r'	! Scroll control 	&

30500	DEF FNI%(Y)=Y 							&

30510	DEF FND6$(D9%)=RIGHT(NUM1$((D9% AND 15%*32%)/32%+100%),2%) 	&
		+ '/'+RIGHT(NUM1$((D9% AND 31%)+100%),2%) 		&
		+ '/'+RIGHT(NUM1$(((SWAP%(D9%) AND 254%)/2%)+100%),2%) 	&

30600	!	********************************************************&
	!	Print top and bottom of page 				&
	!	Format : 						&
	!	PRINT FNPAGE$(<LINES/PAGE>,<CURRENT LINE COUNT>,	&
	!		<PAGE COUNT>,<TOP OF PAGE>,<BOTTOM OF PAGE>) 	&
	!	FNPAGE% = PAGE COUNT 					&
	!	FNLINE% = LINE COUNT 					&
	! 								&
	!	Written by Robert Peterson - July 1981 			&
	!	Version 1 Edition 0 					&
	!	********************************************************&

30610	DEF FNPAGE$(Y0%,Y1%,Y2%,Y0$,Y1$) 				&
\		Y2$='' 							&
\		Y2$=STRING$(Y0%-(Y1%+ 					&
			LEN(XLATE(Y1$,STRING$(10%,0%)+CHR$(10%)))),10%) &
							IF Y1$<>'' 	&
\		PAGE.LINE%=LEN(XLATE(Y0$,STRING$(10%,0%)+CHR$(10%))) 	&
\		Y%=INSTR(1%,Y1$+Y0$,'<<#') 				&
\		Y3%=INSTR(1%,Y1$+Y0$,'#>>') 				&
\		Y$=RIGHT(NUM1$(100000+Y2%),8%-(Y3%-Y%)) 		&
\		Y3%=-3% IF Y%=0% 					&
\		PRINT #OUTPUT.CH%, Y2$;LEFT(Y1$+Y0$,Y%-1%);Y$; 		&
			RIGHT(Y1$+Y0$,Y3%+3%); 				&
\		PAGE%=Y2% 						&
\	FNEND 								&

30650	DEF FNPAGE%=PAGE%+1% 						&

30660	DEF FNLINE%=PAGE.LINE% 						&

30900	!-------------------------------------------------------COMPARE &
	DEF FNCOMP%(Y$,Y2$) 						&
\		Y9%=0% 							&
\		Y9%=-1% IF Y2$='*' 					&
\		Y2$=Y2$+',' 						&

30920		IF Y9%=0% 						&
		THEN	Y1$=LEFT(Y2$,INSTR(1%,Y2$,',')-1%) 		&
\			Y2$=RIGHT(Y2$,LEN(Y1$)+2%) 			&
\			Y1%=INSTR(1%,Y1$,'/') 				&
\			IF Y1%+INSTR(1%,Y1$,'?')=0% 			&
			THEN	Y9%=Y$=Y1$ 				&
			ELSE	IF Y1% 					&
				THEN	Y9%=LEFT(Y1$,Y1%-1%)<=Y$ AND 	&
							Y$<=RIGHT(Y1$, 	&
						Y1%+1%) 		&
				ELSE	CHANGE CVT$$(LEFT(Y$,30%),-1%) 	&
								TO Y% 	&
\					CHANGE CVT$$(LEFT(Y1$,30%),-1%) &
								TO Y1% 	&
\					GOTO 30930 			&
					IF (Y%(Y3%)<>Y1%(Y3%))-(Y1%(Y3%)&
					=63%) FOR Y3%=1% TO Y1%(0%) 	&
\					Y9%=-1% 			&

30930		IF Y2$<>'' AND Y9%=0% 					&
		THEN	GOTO 30920 					&
		ELSE	FNCOMP%=Y9% 					&

30940	FNEND 								&

30999	CHAIN.FLAG%=-1% &

31000	! Chain entry 							&
	GOTO 30 							&

32767	END &

