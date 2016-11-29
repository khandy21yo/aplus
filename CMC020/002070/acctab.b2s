10	!---------------------------------------------------------------&
	!								&
	!	Sku Maintenance Program					&
	!								&
	!	ACCTAB.B2S	V1.0	December 1986			&
	!								&
	! Author - FRANK STARMAN, Computer Management Center, Inc.	&
	!								&
	! Files-PWJACC.DAS	-ISAM					&
	!								&
	!---------------------------------------------------------------&
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

50	ON ERROR GOTO 19000 &
\	DIM DEFAULT$(3%) &

60	OPEN 'KB:' AS FILE #1%, MODE 8%+256% &
					!    8% - Echo Control &
					!   16% - Disable hibernation, Cntl-C &
\	CH%=1%				! Keyboard channel &
\	ESC$=CHR$(155%)			! Escape code for VT100 control &
\	CLSCN$=ESC$+'[H'+ESC$+'[J'	! Clear screen &
\	CLRLIN$=ESC$+'[2K'		! Erase entire line &
\	G.OFF$=ESC$+'[m'		! Select graphic off &
\	B.ON$=ESC$+'[1m'		! Bold face on &
\	BLINK.ON$=ESC$+'[5m'		! Blinking &
\	R.ON$=ESC$+'[7m'		! Reverse video &
\	COLM.ON$=ESC$+'[?3h'		! 132 Column mode &
\	COLM.OFF$=ESC$+'[?3l'		! 80 Column mode &
\	CLRBOT$=ESC$+'[21;1H'+ESC$+'[J'	! Erase cursor to end of screen &
\	LDS.ON$=ESC$+'(0'		! Line drawing set &
\	USASCII$=ESC$+'(B'		! United States ASCII &

80	!COM(THIS) DROP.DEAD.DATE$ = 8, VERSION.NO$ = 6, SERIAL.NO$ = 10 &
	DROP.DEAD.DATE$='        ' &
\	VERSION.NO$='V1.0' &
\	DATE.TIME$=SYS(CHR$(6%)+CHR$(20%)+CHR$(0%)+CHR$(0%)+CHR$(1%)+ &
			CHR$(SWAP%(1%))+STRING$(11%,0%)+CHR$(SWAP%(0%))+ &
			CHR$(1%)+CHR$(SWAP%(1%))) &
\	IF DROP.DEAD.DATE$<>'' &
	THEN	IF DROP.DEAD.DATE$<MID(SYS(DATE.TIME$),7%,8%) &
		THEN	MESG$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(47%)), &
				3%,30%) &
\			PRINT #CH%, MESG$; &
\			V$=SYS(CHR$(9%)) &

100	DEVICE.CH%	= 02% &
\	PWJACC.CH%	= 02% &
\	MENU.CH%   	= 12% &
&
\	LOOP.DATA% = 3% &
\	KEY.LEN%   = 16% &

200	IF FNO%(DEVICE.CH%,'DEVICE.DAT','/RO','')=0% &
	THEN	PWJACC.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) &
			IF FNG%(DEVICE.CH%,'PWJACC')=0% &
\		V%=FNC%(DEVICE.CH%) &

300	V%=FNO%(PWJACC.CH%,PWJACC.DEVICE$+'PWJACC.DAS','/RW/SF','') &
\	V%=FNO%(PWJACC.CH%,PWJACC.DEVICE$+'PWJACC.DAS','/CR:'+ &
	NUM1$(KEY.LEN%)+','+"/SF",'') IF FNS% = 5% &
\	IF FNS% &
	THEN	V$=FNMESS$(CH%,FNS%,'PWJACC.DAS',0%,-1%) &
\		OPT$='Q' &
\		GOTO 1040 &

400	OPEN 'MENU.FIL/RO' FOR INPUT AS FILE MENU.CH% &
\	DIM #12%, A0$(1%)=64% &
\	COMPANY$=CVT$$(RIGHT(A0$(1%),2%),128%) &
\	CLOSE MENU.CH% &

450	OPEN 'NL:' AS FILE 11%, RECORDSIZE 16% &
\	FIELD #11%,	03% AS PWJACC.STORNUM$,	&
			02% AS PWJACC.TYPE$,	&
			08% AS PWJACC.ACCNUM$	&
\	FIELD #11%, 16% AS PWJACC.BUF$ &

500	FRAME$ = FRAME$ + FNP$(NUM1$(I%)+';1')+' '+FNP$(NUM1$(I%)+';80')+' ' &
							FOR I%=2% TO 19% &
\	COMMAND$  = ' ' &
\	COMMAND$  = COMMAND$+RIGHT(NUM1$(I%+100%),2%) + ' ' &
		FOR I%=1% TO LOOP.DATA% &

1000	!--------------------------------------------PROGRAM RESTART POINT &
	IF FNG%(PWJACC.CH%,'')=0% &
	THEN	KEY.POINTER%=FNR(PWJACC.CH%) &
\		GOTO 1010 &

1002	PRINT #CH%, G.OFF$; &
\	GOSUB 1060 &
\	GOTO 1030 &

1005	IF FNN%(PWJACC.CH%) &
	THEN	18910 &
		ELSE	KEY.POINTER%=FNR(PWJACC.CH%) &
\			GOTO 1015 &

1010	PRINT #CH%, COLM.OFF$;FNSR$('1;24');G.OFF$; &
\	GOSUB 1060 &

1015	GOSUB 6000 &

1030	CHNG.FLAG%=0% &
\	PRINT #CH%, CLRBOT$;G.OFF$; &
		'COMMAND: Add Erase Change Blank Initialize Default '; &
		'Find Next Restore ';FNP$('22;10'); &
		'View Quit '; &
\	OPT$=CVT$$(FNINP$(CH%,128%,' ',1%,1%),32%) &
\	OPT$='A' IF OPT$='' &
\	GOTO 1030 IF KEY.POINTER%=0% AND INSTR(1%,'AQD',OPT$)=0% &

1040	IF OPT$='Q' &
	THEN	PRINT #CH%, CLSCN$;FNSR$('1;24'); &
			FNP$('1;75');B.ON$;BLINK.ON$;R.ON$;'WAIT';G.OFF$; &
			FNP$('24;1'); &
\		V%=FNX%('',0%,'') &

1050	GOTO 1000 	IF OPT$='R' 		! Restore	&
\	GOTO 1005 	IF OPT$='N' 		! Next		&
\	GOTO 2000 	IF OPT$='A' OR OPT$='F' ! Add, Find	&
\	GOTO 2200 	IF OPT$='C' OR OPT$='B' ! Change, Blank	&
\	GOTO 2400 	IF OPT$='E' 		! Erase 	&
\	GOTO 2500 	IF OPT$='D' 		! Default	&
\	GOTO 2600 	IF OPT$='I' 		! Initialize	&
\	GOTO 4000	IF OPT$='V' 		! View		&
\	GOTO 1030 		    		&

1060	TEMP$     = 'STORE-VENDOR TABLE Maintenance' &
\	PRINT #CH%, COLM.OFF$;R.ON$;LEFT(TEMP$,39%);SPACE$(40%-LEN(TEMP$)); &
			SPACE$(40%-LEN(COMPANY$));COMPANY$; &
			FRAME$; FNP$('20;1');SPACE$(80%);G.OFF$; &
\	PRINT #CH%, 	FNP$('06;19');'(01) STORE #'; &
			FNP$('08;19');'(02) TYPE'; &
			FNP$('10;19');'(03) ACCOUNT #'; &
\	RETURN &

2000	!-------------------------------------------------SEARCH FOR KEYS &
	OPTION$='ADD ' &
\	OPTION$='FIND ' IF OPT$='F' &
\	INP$='' &
\	FOR LOOP%=1% TO LOOP.DATA% &
\		INP$=DEFAULT$(LOOP%) IF OPT$='A' &
\		GOSUB 6030 &
\	NEXT LOOP% &
\	FOR LOOP%=1% TO 2% &
\		GOSUB 6010 &
\	NEXT LOOP% &
\	SEARCH.KEY$=PWJACC.STORNUM$+PWJACC.TYPE$+'' &
\	IF OPT$='F' OR FNG%(PWJACC.CH%,SEARCH.KEY$)=0% &
		THEN	KEY.POINTER%=FNR(PWJACC.CH%) &
\			GOTO 1015 &

2100	!-----------------------------------------------------ADD RECORD &
  	GOSUB 6010 FOR LOOP%=3% TO LOOP.DATA% &
\	IF FNA%(PWJACC.CH%,PWJACC.BUF$) &
	THEN	V$=FNMESS$(CH%,FNS%,'PWJACC.DAS',0%,-1%) &
\		GOTO 1000 &

2110	KEY.POINTER%=FNR(PWJACC.CH%) &
\	GOTO 1030 &

2200	!----------------------------------------CHANGE OR BLANK RECORD	&
	IF 	OPT$='B' &
	THEN	OPTION$ = 'BLANK ' &
	ELSE	OPTION$ = 'CHANGE ' &
\		CHNG.FLAG% = -1% &

2210	PRINT #CH%, CLRBOT$; OPTION$+':'; &
\	INP$=CVT$$(FNINP$(CH%,128%,' ',2%,1%),32%) &
\	GOTO 1030 IF INP$='' &
\	LOOP%=(INSTR(1%,COMMAND$,' '+INP$+' ')+2%)/3% &
\	GOTO 2210 IF (LOOP%=0% AND OPT$='C') OR (LOOP%<3% AND OPT$='B') &
\	IF OPT$='C' &
	THEN	GOSUB 6010 &
	ELSE	INP$='' &
\		GOSUB 6030 &

2220	GOSUB 2900 &
\	IF 	FNS%=0% &
	THEN	GOTO 2210 &
	ELSE	V$=FNMESS$(CH%,FNS%,'PWJACC.DAS',0%,-1%) &
\		GOTO 1010 &

2400	!------------------------------------------------ ERASE RECORD	&
	PRINT #CH%, CLRBOT$;"Confirm erasure (Yes/No) "; &
\	INP$=CVT$$(FNINP$(CH%,128%," ",1%,1%),32%) &
\	GOTO 1030 IF INP$<>"Y" &

2420	IF FND%(PWJACC.CH%,"") &
	THEN	V$=FNMESS$(CH%,FNS%,'PWJACC.DAS',0%,-1%) &
	ELSE	PRINT #CH%,FNP$('24;1');CLRLIN$;"Record has been erased."; &
			FNP$('24;55');"Hit any key to continue "; &
\		NW$=FNINP$(CH%,128%," ",1%,1%) &

2430	GOTO 1005 &

2500	!-----------------------------------------------SET DEFAULT VALUES &
	OPTION$='DEFAULT ' &
\	FOR LOOP%=1% TO LOOP.DATA% &
\		INP$=DEFAULT$(LOOP%) &
\		GOSUB 6030 &
\	NEXT LOOP% &

2510	PRINT #CH%, CLRBOT$;'Default:'; &
\	INP$=CVT$$(FNINP$(CH%,128%,' ',2%,1%),32%) &
\	GOTO 1010 IF INP$ = '' &
\	LOOP%=(INSTR(1%,COMMAND$,' '+INP$+' ')+2%)/3% &
\	GOTO 2510 IF LOOP%<2% OR LOOP%>LOOP.DATA% &
\	GOSUB 6010 &
\	DEFAULT$(LOOP%)=INP$ &
\	GOTO 2510 &

2600	!-----------------------------------------------INITIALIZATION &
	INITIALIZE$=' ' &
\	INP$='' &
\	GOSUB 6030 FOR LOOP%=1% TO LOOP.DATA% &

2610	PRINT #CH%, CLRBOT$;FNP$('22;1');INITIALIZE$; &
		FNP$('21;1');'Field to initialize:'; &
\	INP$=CVT$$(FNINP$(CH%,128%,' ',2%,1%),32%) &
\	IF INP$<>'' &
	THEN	LOOP%=(INSTR(1%,COMMAND$,' '+INP$+' ')+2%)/3% &
\		INITIALIZE$=INITIALIZE$+INP$+' ' IF &
			LOOP%<>0% AND (INSTR(1%,INITIALIZE$,' '+INP$+' ') &
				+2%)/3%=0% &
\		GOTO 2610 &

2620	GOTO 1010 IF INITIALIZE$='' &
\	OPT$='C' &
\	OPTION$='INITIALIZE ' &
\	CHNG.FLAG%=-1% &
\	INP$='' &
\	GOSUB 6030 FOR LOOP%=1% TO LOOP.DATA% &
\	GOTO 2660 &

2650	GOTO 18910 IF FNN%(PWJACC.CH%) &
\	KEY.POINTER%=FNR(PWJACC.CH%) &

2660	GOSUB 6000 &
\	FOR I%=1% TO LEN(INITIALIZE$)/3% &
\		LOOP%=(INSTR(1%,COMMAND$,MID(INITIALIZE$,I%*3%-2%,3%))+2%)/3% &
\		GOSUB 6010 &
\		IF 	INP$<>'' &
		THEN	GOSUB 2900 &
\			IF 	FNS% &
			THEN	V$=FNMESS$(CH%,FNS%,'PWJACC.DAS',0%,-1%) &
\				GOTO 1010 &

2670	NEXT I% &
\	GOTO 2650 &


2900	!-------------------------------------------------UPDATE A RECORD &
	V%=FNG%(-PWJACC.CH%,NUM1$(KEY.POINTER%)) &
\	V%=FNU%(-PWJACC.CH%,PWJACC.BUF$) &
					IF FNS%=0% &
\	RETURN &

4000	!---------------------------------------80 COLUMN VIEW ROUTINE	&
	NAME.STRING$	= ' STORNUM   TYPE   ACCNUM'+SPACE$(56%) &
\	VIEW.USE$	= ' \ \        \\    \      \' &
\	LINE.POS$	= '001,010,017,080' !Column positions	&
\	VIEW.LINES$	= '' &
\	VIEW.LINES$	= VIEW.LINES$+FNP$('19;'+MID(LINE.POS$,I%,3%))+R.ON$+ &
		LDS.ON$+CHR$(120%)+USASCII$ FOR I%=1% TO LEN(LINE.POS$) STEP 4% &
\	PRINT #CH%,CLRBOT$;FNSR$('11;19');FNP$('19;1');R.ON$;NAME.STRING$; &
		FNP$('20;1');SPACE$(80%);G.OFF$ &
\	PRINTED.LINES%=0% &
\	V%=FNG%(-PWJACC.CH%,NUM1$(KEY.POINTER%)) &

4010	PRINT #CH%,RECORD 252%,CHR$(129%); &
\	GET #CH%,RECORD 8192% &

4020	FIELD #CH%,RECOUNT AS TEST$ &
\	GOTO 1010 IF INSTR(1%,TEST$,CHR$(3%)) &

4030	LSET PWJACC.BUF$=FNL$+'' &
\	PRINT #CH% USING FNP$('19;1')+CHR$(10%)+VIEW.USE$+VIEW.LINES$+G.OFF$, &
		PWJACC.STORNUM$, &
		PWJACC.TYPE$, &
		PWJACC.ACCNUM$, &
\	PRINTED.LINES%=PRINTED.LINES%+1% &
\	GOTO 18910 IF FNN%(PWJACC.CH%) &
\	IF	PRINTED.LINES%>=8% &
	THEN	PRINT #CH%,FNSR$('12;19') &
\		V$=FNMESS$(CH%,0%,'',-1%,-1%) &
\		PRINTED.LINES%=0% &

4040	GOTO 4010 &

6000	!------------------------------------------------DATA ENTRY SECTION &
	RETURN IF FNG%(-PWJACC.CH%,NUM1$(KEY.POINTER%)) &
\	LSET PWJACC.BUF$=FNL$+'' &
\	ON LOOP% GOSUB	6220, 6240, 6260 & 
		FOR LOOP%=1% TO LOOP.DATA% &
\	RETURN &

6010	ON LOOP% GOSUB	6210, 6230, 6250 &
\	RETURN &

6030	ON LOOP% GOSUB	6217, 6237, 6257 &
\	RETURN &

6200	!-----------------------------------------------DATA ENTRY SECTION &

6210	INP$=FNSIO$('','6;38',PWJACC.STORNUM$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJACC.STORNUM$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	GOTO 6210 IF INP$='' AND OPT$='A' &
\	IF 	OPT$='C' OR (OPT$='I' AND INP$<>'') &
	THEN	IF	FNG%(PWJACC.CH%,INP$+SPACE$(3%-LEN(INP$))+ &
					PWJACC.TYPE$)=0% &
		THEN	PRINT #CH%,CLRBOT$;FNP$('24;1');'Duplicate key ';INP$; &
				' ';PWJACC.TYPE$;FNP$('21;1'); &
				'COMMAND: Re-enter Exit '; &
\			IF 	FNINP$(CH%,128%,' ',1%,0%)='R' &
			THEN	GOTO 6210 	ELSE	GOTO 6220 &

6217	LSET PWJACC.STORNUM$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6220	PRINT #CH%, FNP$('6;38');B.ON$;PWJACC.STORNUM$;G.OFF$; &
\	RETURN &

6230	INP$=FNSIO$('','8;38',PWJACC.TYPE$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJACC.TYPE$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	IF 	OPT$='C' OR (OPT$='I' AND INP$<>'') &
	THEN	IF	FNG%(PWJACC.CH%,PWJACC.STORNUM$+INP$+ &
				SPACE$(2%-LEN(INP$)))=0% &
		THEN	PRINT #CH%,CLRBOT$;FNP$('24;1');'Duplicate key '; &
				PWJACC.STORNUM$;' ';INP$; &
				FNP$('21;1');'COMMAND: Re-enter Exit '; &
\			IF 	FNINP$(CH%,128%,' ',1%,0%)='R' &
			THEN	GOTO 6210 	ELSE	GOTO 6220 &

6237	LSET PWJACC.TYPE$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6240	PRINT #CH%, FNP$('8;38');B.ON$;PWJACC.TYPE$;G.OFF$; &
\	RETURN &

6230	INP$=FNSIO$('','8;38',PWJACC.TYPE$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJACC.TYPE$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &
\	IF 	OPT$='C' OR (OPT$='I' AND INP$<>'') &
	THEN	IF	FNG%(PWJACC.CH%,PWJACC.STORNUM$+INP$+ &
				SPACE$(2%-LEN(INP$)))=0% &
		THEN	PRINT #CH%,CLRBOT$;FNP$('24;1');'Duplicate key '; &
				PWJACC.STORNUM$;' ';INP$; &
				FNP$('21;1');'COMMAND: Re-enter Exit '; &
\			IF 	FNINP$(CH%,128%,' ',1%,0%)='R' &
			THEN	GOTO 6230 	ELSE	GOTO 6240 &

6237	LSET PWJACC.TYPE$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6240	PRINT #CH%, FNP$('8;38');B.ON$;PWJACC.TYPE$;G.OFF$; &
\	RETURN &

6250	INP$=FNSIO$('','10;38',PWJACC.ACCNUM$,DEFAULT$(LOOP%),CHR$(CH%)+ &
			STRING$(2%,LEN(PWJACC.ACCNUM$))+CHR$(1%)+CHR$(0%)+ &
			OPTION$+' ALPHA') &

6257	RSET PWJACC.ACCNUM$=INP$ IF INP$<>'' OR CHNG.FLAG%=0% &

6260	PRINT #CH%, FNP$('10;38');B.ON$;PWJACC.ACCNUM$;G.OFF$; &
\	RETURN &

18910	!----------------------------------------------END OF FILE &
	PRINT #CH%, CLRBOT$;FNP$('24;1');'End of file has been reached.'; &
		FNP$('24;55');'Hit any key to continue '; &
\	INP$=FNINP$(CH%,128%,' ',1%,0%) &
\	GOTO 1010 IF OPT$='V' &
\	GOTO 1000 &

19000	!------------------------------------------------ERROR TRAPPING &
	RESUME IF ERR=52% OR ERR=51% 					&
\	RESUME 450 IF ERL=400 						&
\	RESUME 4030 IF ERL=4010%					&
\	RESUME 1010 IF ERR=54% 						&

19999	ON ERROR GOTO 0 						&

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

30200	DEF*FNP$(ROWCOL$)=ESC$+'['+ROWCOL$+'H' 	! Direct Cursor Address &

30250	!-----------------------------------------------MESSAGE HANDLER	&
	DEF*FNMESS$(CHN%,ERRNUM%,DESC$,TO.ERR%,PRINT.TEST%)		&
\		MESG$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERRNUM%)),3%,30%)+	&
			' ' + DESC$					&
\		MESG$=DESC$+'' IF ERRNUM%<1% 				&
\		IF PRINT.TEST%						&
		THEN	PRINT #CHN%, FNP$('24;1');CLRLIN$;MESG$;	&
				FNP$('24;55');				&
				'Hit any key to continue.';		&
\			NW$=FNINP$(CHN%,128%,' ',1%,TO.ERR%)		&

30260		FNMESS$=MESG$						&
\	FNEND 								&

30280	DEF*FNSR$(BEGEND$)=ESC$+'['+BEGEND$+'r'	! Scroll control 	&

30700	!========================================= DATA INPUT FUNCTIONS	&
	DEF*FNSIO$(FRMAT$,ROWCOL$,VARABLE$,DEFAL$,ITEMS$)		&
\		CHN%=ASCII(MID(ITEMS$,1%,1%))				&
\		KIND%=ASCII(MID(ITEMS$,4%,1%))				&
\		DECI=ASCII(MID(ITEMS$,5%,1%))				&
\		OPT$=MID(ITEMS$,6%,1%)					&

30705		PRINT #CHN%, FNP$(ROWCOL$);R.ON$;B.ON$;			&
\		IF OPT$='C'						&
		THEN	ON KIND% GOTO 30710, 30715, 30720, 30725, 30730	&
		ELSE	PRINT #CHN%, SPACE$(ASCII(MID(ITEMS$,2%,1%)));	&
\			GOTO 30735					&

30710		PRINT #CHN%, VARABLE$;			   ! ALPHA	&
\		GOTO 30735						&

30715		PRINT #CHN% USING FRMAT$,ASCII(VARABLE$);  ! ASCII	&
\		GOTO 30735						&

30720		PRINT #CHN% USING FRMAT$, CVT$%(VARABLE$)/(10.**DECI);	&
\		GOTO 30735				   ! INTEGER	&

30725		PRINT #CHN%, FND6$(CVT$%(VARABLE$));	   ! DATE	&
\		GOTO 30735						&

30730		PRINT #CHN% USING FRMAT$,CVT$F(VARABLE$);  ! FLOAT	&

30735		PRINT #CHN%,G.OFF$;CLRBOT$;RIGHT(ITEMS$,6%)+':  ';B.ON$;&
\		FIN$ = FNINP$(CHN%,0%,'_',ASCII(MID(ITEMS$,3%,1%)),1%)	&
\		V% = VAL(FIN$) IF KIND%=3%				&
\		V  = VAL(FIN$) IF KIND%>3%				&
\		IF FIN$=''						&
		THEN	FIN$ = DEFAL$ IF INSTR(1%,'CF',OPT$)=0%		&
\			GOTO 30745					&

30740		IF KIND%=3% OR KIND%=5%					&
		THEN	TEMP = 1.					&
\			TEMP = 10.**DECI IF INSTR(1%,FIN$,'.')=0%	&
\			FIN$ = NUM1$(VAL(FIN$)/TEMP)			&
\			FIN$ = FIN$ + '.' IF INSTR(1%,FIN$,'.')=0%	&

30745		FNSIO$ = FIN$+''					&
\	FNEND								&

32767	END &

