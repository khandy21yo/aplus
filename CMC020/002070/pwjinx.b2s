10	! PRINT INVOICES &
	!-------------------------------------------------------------------- &
	! &
	! &
	!		COPYRIGHT (c) 1984 BY &
	!		COMPUTER MANAGEMENT CENTER, IDAHO FALLS, IDAHO &
	! &
	! THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY BE USED AND &
	! COPIED ONLY IN ACCORDANCE WITH THE TERMS OF SUCH LICENSE AND WITH &
	! THE INCLUSION OF THE ABOVE COPYRIGHT NOTICE.  THIS SOFTWARE OR ANY &
	! OTHER COPIES THEROF MAY NOT BE PROVIDED OR OTHERWISE MADE AVAIL- &
	! ABLE TO ANY OTHER PERSON.  NO TITLE TO AND OWNERSHIP OF THE SOFT- &
	! WARE IS HEREBY TRANSFERRED. &
	! &
	! THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT NO- &
	! TICE AND SHOULD NOT BE CONSTRUED AS A COMMITTMENT BY COMPUTER &
	! MANAGEMENT CENTER. &
	! &
	! CMC ASSUMES NO RESPONSIBILITY FOR THE USE OR RELIABILITY OF &
	! ITS SOFTWARE ON EQUIPMENT WHICH IS NOT SUPPORTED BY CMC. &
	!----------------------------------------------------------------- &

30	ON ERROR GOTO 19000 &
\	JUNK$ = SYS(CHR$(6%) + CHR$(-7%)) &
\	JUNK$ = SYS(CHR$(6%) + CHR$(16%) + CHR$(0%) + CHR$(255%) + &
		CHR$(250%) + STRING$(5%,0%) + CHR$(255%) + &
		STRING$(14%,0%) + CHR$(255%) + CHR$(128%+26%) + STRING$(6%,0%)) &
\	JUNK$ = SYS( CHR$(6%) + CHR$(9%) ) &
\	KBN% = ASCII(MID(JUNK$,2%,1%))/2% &

60	CH%=1% &
\	OPEN "KB:" AS FILE #1%, MODE 8%+256% &
					!    8% - Echo Control &
					!   16% - Disable hibernation, Cntl-C &

70	CH%=1%				! Keyboard channel &
\	ESC$=CHR$(155%)			! Escape code for VT100 control &
\	CLSCN$=ESC$+"[H"+ESC$+"[J"	! Clear screen &
\	CLRLIN$=ESC$+"[2K"		! Erase entire line &
\	G.OFF$=ESC$+"[m"		! Select graphic off &
\	B.ON$=ESC$+"[1m"		! Bold face on &
\	BLINK.ON$=ESC$+"[5m"		! Blinking &
\	R.ON$=ESC$+"[7m"		! Reverse video &
\	ENTER.COPY$=ESC$+"[5i"		! Enter copy mode &
\	EXIT.COPY$=ESC$+"[4i"		! Exit copy mode &
\	CLRBOT$=ESC$+"[21;1H"+ESC$+"[J"	! Erace cursor to end of screen &

100	DIM FIELD.NAME$(30%),FIELD.NAME%(30%,7%), &
		ATTRIBUTE$(30%), ATTRIBUTE(30%), AMOUNT(50%,5%) &
\	ELEMENT%=30% &
\	READ FIELD.NAME$(I%) FOR I%=1% TO ELEMENT% &
\	DEVICE.CH%	= 02% &
\	PWJH.CH%	= 02% &
\	PWJL.CH%	= 04% &
\	CUSTOM.CH%	= 06% &
\	INVOIC.FRM%	= 10% &
\	MENU.CH%	= 12% &
\	LOOP.DATA%	= 5% &
\	OUTPUT.CH%,OUTPUT.CH.RESET%=10% &
\	FROM.KEY$ = "ALL" &
\	TO.KEY$ = "" &
\	OUTDEV$="KB"+NUM1$(KBN%)+":" &
\	TEMP.DATE$=RIGHT(NUM1$((INSTR(1%,"JanFebMarAprMayJunJulAugSepOct"+ &
		"NovDec",MID(DATE$(0%),4%,3%))+2%)/3%+100%),2%)+ &
		LEFT(DATE$(0%),2%)+RIGHT(DATE$(0%),8%) &
\	INVOICE.DATE$,AG.DATE$=FNSDATE$(TEMP.DATE$) &

200	IF FNO%(DEVICE.CH%,'DEVICE.DAT','/RO','')=0% &
	THEN	PWJH.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) &
			IF FNG%(DEVICE.CH%,'PWJH')=0% &
\		PWJL.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) &
			IF FNG%(DEVICE.CH%,'PWJL')=0% &
\		CUSTOM.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) &
			IF FNG%(DEVICE.CH%,'CUSTOM')=0% &
\		V%=FNC%(DEVICE.CH%) &

300	PRINT #CH%, CLSCN$; &
		FNP$('4;28');'INVOICE JOURNAL ENTRY ROUTINE'; &
		FNP$('6;28');'Batch Number <01> '; &
\	JUNK$ = FNINP$(CH%, 128%, '_', 2%) &
\	GOTO 1045 IF JUNK$='%^C' &
\	JUNK$ = "01" IF JUNK$ = "" &
\	IF LEN(JUNK$) = 1% &
	THEN	JUNK$ = '0' + JUNK$ &

305	PRINT #CH%, CLSCN$; &
\	BATCH.NUM$ = JUNK$ &

310	IF FNO%(PWJH.CH%,PWJH.DEVICE$+'PWJH'+BATCH.NUM$+'.DAT','/RW','') &
	THEN	V$=FNMESS$(CH%,FNS%,'PWJH'+BATCH.NUM$+'.DAT',0%,-1%) &
\		GOTO 1045 &

320	IF FNO%(PWJL.CH%,PWJL.DEVICE$+'PWJL'+BATCH.NUM$+'.DAT','/RO','') &
	THEN	V$=FNMESS$(CH%,FNS%,'PWJL'+BATCH.NUM$+'.DAT',0%,-1%) &
\		GOTO 1045 &

330	IF FNO%(CUSTOM.CH%,CUSTOM.DEVICE$+'CUSTOM.DAT','/RO','') &
	THEN	V$=FNMESS$(CH%,FNS%,'CUSTOM.DAT',0%,-1%) &
\		GOTO 1045 &

380	OPEN "INVOIC.FRM" FOR INPUT AS FILE INVOIC.FRM% &

400	OPEN "MENU.FIL/RO" FOR INPUT AS FILE MENU.CH% &
\	DIM #12%, A0$(255%)=64% &
\	COMPANY$=CVT$$(RIGHT(A0$(1%),2%),128%) &
\	ADD1$=CVT$$(RIGHT(A0$(2%),2%),128%) &
\	ADD2$=CVT$$(RIGHT(A0$(3%),2%),128%) &
\	CITST$=CVT$$(RIGHT(A0$(4%),2%),128%) &
\	PHONE$=CVT$$(RIGHT(A0$(5%),2%),128%) &
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
\	FIELD #12%, 	128% AS PWJH.BUF$, &
			128% AS PWJL.BUF$ &

500	COMMAND$ = " FR TO OD DA IN " &

600	!-------------------------------------------------READ FORMS &
	I6%,FIELD.LOOP%=0% &

610	INPUT LINE #INVOIC.FRM%, A$ &
\	GOTO 670 IF INSTR(1%,A$,'*END') &
\	GOTO 650 IF INSTR(1%,A$,'<')=0% &
\	A$=CVT$$(A$,4%) &
\	WHILE A$<>'' &
\		A%=INSTR(1,A$,'<') &
\		A1%=INSTR(1%,A$,'>') &
\		A1%=LEN(A$)+1% IF A1%=0% &
\		A1$=MID(A$,A%+1%,A1%-A%-1%) &
\		A$=RIGHT(A$,A1%+1%) &
\		IF LEFT(A1$,4%)="FRM-" &
		THEN	I6%=I6%+1% &
\			FRM$(I6%)=RIGHT(A1$,5%) &
\			FIELD.LOOP%=0% &
\			GOTO 630 &

620		FIELD.LOOP%=FIELD.LOOP%+1% &
\		FIELD.NAME%(FIELD.LOOP%,I6%)=LOOP% &
			IF FIELD.NAME$(LOOP%)=CVT$$(A1$,-1%) &
				FOR LOOP%=1% TO ELEMENT% &

630	NEXT &
\	GOTO 610 &

650	PRINT.USING$(I6%)=PRINT.USING$(I6%)+A$ &
\	GOTO 610 &

670	FOR LOOP%=1% TO I6% &
\		IF INSTR(1%,FRM$(LOOP%),"BODY") &
		THEN	BODY%=VAL(XLATE(FRM$(LOOP%),STRING$(48%,0%)+ &
				"0123456789")) &

680		IF INSTR(1%,FRM$(LOOP%),"LENGTH") &
		THEN	FRM.LEN%=VAL(XLATE(FRM$(LOOP%),STRING$(48%,0%)+ &
				"0123456789")) &

690	NEXT LOOP% &
\	FOR LOOP%=1% TO I6% &
\	TEST%=1% &

692		TEMP%=INSTR(TEST%,PRINT.USING$(LOOP%),CHR$(27%)+"[") &
\		IF TEMP% &
		THEN	TEST%=TEMP%+1% &
\			PRINT.USING$(LOOP%)=LEFT(PRINT.USING$(LOOP%), &
				TEMP%-1%)+CHR$(155%)+"["+ &
				RIGHT(PRINT.USING$(LOOP%),TEMP%+2%) &
\			GOTO 692 &

695	NEXT LOOP% &
\	CLOSE INVOIC.FRM% &

1000	!--------------------------------------------PROGRAM RESTART POINT &
	OUTPUT.CH%=OUTPUT.CH.RESET% &
\	IF FNG%(PWJH.CH%,"") &
	THEN	PRINT #CH%, FNP$('24;1');CLRLIN$;"The Sales order file is "; &
			"empty.";FNP$('24;55');"Hit any key to continue "; &
\		INP$=FNINP$(CH%,128%," ",1%) &
\		GOTO 1040 &

1010	GOSUB 1060 &
\	GOSUB 6000 &

1030	ALIGN%=0% &
\	PRINT #CH%, CLRBOT$;G.OFF$; &
		"COMMAND: Change Alignment GO Quit"; &
\	OPT$=CVT$$(FNINP$(CH%,128%," ",1%),32%) &
\	GOTO 1010 IF OPT$="%^C" &
\	GOTO 1050 IF OPT$<>'Q' &

1040	! &

1045	PRINT #CH%, CLSCN$;BLINK.ON$;B.ON$;R.ON$;FNP$('1;75'); &
			"WAIT";G.OFF$; &
\	CLOSE OUTPUT.CH% &
\	V%=FNX%("",0%,"") &

1050	GOTO 2200 IF OPT$="C" &
\	GOTO 17000 IF OPT$="G" &
\	GOTO 16000 IF OPT$="A" &
\	GOTO 1030 &

1060	TEMP$     = "INVOICE PRINT ROUTINE" &
\	PRINT #CH%, CLSCN$;R.ON$; &
\	PRINT #CH%,FNP$(NUM1$(I%)+';1');" "; &
			FNP$(NUM1$(I%)+';80');" "; FOR I%=2% TO 19% &
\	PRINT #CH%, FNP$('1;1');LEFT(TEMP$,39%);SPACE$(40%-LEN(TEMP$)); &
		SPACE$(40%-LEN(COMPANY$));COMPANY$; &
		FNP$('20;1');SPACE$(80%);G.OFF$; &
\	PRINT #CH%,	FNP$('4;20'); "(FR) STARTING ORDER"; &
			FNP$('6;20'); "(TO) ENDING ORDER"; &
			FNP$('8;20'); "(OD) OUTPUT TO "; &
			FNP$('10;20');"(DA) INVOICE DATE"; &
			FNP$('12;20');"(IN) INVOICE #"; &
\	RETURN &

2200	!--------------------------------------------------CHANGE RECORD &
	OPTION$ = "CHANGE " &
\	FLAG%, CHNG.FLAG% = -1% &
\	PRINT #CH%, CLRBOT$; "Change:"; &
\	INP$=CVT$$(FNINP$(CH%,128%," ",2%),32%) &
\	GOTO 1010 IF INP$="%^C" &
\	GOTO 1030 IF INP$="" &
\	LOOP%=(INSTR(1%,COMMAND$," "+INP$+" ")+2%)/3% &
\	GOTO 2200 IF LOOP%=0% &
\	GOSUB 6010 &
\	GOTO 1010 IF INP$="%^C" &
\	GOTO 2200 &

6000	!------------------------------------------------DATA ENTRY SECTION &
	FOR LOOP%=1% TO LOOP.DATA% &
\		ON LOOP% GOSUB	6220, 6240, 6260, 6280, 6300 &
\	NEXT LOOP% &
\	RETURN &

6010	ON LOOP% GOSUB	6210, 6230, 6250, 6270, 6290 &
\	RETURN &
		! INPUT DATA &

6200	!-------------------------------------------------BEGIN DATA ENTRY &

6210	PRINT #CH%, FNP$('4;46');R.ON$;B.ON$;FROM.KEY$;SPACE$(6%-LEN(FROM.KEY$)); &
		G.OFF$;CLRBOT$;OPTION$;"ALPHA: ";B.ON$; &
\	INP$=FNINP$(CH%,0%,"_",6%) &
\	GOTO 6220 IF INP$="%^C" &
\	FROM.KEY$=INP$ IF INP$<>"" &

6220	PRINT #CH%, FNP$('4;46');B.ON$;FROM.KEY$;SPACE$(6%-LEN(FROM.KEY$)); &
		G.OFF$; &
\	RETURN &

6230	PRINT #CH%,FNP$('6;46');R.ON$;B.ON$;TO.KEY$;SPACE$(6%-LEN(TO.KEY$)); &
		G.OFF$;CLRBOT$;OPTION$;"ALPHA: ";B.ON$; &
\	INP$=FNINP$(CH%,0%,"_",6%) &
\	GOTO 6240 IF INP$="%^C" &
\	TO.KEY$=INP$ IF INP$<>"" &

6240	PRINT #CH%, FNP$('6;46');B.ON$;TO.KEY$;SPACE$(6%-LEN(TO.KEY$)); &
		G.OFF$; &
\	RETURN &

6250	PRINT #CH%, FNP$('8;46');R.ON$;B.ON$;OUTDEV$;SPACE$(20%-LEN( &
		OUTDEV$));G.OFF$;CLRBOT$;OPTION$;"ALPHA: ";B.ON$; &
\	INP$=FNINP$(CH%,0%,"_",20%) &
\	RETURN IF INP$="%^C" &
\	IF INP$<>"" &
	THEN	CLOSE OUTPUT.CH% &
\		TEMP$=FNOUTPUT$(INP$,OUTPUT.CH%,128%) &
\		INP$='KB:' IF TEMP$="%ERROR" &
\		OUTDEV$=INP$ &

6260	PRINT #CH%, FNP$('8;46');B.ON$;OUTDEV$;SPACE$(20%-LEN( &
		OUTDEV$));G.OFF$; &
\	RETURN &

6270	PRINT #CH%,FNP$('10;46');R.ON$;B.ON$;FNRDATE$( &
		INVOICE.DATE$);G.OFF$;CLRBOT$;OPTION$;"ALPHA: ";B.ON$; &
\	INP$=FNINP$(CH%,0%,"_",6%) &
\	INP$=XLATE(INP$,STRING$(48%,0%)+"0123456789") &
\	INVOICE.DATE$=FNSDATE$(INP$) IF INP$<>"" &

6280	PRINT #CH%, FNP$('10;46');B.ON$;FNRDATE$(INVOICE.DATE$);G.OFF$; &
\	RETURN &

6290	PRINT #CH%, FNP$('12;46');R.ON$;B.ON$;INVNUM$;SPACE$(8%-LEN(INVNUM$)); &
		G.OFF$;CLRBOT$;OPTION$;"ALPHA: ";B.ON$; &
\	INP$=FNINP$(CH%,0%,"_",8%) &
\	GOTO 6300 IF INP$="%^C" &
\	INVNUM$=INP$ IF INP$<>"" &
\	INV.LEN%=LEN(INVNUM$) &
\	INVNUM$=NUM1$(VAL(INVNUM$)) &
\	INVNUM$=STRING$(INV.LEN%-LEN(INVNUM$),48%)+INVNUM$ &

6300	PRINT #CH%, FNP$('12;46');B.ON$;INVNUM$;SPACE$(8%-LEN(INVNUM$)); &
		G.OFF$; &
\	RETURN &

16000	!----------------------------------------------PRINT ALIGNMENT FORM &
	OUTPUT.CH%=CH% IF OUTDEV$="KB:" OR OUTDEV$='' OR &
		OUTDEV$="KB"+NUM1$(KBN%)+":" &
\	LF.COUNT%=0% &
\	IF OUTPUT.CH%=CH% &
	THEN	PRINT #CH%, CLRBOT$;"Turn on print and set the forms "; &
			FNP$('24;55');"Hit any key to continue"; &
\		INP$=FNINP$(CH%,128%," ",1%) &
\		GOTO 1000 IF INP$="%^C" &

16010	PRINT #CH%,FNP$('24;1');CLRLIN$;"Printing. . ."; &
\	PRINT #OUTPUT.CH%, ENTER.COPY$; IF OUTPUT.CH%=CH% &
\	ATTRIBUTE$(I%)="XXXXXXXXXXXXXXXXXXXXXXXXXXXX" FOR I%=1% TO ELEMENT% &
\	ATTRIBUTE(I%)=0. FOR I%=1% TO ELEMENT% &
\	FOR FRM%=1% TO I6% &
\		GOSUB 17800 IF FRM$(FRM%)="HEADER" &
\		GOSUB 17800 IF INSTR(1%,FRM$(FRM%),"BODY") &
\		GOSUB 17800 IF INSTR(1%,FRM$(FRM%),"BODY") &
\		GOSUB 17800 IF INSTR(1%,FRM$(FRM%),"BODY") &
\		PRINT #OUTPUT.CH%, STRING$(BODY%-3%,10%);CHR$(13%); &
			IF INSTR(1%,FRM$(FRM%),"BODY") &
\		GOSUB 17800 IF FRM$(FRM%)="GROSS" &
\		GOSUB 17800 IF FRM$(FRM%)="FRIGHT" &
\		GOSUB 17800 IF FRM$(FRM%)="BROKER" &
\		GOSUB 17800 IF FRM$(FRM%)="TOTAL" &
\	NEXT FRM% &
\	PRINT #CH%, EXIT.COPY$;COLM.OFF$; &
\	GOTO 1010 &

17000	!-------------------------------------------------OUTPUT REPORT &
	INP$=CVT$$(FNINP$(CH%,128%,' ',1%),-1%) &
\	GOTO 1030 IF INP$<>'O' &
\	START.TEMP$="" &
\	START.TEMP$=FROM.KEY$ IF FROM.KEY$<>"ALL" &
\	START.TEMP$=START.TEMP$+SPACE$(6%-LEN(START.TEMP$)) &
		IF START.TEMP$<>'' &
\	TO.KEY$=CVT$$(TO.KEY$,140%) &
\	INVNUM=VAL(INVNUM$)-1. &
\	V%=FNG%(PWJH.CH%,START.TEMP$) &
\	OUTPUT.CH%=CH% IF OUTDEV$="KB:" OR OUTDEV$='' OR &
		OUTDEV$="KB"+NUM1$(KBN%)+":" &
\	OUTPUT.CH%=CH% IF DISPLAY$='Y' &
\	IF OUTPUT.CH%=CH% &
	THEN	PRINT #CH%, CLRBOT$;"Turn on print and set the invoices "; &
			FNP$('24;55');"Hit any key to continue"; &
\		INP$=FNINP$(CH%,128%," ",1%) &
\		GOTO 1000 IF INP$="%^C" &

17010	OPEN "KB:" AS FILE CH% IF DISPLAY$<>'Y' &
\	PRINT #CH%, FNP$('24;1');CLRLIN$;"Printing. . ."; &
\	PRINT #OUTPUT.CH%, ENTER.COPY$; IF OUTPUT.CH%=CH% &

17020	LSET PWJH.BUF$=FNL$+'' &
\	GOTO 17400 IF TO.KEY$<PWJH.ORDNUM$ AND TO.KEY$<>"" &
\	GOTO 17300 IF CVT$$(PWJH.INVNUM$,1%)<>'' &
\	PRINT.FLAG%, LF.COUNT%, LINE.COUNT%=0% &
\	PAGE%,PRINTED.LINE%=0% &
\	FOR FRM%=1% TO I6% &
\		GOSUB 17700 IF FRM$(FRM%)="HEADER" &
\		GOSUB 17600 IF INSTR(1%,FRM$(FRM%),"BODY") &
\		GOSUB 17650 IF FRM$(FRM%)="GROSS" &
\	NEXT FRM% &

17300	GOTO 17020 IF FNN%(PWJH.CH%)=0% &

17400	! &

17500	PRINT #CH%, EXIT.COPY$; &
\	OPEN 'KB:' AS FILE #CH%, MODE 8%+256% &
\	GOTO 1000 &

17600	!----------------------------------------------PRINT STATEMENT &
	GOTO 17635 IF FNG%(PWJL.CH%,PWJH.ORDNUM$) &

17605	CREDIT.FLAG%,PRINTED.LINE%=0% &
\	ATTRIBUTE(19%)=0. &
\	AMOUNT(I%,J%)=0. FOR I%=0% TO CVT$%(PWJH.LINE$(2%)) &
				FOR J%=1% TO 5% &
\	BROKER.AMT(I%)=0. FOR I%=1% TO 5% &

17610	LSET PWJL.BUF$=FNL$ &
\	GOTO 17635 IF PWJH.ORDNUM$<>PWJL.ORDNUM$ &
\	IF PWJL.WINDOW$='2' &
	THEN	GOSUB 17670 IF CREDIT.FLAG%=1% &
	ELSE	GOSUB 17660 &
\		GOTO 17630 &

17615	ATTRIBUTE(14%) =CVT$F(PWJL.QTY$) &
\	ATTRIBUTE$(15%)=PWJL.PRONUM$+'' &
\	ATTRIBUTE$(16%)=PWJL.DESC$+'' &
\	ATTRIBUTE$(21%)=PWJL.STONUM$+'' &
\	ATTRIBUTE(17%) =CVT$F(PWJL.PRICE$) &
\	ATTRIBUTE(18%) =FNZ(CVT$F(PWJL.EXT$),2%) &
\	ATTRIBUTE(19%) =ATTRIBUTE(19%)+ATTRIBUTE(18%) &
\	ATTRIBUTE$(20%)='CWT' &
\	ATTRIBUTE$(20%)='Unt' IF PWJL.PRTYPE$='U' &

17620	IF ATTRIBUTE(14%)<>0. &
	THEN	GOSUB 17800 &
\		PRINTED.LINE%=PRINTED.LINE%+1% &

17625	IF	BODY%<=PRINTED.LINE% &
	THEN	TEMP%=FRM% &
\		PRINT #OUTPUT.CH%, STRING$(FRM.LEN%-LF.COUNT%,10%); &
			CHR$(13%); &
\		LF.COUNT%=0% &
\		FOR FRM%=1% TO I6% &
\			GOSUB 17700 IF FRM$(FRM%)="HEADER" &
\		NEXT FRM% &
\		FRM%=TEMP% &
\		PRINTED.LINE%=0% &

17630	IF FNN%(PWJL.CH%)=0% &
	THEN	GOTO 17610 &

17635	PRINT #OUTPUT.CH%, STRING$(BODY%-PRINTED.LINE%,10%);CHR$(13%); &
\	LF.COUNT%=LF.COUNT%+(BODY%-PRINTED.LINE%) &

17640	RETURN &

17650	!----------------------------PRINT FOOT ON ATTACHMENT &
	ATTRIBUTE(22%)=-BROKER.AMT(5%)-AMOUNT(0%,5%) &
\	ATTRIBUTE(23%)=-BROKER.AMT(3%)-AMOUNT(0%,3%) &
\	LM%=SGN(ABS(ATTRIBUTE(23%)))+SGN(ABS(ATTRIBUTE(22%))) &
\	LN%=2%-LM% &
\	GOSUB 17800 IF LN%<2% &
\	FRM%=FRM%+1% &
\	GOSUB 17800 IF ATTRIBUTE(22%)<>0. &
\	FRM%=FRM%+1% &
\	GOSUB 17800 IF ATTRIBUTE(23%)<>0. &
\	PRINT #OUTPUT.CH%,STRING$(LN%+1%-SGN(LM%),10%);CHR$(13%); &
\	LF.COUNT%=LF.COUNT%+LN%+1%-SGN(LM%) &
\	FRM%=FRM%+1% &
\	ATTRIBUTE(19%)=ATTRIBUTE(19%)+ATTRIBUTE(23%)+ATTRIBUTE(22%) &
\	GOSUB 17800 &
\	PRINT #OUTPUT.CH%, STRING$(FRM.LEN%-LF.COUNT%,10%);CHR$(13%); &
\	RETURN &

17660	TYP$   =CVT$$(PWJL.STONUM$,-1%) &
\	LOOP%   =(INSTR(1%,' OP BR BC FR FC ',TYP$)+1%)/3% &
\	GOTO 17665 IF LOOP%<>3% AND LOOP%<>5% &
\	IND%    =CVT$F(PWJL.POUNDS$) &
\	AMOUNT(IND%,LOOP%)   =AMOUNT(IND%,LOOP%)+FNZ(CVT$F(PWJL.EXT$),2%) &
\	CREDIT.FLAG%=1% &

17665	RETURN &

17670	FOR I%=3% TO 5% STEP 2% &
\		AMOUNT(CVT$%(PWJL.LINE$),I%)=AMOUNT(CVT$%(PWJL.LINE$)-1%,I%) &
			IF CVT$%(PWJL.LINE$)>1% AND &
				AMOUNT(CVT$%(PWJL.LINE$),I%)=0. & 
\		BROKER.AMT(I%)=BROKER.AMT(I%)+ &
			FNZ(AMOUNT(CVT$%(PWJL.LINE$),I%)*CVT$F(PWJL.POUNDS$)*0.01,2%) &
\	NEXT I% &

17675	RETURN &

17700	!-------------------------------------------------PRINT HEADING &
	INVNUM=INVNUM+1. &
\	IF PAGE%=0% &
	THEN	RSET PWJH.INVNUM$=NUM1$(INVNUM) &
\		LSET PWJH.INVDAT$=INVOICE.DATE$	 &
\		STOP IF FNU%(PWJH.CH%,PWJH.BUF$) &

17705	ATTRIBUTE$(I%)='' FOR I%=1% TO 8% &
\	GOTO 17720 IF FNG%(CUSTOM.CH%,PWJH.SOLDTO$) &

17710	FIELD #CUSTOM.CH%+1%, FNL% AS TEMP$, &
			06% AS ATTR$(1%),	! CUSTOMER NUMBER &
			25% AS ATTR$(2%),	! CUSTOMER NAME &
			25% AS ATTR$(3%),	! CUSTOMER ADD 1 &
			21% AS ATTR$(4%),	! CUSTOMER ADD 2 &
			15% AS ATTR$(5%),	! CITY &
			02% AS ATTR$(6%),	! STATE &
			05% AS ATTR$(7%),	! COUNTRY &
			10% AS ATTR$(8%)	! ZIP &
\		ATTRIBUTE$(I%)=ATTR$(I%)+'' FOR I%=2% TO 8% &
\		ATTRIBUTE$(I%+22%)=ATTR$(I%)+'' FOR I%=2% TO 8% &

17720	GOTO 17730 IF CVT$$(PWJH.SHIPTO$,-1%)='' OR PWJH.SHIPTO$=PWJH.SOLDTO$ &
\	ATTRIBUTE$(I%+22%)='' FOR I%=2% TO 8% &
\	GOTO 17730 IF FNG%(CUSTOM.CH%,PWJH.SHIPTO$) &

17725	FIELD #CUSTOM.CH%+1%, FNL% AS TEMP$, &
			06% AS ATTR$(1%),	! CUSTOMER NUMBER &
			25% AS ATTR$(2%),	! CUSTOMER NAME &
			25% AS ATTR$(3%),	! CUSTOMER ADD 1 &
			21% AS ATTR$(4%),	! CUSTOMER ADD 2 &
			15% AS ATTR$(5%),	! CITY &
			02% AS ATTR$(6%),	! STATE &
			05% AS ATTR$(7%),	! COUNTRY &
			10% AS ATTR$(8%)	! ZIP &
\		ATTRIBUTE$(I%+22%)=ATTR$(I%)+'' FOR I%=2% TO 8% &

17730	ATTRIBUTE$(1%)=PWJH.SOLDTO$+'' &
\	ATTRIBUTE$(9%)=NUM1$(INVNUM)			! Invoice Number &
\	ATTRIBUTE$(10%)=FNRDATE$(INVOICE.DATE$)+''	! Invoice Date &
\	ATTRIBUTE$(11%)=PWJH.CARNAM$+''		! Carrier &
\	ATTRIBUTE$(12%)=PWJH.CUSPO$+''		! PO number &
\	PAGE%=PAGE%+1% &
\	ATTRIBUTE(13%)=PAGE% &
\	GOSUB 17800 &
\	RETURN &

17800	!------------------------------------PRINT OUTPUT &
	I5%,I1%=1% &
\	I3%=0% &
\	PU$=PRINT.USING$(FRM%) &

17810	I%=INSTR(I1%,PU$,"\") &
\	TEST%=INSTR(I1%,PU$,"#") &
\	TEST1%=INSTR(I1%,PU$,"$") &
\	TEST%=TEST1% IF TEST1%<TEST% AND TEST1%<>0% &
\	I%=TEST% IF TEST% AND TEST%<I% OR TEST% AND I%=0% &
\	TEST%=0% IF TEST%<>I% &
\	I3%,I%=LEN(PU$)+2% IF I%=0% &
\	PRINT #OUTPUT.CH%, MID(PU$,I1%,I%-I1%); &
\	LF.COUNT%=LF.COUNT%+LEN(XLATE(MID(PU$,I1%,I%-I1%),STRING$(10%,0%)+ &
		CHR$(10%))) &
\	IF I%=I3% &
	THEN	GOTO 17830 &
	ELSE	I2%=INSTR(I%+1%,PU$,"\") &
\		TEST1%=INSTR(I%+1%,PU$," ") &
\		I2%=TEST1% IF TEST% AND TEST1%<I2% OR I2%=0% AND TEST% &
\		IF TEST% &
		THEN	LOOP%=LEN(PU$) &
\			LOOP%=I2% IF I2%<>0% &
\			GOTO 17820 IF INSTR(I2%,PU$,"#") &
				FOR I2%=LOOP% TO 1% STEP -1% &

17820	P$=MID(PU$,I%,I2%+1%-I%) &
\	IF INSTR(1%,P$,"\") &
	THEN	PRINT #OUTPUT.CH% USING P$,ATTRIBUTE$(FIELD.NAME%(I5%,FRM%)); &
	ELSE	PRINT #OUTPUT.CH% USING P$,ATTRIBUTE(FIELD.NAME%(I5%,FRM%)); &

17825	I5%=I5%+1% &
\	I1%=I2%+1% &
\	GOTO 17810 &

17830	RETURN &

19000	!-------------------------------------------------ERROR TRAPPING &
	RESUME IF ERR=51% OR ERR=52% &
\	RESUME 690 IF ERR=11% &
\	IF	ERR=28% &
	THEN	JUNK$ = SYS(CHR$(6%) + CHR$(-7%)) &
\		RESUME 17500 &

19025	IF ERL=400% &
	THEN	RESUME 450 &

19030	IF ERL=370% &
	THEN	RESUME 380 &

19040	IF ERL>=30800% AND ERL<=30830% &
	THEN	IF INSTR(1%,Y$,".")<>0% &
		THEN	PRINT #CH%, FNP$('24;1');CLRLIN$; &
				"That file presently exists.  Enter a"; &
				" '0' to supersede or a '2' to add to file "; &
\			Y1$=FNINP$(CH%,128%,"_",1%) &
\			PRINT #CH%, FNP$('24;1');CLRLIN$; &
\			Y1$=CVT$$(LEFT(Y1$,1%),-1%) &
\			Y1%=VAL(Y1$) IF Y1$="0" OR Y1$="2" &
\			RESUME 30820 IF Y1$="2" &
\			RESUME 30825 IF Y1$="1" &
\			Y$="%ERROR" IF Y$<>"%^C" &
\			RESUME 30840 &

19050	IF ERL>=30800% AND ERL<=30830% &
	THEN	PRINT #CH%, FNP$('24;1');CLRLIN$;"Error";ERR; &
		"while opening ";Y$;".  Please try again."; &
		FNP$('24;55'); "Hit any key to continue"; &
\		Y$=FNINP$(CH%,128%," ",1%) &
\		PRINT #CH%, FNP$('24;1');CLRLIN$; &
\		Y$="%ERROR" &
\		RESUME 30840 &

19900	ON ERROR GOTO 0 &

20100	DATA	"SOLDTO", &
		"NAME", &
		"ADD1", &
		"ADD2", &
		"CITY", &
		"STATE", &
		"COUNTRY", &
		"ZIP" &

20110	DATA	"INVNUM", &
		"INVDAT", &
		"CARNUM", &
		"CUSPO", &
                "PAGE", &
		"QTY", &
		"PRONUM", &
		"DESC", &
		"PRICE", &
		"EXT", &
		"TOTAL", &
		"PRTFLG", &
		"SOURCE", &
		"FREIGHT.AMT", &		
		"BROKER.AMT" &		

20120	DATA	"NAME2", &
		"ADD12", &
		"ADD22", &
		"CITY2", &
		"STATE2", &
		"COUNTRY2", &
		"ZIP2" &

30000	DEF FNINP$(CHN%,KYP%,FILLCHAR$,INPUTLEN%) &
\		PRINT #CHN%, STRING$(INPUTLEN%,ASCII(FILLCHAR$)); &
			  STRING$(INPUTLEN%,8%); &
\		PRINT #CHN%, RECORD 256%, CHR$(KYP%+INPUTLEN%)+FILLCHAR$; &
\		GET #CHN% &
\		FIELD #CHN%, RECOUNT AS BUFFER$ &
\		IF ASCII(BUFFER$)=128% OR ASCII(BUFFER$)=8% &
			OR ASCII(BUFFER$)=23% &
		THEN	BUFFER$="%END" IF MID(BUFFER$,2%,4%)="[21~" &
				OR ASCII(BUFFER$)=8% &
\			BUFFER$="%ABORT" IF MID(BUFFER$,2%,4%)="[19~" &
				OR ASCII(BUFFER$)=23% &

30010		BUFFER$="%^C" IF INSTR(1%,BUFFER$,CHR$(3%)) &
\		FNINP$=CVT$$(BUFFER$,4%) &
\	FNEND &

30050	!------------------------------------------------------------ &
	! A N S I   D I R E C T   C U R S O R   A D D R E S S I N G &
	!------------------------------------------------------------ &
	DEF*FNP$(BEGEND$)=ESC$+"["+BEGEND$+"H" &
	&

30060	DEF FNROUND(A.VALUE)=INT(A.VALUE*100.+.5)/100. &

30080	!-----------------------------------------PRINT A DATE &
	DEF*FNDATE.PRINT$(Y$) = &
		LEFT(Y$,2%)+"/"+MID(Y$,3%,2%)+"/"+RIGHT(Y$,5%) &

30120	DEF FNSDATE$(Y$)=CVT%$(VAL(MID(Y$,3%,2%))+VAL(LEFT(Y$,2%))*32%+ &
		FNI%(VAL(RIGHT(Y$,5%)))*512%) &

30130	DEF FNI%(Y)=Y &

30140	DEF FNRDATE$(Y$) &
\		Y%=CVT$%(Y$) &
\		FNRDATE$=RIGHT(NUM1$((Y% AND 480%)/32%+100%),2%)+'/'+ &
			RIGHT(NUM1$((Y% AND 31%)+100%),2%)+'/'+ &
			RIGHT(NUM1$((SWAP%(Y%) AND 254%)/2%+100%),2%) &
\	FNEND &

30250	!---------------------------------------------MESSAGE HANDLER &
	DEF*FNMESS$(CHN%,ERRNUM%,DESC$,TO.ERR%,PRINT.TEST%) &
\		MESG$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERRNUM%)),3%,30%)+ &
			DESC$ &
\		IF	PRINT.TEST% &
		THEN	PRINT #CHN%, FNP$('24;1'); CLRLIN$; MESG$; &
				FNP$('24;55');'Hit any key to continue'; &
\			NW$=FNINP$(CHN%,128%,' ',1%) &

30260		FNMESS$=MESG$ &
\	FNEND &

30500	DEF FNZ(Y,N%)=SGN(Y)*10.^(-N%)*INT(ABS(Y)*10.^N%+.5001)  	&

30800	!	************************************************************* &

30810	DEF FNOUTPUT$(Y$,Y%,Y1%) &

30820		IF Y1%=128% &
		THEN	OPEN Y$ FOR OUTPUT AS FILE Y% &

30825		KILL Y$ IF Y1%=0% &
\		OPEN Y$ AS FILE Y%, MODE Y1% IF Y1%=0% &

30840		FNOUTPUT$=Y$ &
\	FNEND &

30900	&

32767	END &

