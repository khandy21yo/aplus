10	  ! &
	  ! Program name: accpur		Compiled with SCALE 0 on V09.7 &
	  ! Decompiled on 29-Dec-16 at 02:12 PM
50	  ON ERROR GOTO 19000 &
	\ TEMP$=SYS(CHR$(6%)+CHR$(-7%))
60	  OPEN "KB:" AS FILE 1%, MODE 8%+256% &
	\ CH%=1% &
	\ ESC$=CHR$(155%) &
	\ CLSCN$=ESC$+"[H"+ESC$+"[J" &
	\ CLRLIN$=ESC$+"[2K" &
	\ G.OFF$=ESC$+"[m" &
	\ B.ON$=ESC$+"[1m" &
	\ BLINK.ON$=ESC$+"[5m" &
	\ R.ON$=ESC$+"[7m" &
	\ COLM.ON$=ESC$+"[?3h" &
	\ COLM.OFF$=ESC$+"[?3l" &
	\ CLRBOT$=ESC$+"[21;1H"+ESC$+"[J" &
	\ LDS.ON$=ESC$+"(0" &
	\ USASCII$=ESC$+"(B" &
	\ HID.CURSOR$=ESC$+"[?25l"+ESC$+"[24;1h" &
	\ CUR.ON$=ESC$+"[?25h"
80	  DROP.DEAD.DATE$="        " &
	\ VERSION.NO$="V1.0" &
	\ DATE.TIME$=SYS(CHR$(6%)+CHR$(20%)+CHR$(0%)+CHR$(0%)+CHR$(1%)+CHR$(SWAP%(1%))+STRING$(11%,0%)+CHR$(SWAP%(0%))+CHR$(1%)+CHR$(SWAP%(1%))) &
	\ PURGE.DATE$=MID(DATE.TIME$,10%,2%)+"/"+MID(DATE.TIME$,13%,2%)+"/"+MID(DATE.TIME$,7%,2%) &
	\ DATE.TIME$=MID(DATE.TIME$,10%,2%)+MID(DATE.TIME$,7%,2%) &
	\ IF DROP.DEAD.DATE$<>"" THEN  &
		  IF DROP.DEAD.DATE$<MID(SYS(CHR$(6%)+CHR$(20%)+CHR$(0%)+CHR$(0%)+CHR$(1%)+CHR$(SWAP%(1%))),7%,8%) THEN  &
			  MESG$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(47%)),3%,30%) &
			\ PRINT #CH%, MESG$; &
			\ V$=SYS(CHR$(9%))
100	  MONTH%=INSTR(1%,"  JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC",CVT$$(MID(DATE$(0%),4%,3%),-1%))/3% &
	\ MONTH$=RIGHT(NUM1$(100%+MONTH%),2%) &
	\ LAST.UPDATE$=CVT%$(FND6%(MONTH$+LEFT(DATE$(0%),2%)+RIGHT(DATE$(0%),8%))) &
	\ DEVICE.CH%=2% &
	\ WIPDES.CH%=2% &
	\ WIPACC.CH%=4% &
	\ WIPORG.CH%=8% &
	\ WORK.CH%=6% &
	\ MENU.CH%=12%
200	  IF FNO%(DEVICE.CH%,"DEVICE.DAT","/RO","")=0% THEN  &
		  WIPDES.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) IF FNG%(DEVICE.CH%,"WIPDES")=0% &
		\ WIPACC.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) IF FNG%(DEVICE.CH%,"WIPACC")=0% &
		\ V%=FNC%(DEVICE.CH%)
300	  IF FNO%(WIPDES.CH%,WIPDES.DEVICE$+"WIPDES.DAT","/RO","") THEN  &
		  V$=FNMESS$(CH%,FNS%,"WIPDES.DAT",0%,-1%) &
		\ GOTO 1040
320	  IF FNO%(WIPACC.CH%,WIPACC.DEVICE$+"WIPACC.DAT","/RW","") THEN  &
		  V$=FNMESS$(CH%,FNS%,"WIPACC.DAT",0%,-1%) &
		\ GOTO 1040
330	  !
350	  OPEN "ACCPUR.FIL" FOR OUTPUT AS FILE WORK.CH%
400	  OPEN "MENU.FIL/RO" FOR INPUT AS FILE 12% &
	\ DIM #12%, A0$(255%)=64% &
	\ COMPANY$=CVT$$(RIGHT(A0$(1%),2%),128%) &
	\ CLOSE 12%
450	  OPEN "NL:" AS FILE 12%, RECORDSIZE 64%+64% &
	\ FIELD #12%, 6% AS WIPDES.WONUM$,25% AS WIPDES.DESC$,8% AS WIPDES.STRT.DATE$,8% AS WIPDES.EXPCOMP.DATE$,8% AS WIPDES.ACTCOMP.DATE$,2% AS WIPDES.WOTYPE$,6% AS WIPDES.DEPT$,1% AS WIPDES.PURGE$ &
	\ FIELD #12%, 64% AS TEMP$,6% AS WIPACC.SUBACC$,8% AS WIPACC.ACC$,8% AS WIPACC.BUDG$,8% AS WIPACC.BUDG.A$,8% AS WIPACC.CTD$,8% AS WIPACC.YTD$,8% AS WIPACC.MTD$ &
	\ FIELD #12%, 64% AS WIPDES.BUF$,64% AS WIPACC.BUF$
510	  FRAME$=FRAME$+FNP$(NUM1$(I%)+";1")+" "+FNP$(NUM1$(I%)+";80")+" " FOR I%=2% TO 19% &
	\ PERCENT$="DON'T FORGET TO PRINT OUT THE LATEST PURGE REPORT  "
1000	  !
1010	  TEMP$="BLANK BALANCE FOR PURGED WORK ORDERS" &
	\ PRINT #CH%, COLM.OFF$;R.ON$;LEFT(TEMP$,40%);SPACE$(40%-LEN(TEMP$));SPACE$(40%-LEN(COMPANY$));COMPANY$;FRAME$;FNP$("20;1");SPACE$(80%);G.OFF$; &
	\ PRINT #CH%, FNP$("11;10");"0% 10%  20%  30%  40%  50%  60%  70%  80%  90% 100% "
1030	  PRINT #CH%, CLRBOT$;G.OFF$;"Confirm (Y/N) "; &
	\ OPT$=CVT$$(FNINP$(CH%,128%," ",1%,0%),32%) &
	\ GOTO 1010 IF OPT$="%^C" &
	\ GOTO 3100 IF OPT$="Y"
1040	  PRINT #CH%, CLSCN$;FNSR$("1;24");FNP$("1;75");CUR.ON$;B.ON$;BLINK.ON$;R.ON$;"WAIT";G.OFF$;FNP$("24;1"); &
	\ V%=FNX%("",0%,"")
3100	  PRINT #CH%, CLRBOT$;FNP$("24;1");"Processing. . . ";FNP$("24;79"); &
	\ PRINT #WORK.CH%, SPACE$(5%)+"WORK ORDER    ACCT     AMOUNT"+"   DATE OF PURGE"+SPACE$(7%) &
	\ PRINT #WORK.CH%, STRING$(80%,61%) &
	\ PRINT #WORK.CH%, SPACE$(60%)+PURGE.DATE$
3105	  PRECOL%=10% &
	\ LONG%=52% &
	\ IF FNG%(WIPDES.CH%,"") THEN  &
		  PRINT #CH%, FNP$("24;1");"WO REG FILE IF EMPTY";FNP$("24;55");"Hit any key to continue"; &
		\ INP$=FNINP$(CH%,128%," ",1%,0%) &
		\ GOTO 1040
3106	  TNF=FNT
3110	  LSET WIPDES.BUF$=FNL$ &
	\ IF WIPDES.WONUM$=TEST.WONUM$ THEN  &
		  STOP IF FND%(-WIPDES.CH%,"") &
		\ PRINT WIPDES.WONUM$ &
		\ GOTO 3180
3112	  KEY.POINT%=FNR(WIPDES.CH%) &
	\ DATA.POINT%=FNR(WIPDES.CH%+1%) &
	\ PURGE.FLAG%=0% &
	\ GOTO 3180 IF LEFT(WIPDES.DESC$,1%)<>"*" &
	\ GOTO 3180 IF WIPDES.PURGE$="*" &
	\ GOTO 3180 IF WIPDES.WONUM$="306095" &
	\ GOTO 3180 IF WIPDES.WONUM$="307724" &
	\ GOTO 3180 IF WIPDES.WONUM$="304963" &
	\ GOTO 3180 IF WIPDES.WONUM$="305022" &
	\ GOTO 3180 IF WIPDES.WONUM$="305833"
3115	  GOTO 3180 IF FNG%(WIPACC.CH%,WIPDES.WONUM$)
3120	  LSET WIPACC.BUF$=FNL$+"" &
	\ GOTO 3180 IF WIPACC.SUBACC$<>WIPDES.WONUM$ &
	\ IF CVT$F(WIPACC.MTD$)=0. THEN  &
		  PRINT #CH%, WIPACC.SUBACC$;" ";WIPACC.ACC$;CHR$(13%); &
		\ GOSUB 4000 &
		\ LSET WIPACC.CTD$=CVTF$(0.) &
		\ LSET WIPACC.YTD$=CVTF$(0.) &
		\ STOP IF FNU%(WIPACC.CH%,WIPACC.BUF$)
3125	  GOTO 3120 IF FNN%(WIPACC.CH%)=0%
3180	  COL%=KEY.POINT%/TNF*LONG%-PRECOL%+10% &
	\ IF COL% THEN  &
		  PRINT #CH%, FNP$("11;"+NUM1$(PRECOL%));R.ON$;MID(PERCENT$,PRECOL%-9%,COL%);G.OFF$;HID.CURSOR$; &
		\ PRECOL%=PRECOL%+COL%
3195	  GOTO 3110 IF FNN%(WIPDES.CH%)=0%
3199	  CLOSE WORK.CH% &
	\ GOTO 1040
4000	  !
4010	  PRINT #WORK.CH%, USING SPACE$(5%)+"\    \   \        \ "+"#,###,###.##", WIPDES.WONUM$,WIPACC.ACC$,CVT$F(WIPACC.YTD$)
4015	  RETURN
19000	  !
19010	  RESUME 450 IF ERL=400%
19990	  ON ERROR GOTO 0
30000	  DEF FNINP$(CHN%,KYP%,FILLCHAR$,INPUTLEN%,TO.ERR%) &
	\ PRINT #CHN%, STRING$(INPUTLEN%,ASCII(FILLCHAR$));STRING$(INPUTLEN%,8%); &
	\ PRINT #CHN%, RECORD 256%,CHR$(KYP%+INPUTLEN%)+FILLCHAR$; &
	\ GET #CHN% &
	\ FIELD #CHN%, RECOUNT AS BUFFER$ &
	\ BUFFER$="%^C" IF INSTR(1%,BUFFER$,CHR$(3%)) &
	\ FNINP$=CVT$$(BUFFER$,4%) &
	\ V=SQR(-1.) IF BUFFER$="%^C" AND TO.ERR% &
	\ FNEND
30200	  DEF FNP$(ROWCOL$) &
	\ FNP$=CHR$(155%)+"["+ROWCOL$+"H" &
	\ FNEND
30210	  DEF FNSR$(BEGEND$) &
	\ FNSR$=CHR$(155%)+"["+BEGEND$+"r" &
	\ FNEND
30250	  DEF FNMESS$(CHN%,ERRNUM%,DESC$,TO.ERR%,PRINT.TEST%) &
	\ MESG$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERRNUM%)),3%,30%)+DESC$ &
	\ IF PRINT.TEST% THEN  &
		  PRINT #CHN%, FNP$("24;1");CLRLIN$;MESG$;FNP$("24;55");"Hit any key to continue."; &
		\ NW$=FNINP$(CHN%,128%," ",1%,TO.ERR%)
30260	  FNMESS$=MESG$ &
	\ FNEND
30400	  DEF FND8%(D8) &
	\ FND8%=D8 &
	\ FNEND
30410	  DEF FND6%(D9$) &
	\ FND6%=VAL(MID(D9$,3%,2%))+VAL(LEFT(D9$,2%))*32%+FND8%(VAL(RIGHT(D9$,5%)))*512% &
	\ FNEND
30420	  DEF FND6$(D9%) &
	\ FND6$=RIGHT(NUM1$((D9% AND 15%*32%)/32%+100%),2%)+RIGHT(NUM1$((D9% AND 31%)+100%),2%)+RIGHT(NUM1$((SWAP%(D9%) AND 254%)/2%+100%),2%) &
	\ FNEND
30430	  DEF FNZ(Y,N%) &
	\ FNZ=SGN(Y)*10.^-N%*INT(ABS(Y)*10.^N%+-8.9064364146906883e-05) &
	\ FNEND &
	\ V%=FNX%("",0%,"")
32767	  END
