10	  ! &
	  ! Program name: glr014		Compiled with SCALE 0 on V09.7 &
	  ! Decompiled on 29-Dec-16 at 02:12 PM
20	  PRINT "?Please run REPORT" &
	\ M$=SYS(CHR$(9%))
30	  ON ERROR GOTO 19000 &
	\ JUNK$=SYS(CHR$(6%)+CHR$(-7%)) &
	\ JUNK$=SYS(CHR$(6%)+CHR$(9%)) &
	\ KBN%=ASCII(MID(JUNK$,2%,1%))/2%
60	  CH%,OUTPUT.CH%=1% &
	\ OPEN "KB:" AS FILE 1%, MODE 8%+256%
70	  ESC$=CHR$(155%) &
	\ CLSCN$=ESC$+"[H"+ESC$+"[J" &
	\ COLM.ON$=ESC$+"[?3h" &
	\ COLM.OFF$=ESC$+"[?3l" &
	\ CLRLIN$=ESC$+"[2K" &
	\ ENTER.COPY$=ESC$+"[5i" &
	\ EXIT.COPY$=ESC$+"[4i"
80	  DROP.DEAD.DATE$="        " &
	\ VERSION.NO$="V1.0" &
	\ IF DROP.DEAD.DATE$<>"" THEN  &
		  IF DROP.DEAD.DATE$<MID(SYS(CHR$(6%)+CHR$(20%)+CHR$(0%)+CHR$(0%)+CHR$(1%)+CHR$(SWAP%(1%))),7%,8%) THEN  &
			  MESG$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(47%)),3%,30%) &
			\ PRINT #CH%, MESG$; &
			\ V$=SYS(CHR$(9%))
100	  DEVICE.CH%,PPRINT.CH%=2% &
	\ GLMMM.CH%=4% &
	\ MENU.CH%=12%
150	  IF FNO%(PPRINT.CH%,"PPRINT.DAT","/RO","") THEN  &
		  V$=FNMESS$(CH%,FNS%,"PPRINT.DAT",0%,-1%) &
		\ GOTO 17540
160	  IF FNG%(PPRINT.CH%,REP.NUM$) THEN  &
		  V$=FNMESS$(CH%,FNS%,"PPRINT.DAT "+REP.NUM$,0%,-1%) &
		\ GOTO 17540
170	  FIELD #PPRINT.CH%+1%, FNL%+6% AS PPRINT.MUNG$,20% AS PPRINT.REPDESC$,30% AS PPRINT.MUNG$,1% AS PPRINT.HPITCH$,1% AS PPRINT.VPITCH$,1% AS PPRINT.LPAGE$,20% AS PPRINT.OUTDEV$,6% AS PPRINT.SPOOLR$,2% AS PPRINT.SPAGE$,2% AS PPRINT.EPAGE$,2% AS PPRINT.COPIES$,20% AS PPRINT.REPDATE$,44% AS JUNK$,20% AS PPRINT.UDF$(1%),44% AS JUNK$,20% AS PPRINT.UDF$(2%),44% AS JUNK$,20% AS PPRINT.UDF$(3%),44% AS JUNK$,20% AS PPRINT.UDF$(4%),44% AS JUNK$,20% AS PPRINT.UDF$(5%) &
	\ FIELD #PPRINT.CH%+1%, FNL%+441% AS JUNK$,3% AS B.SYSNAM$,8% AS MUNG$,1% AS B.DISPLAY$ &
	\ REPDESC$=PPRINT.REPDESC$+"" &
	\ HPITCH%=ASCII(PPRINT.HPITCH$) &
	\ VPITCH%=ASCII(PPRINT.VPITCH$) &
	\ LIN.PER.PAGE%=ASCII(PPRINT.LPAGE$) &
	\ OUTDEV$=PPRINT.OUTDEV$+"" &
	\ SPAGE%=CVT$%(PPRINT.SPAGE$) &
	\ EPAGE%=CVT$%(PPRINT.EPAGE$) &
	\ COPIES%=CVT$%(PPRINT.COPIES$) &
	\ REPDATE$=CVT$$(PPRINT.REPDATE$,132%) &
	\ DISPLAY$=B.DISPLAY$+""
180	  MONTH$=LEFT(CVT$$(PPRINT.UDF$(2%),-1%),3%) &
	\ YEAR$=RIGHT(CVT$$(PPRINT.UDF$(2%),-1%),4%) &
	\ MONTH1$=LEFT(CVT$$(PPRINT.UDF$(3%),-1%),3%) &
	\ YEAR1$=RIGHT(CVT$$(PPRINT.UDF$(3%),-1%),4%) &
	\ SRCH$=CVT$$(PPRINT.UDF$(4%),-1%) &
	\ WLDCRD$=CVT$$(PPRINT.UDF$(5%),-1%)
190	  V%=FNC%(PPRINT.CH%)
200	  IF FNO%(DEVICE.CH%,"DEVICE.DAT","/RO","")=0% THEN  &
		  GLMMM.DEVICE$=MID(FNL$,7%,20%)+" IF FNG%(DEVICE.CH%,"GLMMM")=0% &
		\ V%=FNC%(DEVICE.CH%)
300	  !
400	  OPEN "MENU.FIL/RO" FOR INPUT AS FILE MENU.CH% &
	\ DIM #12%, A0$(1%)=64% &
	\ COMPANY$=CVT$$(RIGHT(A0$(1%),2%),128%) &
	\ CLOSE MENU.CH%
450	  !
510	  TITLE$="GENERAL LEDGER AUDIT LISTING" &
	\ REPDATE$="From "+MONTH$+", "+YEAR$+"to "+MONTH1$+", "+YEAR1$ &
	\ PAGE.TOP$=STRING$(3%,10%)+CHR$(13%)+"Date: "+DATE$(0%)+SPACE$(50%-LEN(COMPANY$)/2%)+COMPANY$+SPACE$(57%-LEN(COMPANY$)/2%)+"Page <<###>>"+CHR$(10%)+CHR$(13%)+"Time: "+TIME$(0%)+SPACE$(51%-LEN(TITLE$)/2%)+TITLE$+SPACE$(57%-LEN(TITLE$)/2%)+"V1.0"+CHR$(10%)+CHR$(13%)+SPACE$(65%-LEN(REPDATE$)/2%)+REPDATE$+STRING$(2%,10%)+CHR$(13%)+STRING$(131%,61%)+CHR$(10%)+CHR$(13%) &
	\ PAGE.TOP1$=" REC #   "+"ACCT #    "+"TRANDATE  "+"SC  "+"REFERENCE   "+"CHECK     "+"VOUCH   "+"DESCRIPTION           "+"XREF #  "+"SUBCD   "+"        AMOUNT"+"  AMOUNT RUN TOT" &
	\ USE.1$="######   "+"\      \  "+"\      \  "+"\\  "+"\        \  "+"\      \  "+"\    \  "+"\                  \  "+"\    \  "+"\    \  "+"###,###,###.##"+"  ###,###,###.##" &
	\ PAGE.BOT$=STRING$(6%,10%) &
	\ BOT.MARGIN%=9% &
	\ MORE.LINES%=2%
10000	  IF DISPLAY$<>"Y" THEN  &
		  CLOSE CH% &
		\ RESET.CH%=1% &
		\ TEMP.CH%=12% &
		\ OPEN OUTDEV$ AS FILE OUTPUT.CH%, MODE 2% &
		\ IF SPAGE%>1% THEN  &
			  OPEN "NL:" AS FILE TEMP.CH% &
			\ OUTPUT.CH%=TEMP.CH%
17000	  PAGE.BREAK%=LIN.PER.PAGE%-BOT.MARGIN% &
	\ I%=INSTR(1%,"JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC",MONTH1$) &
	\ LAST.MON$=YEAR1$+LEFT("00",2%-LEN(NUM1$(I%)))+NUM1$(I%)
17010	  IF DISPLAY$="Y" THEN &
		  PRINT #OUTPUT.CH%, CLSCN$;COLM.ON$;PAGE.TOP1$;FNSR$("2;19");FNP$("19;1"); &
	  ELSE &
		  PRINT #OUTPUT.CH%, ENTER.COPY$;FNPAGE$(LIN.PER.PAGE%,0%,1%,PAGE.TOP$+PAGE.TOP1$+CHR$(10%)+CHR$(13%),""); &
		\ LINE.COUNT%=FNLINE%
17020	  V%=FNO%(GLMMM.CH%,GLMMM.DEVICE$+"GL"+MONTH$+"."+YEAR$+"S","/RO/SF/NS","") &
	\ V%=FNO%(GLMMM.CH%,GLMMM.DEVICE$+"GL"+MONTH$+".DAS","/RO/SF/NS","") IF FNS%=5% &
	\ IF FNS% THEN  &
		  V$=FNMESS$(CH%,FNS%,"GL"+MONTH$+".DAS",0%,-1%) &
		\ GOTO 17540
17025	  PRINT #OUTPUT.CH%, "..... Read GL file "+MONTH$+"."+YEAR$ &
	\ FIRST.REC%=1% &
	\ IF FNG%(GLMMM.CH%,"") THEN  &
		  MESG$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(FNS%)),3%,30%)+"GL"+MONTH$+"REC # "+NUM1$(1%) &
		\ PRINT #CH%, MESG$;FNP$("24;55");"Hit any key to continue"; &
		\ INP$=FNINP$(CH%,128%," ",1%,0%) &
		\ GOTO 17540
17030	  FIELD #GLMMM.CH%, FNL% AS TEMP$,8% AS GL.ACCNO$,2% AS GL.SOURCE$,16% AS GL.REFNO$,2% AS GL.TRANDAT$,26% AS GL.DESC$,8% AS GL.AMOUNT$,6% AS GL.XREFNO$,2% AS GL.POSTIM$,2% AS GL.POSDAT$,6% AS GL.BNKCDE$,8% AS GL.CKNO$,6% AS GL.VCHRNO$,6% AS GL.SUBACC$,6% AS GL.PHASE$,8% AS GL.REGQTY$,8% AS GL.PREQTY$,2% AS GL.UPDATE$,4% AS TEMP$,2% AS GL.POINTER$ &
	\ GOTO 17350 IF FNCOMP%(CVT$$(GL.ACCNO$,-1%),WLDCRD$)=0% AND WLDCRD$<>"" AND SRCH$="A" &
	\ GOTO 17350 IF FNCOMP%(CVT$$(GL.XREFNO$,-1%),WLDCRD$)=0% AND WLDCRD$<>"" AND SRCH$="X" &
	\ GOTO 17350 IF FNCOMP%(CVT$$(GL.SUBACC$,-1%),WLDCRD$)=0% AND WLDCRD$<>"" AND SRCH$="S" &
	\ GOTO 17350 IF CVT$%(GL.POINTER$)<=0% &
	\ AMOUNT=FNZ(CVT$F(GL.AMOUNT$)) &
	\ SOURCE$=GL.SOURCE$+"" &
	\ SOURCE$="CD" IF GL.SOURCE$="PR" OR GL.SOURCE$="PJ" OR GL.SOURCE$="SP" &
	\ SOURCE$="CR" IF SOURCE$="SJ"
17100	  IF SRCH$="B" AND SOURCE$<>TEST.SOURCE$ THEN  &
		  F.REC%=FIRST.REC% &
		\ FIRST.REC%=FNR(GLMMM.CH%) &
		\ IF RUNTOT<>0. THEN  &
			  PRINT #OUTPUT.CH%, "Record # ";F.REC%;" - ";LAST.REC%;" is out of balance ";RUNTOT &
			\ RUNTOT=0.
17110	  RUNTOT=FNZ(RUNTOT+AMOUNT) &
	\ LAST.REC%=FNR(GLMMM.CH%) &
	\ TEST.SOURCE$=SOURCE$+"" &
	\ IF SRCH$<>"B" THEN  &
		  PRINT #OUTPUT.CH%, USING USE.1$, FNR(GLMMM.CH%),CVT$$(GL.ACCNO$,140%),FND6$(CVT$%(GL.TRANDAT$)),CVT$$(GL.SOURCE$,132%),CVT$$(GL.REFNO$,140%),CVT$$(GL.CKNO$,140%),CVT$$(GL.VCHRNO$,132%),CVT$$(GL.DESC$,132%),CVT$$(GL.XREFNO$,140%),CVT$$(GL.SUBACC$,140%),AMOUNT,RUNTOT &
		\ LINE.COUNT%=LINE.COUNT%+1% &
		\ GOSUB 17600 &
		\ GOTO 17400 IF END.FLAG%
17350	  IF FNN%(GLMMM.CH%)=0% THEN  &
		  GOTO 17030
17355	  IF SRCH$="B" AND SOURCE$<>TEST.SOURCE$ AND RUNTOT<>0. THEN  &
		  PRINT #OUTPUT.CH%, "Record # ";LAST.REC%;" is out of balance ";RUNTOT &
		\ RUNTOT=0.
17360	  V%=FNC%(GLMMM.CH%) &
	\ I%=INSTR(1%,"JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC",MONTH$) &
	\ IF MONTH$<>"DEC" THEN &
		  MONTH$=MID("JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC",I%+3%,3%) &
	  ELSE &
		  MONTH$="JAN" &
		\ YEAR$=NUM1$(VAL(YEAR$)+1%)
17370	  I%=INSTR(1%,"JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC",MONTH$) &
	\ NEXT.MON$=YEAR$+LEFT("00",2%-LEN(NUM1$(I%)))+NUM1$(I%) &
	\ GOTO 17020 IF NEXT.MON$<=LAST.MON$
17400	  IF END.FLAG%=0% THEN  &
		  GOSUB 17600
17500	  IF DISPLAY$<>"Y" THEN &
		  PRINT #OUTPUT.CH%, FNPAGE$(LIN.PER.PAGE%,LINE.COUNT%,FNPAGE%,"",PAGE.BOT$); &
	  ELSE &
		  PRINT #OUTPUT.CH%, FNP$("24;1");"End of report";FNP$("24;105");"Hit any key to continue"; &
		\ INP$=FNINP$(CH%,128%," ",1%,0%)
17540	  PRINT #OUTPUT.CH%, EXIT.COPY$; &
	\ CLOSE OUTPUT.CH% &
	\ V%=FNX%("",0%,"")
17600	  IF DISPLAY$<>"Y" AND LINE.COUNT%+2%>PAGE.BREAK% THEN  &
		  END.FLAG%=-1% IF PAGE%>=EPAGE% AND EPAGE%<>0% &
		\ RETURN IF END.FLAG% &
		\ PRINT #OUTPUT.CH%, FNPAGE$(LIN.PER.PAGE%,LINE.COUNT%,FNPAGE%,PAGE.TOP$+PAGE.TOP1$+CHR$(10%)+CHR$(13%),PAGE.BOT$); &
		\ LINE.COUNT%=FNLINE% &
		\ IF SPAGE%>1% THEN  &
			  IF SPAGE%<=PAGE% THEN  &
				  OUTPUT.CH%=RESET.CH% &
				\ PRINT #OUTPUT.CH%, ENTER.COPY$;FNPAGE$(LIN.PER.PAGE%,0%,PAGE%,PAGE.TOP$+PAGE.TOP1$+CHR$(10%)+CHR$(13%),""); &
				\ LINE.COUNT%=FNLINE% &
				\ SPAGE%=0%
17650	  IF DISPLAY$="Y" AND LINE.COUNT%>15% THEN  &
		  PRINT #OUTPUT.CH%, FNP$("24;105");"Hit any key to continue"; &
		\ INP$=FNINP$(CH%,128%," ",1%,1%) &
		\ LINE.COUNT%=0% &
		\ PRINT #OUTPUT.CH%, FNP$("24;1");CLRLIN$;FNP$("18;132")
17670	  RETURN
19000	  RESUME  IF ERR=52% &
	\ RESUME 450 IF ERL=400% &
	\ RESUME 70 IF ERL=60% AND ERR=27% &
	\ RESUME 17540 IF ERR=54%
19010	  IF ERR=28% THEN  &
		  JUNK$=SYS(CHR$(6%)+CHR$(-7%)) &
		\ RESUME 17500 IF OUTPUT.CH%=CH% &
		\ RESUME 17540
19900	  ON ERROR GOTO 0
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
	\ FNP$=ESC$+"["+ROWCOL$+"H" &
	\ FNEND
30230	  DEF FNZ(Z) &
	\ FNZ=INT(ABS(Z)*100.+-5512.88818359375)/100.*SGN(Z) &
	\ FNEND
30250	  DEF FNMESS$(CHN%,ERRNUM%,DESC$,TO.ERR%,PRINT.TEST%) &
	\ MESG$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERRNUM%)),3%,30%)+" "+DESC$ &
	\ IF PRINT.TEST% THEN  &
		  PRINT #CHN%, FNP$("24;1");CLRLIN$;MESG$;FNP$("24;55");"Hit any key to continue."; &
		\ NW$=FNINP$(CHN%,128%," ",1%,TO.ERR%)
30260	  FNMESS$=MESG$ &
	\ FNEND
30300	  DEF FNSR$(BEGEND$) &
	\ FNSR$=ESC$+"["+BEGEND$+"r" &
	\ FNEND
30500	  DEF FNI%(Y) &
	\ FNI%=Y &
	\ FNEND
30510	  DEF FND6$(D9%) &
	\ FND6$=RIGHT(NUM1$((D9% AND 15%*32%)/32%+100%),2%)+"/"+RIGHT(NUM1$((D9% AND 31%)+100%),2%)+"/"+RIGHT(NUM1$((SWAP%(D9%) AND 254%)/2%+100%),2%) &
	\ FNEND
30600	  !
30610	  DEF FNPAGE$(Y0%,Y1%,Y2%,Y0$,Y1$) &
	\ Y2$="" &
	\ Y2$=STRING$(Y0%-(Y1%+LEN(XLATE(Y1$,STRING$(10%,0%)+CHR$(10%)))),10%) IF Y1$<>"" &
	\ PAGE.LINE%=LEN(XLATE(Y0$,STRING$(10%,0%)+CHR$(10%))) &
	\ Y%=INSTR(1%,Y1$+Y0$,"<<#") &
	\ Y3%=INSTR(1%,Y1$+Y0$,"#>>") &
	\ Y$=RIGHT(NUM1$(0.+Y2%),8%-(Y3%-Y%)) &
	\ Y3%=-3% IF Y%=0% &
	\ PRINT #OUTPUT.CH%, Y2$;LEFT(Y1$+Y0$,Y%-1%);Y$;RIGHT(Y1$+Y0$,Y3%+3%); &
	\ PAGE%=Y2% &
	\ FNEND
30650	  DEF FNPAGE% &
	\ FNPAGE%=PAGE%+1% &
	\ FNEND
30660	  DEF FNLINE% &
	\ FNLINE%=PAGE.LINE% &
	\ FNEND
30900	  DEF FNCOMP%(Y$,Y2$) &
	\ Y9%=0% &
	\ Y9%=-1% IF Y2$="*" &
	\ Y2$=Y2$+","
30920	  IF Y9%=0% THEN  &
		  Y1$=LEFT(Y2$,INSTR(1%,Y2$,",")-1%) &
		\ Y2$=RIGHT(Y2$,LEN(Y1$)+2%) &
		\ Y1%=INSTR(1%,Y1$,"/") &
		\ IF Y1%+INSTR(1%,Y1$,"?")=0% THEN &
			  Y9%=Y$=Y1$ &
		  ELSE &
			  IF Y1% THEN &
				  Y9%=LEFT(Y1$,Y1%-1%)<=Y$ AND Y$<=RIGHT(Y1$,Y1%+1%) &
			  ELSE &
				  CHANGE CVT$$(LEFT(Y$,30%),-1%) TO Y% &
				\ CHANGE CVT$$(LEFT(Y1$,30%),-1%) TO Y1% &
				\ GOTO 30930 IF (Y%(Y3%)<>Y1%(Y3%))-(Y1%(Y3%)=63%) FOR Y3%=1% TO Y1%(0%) &
				\ Y9%=-1%
30930	  IF Y2$<>"" AND Y9%=0% THEN &
		  GOTO 30920 &
	  ELSE &
		  FNCOMP%=Y9%
30940	  FNEND
31000	  M$=SYS(CHR$(7%)) &
	\ REP.NUM$=MID(M$,INSTR(1%,M$,"~")+1%,6%) &
	\ GOTO 30
32767	  END