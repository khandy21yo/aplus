10	  ! &
	  ! Program name: sumhis		Compiled with SCALE 0 on V08.0 &
	  ! Decompiled on 28-Nov-16 at 05:11 PM
30	  ON ERROR GOTO 19000 &
	\ JUNK$=SYS(CHR$(6%)+CHR$(-7%)) &
	\ TEMP$=SYS(CHR$(12%)) &
	\ PRJPRG$=MID(TEMP$,23%,2%)+NUM1$(ASCII(MID(TEMP$,25%,1%)))+":"+"["+NUM1$(ASCII(MID(TEMP$,6%,1%)))+","+NUM1$(ASCII(MID(TEMP$,5%,1%)))+"]"+RAD$(ASCII(MID(TEMP$,7%,1%))+SWAP%(ASCII(MID(TEMP$,8%,1%))))+RAD$(ASCII(MID(TEMP$,9%,1%))+SWAP%(ASCII(MID(TEMP$,10%,1%)))) &
	\ TEMP$=SYS(CHR$(6%)+CHR$(9%)) &
	\ JJ%=ASCII(LEFT(TEMP$,1%))/2% &
	\ JJ$=RIGHT(NUM1$(JJ%+100%),2%)
60	  CH%,OUTPUT.CH%=1% &
	\ OPEN "KB:" AS FILE 1%, MODE 8%+256%
70	  ESC$=CHR$(155%) &
	\ COLM.ON$=ESC$+"[?3h" &
	\ COLM.OFF$=ESC$+"[?3l" &
	\ R.ON$=ESC$+"[7m" &
	\ G.OFF$=ESC$+"[m" &
	\ CLRLIN$=ESC$+"[2K" &
	\ CLSCN$=ESC$+"[H"+ESC$+"[J" &
	\ ENTER.COPY$=ESC$+"[5i" &
	\ EXIT.COPY$=ESC$+"[4i" &
	\ HID.CURSOR$=ESC$+"[?25l"+ESC$+"[24;1h" &
	\ CUR.ON$=ESC$+"[?25h"
80	  DROP.DEAD.DATE$="        " &
	\ VERSION.NO$="V1.0" &
	\ DATE.TIME$=SYS(CHR$(6%)+CHR$(20%)+CHR$(0%)+CHR$(0%)+CHR$(1%)+CHR$(SWAP%(1%))+STRING$(11%,0%)+CHR$(SWAP%(0%))+CHR$(1%)+CHR$(SWAP%(1%))) &
	\ IF DROP.DEAD.DATE$<>"" THEN  &
		  IF DROP.DEAD.DATE$<MID(DATE.TIME$,7%,8%) THEN  &
			  MESG$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(47%)),3%,30%) &
			\ PRINT #CH%, MESG$;SYS(CHR$(9%))
100	  DEVICE.CH%,PRNT.TMP%=2% &
	\ WOJREL.CH%=2% &
	\ MENU.CH%=12% &
	\ PRINT.WIDTH%=132% &
	\ WIDTH%=PRINT.WIDTH%-32%
120	  OPEN "PRNT"+JJ$+".TMP" FOR INPUT AS FILE PRNT.TMP% &
	\ NEXT.REPORT%=0% &
	\ CHAIN.LINE%=0% &
	\ CHAIN.PROGRAM$=""
130	  INPUT LINE #PRNT.TMP%, TEMP$ &
	\ PR$=LEFT(TEMP$,2%) &
	\ CMD$=CVT$$(RIGHT(TEMP$,4%),140%) &
	\ IF PR$="RN" THEN  &
		  GOTO 160 IF NEXT.REPORT% &
		\ NEXT.REPORT%=-1%
140	  LIN.PER.PAGE%=VAL(CMD$) IF PR$="LP" &
	\ SPAGE%=VAL(CMD$) IF PR$="SP" &
	\ EPAGE%=VAL(CMD$) IF PR$="EP" &
	\ COPIES%=VAL(CMD$) IF PR$="CP" &
	\ REPDATE$=CMD$ IF PR$="RD" &
	\ DISPLAY$=CMD$ IF PR$="DP" &
	\ AUTOSCROLL$=CMD$ IF PR$="AS" &
	\ SPOOLR$=CMD$ IF PR$="SL" &
	\ OUTDEV$=CMD$ IF PR$="OD" &
	\ MS$=CMD$ IF PR$="MS" &
	\ UD1$=CMD$ IF PR$="U1" &
	\ UD2$=CMD$ IF PR$="U2" &
	\ UD3$=CMD$ IF PR$="U3" &
	\ UD4$=CMD$ IF PR$="U4" &
	\ UD5$=CMD$ IF PR$="U5" &
	\ IF PR$="PC" THEN  &
		  TEMP%=1% &
		\ TEMP%=2% IF LEFT(CMD$,1%)="\" &
		\ PC$=PC$+CHR$(VAL(MID(CMD$,TEMP%+(LOOP%-1%),3%))) FOR LOOP%=1% TO LEN(CMD$) STEP 4%
150	  GOTO 130
160	  GOTO 190 IF CHAIN.FLAG%=0% &
	\ KILL "PRNT"+JJ$+".TMP" &
	\ GOTO 190 IF PR$<>"RN"
170	  OPEN "PRNT"+JJ$+".TMP" FOR OUTPUT AS FILE PRNT.TMP%+1% &
	\ CHAIN.LINE%=0%
180	  PRINT #PRNT.TMP%+1%, TEMP$; &
	\ INPUT LINE #PRNT.TMP%, TEMP$ &
	\ CHAIN.PROGRAM$=CVT$$(RIGHT(TEMP$,4%),140%) IF LEFT(TEMP$,2%)="PG" AND CHAIN.PROGRAM$="" &
	\ GOTO 180
190	  FROM.ITEM$="ALL" &
	\ FROM.ITEM$=CVT$$(UD1$,132%) IF UD1$<>"" &
	\ TO.ITEM$=CVT$$(UD2$,132%) &
	\ CLOSE PRNT.TMP% &
	\ CLOSE PRNT.TMP%+1%
200	  IF FNO%(DEVICE.CH%,"DEVICE.DAT","/RO","")=0% THEN  &
		  WOJREL.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) IF FNG%(DEVICE.CH%,"WOJREL")=0% &
		\ V%=FNC%(DEVICE.CH%)
300	  V%=FNO%(WOJREL.CH%,WOJREL.DEVICE$+"WOJREL.DAT","/RO","") &
	\ IF FNS% THEN  &
		  V$=FNMESS$(CH%,FNS%," WOJREL.DAT ",0%,-1%) &
		\ GOTO 17400
400	  OPEN "MENU.FIL/RO" FOR INPUT AS FILE MENU.CH% &
	\ DIM #12%, A0$(1%)=64% &
	\ COMPANY$=CVT$$(RIGHT(A0$(1%),2%),128%) &
	\ CLOSE MENU.CH%
450	  OPEN "NL:" AS FILE 12%, RECORDSIZE 128% &
	\ FIELD #12%, 6% AS WOJREL.WONUM$,2% AS WOJREL.ITEM$,2% AS WOJREL.TYPE$,15% AS WOJREL.PRONUM$,3% AS WOJREL.STORNUM$,30% AS WOJREL.DESC$,8% AS WOJREL.QTY$,2% AS WOJREL.BEGDAT$,2% AS WOJREL.ENDDAT$,8% AS WOJREL.ACCNUM$,8% AS WOJREL.AMOUNT$ &
	\ FIELD #12%, 128% AS WOJREL.BUF$
510	  COLM.ON$="" IF PRINT.WIDTH%=80% &
	\ TITLE$="WORK ORDERS IN PROCESS" &
	\ TITLE$=LEFT(SPACE$((WIDTH%-LEN(TITLE$))/2%)+TITLE$+SPACE$(WIDTH%),WIDTH%) &
	\ COMPANY$=CVT$$(LEFT(COMPANY$,WIDTH%),128%) &
	\ COMPANY$=LEFT(SPACE$((WIDTH%-LEN(COMPANY$))/2%)+COMPANY$+SPACE$(WIDTH%),WIDTH%) &
	\ PAGE.TOP$=STRING$(2%,10%)+CHR$(13%)+"Date: "+DATE$(0%)+" "+COMPANY$+"        Page <<###>>"+CHR$(10%)+CHR$(13%)+"Time: "+TIME$(0%)+"  "+TITLE$+"        "+VERSION.NO$+CHR$(10%)+CHR$(13%)+SPACE$((PRINT.WIDTH%-LEN(REPDATE$))/2%)+REPDATE$+STRING$(2%,10%)+CHR$(13%)+STRING$(PRINT.WIDTH%,61%)+CHR$(10%)+CHR$(13%) &
	\ PAGE.TOP1$="WO #   ITM PRODUCT #      DESCRIPTION"+SPACE$(21%)+"  ST-DATE  DUE-DATE  ORDERED   B-OFF CANCELD SER-NUM "+"   BALANCE  CLO-DATE"+CHR$(10%)+CHR$(13%) &
	\ PAGE.A$=STRING$(PRINT.WIDTH%,61%)+CHR$(10%)+CHR$(13%) &
	\ USE$="\    \ ### \             \ \"+SPACE$(28%)+"\"+"  \      \ \      \  ###,### ###,### ###,### ###,### "+"###,###.##  \      \" &
	\ BOT.MARGIN%=6% &
	\ PAGE.BOT$=STRING$(BOT.MARGIN%,10%)+CHR$(13%) &
	\ PAGE.BREAK%=LIN.PER.PAGE%-BOT.MARGIN% &
	\ MORE.LINES%=2% &
	\ DISPLAY.CONTROL%=13% &
	\ IF FROM.ITEM$<>"ALL" THEN &
		  START.TEMP$=FROM.ITEM$ &
	  ELSE &
		  START.TEMP$=""
10000	  IF DISPLAY$<>"Y" THEN  &
		  CLOSE CH% &
		\ RESET.CH%=1% &
		\ TEMP.CH%=12% &
		\ OPEN OUTDEV$ AS FILE OUTPUT.CH%, MODE 2% &
		\ IF SPAGE%>1% THEN  &
			  OPEN "NL:" AS FILE TEMP.CH% &
			\ OUTPUT.CH%=TEMP.CH%
17000	  V%=FNG%(WOJREL.CH%,START.TEMP$) &
	\ IF DISPLAY$="Y" THEN &
		  PRINT #OUTPUT.CH%, CLSCN$;R.ON$;COLM.ON$;PAGE.TOP1$;G.OFF$;FNSR$("2;19");FNP$("19;01"); &
	  ELSE &
		  PRINT #OUTPUT.CH%, ENTER.COPY$;FNPAGE$(LIN.PER.PAGE%,0%,1%,PAGE.TOP$+PAGE.TOP1$+PAGE.A$,""); &
		\ LINE.COUNT%=FNLINE%
17030	  LSET WOJREL.BUF$=FNL$ &
	\ GOTO 17400 IF TO.ITEM$<WOJREL.WONUM$ AND TO.ITEM$<>""
17035	  L.WONUM$=WOJREL.WONUM$+"" &
	\ L.ITEM$=WOJREL.ITEM$+"" &
	\ L.DESC$=WOJREL.DESC$+"" &
	\ L.PRONUM$=WOJREL.PRONUM$+"" &
	\ DATE3$=CVT%$(FND6%("000000")) &
	\ WHILE WOJREL.WONUM$+WOJREL.ITEM$=L.WONUM$+L.ITEM$ AND FNS%=0% &
		\ TP%=VAL(WOJREL.TYPE$) &
		\ DATE1$=WOJREL.BEGDAT$+"" IF TP%=1% &
		\ DATE2$=WOJREL.ENDDAT$+"" IF TP%=1% &
		\ DATE3$=WOJREL.BEGDAT$+"" IF TP%=0% &
		\ QTY(TP%)=QTY(TP%)+CVT$F(WOJREL.QTY$) &
		\ CON.FL%=FNN%(WOJREL.CH%) &
		\ LSET WOJREL.BUF$=FNL$ &
	\ NEXT
17040	  GOSUB 18000 &
	\ GOTO 17030 IF CON.FL%=0%
17400	  !
17500	  IF DISPLAY$<>"Y" THEN &
		  PRINT #OUTPUT.CH%, FNPAGE$(LIN.PER.PAGE%,LINE.COUNT%,FNPAGE%,"",PAGE.BOT$); &
	  ELSE &
		  PRINT #OUTPUT.CH%, STRING$(15%-LINE.COUNT%,10%); &
		\ V$=FNMESS$(OUTPUT.CH%,0%,"End of report",0%,-1%)
17540	  !
17550	  PRINT #OUTPUT.CH%, EXIT.COPY$; IF DISPLAY$<>"Y" &
	\ PRINT #OUTPUT.CH%, FNSR$("1;24");COLM.OFF$; IF DISPLAY$="Y" &
	\ CLOSE OUTPUT.CH% &
	\ V%=FNX%(CHAIN.PROGRAM$,CHAIN.LINE%,"")
17600	  IF DISPLAY$<>"Y" AND LINE.COUNT%+MORE.LINES%>PAGE.BREAK% THEN  &
		  END.FLAG%=-1% IF PAGE%>=EPAGE% AND EPAGE%<>0% &
		\ RETURN IF END.FLAG% &
		\ PRINT #OUTPUT.CH%, FNPAGE$(LIN.PER.PAGE%,LINE.COUNT%,FNPAGE%,PAGE.TOP$+PAGE.TOP1$+PAGE.A$,PAGE.BOT$); &
		\ LINE.COUNT%=FNLINE% &
		\ IF SPAGE%>1% THEN  &
			  IF SPAGE%<=PAGE% THEN  &
				  OUTPUT.CH%=RESET.CH% &
				\ PRINT #OUTPUT.CH%, ENTER.COPY$;FNPAGE$(LIN.PER.PAGE%,0%,PAGE%,PAGE.TOP$+PAGE.TOP1$+PAGE.A$,""); &
				\ LINE.COUNT%=FNLINE% &
				\ SPAGE%=0%
17650	  IF DISPLAY$="Y" AND LINE.COUNT%>DISPLAY.CONTROL% THEN  &
		  V$=FNMESS$(OUTPUT.CH%,0%,"",-1%,-1%) &
		\ LINE.COUNT%=0% &
		\ PRINT #OUTPUT.CH%, FNP$("24;1");CLRLIN$;FNP$("18;132")
17670	  RETURN
18000	  PRINT #OUTPUT.CH% &
	\ LINE.COUNT%=LINE.COUNT%+1%
18010	  PRINT #OUTPUT.CH%, USING USE$, L.WONUM$,CVT$%(L.ITEM$),L.PRONUM$,L.DESC$,FND6$(CVT$%(DATE1$)),FND6$(CVT$%(DATE2$)),QTY(1%),-QTY(3%),-QTY(2%),-QTY(4%),QTY(1%)+QTY(2%)+QTY(3%)+QTY(4%),FND6$(CVT$%(DATE3$)) &
	\ LINE.COUNT%=LINE.COUNT%+1% &
	\ GOSUB 17600 &
	\ QTY(I%)=0. FOR I%=1% TO 4% &
	\ RETURN
18900	  !
19000	  RESUME 17500 IF ERR=11% AND ERL=17030% &
	\ RESUME 510 IF ERL=400% &
	\ RESUME 17540 IF ERR=54% &
	\ RESUME 17550 IF ERL=17540% &
	\ RESUME 160 IF ERL=130% &
	\ RESUME 190 IF ERL=160% OR ERL=180% &
	\ IF ERR=28% THEN  &
		  JUNK$=SYS(CHR$(6%)+CHR$(-7%)) &
		\ RESUME 17500 IF OUTPUT.CH%=CH%
19010	  IF ERR=27% THEN  &
		  RESUME 70 IF ERL=60% &
		\ RESUME 18900
19020	  IF ERL=120% THEN  &
		  V$=FNMESS$(CH%,0%,"Missing print control file.  "+"Aborting",0%,-1%) &
		\ RESUME 17500
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
30250	  DEF FNMESS$(CHN%,ERRNUM%,DESC$,TO.ERR%,PRINT.TEST%) &
	\ MESG$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERRNUM%)),3%,30%)+DESC$ &
	\ MESG$=DESC$+"" IF ERRNUM%<1% &
	\ IF PRINT.TEST% THEN  &
		  PRINT #CHN%, FNP$("24;1");CLRLIN$;MESG$;FNP$("24;"+NUM1$(PRINT.WIDTH%-25%))+"Hit any key to continue"; &
		\ NW$=FNINP$(CHN%,128%," ",1%,TO.ERR%) &
		\ PRINT #CHN%, CLRBOT$;
30260	  FNMESS$=MESG$ &
	\ FNEND
30280	  DEF FNSR$(BEGEND$) &
	\ FNSR$=ESC$+"["+BEGEND$+"r" &
	\ FNEND
30310	  DEF FNT24P$(TYME$) &
	\ FNT24P$=RIGHT(NUM1$(100%+ASCII(LEFT(TYME$,1%))),2%)+":"+RIGHT(NUM1$(100%+ASCII(RIGHT(TYME$,2%))),2%) &
	\ FNEND
30400	  DEF FND8%(D8) &
	\ FND8%=D8 &
	\ FNEND
30410	  DEF FND6%(D9$) &
	\ FND6%=VAL(MID(D9$,3%,2%))+VAL(LEFT(D9$,2%))*32%+FND8%(VAL(RIGHT(D9$,5%)))*512% &
	\ FNEND
30420	  DEF FND6$(D9%) &
	\ FND6$=RIGHT(NUM1$((D9% AND 15%*32%)/32%+100%),2%)+"/"+RIGHT(NUM1$((D9% AND 31%)+100%),2%)+"/"+RIGHT(NUM1$((SWAP%(D9%) AND 254%)/2%+100%),2%) &
	\ FNEND
30600	  DEF FNPAGE$(Y0%,Y1%,Y2%,Y0$,Y1$) &
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
30800	  DEF FNCOMP%(Y$,Y2$) &
	\ Y9%=0% &
	\ Y9%=-1% IF Y2$="*" &
	\ Y2$=Y2$+","
30820	  IF Y9%=0% THEN  &
		  Y1$=LEFT(Y2$,INSTR(1%,Y2$,",")-1%) &
		\ Y2$=RIGHT(Y2$,LEN(Y1$)+2%) &
		\ Y1%=INSTR(1%,Y1$,"/") &
		\ Y2%=LEN(Y1$)-Y1% &
		\ IF Y1%+INSTR(1%,Y1$,"?")=0% THEN &
			  Y9%=Y$=Y1$ &
		  ELSE &
			  IF Y1% THEN &
				  Y9%=LEFT(Y1$,Y1%-1%)<=LEFT(Y$,Y1%-1%) AND LEFT(Y$,Y2%)<=RIGHT(Y1$,Y1%+1%) &
			  ELSE &
				  CHANGE CVT$$(LEFT(Y$,30%),-1%) TO Y% &
				\ CHANGE CVT$$(LEFT(Y1$,30%),-1%) TO Y1% &
				\ GOTO 30830 IF (Y%(Y3%)<>Y1%(Y3%))-(Y1%(Y3%)=63%) FOR Y3%=1% TO Y1%(0%) &
				\ Y9%=-1%
30830	  GOTO 30820 IF Y2$<>"" AND Y9%=0% &
	\ FNCOMP%=Y9% &
	\ FNEND
30900	  CHAIN.FLAG%=-1%
31000	  GOTO 30
32767	  END
