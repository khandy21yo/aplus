10	  ! &
	  ! Program name: progr2		Compiled with SCALE 0 on V09.3 &
	  ! Decompiled on 28-Nov-16 at 05:11 PM
30	  ON ERROR GOTO 19000 &
	\ JUNK$=SYS(CHR$(6%)+CHR$(-7%)) &
	\ TEMP$=SYS(CHR$(12%)) &
	\ PRJPRG$=MID(TEMP$,23%,1%)+MID(TEMP$,24%,1%)+NUM1$(ASCII(MID(TEMP$,25%,1%)))+":"+"["+NUM1$(ASCII(MID(TEMP$,6%,1%)))+","+NUM1$(ASCII(MID(TEMP$,5%,1%)))+"]"+RAD$(ASCII(MID(TEMP$,7%,1%))+SWAP%(ASCII(MID(TEMP$,8%,1%))))+RAD$(ASCII(MID(TEMP$,9%,1%))+SWAP%(ASCII(MID(TEMP$,10%,1%)))) &
	\ JUNK$=SYS(CHR$(6%)+CHR$(9%)) &
	\ JJ%=ASCII(LEFT(JUNK$,1%))/2% &
	\ JJ$=RIGHT(NUM1$(JJ%+100%),2%)
60	  CH%=1% &
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
	\ IF DROP.DEAD.DATE$<>"" THEN  &
		  IF DROP.DEAD.DATE$<MID(SYS(CHR$(6%)+CHR$(20%)+CHR$(0%)+CHR$(0%)+CHR$(1%)+CHR$(SWAP%(1%))),7%,8%) THEN  &
			  MESG$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(47%)),3%,30%) &
			\ PRINT #CH%, MESG$; &
			\ V$=SYS(CHR$(9%))
100	  DIM FILE.NAME$(300%), SYS.CALL%(30%), FIL.NAM%(30%), Y%(32%), Y1%(32%), INFO%(30%) &
	\ PROGRAM.CH%=2% &
	\ LOG.CH%=4% &
	\ PPN%=-1%
2000	  PRINT #CH%, COLM.OFF$;FNP$("4;28");"DATA PASTE PROGRAM ";
2005	  PRINT #CH%, FNP$("6;28");CLRLIN$;"Data to search for "; &
	\ PRGRM$=CVT$$(FNINP$(CH%,0%,"_",20%,0%),-1%) &
	\ COMMA%=INSTR(1%,PRGRM$,",") &
	\ BRACK%=INSTR(1%,PRGRM$,")") &
	\ LFT$=MID(PRGRM$,2%,COMMA%-2%) &
	\ LFT$="255" IF LFT$="*" &
	\ LFT%=VAL(LFT$) &
	\ RGH$=MID(PRGRM$,COMMA%+1%,BRACK%-COMMA%-1%) &
	\ RGH$="255" IF RGH$="*" &
	\ RGH%=VAL(RGH$) &
	\ PRGRM$=RIGHT(PRGRM$,BRACK%+1%)
2010	  PRINT #CH%, FNP$("8;28");"Log file name "; &
	\ LOG.FILE$=CVT$$(FNINP$(CH%,0%,"_",20%,0%),-1%)
2015	  PRINT #CH%, FNP$("10;28");"Type of File [I]) "; &
	\ ABC$=FNINP$(CH%,0%,"_",1%,0%) &
	\ ABC$="I" IF ABC$="" &
	\ INDEX%=-1% IF ABC$="I"
2020	  PRINT #CH%, FNP$("12;28");"Manual/Auto Control [M]) "; &
	\ ABC$=FNINP$(CH%,0%,"_",1%,0%) &
	\ AUTO%=-1% IF ABC$="A"
2040	  PRINT #CH%, CLSCN$;HID.CURSOR$;FNSR$("1;19");
2050	  PPN%=PPN%+1% &
	\ CHANGE SYS(CHR$(6%)+CHR$(25%)+CHR$(PPN%)+CHR$(SWAP%(PPN%))+CHR$(RGH%)+CHR$(LFT%)+STRING$(30%,0%)) TO INFO% &
	\ ACCOUNT$="["+NUM1$(INFO%(6%))+","+NUM1$(INFO%(5%))+"]" &
	\ PROGRAM$=ACCOUNT$+PRGRM$ &
	\ FILES%=FILES%+FNFILE%(PROGRAM$,FILES%) &
	\ PRINT #CH%, FNP$("24;1");CLRLIN$;"Searching. . .";ACCOUNT$; &
	\ GOTO 2050
2060	  PRINT #CH%, CLSCN$;FNSR$("1;19");FNP$("5;50");FILES%;" FILES";
2100	  STOP &
	\ OPEN LOG.FILE$ FOR OUTPUT AS FILE LOG.CH%
2200	  OPEN FILE.NAME$(1%) FOR INPUT AS FILE PROGRAM.CH% &
	\ PRINT #CH%, FNP$("24;1");CLRLIN$;"Updating. . .";FILE.NAME$(1%)
2210	  GET #PROGRAM.CH% &
	\ PUT #LOG.CH%+SWAP%(PROGRAM.CH%)
2220	  GOTO 2210
2250	  CLOSE LOG.CH% &
	\ CLOSE PROGRAM.CH% &
	\ GOTO 2400 IF INDEX%=0%
2300	  LENGT%=LEN(LOG.FILE$) &
	\ DATA.LOG$=LEFT(LOG.FILE$,LENGT%-1%)+"1" &
	\ LENGT%=LEN(FILE.NAME$(1%)) &
	\ DATA.FILE$=LEFT(FILE.NAME$(1%),LENGT%-1%)+"1" &
	\ OPEN DATA.LOG$ FOR OUTPUT AS FILE LOG.CH% &
	\ OPEN DATA.FILE$ FOR INPUT AS FILE PROGRAM.CH%
2310	  GET #PROGRAM.CH% &
	\ PUT #LOG.CH%+SWAP%(PROGRAM.CH%)
2320	  GOTO 2310
2350	  CLOSE LOG.CH% &
	\ CLOSE PROGRAM.CH%
2400	  STOP IF FNO%(LOG.CH%,LOG.FILE$,"/RW","")
2500	  FOR LOOP%=2% TO FILES% &
		\ IF AUTO%=0% THEN  &
			  PRINT #CH%, FNP$("14;1");CLRLIN$;"Paste the following File . . . ?";FILE.NAME$(LOOP%);" "; &
			\ ABC$=CVT$$(FNINP$(CH%,128%,"_",1%,0%),-1%) &
			\ GOTO 2599 IF ABC$<>"Y" AND ABC$<>"E" &
			\ GOTO 3000 IF ABC$<>"Y"
2510		  STOP IF FNO%(PROGRAM.CH%,FILE.NAME$(LOOP%),"/RO","") &
		\ GOTO 2590 IF FNG%(PROGRAM.CH%,"")
2520		  PRINT #CH%, FNP$("24;1");CLRLIN$;"Updating. . .";FILE.NAME$(LOOP%)
2550		  STOP IF FNA%(LOG.CH%,FNL$) &
		\ GOTO 2550 IF FNN%(PROGRAM.CH%)=0%
2590		  V%=FNC%(PROGRAM.CH%)
2599			  NEXT LOOP%
3000	  PRINT #CH%, CLSCN$;CUR.ON$;FNSR$("1;24"); &
	\ V%=FNX%("",1%,"")
19000	  RESUME  IF ERR=51% OR ERR=52% &
	\ RESUME 2250 IF ERL=2210% AND ERR=11% &
	\ RESUME 2350 IF ERL=2310% AND ERR=11% &
	\ RESUME 30130 IF ERL=30110% &
	\ RESUME 2060 IF ERL=2050% AND ERR=5%
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
30100	  DEF FNFILE%(FILE.SPEC$,COUNTER%) &
	\ WLDCNT%=COUNTER% &
	\ WLDCRD.FLAG%=0%
30110	  NEXXT%=0% &
	\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+FILE.SPEC$) TO SYS.CALL% &
	\ SYS.CALL%(0%)=30% &
	\ SYS.CALL%(1%)=6% &
	\ SYS.CALL%(2%)=17% &
	\ SYS.CALL%(3%)=WLDCRD.FLAG% &
	\ SYS.CALL%(4%)=SWAP%(WLDCRD.FLAG%) &
	\ WLDCRD.FLAG%=WLDCRD.FLAG%+1% &
	\ CHANGE SYS.CALL% TO SYS.CALL$ &
	\ CHANGE SYS(SYS.CALL$) TO FIL.NAM% &
	\ FIL.NAM%(23%)=ASCII("S") IF FIL.NAM%(23%)=0% &
	\ FIL.NAM%(24%)=ASCII("Y") IF FIL.NAM%(24%)=0% &
	\ WILD.FILE$=CHR$(FIL.NAM%(23%))+CHR$(FIL.NAM%(24%))+NUM1$(FIL.NAM%(25%))+":"+"["+NUM1$(FIL.NAM%(6%))+","+NUM1$(FIL.NAM%(5%))+"]"+RAD$(FIL.NAM%(7%)+SWAP%(FIL.NAM%(8%)))+RAD$(FIL.NAM%(9%)+SWAP%(FIL.NAM%(10%)))+"."+RAD$(FIL.NAM%(11%)+SWAP%(FIL.NAM%(12%)))
30120	  WLDCNT%=WLDCNT%+1% &
	\ FILE.NAME$(WLDCNT%)=WILD.FILE$
30125	  GOTO 30110
30130	  FNFILE%=WLDCNT%-COUNTER% &
	\ FNEND
30200	  DEF FNP$(ROWCOL$) &
	\ FNP$=ESC$+"["+ROWCOL$+"H" &
	\ FNEND
30250	  DEF FNMESS$(CHN%,ERRNUM%,DESC$,TO.ERR%) &
	\ MESG$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(ERRNUM%)),3%,30%)+DESC$ &
	\ PRINT #CHN%, FNP$("24;1");MESG$;FNP$("24;55");"Hit any key to continue."; &
	\ NW$=FNINP$(CHN%,128%," ",1%,TO.ERR%) &
	\ FNEND
30300	  DEF FNSR$(BEGEND$) &
	\ FNSR$=ESC$+"["+BEGEND$+"r" &
	\ FNEND
30500	  DEF FNZER$(Z%) &
	\ FNZER$=RIGHT(NUM1$(100%+Z%),2%) &
	\ FNEND
32767	  END
