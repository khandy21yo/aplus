10	  ! &
	  ! Program name: progr4		Compiled with SCALE 0 on V08.0 &
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
	\ EXIT.COPY$=ESC$+"[4i"
80	  DROP.DEAD.DATE$="        " &
	\ VERSION.NO$="V1.0" &
	\ IF DROP.DEAD.DATE$<>"" THEN  &
		  IF DROP.DEAD.DATE$<MID(SYS(CHR$(6%)+CHR$(20%)+CHR$(0%)+CHR$(0%)+CHR$(1%)+CHR$(SWAP%(1%))),7%,8%) THEN  &
			  MESG$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(47%)),3%,30%) &
			\ PRINT #CH%, MESG$; &
			\ V$=SYS(CHR$(9%))
100	  DIM FILE.NAME$(300%), SYS.CALL%(30%), FIL.NAM%(30%), Y%(32%), Y1%(32%) &
	\ PROGRAM.CH%=2% &
	\ LOG.CH%=4%
2000	  PRINT #CH%, COLM.OFF$;FNP$("4;28");"PROGRAM ANALYSIS";FNP$("6;28");"Program to search for "; &
	\ PROGRAM$=CVT$$(FNINP$(CH%,0%,"_",20%,0%),-1%) &
	\ GOTO 32767 IF PROGRAM$="E" &
	\ PRINT #CH%, CLSCN$;FNP$("24;1");CLRLIN$;"Searching. . .";R.ON$; &
	\ LIN%=1% &
	\ COL%=-11% &
	\ FILES%=FNFILE%(PROGRAM$) &
	\ PRINT #CH%, G.OFF$;FNP$("23;70");CLRLIN$;FILES%;" FILES";
2010	  PRINT #CH%, FNP$("24;1");CLRLIN$;"Log file name "; &
	\ LOG.FILE$=CVT$$(FNINP$(CH%,0%,"_",20%,0%),-1%) &
	\ J%=0%
2020	  PRINT #CH%, FNP$("24;1");CLRLIN$;"Search from line "; &
	\ SEARCH$=FNINP$(CH%,0%,"_",5%,0%) &
	\ GOTO 2000 IF SEARCH$="E" &
	\ GOTO 2100 IF SEARCH$="" &
	\ J%=J%+1% &
	\ SEARCH$=XLATE(SEARCH$,STRING$(48%,0%)+"0123456789") &
	\ SEARCH%(J%,1%)=VAL(SEARCH$) &
	\ PRINT #CH%, FNP$(NUM1$(J%+1%)+";60");SEARCH$;
2030	  PRINT #CH%, FNP$("24;40");"to line "; &
	\ SEARCH$=FNINP$(CH%,0%,"_",5%,0%) &
	\ SEARCH$=XLATE(SEARCH$,STRING$(48%,0%)+"0123456789") &
	\ SEARCH%(J%,2%)=VAL(SEARCH$) &
	\ PRINT #CH%, FNP$(NUM1$(J%+1%)+";74");SEARCH$; &
	\ GOTO 2020
2100	  OPEN LOG.FILE$ FOR OUTPUT AS FILE LOG.CH% &
	\ J%=1% &
	\ PRINT #LOG.CH%, "**********"+LOG.FILE$ &
	\ WHILE SEARCH%(J%,2%)<>0% &
		\ PRINT #LOG.CH%, NUM1$(SEARCH%(J%,1%))+"-"+NUM1$(SEARCH%(J%,2%)) &
		\ J%=J%+1% &
	\ NEXT &
	\ PRINT #LOG.CH%, "*****************************************************" &
	\ LIN%=1% &
	\ COL%=-11%
2200	  FOR LOOP%=1% TO FILES% &
		\ OPEN FILE.NAME$(LOOP%) FOR INPUT AS FILE PROGRAM.CH% &
		\ R.BRACK%=INSTR(1%,FILE.NAME$(LOOP%),"]") &
		\ LIN%=LIN%+1% &
		\ LIN%=2% IF LIN%=23% &
		\ COL%=COL%+13% IF LIN%=2% &
		\ PRINT #CH%, FNP$(NUM1$(LIN%)+";"+NUM1$(COL%))+RIGHT(FILE.NAME$(LOOP%),R.BRACK%+1%); &
		\ PRINT #CH%, FNP$("24;1");CLRLIN$;"Searching. . . ";NUM1$(LOOP%);" ";FILE.NAME$(LOOP%);" "; &
		\ STORE%=0% &
		\ J%=1% &
		\ PRINT #LOG.CH% &
		\ PRINT #LOG.CH%, "*****TASK NAME = ";FILE.NAME$(LOOP%) &
		\ PRINT #LOG.CH%
2300		  INPUT LINE #PROGRAM.CH%, A$ &
		\ LINNE%=VAL(XLATE(LEFT(A$,5%),STRING$(48%,0%)+"0123456789")) &
		\ IF SEARCH%(J%,2%)<LINNE% THEN  &
			  J%=J%+1% &
			\ STORE%=0%
2310		  GOTO 2900 IF SEARCH%(J%,2%)=0% &
		\ IF SEARCH%(J%,1%)<=LINNE% AND LINNE%<=SEARCH%(J%,2%) THEN  &
			  STORE%=-1%
2320		  IF STORE% THEN  &
			  PRINT #LOG.CH%, A$;
2600		  PRINT #CH%, RECORD 256%,CHR$(129%); &
		\ GET #CH%, RECORD 8192%
2700		  FIELD #CH%, RECOUNT AS TEST$ &
		\ GOTO 3000 IF INSTR(1%,TEST$,CHR$(3%))
2800		  GOTO 2300
2900		  CLOSE PROGRAM.CH% &
	\ NEXT LOOP%
3000	  CLOSE LOG.CH% &
	\ CLOSE PROGRAM.CH% &
	\ PRINT #CH%, CLSCN$;FNSR$("1;24"); &
	\ GOTO 32767
19000	  RESUME 2900 IF ERL=2300% &
	\ RESUME 2800 IF ERL=2600% &
	\ RESUME 30130 IF ERL=30110%
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
30100	  DEF FNFILE%(FILE.SPEC$) &
	\ WLDCNT%=0% &
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
	\ FILE.NAME$(WLDCNT%)=WILD.FILE$ &
	\ R.BRACK%=INSTR(1%,WILD.FILE$,"]") &
	\ LIN%=LIN%+1% &
	\ LIN%=2% IF LIN%=23% &
	\ COL%=COL%+13% IF LIN%=2% &
	\ PRINT #CH%, FNP$(NUM1$(LIN%)+";"+NUM1$(COL%))+RIGHT(WILD.FILE$,R.BRACK%+1%);
30125	  GOTO 30110
30130	  FNFILE%=WLDCNT% &
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
32767	  END
