10	  ! &
	  ! Program name: glset		Compiled with SCALE 0 on V09.3 &
	  ! Decompiled on 29-Dec-16 at 03:12 PM
50	  ON ERROR GOTO 19000
60	  CH%=1% &
	\ OPEN "KB:" AS FILE 1%, MODE 8%+256%
70	  ESC$=CHR$(155%) &
	\ CLSCN$=ESC$+"[H"+ESC$+"[J" &
	\ CLRLIN$=ESC$+"[2K" &
	\ CLEOS$=ESC$+"[J" &
	\ INSERT.LIN$=ESC$+"[1L" &
	\ G.OFF$=ESC$+"[m" &
	\ B.ON$=ESC$+"[1m" &
	\ BLINK.ON$=ESC$+"[5m" &
	\ R.ON$=ESC$+"[7m" &
	\ SCROLL.ON$=ESC$+"[1;24r" &
	\ SCROLL.REGION$=ESC$+"[5;19r" &
	\ CLRBOT$=ESC$+"[21;1H"+ESC$+"[J"
80	  DROP.DEAD.DATE$="        " &
	\ IF DROP.DEAD.DATE$<>"" THEN  &
		  IF DROP.DEAD.DATE$<MID(SYS(CHR$(6%)+CHR$(20%)+CHR$(0%)+CHR$(0%)+CHR$(1%)+CHR$(SWAP%(1%))),7%,8%) THEN  &
			  ERR.MES$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(47%)),3%,30%) &
			\ PRINT #CH%, ERR.MES$; &
			\ V$=SYS(CHR$(9%))
100	  DIM Y%(100%), Y1%(100%) &
	\ DEVICE.CH%=2% &
	\ GL.CH%=10% &
	\ MENU.CH%=12% &
	\ SLMCTL.CH%=8% &
	\ LOOP.DATA%=7% &
	\ LOOP.KEY%=1% &
	\ KEY.LEN%=16% &
	\ DATA.LEN%=64%
150	  TEMP$=SYS(CHR$(7%)) &
	\ TEMP%=INSTR(1%,TEMP$,CHR$(255%)) &
	\ SYSNAM$=CVT$$(MID(TEMP$,TEMP%+1%,3%),-1%) &
	\ IF SYSNAM$="" OR TEMP%=0% THEN  &
		  PRINT #CH%, CLSCN$;FNP$("4;1");"PLEASE ENTER THE SYSTEM ";"NAME "; &
		\ SYSNAM$=FNINP$(CH%,0%,"_",3%) &
		\ IF SYSNAM$="" OR SYSNAM$="%^C" THEN  &
			  V%=FNX%("",0%,"")
160	  PRINT #CH%, CLSCN$;B.ON$;R.ON$;BLINK.ON$;FNP$("1;75");"WAIT";G.OFF$;FNP$("24;1");
200	  IF FNO%(DEVICE.CH%,"DEVICE.DAT","/R0","")=0% THEN  &
		  SLMCTL.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) IF FNG%(DEVICE.CH%,"SLMCTL")=0% &
		\ GLMMM.DEVICE$=CVT$$(MID(FNL$,7%,20%),-1%) IF FNG%(DEVICE.CH%,"GLMMM")=0% &
		\ V%=FNC%(DEVICE.CH%)
300	  IF FNO%(SLMCTL.CH%,SLMCTL.DEVICE$+"SLMCTL.DAT","/RW","") THEN  &
		  ERR.MES$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(FNS%)),3%,30%)+" While opening 'SLMCTL.DAT'" &
		\ PRINT #CH%, FNP$("24;1");CLRLIN$;ERR.MES$;FNP$("24;55");"Hit any key to continue"; &
		\ INP$=FNINP$(CH%,128%," ",1%) &
		\ PRINT #CH%, SCROLL.ON$;CLSCN$;B.ON$;R.ON$;BLINK.ON$;R.ON$;FNP$("1;75");"WAIT";G.OFF$;FNP$("24;1"); &
		\ V%=FNX%("",0%,"")
400	  OPEN "MENU.FIL/RO" FOR INPUT AS FILE MENU.CH% &
	\ DIM #12%, A0$(255%)=64% &
	\ COMPANY$=CVT$$(RIGHT(A0$(1%),2%),128%) &
	\ CLOSE MENU.CH%
450	  OPEN "NL:" AS FILE 12%, RECORDSIZE 128%+512% &
	\ FIELD #12%, 128% AS GL.BUF$,512% AS SLMCTL.BUF$ &
	\ FIELD #12%, 8% AS GL.ACCNUM$,2% AS GL.SOURCE$,16% AS GL.REFNUM$,2% AS GL.TRXDAT$,26% AS GL.DESC$,8% AS GL.AMOUNT$,6% AS GL.CRXREF$,2% AS GL.POSTIM$,2% AS GL.POSDAT$,6% AS GL.BANKCD$,8% AS GL.CHKDEP$,6% AS GL.VOUCHR$,6% AS GL.SUBACC$,6% AS GL.PHASE$,8% AS GL.REGQTY$,8% AS GL.PREQTY$,2% AS GL.UPDATE$ &
	\ FIELD #12%, 128% AS TEMP$,6% AS SLMCTL.RECORD.KEY$,8% AS TEMP$,2% AS SLMCTL.CUR.YR$,2% AS SLMCTL.FIR.MO$ &
	\ FIELD #12%, 128%+16%+LOOP%*2% AS TEMP$,2% AS SLMCTL.MON.UP$(LOOP%) FOR LOOP%=1% TO 7% &
	\ FIELD #12%, 128%+30%+LOOP%*2% AS TEMP$,2% AS SLMCTL.REC.CO$(LOOP%) FOR LOOP%=1% TO 7% &
	\ FIELD #12%, 128%+46% AS TEMP$,2% AS SLMCTL.START.COUNT$,2% AS SLMCTL.FINISH.COUNT$,2% AS SLMCTL.FUNCTION$,6% AS SLMCTL.TEMP$,6% AS SLMCTL.PASSWORD$ &
	\ FIELD #12%, 128%+64%+48%*(LOOP%-1%) AS TEMP$,18% AS SLMCTL.NAME$(LOOP%),30% AS SLMCTL.ACC$(LOOP%) FOR LOOP%=1% TO 8% &
	\ FIELD #12%, 128%+458% AS TEMP$,54% AS SLMCTL.TITLE$
470	  IF FNG%(SLMCTL.CH%,SYSNAM$) THEN  &
		  ERR.MES$=MID(SYS(CHR$(6%)+CHR$(9%)+CHR$(FNS%)),3%,30%)+" While looking "+SYSNAM$ &
		\ PRINT #CH%, FNP$("24;1");CLRLIN$;ERR.MES$;FNP$("24;55");"Hit any key to continue"; &
		\ INP$=FNINP$(CH%,128%," ",1%) &
		\ PRINT #CH%, SCROLL.ON$;CLSCN$;B.ON$;R.ON$;BLINK.ON$;R.ON$;FNP$("1;75");"WAIT";G.OFF$;FNP$("24;1"); &
		\ V%=FNX%("",0%,"")
480	  LSET SLMCTL.BUF$=FNL$+"" &
	\ KEY.POINTER%=FNR(SLMCTL.CH%) &
	\ DATA.POINTER%=FNR(SLMCTL.CH%+1%)
500	  YEAR%=CVT$%(SLMCTL.CUR.YR$) &
	\ M%(I%)=CVT$%(SLMCTL.MON.UP$(I%)) FOR I%=1% TO 7% &
	\ R%(I%)=CVT$%(SLMCTL.REC.CO$(I%)) FOR I%=1% TO 7% &
	\ START%=CVT$%(SLMCTL.START.COUNT$) &
	\ FINISH%=CVT$%(SLMCTL.FINISH.COUNT$) &
	\ FUNCTION%=CVT$%(SLMCTL.FUNCTION$) &
	\ IF M%(2%)=0% THEN  &
		  M%(2%)=M%(1%)+1% &
		\ M%(2%)=1% IF M%(2%)>12% &
		\ IF M%(2%)=0% THEN  &
			  PRINT #CH%, CLSCN$;"Unable to determine the month to be updated." &
			\ PRINT #CH%, FNP$("24;1");"Use the control record maintenance to correct this ";FNP$("24;55");"Hit any key to continue"; &
			\ INP$=FNINP$(CH%,128%," ",1%) &
			\ PRINT #CH%, CLSCN$;B.ON$;BLINK.ON$;R.ON$;FNP$("1;7");"WAIT";G.OFF$;FNP$("24;1") &
			\ V%=FNX%("",0%,"")
570	  IF FUNCTION% AND FUNCTION%<>1% THEN  &
		  PRINT #CH%, CLSCN$;"Function number ";FUNCTION%;"was started and not completed." &
		\ PRINT #CH%, "You must complete that function before starting";" this routine." &
		\ PRINT #CH%, "Consult the user guide for help. . ." &
		\ PRINT #CH%, FNP$("24;55");"Hit any key to continue"; &
		\ INP$=FNINP$(CH%,128%," ",1%) &
		\ PRINT #CH%, CLSCN$;B.ON$;BLINK.ON$;R.ON$;FNP$("1;75");"WAIT";G.OFF$;FNP$("24;1"); &
		\ V%=FNX%("",0%,"")
600	  STOP IF FNO%(GL.CH%,"GLAUG.DAS","/SF/NS/CR:128","") &
	\ E$="JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC" &
	\ PRINT #CH%, CLSCN$;"At the prompt type an 'No' if you don't want the interim" &
	\ PRINT #CH%, "     status to be set for that month." &
	\ K$="" &
	\ TEMP%=M%(1%) &
	\ TEMP1%=YEAR% &
	\ FOR I%=2% TO 7% &
		\ IF M%(I%) THEN &
			  TEMP1%=TEMP1%+1% IF M%(I%)=1% &
		  ELSE &
			  M%(I%)=TEMP%+1% &
			\ TEMP1%=TEMP1%+1% IF M%(I%)>12% OR M%(I%)=1% &
			\ M%(I%)=1% IF M%(I%)>12%
620		  TEMP%=M%(I%) &
		\ M$(I%)="GL"+MID(E$,M%(I%)*3%-2%,3%)+".DAS" &
		\ PRINT #CH%, FNP$("10;1");CLEOS$;"Opening ";M$(I%); &
		\ IF FNO%(I%,GL.MMM$+M$(I%),"/RW/SF/NS","") THEN  &
			  IF FNS%=5% THEN  &
				  M$(I%)=LEFT(M$(I%),LEN(M$(I%))-3%)+RIGHT(NUM1$(TEMP1%+100%),2%)+"S" &
				\ V%=FNO%(I%,GL.MMM$+M$(I%),"/RW/SF/NS","")
670		  IF FNS% THEN &
			  M$(I%)="" &
		  ELSE &
			  V%=FNG%(I%,"") &
			\ R1%(I%)=FNT
675		  IF FNS%=10% THEN  &
			  PRINT #CH%, FNP$("24;1");CLRLIN$;"Unable to gain write privileges for ";M$(I%);" Continuing. . . " &
			\ M$(I%)=""
680		  M$(I%)="" IF R1%(I%)=R%(I%)-1% OR R1%(I%)=0% &
		\ IF M$(I%)<>"" THEN  &
			  PRINT #CH%, FNP$("10;1");CLEOS$;"Set status for ";M$(I%);" (Yes/No) <Yes> "; &
			\ INP$=CVT$$(FNINP$(CH%,128%,"_",1%),-1%) &
			\ PRINT #CH%, CLSCN$;B.ON$;R.ON$;BLINK.ON$;FNP$("1;75");"WAIT";G.OFF$;FNP$("24;1");FNX%("",0%,"") IF INP$="%^C" &
			\ IF INP$="N" THEN  &
				  M$(I%)="" &
				\ GOTO 690
685		  IF M$(I%)<>"" THEN  &
			  PRINT #CH%, FNP$("11;1"); &
			\ PRINT #CH%, "Update will start with record #";R%(I%); &
			\ PRINT #CH%, " Record number to end with <";NUM1$(R1%(I%));">"; &
			\ INP$=FNINP$(CH%,0%,"_",5%) &
			\ PRINT #CH%, CLSCN$;B.ON$;R.ON$;BLINK.ON$;FNP$("1;75");"WAIT";G.OFF$;FNP$("24;1");FNX%("",0%,"") IF INP$="%^C" &
			\ R%=VAL(INP$) &
			\ IF R%=0% AND INP$<>"" THEN &
				  M$(I%)="" &
			  ELSE &
				  IF R%>R%(I%) AND R%<R1%(I%) THEN  &
					  R1%(I%)=R%
690		  K$=K$+M$(I%) &
		\ M%(I%)=0% IF R%(I%)=0% AND R1%(I%)=0% &
		\ REC.CNT=REC.CNT+(R1%(I%)-(R%(I%)-1%)) IF M$(I%)<>""
700			  NEXT I% &
	\ IF K$="" THEN  &
		  PRINT #CH%, CLSCN$;"Unable find anything to update." &
		\ PRINT #CH%, FNP$("24;55");"Hit any key to continue"; &
		\ INP$=FNINP$(CH%,128%," ",1%) &
		\ PRINT #CH%, CLSCN$;B.ON$;BLINK.ON$;R.ON$;FNP$("1;75");"WAIT";G.OFF$;FNP$("24;1"); &
		\ V%=FNX%("",0%,"")
2000	  V%=FNG%(-SLMCTL.CH%,NUM1$(KEY.POINTER%))+FNG%(-SLMCTL.CH%-1%,NUM1$(DATA.POINTER%))
2015	  INTERRUPT%=0% &
	\ IF START%<>FINISH% THEN  &
		  INTERRUPT%=-1%
2020	  FUNCTION%=1% &
	\ START%=START%+1% IF START%=FINISH% &
	\ IF FNU%(SLMCTL.CH%,LEFT(FNL$,46%)+CVT%$(START%)+MID(FNL$,49%,2%)+CVT%$(FUNCTION%)+RIGHT(FNL$,53%)) THEN  &
		  PRINT #CH%, CLSCN$;"Unable to change status of flag record." &
		\ PRINT #CH%, FNP$("24;1");"Hit any key to continue"; &
		\ INP$=FNINP$(CH%,128%," ",1%) &
		\ PRINT #CH%, CLSCN$;B.ON$;BLINK.ON$;R.ON$;FNP$("1;75");"WAIT";G.OFF$;FNP$("24;1"); &
		\ V%=FNX%("",0%,"")
2025	  PC$=FNPC.SPACE$(REC.CNT) &
	\ REC.CNT=FNPC(REC.CNT) &
	\ COUNTER%,PERCENT%=0% &
	\ COL%=10% &
	\ TEMP$="STATUS SET FOR "+LEFT(SLMCTL.TITLE$,24%) &
	\ PRINT #CH%, CLSCN$+R.ON$+LEFT(TEMP$,39%)+SPACE$(40%-LEN(TEMP$))+SPACE$(40%-LEN(COMPANY$))+COMPANY$; &
	\ PRINT #CH%, FNP$(NUM1$(I%)+";1")+" "+FNP$(NUM1$(I%)+";80")+" "; FOR I%=2% TO 19% &
	\ PRINT #CH%, FNP$("20;1")+SPACE$(80%)+G.OFF$ &
	\ PRINT #CH%, FNP$("19;2");"Starting to update the current status ";"for ";SYSNAM$;FNP$("11;10");"0%   10%  20%  30%  40%  50%";"  60%  70%  80%  90% 100%";FNP$("24;80"); &
	\ UPDATE%,ADD%,R%,ABORT%=0% &
	\ FOR LOOP%=2% TO 7% &
		\ GOTO 2070 IF M$(LOOP%)="" &
		\ R%(LOOP%)=1% IF R%(LOOP%)=0% &
		\ R2%(LOOP%)=R%(LOOP%) &
		\ IF FNG%(-LOOP%,NUM1$(R%(LOOP%))) THEN  &
			  PRINT #CH%, CLSCN$;"Unable to find record number";R%(LOOP%);"in ";M$(LOOP%);"." &
			\ PRINT #CH%, "Please check problem and restart."; &
			\ PRINT #CH%, FNP$("24;1");"Hit any key to continue"; &
			\ INP$=FNINP$(CH%,128%," ",1%) &
			\ PRINT #CH%, CLSCN$;B.ON$;BLINK.ON$;R.ON$;FNP$("1;75");"WAIT";G.OFF$;FNP$("24;1"); &
			\ V%=FNX%("",0%,"")
2030		  COUNTER%=COUNTER%+1% &
		\ IF COUNTER%>=REC.CNT THEN  &
			  COUNTER%=0% &
			\ PERCENT%=PERCENT%+LEN(PC$)*2% &
			\ IF PERCENT%<100% THEN  &
				  PRINT #CH%, FNP$("12;"+NUM1$(COL%));R.ON$;PC$;G.OFF$;FNP$("24;80"); &
				\ COL%=COL%+LEN(PC$)
2040		  R%=R%+1% &
		\ LSET GL.BUF$=FNL$ &
		\ IF CVT$$(GL.ACCNUM$,-1%)<>"" AND GL.SOURCE$<>"CE" THEN  &
			  IF INTERRUPT%=-1% THEN  &
				  GOSUB 3000 IF FNCOMP%(CVT$$(GL.ACCNUM$,-1%),CVT$$(SLMCTL.ACC$(FLAG%),-1%)) AND CVT$$(SLMCTL.ACC$(FLAG%),-1%)<>"" FOR FLAG%=1% TO 8%
2060		  IF ABORT% THEN  &
			  GOTO 2100
2065		  R%(LOOP%)=FNR(LOOP%) &
		\ GOTO 2030 IF FNN%(LOOP%)=0% AND R1%(LOOP%)>=R%(LOOP%) &
		\ IF FNS% THEN  &
			  R%(LOOP%)=FNT
2070			  NEXT LOOP%
2100	  PRINT #CH%, CLSCN$;SYSNAM$;"  ";SLMCTL.TITLE$ &
	\ PRINT #CH%, "Current status update routine" &
	\ PRINT #CH%, "Update counter ";START% &
	\ PRINT #CH% &
	\ PRINT #CH%, USING "Records read in the general ledger file  ######", R% &
	\ PRINT #CH%, USING "Records add to subsidiary ######", ADD% &
	\ FOR LOOP%=2% TO 7% &
		\ PRINT #CH%, FNP$("8;1");CLEOS$; &
		\ IF M$(LOOP%)<>"" THEN  &
			  PRINT #CH% &
			\ PRINT #CH%, USING "\          \ record series ##### - ######", M$(LOOP%),R2%(LOOP%),R%(LOOP%) &
			\ PRINT #CH%, USING "     \"+SPACE$(15%)+"\ ###,###,###.##", SLMCTL.NAME$(I%),CUR(LOOP%,I%) IF CUR(LOOP%,I%)<>0. FOR I%=1% TO 8% &
			\ PRINT #CH%, FNP$("24;55");"Hit any key continue"; &
			\ INP$=FNINP$(CH%,128%," ",1%)
2120			  NEXT LOOP% &
	\ PRINT #CH%, FNP$("8;1");CLEOS$; &
	\ PRINT #CH%, USING SPACE$(10%)+"UPDATE Amount###,###,###.##", TOTAL &
	\ V%=FNG%(-SLMCTL.CH%,NUM1$(KEY.POINTER%))+FNG%(-SLMCTL.CH%-1%,NUM1$(DATA.POINTER%)) &
	\ IF FNS% THEN  &
		  PRINT #CH%, FNP$("24;1");CLRLIN$;"Unable to find flag record.";FNP$("24;55");"Hit any key to continue"; &
		\ INP$=FNINP$(CH%,128%," ",1%) &
		\ PRINT #CH%, CLSCN$;B.ON$;BLINK.ON$;R.ON$;FNP$("1;75");"WAIT";G.OFF$;FNP$("24;1"); &
		\ V%=FNX%("",0%,"")
2130	  PRINT #CH%, CLRBOT$;FNP$("24;1");"Hit any key to continue"; &
	\ INP$=FNINP$(CH%,128%," ",1%)
2140	  PRINT #CH%, CLSCN$;B.ON$;BLINK.ON$;R.ON$;FNP$("1;75");"WAIT";G.OFF$;FNP$("24;1"); &
	\ V%=FNX%("",0%,"")
3000	  !
4000	  !
5000	  AMOUNT=CVT$F(GL.AMOUNT$) &
	\ TOTAL=TOTAL+AMOUNT &
	\ CUR(LOOP%,FLAG%)=CUR(LOOP%,FLAG%)+AMOUNT &
	\ ADD%=ADD%+1% &
	\ IF FNA%(GL.CH%,GL.BUF$)=0% THEN &
		  RETURN &
	  ELSE &
		  PRINT #CH%, CLRBOT$;"Unable to add ";FILE.KEY$;" to file."; &
		\ PRINT #CH%, FNP$("24;1");"Aborting. . . ";FNP$("24;55");"Hit any key to continue"; &
		\ INP$=FNINP$(CH%,128%," ",1%) &
		\ ABORT%=-1% &
		\ RETURN
19000	  !
19020	  IF ERL=90% THEN  &
		  RESUME 100
19040	  IF ERR=52% OR ERR=51% THEN  &
		  RESUME 
19990	  ON ERROR GOTO 0
30000	  DEF FNINP$(CHN%,KYP%,FILLCHAR$,INPUTLEN%) &
	\ PRINT #CHN%, STRING$(INPUTLEN%,ASCII(FILLCHAR$));STRING$(INPUTLEN%,8%); &
	\ PRINT #CHN%, RECORD 256%,CHR$(KYP%+INPUTLEN%)+FILLCHAR$; &
	\ GET #CHN% &
	\ FIELD #CHN%, RECOUNT AS BUFFER$ &
	\ IF ASCII(BUFFER$)=128% OR ASCII(BUFFER$)=8% OR ASCII(BUFFER$)=23% THEN  &
		  BUFFER$="%END" IF MID(BUFFER$,2%,4%)="[21~" OR MID(BUFFER$,2%,2%)="OP" &
		\ BUFFER$="%ABORT" IF MID(BUFFER$,2%,4%)="[19~" OR ASCII(BUFFER$)=23%
30010	  BUFFER$="%^C" IF ASCII(BUFFER$)=3% &
	\ FNINP$=CVT$$(BUFFER$,4%) &
	\ FNEND
30050	  DEF FNP$(DCA$) &
	\ FNP$=ESC$+"["+DCA$+"H" &
	\ FNEND
30080	  DEF FNDATE.PRINT$(Y$) &
	\ FNDATE.PRINT$=LEFT(Y$,2%)+"/"+MID(Y$,3%,2%)+"/"+RIGHT(Y$,5%) &
	\ FNEND
30140	  DEF FNRDATE$(Y$) &
	\ Y%=CVT$%(Y$) &
	\ FNRDATE$=RIGHT(NUM1$((Y% AND 480%)/32%+100%),2%)+RIGHT(NUM1$((Y% AND 31%)+100%),2%)+RIGHT(NUM1$((SWAP%(Y%) AND 254%)/2%+100%),2%) &
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
		  FNCOMP%=Y9% &
		\ FNEND
30950	  DEF FNPC.SPACE$(TOTREC) &
	\ TEMP=TOTREC/50. &
	\ SP%=1% &
	\ SP%=2% IF TEMP<-151996493463552. &
	\ SP%=5% IF TEMP<-3.376044235836595e-11 &
	\ SP%=10% IF TEMP<6.8213185310482798e-15 &
	\ SP%=25% IF TEMP<-7.46316226289004e-36 &
	\ SP%=50% IF TEMP<47559113466445824. &
	\ FNPC.SPACE$=SPACE$(SP%) &
	\ FNEND
30960	  DEF FNPC(TOTREC) &
	\ TEMP=TOTREC/50. &
	\ PC=INT(TOTREC/50.) &
	\ PC=INT(TOTREC/25.) IF TEMP<-151996493463552. &
	\ PC=INT(TOTREC/10.) IF TEMP<-3.376044235836595e-11 &
	\ PC=INT(TOTREC/5.) IF TEMP<6.8213185310482798e-15 &
	\ PC=INT(TOTREC/2.) IF TEMP<-7.46316226289004e-36 &
	\ PC=INT(TOTREC/1.) IF TEMP<47559113466445824. &
	\ FNPC=PC &
	\ FNEND
32767	  END
