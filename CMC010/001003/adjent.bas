10	  ! &
	  ! Program name: adjent		Compiled with SCALE 0 on V08.0 &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
20	  DIM C1$(9%), F$(8%), J1$(9%), J$(6%), J(1%), J%(2%)
30	  OPEN "KB:" AS FILE 12% &
	\ IF FNO%(6%,"CHART.DAT","/RO","") THEN  &
		  PRINT "ERROR";FNS%;"IN OPENING CHART FILE.  ABORT." &
		\ GOTO 10000
40	  A$=FNX$ &
	\ IF FNX$="" THEN  &
		  A$=FNI$("YEAR (YY OR <RETURN> FOR THIS YEAR)? ",A$) &
		\ A$=RIGHT(DATE$(0%),8%) IF A$="" &
		\ GOTO 40 IF LEN(A$)<>2% &
		\ A$="ADJE"+A$+".DAT"
50	  UNLESS FNO%(2%,A$,"","") THEN  &
		  PRINT A$;" OPENED." IF FNX$="" &
		\ GOTO 80
60	  IF FNS%<>5% THEN  &
		  PRINT "ERROR";FNS%;"IN OPENING ADJENT." &
		\ GOTO 10000
70	  GOTO 10000 IF FNI$("AN ADJENT FILE DOES NOT EXIST.  "+"SHALL I CREATE ONE (Y/N)?  ",V$)<>"Y" &
	\ UNLESS FNO%(2%,A$,"/CR:16,64","") THEN &
		  PRINT A$+" CREATED." &
	  ELSE &
		  PRINT "ERROR";FNS%;"IN OPENING ";A$;"." &
		\ GOTO 10000
80	  OPEN "KB:" AS FILE 1%, RECORDSIZE 64%+64% &
	\ FIELD #1%, 6% AS J1$(1%),8% AS J1$(2%),6% AS J1$(3%),2% AS J1$(4%),4% AS J1$(5%),2% AS J1$(6%),27% AS J1$(7%),6% AS J1$(8%),1% AS J1$(9%),2% AS V$ &
	\ FIELD #1%, 64% AS T2$
90	  FIELD #1%, 64% AS V$,6% AS C1$(1%),8% AS C1$(2%),2% AS C1$(3%),8% AS C1$(4%),2% AS C1$(5%),2% AS C1$(6%),2% AS C1$(7%),28% AS C1$(8%),6% AS C1$(9%) &
	\ FIELD #1%, 64% AS V$,64% AS T4$
1000	  !
1020	  T=0. &
	\ PRINT  &
	\ K$=LEFT(FNI$("OPTION? ",K$),3%) &
	\ GOTO 1030 IF K$="" &
	\ GOTO 3000 IF K$="ENT" &
	\ GOTO 3500 IF K$="CHA" &
	\ GOTO 4000 IF K$="DEL" &
	\ GOTO 5000 IF K$="PRI" &
	\ GOTO 6000 IF K$="EXA" &
	\ GOTO 7000 IF K$="REM" &
	\ GOTO 8000 IF K$="UPD" &
	\ GOTO 10000 IF K$="END" &
	\ PRINT "TYPE <RETURN> FOR AN OPTIONS LIST." &
	\ GOTO 1020
1030	  PRINT "OPTIONS:" &
	\ PRINT "ENTER    JOURNAL ENTRY RECORDS" &
	\ PRINT "CHANGE   RECORDS" &
	\ PRINT "DELETE   RECORDS" &
	\ PRINT "PRINT    JOURNAL ENTRIES" &
	\ PRINT "EXAMINE  ONE JOURNAL ENTRY" &
	\ PRINT "REMOVE   COMPLETE JOURNAL ENTRY" &
	\ PRINT "UPDATE   JOURNAL ENTRIES TO CKDATA FILE" &
	\ PRINT "END      PROGRAM" &
	\ GOTO 1020
2000	  !
2010	  LSET T2$=FNL$ &
	\ J$(I%)=J1$(I%)+"" FOR I%=1% TO 4% &
	\ RETURN IF LEFT(J$(2%),5%)="99999" &
	\ J(1%)=FNN4(J1$(5%)) &
	\ J%(1%)=CVT$%(J1$(6%)) &
	\ J$(I%-2%)=J1$(I%)+"" FOR I%=7% TO 8% &
	\ J%(2%)=ASCII(J1$(9%)) &
	\ RETURN
2100	  !
2110	  LSET J1$(1%)=J$(1%) &
	\ RETURN IF J$(2%)="99999.99" &
	\ LSET J1$(I%)=J$(I%) FOR I%=2% TO 4% &
	\ LSET J1$(5%)=FNN4$(J(1%)) &
	\ LSET J1$(6%)=CVT%$(J%(1%)) &
	\ LSET J1$(I%)=J$(I%-2%) FOR I%=7% TO 8% &
	\ LSET J1$(9%)=CHR$(J%(2%)) &
	\ RETURN
2300	  !
2310	  IF LEFT(J$(2%),5%)<>"99999" AND FNS%=0% THEN  &
		  PRINT USING "\      \  \\  \    \  \"+SPACE$(25%)+"\ \    \ \      \"+"###,###,###.##", FND6$(J%(1%)),J$(4%),J$(3%),J$(5%),J$(6%),J$(2%),J(1%) &
		\ X%=X%+1% &
		\ T=T+J(1%) &
		\ RETURN
2330	  GOSUB 2340 IF Z9%=0% &
	\ PRINT TAB(10%);CVT$$(RIGHT(T2$,15%),128%) &
	\ X%=X%+1% &
	\ RETURN
2340	  PRINT USING TAB(48%)+"---TOTAL         ###,###,###.##", INT(100.*T+6.8213185310482798e-15)/100. &
	\ K2$=K$ &
	\ T1=T1+T &
	\ T=0. &
	\ X%=X%+1% &
	\ Z9%=-1% &
	\ RETURN
2400	  !
2410	  PRINT STRING$(4%,10%); &
	\ PRINT SPACE$(20%);"ADJUSTING JOURNAL ENTRY REGISTER" &
	\ PRINT SPACE$(32%);"FOR 19";MID(A$,5%,2%) &
	\ PRINT SPACE$(24%);TIME$(0%);"        ";DATE$(0%) &
	\ PRINT  &
	\ PRINT  &
	\ X%=9% &
	\ RETURN
2600	  !
2610	  PRINT "    DATE SRC   CHECK  CKDATA DESCRIPTION          JOB#      "+"ACCT#        AMOUNT" &
	\ PRINT  &
	\ X%=X%+2% &
	\ RETURN
3000	  !
3005	  PRINT "NOTE: DO NOT USE DASHES (-) IN JOURNAL ENTRY NUMBERS."
3010	  V$=SYS(CHR$(3%)) &
	\ PRINT  &
	\ K$=FNI$("J.E. # ? ",K$) &
	\ IF LEN(K$)>6% THEN  &
		  PRINT "TOO LONG!  USE 6 OR LESS CHARACTERS." &
		\ GOTO 3010
3015	  IF K$="." OR K$="" THEN  &
		  PRINT K$ &
		\ V$=SYS(CHR$(2%)) &
		\ GOTO 10020
3020	  K$=SPACE$(6%-LEN(K$))+K$ &
	\ UNLESS FNG%(2%,K$) THEN  &
		  GOSUB 2010 &
		\ PRINT J$(1%) &
		\ GOTO 3050 IF J%(2%)=0% OR LEFT(J$(2%),5%)="99999" &
		\ PRINT "JE #";CVT$$(J$(1%),-1%);" HAS BEEN UPDATED.  ";"NO FURTHER ENTRY ALLOWED." &
		\ GOTO 3010
3030	  J$(1%)=K$ &
	\ PRINT J$(1%) &
	\ K2%=-1% &
	\ PRINT "DESCRIPTION FOR JE# ";CVT$$(J$(1%),-1%);":" &
	\ K1%=1% &
	\ V$=SYS(CHR$(2%))
3040	  K$=FNI$("LINE "+NUM1$(K1%)+"--",K$) &
	\ IF K$<>"" AND K$<>"." THEN  &
		  STOP IF FNA%(2%,J$(1%)+"99999."+RIGHT(NUM1$(K1%+100%),2%)+K$) &
		\ K1%=K1%+1% &
		\ GOTO 3040
3050	  V$=SYS(CHR$(3%)) &
	\ PRINT  &
	\ PRINT "   CK #    ACCT#  SRC  JOB #       DATE            AMOUNT"
3060	  PRINT ">"; &
	\ K$=FNI$("",K$) &
	\ IF K$="." THEN  &
		  PRINT  &
		\ UNLESS K2% THEN &
			  GOTO 3010 &
		  ELSE &
			  GOTO 3010 IF FNI$("DELETE DESCRIPTIONS OF THIS ENTRY (Y/N)?  ",K$)<>"Y" &
			\ V%=FND%(2%,J$(1%)+"99999.") UNTIL FNS% &
			\ GOTO 3010
3065	  K$=J$(3%) IF K$="" &
	\ GOTO 3120 IF K$="" &
	\ K$=SPACE$(6%-LEN(K$))+K$ &
	\ J$(3%)=K$ &
	\ PRINT USING "\    \  ", J$(3%); &
	\ K%=2% &
	\ GOSUB 3300 &
	\ K$=CVT$$(K$,2%) &
	\ K$=K$+".00" IF LEN(K$)<4% &
	\ K$=LEFT(K$,LEN(K$)-2%)+"."+RIGHT(K$,LEN(K$)-1%) IF INSTR(1%,K$,".")=0% &
	\ K$=SPACE$(8%-LEN(K$))+K$ &
	\ IF FNG%(6%,K$) THEN  &
		  PRINT "NONEXISTENT G/L #.  "; &
		\ GOTO 3120
3070	  J$(2%)=K$ &
	\ PRINT USING "\      \  ", J$(2%); &
	\ K%=4% &
	\ K$="" &
	\ GOSUB 3300 UNTIL K$<>"" &
	\ J$(4%)=K$ &
	\ PRINT USING "\\  ", J$(4%); &
	\ K%=6% &
	\ GOSUB 3300 &
	\ K$="" IF K$="0" &
	\ J$(6%)=K$ &
	\ PRINT USING "\    \  ", K$;
3080	  K$=FNI$("",K$) &
	\ K$=K1$ IF K$="" &
	\ GOTO 3120 IF K$="-" &
	\ K$=FND7$(K$) &
	\ GOTO 3080 IF FND7%(K$) &
	\ PRINT USING "\      \  ", K$; &
	\ J%(1%)=FND6%(K$) &
	\ K1$=K$
3085	  ON ERROR GOTO 3130 &
	\ K$=FNI$("",K$) &
	\ J(1%)=VAL(K$)/100. &
	\ PRINT USING "######,###,##.## ", J(1%); &
	\ ON ERROR GOTO 0
3090	  V$=SYS(CHR$(2%)) &
	\ PRINT  &
	\ K$=FNI$("CKDATA DESC-->JE#"+CVT$$(J$(1%),2%)+"-",K$) &
	\ IF K$="?" THEN  &
		  PRINT "TYPE NEW DESCRIPTION, <CR> TO COPY LAST, OR 0 FOR NONE."; &
		\ GOTO 3090
3095	  GOTO 3120 IF K$="-" &
	\ X%=LEN("JE#"+CVT$$(J$(1%),2%)+"-") &
	\ IF X%+LEN(K$)>27% THEN  &
		  GOTO 3090 UNLESS FNI$("ENTRY TOO LONG BY"+NUM1$(27%-LEN(J$(5%)))+"SPACES.  REENTER (Y/N)?  ",V$)="N"
3105	  V%=0% &
	\ V%=-1% IF K$<>"" AND K$<>"0" &
	\ K$=RIGHT(J$(5%),INSTR(1%,J$(5%),"-")+1%) IF K$="" &
	\ K$="" IF K$="0" &
	\ J$(5%)="JE#"+CVT$$(J$(1%),2%)+"-"+K$ &
	\ J$(5%)=LEFT(J$(5%),27%) &
	\ PRINT TAB(14%+X%);RIGHT(J$(5%),X%+1%) UNLESS V% &
	\ V$=SYS(CHR$(3%)) &
	\ J%(2%)=0% &
	\ GOSUB 2100 &
	\ K2%=0% &
	\ STOP IF FNA%(2%,T2$) &
	\ GOTO 3060
3120	  PRINT "VOID!" &
	\ V$=SYS(CHR$(3%)) &
	\ J$(I%)="" FOR I%=2% TO 6% &
	\ GOTO 3060
3130	  PRINT "ERROR--PLEASE REENTER AMOUNT."; &
	\ PRINT CHR$(10%);STRING$(29%,8%);STRING$(4%,7%); &
	\ RESUME 
3300	  K$=FNI$("",K$) &
	\ GOTO 3120 IF K$="-" &
	\ K$=J$(K%) IF K$="" &
	\ RETURN
3500	  !
3510	  PRINT  &
	\ F$(0%)="JE!AC!CK!SR!DC!JO!AM!DA!UP!" &
	\ F$(1%)="JOURNAL ENTRY #" &
	\ F$(2%)="ACCT #" &
	\ F$(3%)="CHECK #" &
	\ F$(4%)="SOURCE" &
	\ F$(5%)="CKDATA DESCRIPTION" &
	\ F$(6%)="JOB NUMBER" &
	\ F$(7%)="AMOUNT" &
	\ F$(8%)="DATE" &
	\ K$=FNI$("JE #? ",K$) &
	\ GOTO 10020 IF K$="" &
	\ K$=SPACE$(6%-LEN(K$))+K$ &
	\ IF FNG%(2%,K$) THEN  &
		  PRINT "JE #";CVT$$(K$,-1%);" NOT FOUND!" &
		\ GOTO 3510
3520	  GOSUB 2010 &
	\ GOTO 3540 IF J%(2%) &
	\ INPUT "ACCOUNT # ('DE' FOR DESCRIPTION)";K1$ &
	\ GOTO 3800 IF K1$="DE" &
	\ K1$=SPACE$(8%-LEN(K1$))+K1$ &
	\ IF FNG%(2%,K$+K1$) THEN  &
		  PRINT "RECORD NOT FOUND!" &
		\ GOTO 3510
3530	  GOSUB 2010 &
	\ GOSUB 2310 &
	\ INPUT "CONFIRM (Y/N/+/-)";K$ &
	\ GOTO 3510 IF INSTR(1%," Y+-",K$)<2% &
	\ IF K$<>"Y" THEN  &
		  V%=1% &
		\ V%=-1% IF K$="-" &
		\ GOTO 3510 IF FNN%(V%*2%) &
		\ GOTO 3530
3540	  IF J%(2%) THEN  &
		  PRINT "THAT JOURNAL ENTRY HAS ALREADY BEEN UPDATED!  CHANGES NOT ALLOWED." &
		\ GOTO 3510
3550	  K%=0% &
	\ PRINT  &
	\ INPUT "ITEM TO CHANGE (? FOR HELP)";K$ &
	\ GOTO 3630 IF K$="" &
	\ IF K$="?" THEN  &
		  PRINT  &
		\ PRINT "ENTER","TO CHANGE" &
		\ PRINT I%;"OR ";MID(F$(0%),I%*3%-2%,2%)F$(I%) FOR I%=1% TO 8% &
		\ PRINT  &
		\ GOTO 3540
3552	  K$=LEFT(K$+" ",2%)+"!" &
	\ K%=(INSTR(1%,F$(0%),K$)+2%)/3% &
	\ ON ERROR GOTO 3554 &
	\ K%=VAL(LEFT(K$,2%)) IF K%=0% &
	\ GOTO 3558
3554	  RESUME 3550
3558	  ON ERROR GOTO 0 &
	\ GOTO 3550 IF K%<1% OR K%>8% &
	\ GOTO 3600 IF K%>6% &
	\ IF K%=0% THEN  &
		  PRINT "UNDEFINED CODE!" &
		\ GOTO 3540
3560	  PRINT 'TYPE "0" TO BLANK OUT THIS ITEM.' IF K%=5% OR K%=6% &
	\ PRINT "OLD ";F$(K%);"--"; &
	\ PRINT J$(K%);"; "; IF K%<>5% &
	\ PRINT RIGHT(J$(K%),INSTR(1%,J$(K%),"-")+1%); IF K%=5% &
	\ PRINT  IF K%=5% &
	\ K$=FNI$("NEW--",K$) &
	\ GOTO 3540 IF K$="" &
	\ K$="" IF K$="0" AND (K%=5% OR K%=6%) &
	\ K$=SPACE$(6%-LEN(K$))+K$ IF K%=1% OR K%=3% &
	\ K$=SPACE$(8%-LEN(K$))+K$ IF K%=2% &
	\ IF FNG%(6%,K$) AND K%=2% THEN  &
		  PRINT "INVALID G/L #." &
		\ GOTO 3560
3570	  K$=LEFT(J$(5%),INSTR(1%,J$(5%),"-"))+K$ IF K%=5% &
	\ J$(K%)=K$ &
	\ J$(5%)="JE#"+CVT$$(J$(1%),-1%)+RIGHT(J$(5%),INSTR(1%,J$(5%),"-") IF K%=1% &
	\ GOTO 3540 IF K%>2% &
	\ GOSUB 2100 &
	\ GOTO 3510 UNLESS FNU%(-2%,T2$) &
	\ PRINT "ERROR";FNS%;"IN CHANGING RECORD.  ABORT." &
	\ GOTO 10000
3600	  IF K%=7% THEN  &
		  PRINT USING "OLD AMOUNT=###,###,##.##  NEW=", J(1%); &
		\ K$=FNI$("",K$) &
		\ J(1%)=VAL(K$) IF K$<>"" &
		\ GOTO 3540
3610	  PRINT USING "OLD DATE--\      \; NEW--", FND6$(J%(1%)); &
	\ K$=FNI$("",K$) &
	\ GOTO 3540 IF K$="" &
	\ K$=FND7$(K$) &
	\ IF FND7%(K$) THEN  &
		  PRINT "ILLEGAL DATE" &
		\ GOTO 3610
3620	  J%(1%)=FND6%(K$) &
	\ GOTO 3540
3630	  GOSUB 2100 &
	\ GOTO 3510 UNLESS FNU%(2%,T2$) &
	\ PRINT "ERROR";FNS%;"IN CHANGING RECORD.  ABORT." &
	\ GOTO 10000
3800	  !
3810	  IF FNG%(2%,K$+"99999") THEN  &
		  PRINT "THERE IS NO DESCRIPTION FOR JE# ";K$ &
		\ GOTO 3860 IF FNI$("DO YOU WISH TO ENTER ONE (Y/N)? ",K1$)="Y" &
		\ GOTO 3510
3820	  K1%=VAL(MID(FNL$,13%,2%)) &
	\ PRINT "LINE #";K1%;":  ";RIGHT(FNL$,15%) &
	\ V%=FNN%(2%) &
	\ GOTO 3820 UNLESS V% OR LEFT(FNL$,12%)<>K$+"99999."
3830	  K1$=FNI$('LINE NUMBER TO REENTER (<RETURN> FOR NONE; "99" FOR ALL)? ',K1$) &
	\ GOTO 3850 IF K1$="99" &
	\ GOTO 3510 IF K1$="" &
	\ GOTO 3830 UNTIL VAL(K1$)>0% AND VAL(K1$)<K1%+1% &
	\ K1$="0"+K1$ IF LEN(K1$)=1% &
	\ K2$=FNI$("LINE # "+NUM1$(VAL(K1$))+" :  ",K2$) &
	\ GOTO 3830 IF K2$="" &
	\ IF FNG%(2%,K$+"99999."+K1$) THEN  &
		  PRINT "ERROR";FNS%;"IN GETTING RECORD.  ABORT." &
		\ GOTO 10000
3840	  LSET T2$=LEFT(FNL$,14%)+K2$ &
	\ GOTO 3830 UNLESS FNU%(2%,T2$) &
	\ PRINT "ERROR";FNS%;"IN CHANGING DESCRIPTION.  ABORT." &
	\ GOTO 10000
3850	  V%=FNG%(2%,K$+"99999.") &
	\ UNLESS V% THEN  &
		  GOTO 3850 UNLESS FND%(2%,"") &
		\ PRINT "ERROR";FNS%;"IN DELETING RECORD.  ABORT." &
		\ GOTO 10000
3860	  PRINT "DESCRIPTION FOR JE# ";CVT$$(K$,-1%) &
	\ K1%=0% &
	\ V$=SYS(CHR$(2%))
3870	  K1$=FNI$("LINE # "+NUM1$(K1%+1%)+" :  ",K1$) &
	\ GOTO 10020 UNLESS K1$<>"" AND K1$<>"." &
	\ K1%=K1%+1% &
	\ GOTO 3870 UNLESS FNA%(2%,K$+"99999."+RIGHT(NUM1$(K1%+100%),2%)+K1$) &
	\ PRINT "ERROR";FNS%;"IN ADDING RECORD.  ABORT." &
	\ GOTO 10000
4000	  !
4010	  K$=FNI$("JE #? ",K$) &
	\ GOTO 1020 IF K$="" &
	\ K$=SPACE$(6%-LEN(K$))+K$ &
	\ IF FNG%(2%,K$) THEN  &
		  PRINT "JE #";CVT$$(K$,-1%);" NOT FOUND!" &
		\ GOTO 4010
4020	  IF MID(FNL$,7%,5%)<>"99999" THEN  &
		  GOSUB 2010 &
		\ GOTO 4080 IF J%(2%)
4030	  INPUT "ACCOUNT # ";K1$ &
	\ K1$=SPACE$(8%-LEN(K1$))+K1$ &
	\ IF FNG%(2%,K$+K1$) THEN  &
		  PRINT "ITEM NOT FOUND!" &
		\ GOTO 4010
4040	  GOSUB 2010 &
	\ GOSUB 2310 &
	\ INPUT "CONFIRM (Y/N/+/-)";K1$ &
	\ GOTO 4010 IF INSTR(1%," Y+-",K1$)<2% &
	\ IF K1$<>"Y" THEN  &
		  V%=1% &
		\ V%=-1% IF K1$="-" &
		\ GOTO 4010 IF FNN%(V%*2%) &
		\ GOTO 4040
4050	  UNLESS FND%(2%,"") THEN &
		  PRINT "DELETED." &
	  ELSE &
		  PRINT "ERROR";FNS%;"IN DELETING RECORD.  ABORT." &
		\ GOTO 10000
4060	  V%=FNN%(2%) &
	\ GOTO 4010 IF V% OR LEFT(FNL$,12%)<>K$+"99999." &
	\ GOTO 4060 UNLESS FND%(2%,"") &
	\ PRINT "ERROR";FNS%;"IN DELETING RECORD.  ABORT." &
	\ GOTO 10000
4080	  PRINT "JE# ";CVT$$(K$,-1%);" HAS ALREADY BEEN UPDATED."+"  DELETION NOT ALLOWED." &
	\ GOTO 4010
5000	  !
5010	  GOTO 1020 IF FNG%(2%,"") &
	\ K%=-1% &
	\ INPUT "UPDATED, NOT UPDATED, OR BOTH (U,N, OR B)";K1$ &
	\ K1$="B" UNLESS K1$="U" OR K1$="N" &
	\ IF K1$="U" THEN  &
		  INPUT "MONTH # (<CR> FOR ALL)";K% UNTIL K%>-1% AND K%<13%
5015	  INPUT "SET PAGE";V$ &
	\ K$,K2$="" &
	\ GOSUB 2400 &
	\ GOSUB 2600
5020	  GOSUB 2010 &
	\ GOTO 5030 IF K1$="N" AND J%(2%) OR J%(2%)=0% AND K1$="U" OR K1$="U" AND K%<>0% AND J%(2%)<>K% &
	\ GOSUB 5050 IF J$(1%)<>K$ AND K2$=K$ &
	\ IF J$(1%)<>K$ THEN  &
		  GOSUB 2340 &
		\ GOSUB 5050 &
		\ V%=FNN%(-2%) &
		\ GOTO 5030
5025	  IF X%>60% THEN  &
		  PRINT STRING$(70%-X%,10%); &
		\ X%=4% &
		\ GOSUB 2600
5027	  GOSUB 2310
5030	  GOTO 5020 UNLESS FNN%(2%) &
	\ GOSUB 2340 UNLESS Z9% &
	\ PRINT  &
	\ PRINT USING TAB(48%)+"---GRAND TOTAL $$###,###,###.##-", INT(T1*100.+6.8213185310482798e-15)/100. &
	\ T1,X%=0. &
	\ K2$="" &
	\ GOTO 1020
5050	  IF X%>58% THEN  &
		  PRINT STRING$(70%-X%,10%); &
		\ X%=4% &
		\ GOSUB 2600
5060	  Z9%=0% &
	\ PRINT  &
	\ PRINT "JE # ";CVT$$(J$(1%),-1%); &
	\ PRINT " - UPDATED TO MONTH #";J%(2%) IF J%(2%) &
	\ PRINT  IF J%(2%)=0% &
	\ K$=J$(1%) &
	\ X%=X%+2% &
	\ RETURN
6000	  !
6010	  T1,Z9%=0. &
	\ PRINT  &
	\ K$=FNI$("EXAMINE JE # ",K$) &
	\ GOTO 1020 IF K$="" &
	\ K$=SPACE$(6%-LEN(K$))+K$ &
	\ IF FNG%(2%,K$) THEN  &
		  PRINT "JE NOT FOUND." &
		\ GOTO 6010
6020	  GOSUB 2010 &
	\ GOSUB 2600 &
	\ PRINT "JE # ";CVT$$(J$(1%),-1%); &
	\ PRINT " - UPDATED TO MONTH #";J%(2%) IF J%(2%) &
	\ PRINT  IF J%(2%)=0%
6030	  GOSUB 2010 &
	\ IF J$(1%)=K$ THEN &
		  GOSUB 2310 &
	  ELSE &
		  GOSUB 2340 IF Z9%=0% &
		\ GOTO 6010
6035	  GOTO 6030 UNLESS FNN%(2%) &
	\ GOTO 6010
7000	  !
7010	  K$=FNI$("REMOVE JE # ",K$) &
	\ GOTO 1020 IF K$="" &
	\ K$=SPACE$(6%-LEN(K$))+K$ &
	\ IF FNG%(2%,K$) THEN  &
		  PRINT "JOURNAL ENTRY NOT FOUND." &
		\ GOTO 7010
7020	  GOSUB 2010 &
	\ PRINT "JE # ";CVT$$(J$(1%),-1%); &
	\ PRINT " - UPDATED TO MONTH #";J%(2%); IF J%(2%) &
	\ PRINT  &
	\ GOTO 7010 UNLESS FNI$("CONFIRM REMOVING THIS JOURNAL ENTRY (Y/N)?  ",V$)="Y" &
	\ STOP IF FNG%(2%,K$)
7030	  STOP IF FND%(2%,LEFT(FNL$,14%)) &
	\ GOTO 7010 IF FNN%(2%) &
	\ GOTO 7010 IF LEFT(FNL$,6%)<>K$ &
	\ GOTO 7030
8000	  !
8005	  PRINT "THIS OPTION TRANSFERS JOURNAL ENTRIES TO THE GENERAL LEDGER.  YOU" &
	\ PRINT "MAY REMOVE THEM FROM THE ADJENT FILE OR KEEP THEM FOR LATER "+"REFERENCE." &
	\ INPUT "DO YOU WISH TO KEEP THEM (Y/N)";K6$ &
	\ K6$="Y" IF K6$<>"N"
8010	  PRINT  &
	\ V$=LEFT(FNI$("GENERAL LEDGER MONTH (MMM)? ",V$),3%) &
	\ GOTO 1020 IF V$="" &
	\ V%=INSTR(1%,"JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC",V$) &
	\ IF V%=0% THEN  &
		  Z9%=VAL(RIGHT(V$,2%)) &
		\ GOTO 8012
8011	  Z9%=(V%+2%)/3%
8012	  J%(2%)=Z9% &
	\ V%=FNC%(4%) &
	\ IF FNO%(4%,"CK"+V$+".DAT","","") THEN  &
		  PRINT "CKDATA FILE FOR THIS MONTH DOES NOT EXIST" &
		\ GOTO 8010
8020	  X%=0% &
	\ PRINT  &
	\ K$=FNI$("UPDATE JE#'S (TYPE ? FOR HELP)? ",K$)+"," &
	\ GOTO 8300 IF K$="?," &
	\ GOTO 1020 IF K$="," &
	\ V$=FNI$("UPDATE "+LEFT(K$,LEN(K$)-1%)+" (Y/N)? ",V$) &
	\ GOTO 1020 IF V$<>"Y"
8030	  I%=INSTR(1%,K$,",") &
	\ IF I% THEN  &
		  K1$=LEFT(K$,I%-1%) &
		\ K$=RIGHT(K$,I%+1%) &
		\ GOSUB 8050 &
		\ GOTO 8030
8040	  STOP IF FNC%(4%) &
	\ GOTO 1020 IF X% &
	\ PRINT "UPDATE COMPLETE." &
	\ GOTO 10020
8050	  !
8060	  PRINT "WORKING." &
	\ I%=INSTR(1%,K1$,"-") &
	\ K2$,K3$=K1$ &
	\ IF I% THEN  &
		  K2$=LEFT(K1$,I%-1%) &
		\ K3$=RIGHT(K1$,I%+1%)
8070	  K2$=SPACE$(6%-LEN(K2$))+K2$ &
	\ K3$=SPACE$(6%-LEN(K3$))+K3$ &
	\ IF K2$>K3$ THEN  &
		  PRINT "JE #";K2$;" IS GREATER THAN JE#";K3$;", ACCOUNTS NOT UPDATED" &
		\ X%=-1% &
		\ RETURN
8080	  I%=FNG%(2%,K2$) &
	\ T=0. &
	\ K%=0% &
	\ K5$="********"
8090	  IF MID(FNL$,7%,5%)<>"99999" THEN  &
		  GOSUB 2010 &
		\ GOTO 8250 IF FIX(T*100.)<>0. AND K5$<>J$(1%) AND K2$<>K3$ &
		\ K5$=J$(1%)+"" &
		\ GOTO 8110 IF J$(1%)>K3$ OR J$(1%)<K2$ &
		\ T=T+J(1%) &
		\ K%=K%+1% &
		\ IF J%(2%)<>0% THEN  &
			  PRINT "JE #";CVT$$(J$(1%),-1%);" ALREADY UPDATED IN MONTH";J%(2%); &
			\ PRINT "IN GROUP ";K1$; IF INSTR(1%,K1$,"-") &
			\ PRINT  &
			\ PRINT "     THIS GROUP NOT UPDATED!" &
			\ X%=-1% &
			\ RETURN
8100	  GOTO 8090 UNLESS FNN%(2%)
8110	  IF K%=0% THEN  &
		  PRINT "NO RECORDS UPDATED FOR JE #";CVT$$(K1$,-1%) &
		\ X%=-1% &
		\ RETURN
8120	  IF FIX(T*100.)<>0. THEN  &
		  PRINT "JE #";CVT$$(K1$,-1%);" DOES NOT BALANCE BY ";T &
		\ INPUT "UPDATE ANYWAY (Y/N)",V$ &
		\ IF V$<>"Y" THEN  &
			  PRINT "NOT UPDATED!" &
			\ X%=-1% &
			\ RETURN
8200	  !
8210	  I%=FNG%(2%,K2$) &
	\ K%=0%
8220	  IF MID(FNL$,7%,5%)="99999" THEN  &
		  GOTO 8230 UNLESS K6$="N" AND (LEFT(FNL$,6%)>=K2$ AND LEFT(FNL$,6%)<=K3$) &
		\ STOP IF FND%(2%,LEFT(FNL$,14%)) &
		\ GOTO 8230
8225	  GOSUB 2010 &
	\ GOTO 8240 IF J$(1%)>K3$ OR J$(1%)<K2$ OR J%(2%)<>0% &
	\ K%=K%+1% &
	\ LSET C1$(1%)=J1$(3%) &
	\ RSET C1$(2%)=J1$(2%)+" " &
	\ LSET C1$(3%)=J1$(4%) &
	\ LSET C1$(4%)=CVTF$(FNN4(J1$(5%))) &
	\ K4$=FND6$(CVT$%(J1$(6%))) &
	\ K1%=INSTR(1%,K4$,".") &
	\ K2%=INSTR(K1%+1%,K4$,".") &
	\ LSET C1$(5%)=CVT%$(VAL(LEFT(K4$,K1%-1%))) &
	\ LSET C1$(6%)=CVT%$(VAL(MID(K4$,K1%+1%,K2%-K1%-1%))) &
	\ LSET C1$(7%)=CVT%$(VAL(RIGHT(K4$,K2%+1%))) &
	\ LSET C1$(8%)=J1$(7%) &
	\ LSET C1$(9%)=J1$(8%) &
	\ STOP IF FNA%(4%,T4$) &
	\ IF K6$="N" THEN &
		  STOP IF FND%(2%,LEFT(T2$,14%)) &
	  ELSE &
		  J%(2%)=Z9% &
		\ GOSUB 2100 &
		\ STOP IF FNU%(2%,T2$)
8230	  GOTO 8220 UNLESS FNN%(2%)
8240	  PRINT USING "### JE'S UPDATED FOR ", K%; &
	\ PRINT K1$ &
	\ RETURN
8250	  PRINT "JE ";CVT$$(K5$,-1%);" DOES NOT BALANCE IN GROUP ";K1$;", ENTIRE GROUP NOT UPDATED." &
	\ PRINT "FIX ";CVT$$(K5$,-1%);" OR UPDATE SINGLY." &
	\ X%=-1% &
	\ RETURN
8300	  !
8310	  PRINT  &
	\ PRINT "TO UPDATE MULTIPLE JOURNAL ENTRIES, EITHER TYPE EACH JE #" &
	\ PRINT "SEPARATED BY A COMMA (,) OR, IF ALL JE #'S TO BE UPDATED" &
	\ PRINT "APPEAR IN ORDER IN THE PRINTOUT, TYPE THE FIRST AND LAST" &
	\ PRINT "SEPARATED BY A DASH (-).  EXAMPLE:  01.01,01.03,01.05-01.09" &
	\ GOTO 8020
10000	  !
10005	  V$,K$="" &
	\ I%=0%
10010	  CLOSE 1% &
	\ CLOSE 12% &
	\ V%=FNX%(V$,I%,K$)
10020	  V$="[1,3]ADJENT" &
	\ K$=A$ &
	\ I%=10% &
	\ GOTO 10010
14000	  DEF FNI$(V$,K$) &
	\ PRINT V$; &
	\ INPUT LINE #12%, K$ &
	\ FNI$=CVT$$(K$,140%) &
	\ FNEND
14030	  DEF FND8%(T) &
	\ FND8%=T &
	\ FNEND
14040	  DEF FND6%(V$) &
	\ FND6%=VAL(MID(V$,4%,2%))+VAL(LEFT(V$,2%))*32%+FND8%(VAL(RIGHT(V$,7%)))*512% &
	\ FNEND
14050	  DEF FND6$(V%) &
	\ FND6$=RIGHT(NUM1$((V% AND 15%*32%)/32%+100%),2%)+"."+RIGHT(NUM1$((V% AND 31%)+100%),2%)+"."+RIGHT(NUM1$((SWAP%(V%) AND 254%)/2%+100%),2%) &
	\ FNEND
14210	  DEF FND7%(V$) &
	\ ON ERROR GOTO 14220 &
	\ GOTO 14220 IF INSTR(1%,V$,".")<>3% OR INSTR(4%,V$,".")<>6% OR INSTR(7%,V$,".")<>0% OR LEN(V$)<>8% &
	\ D7%=VAL(LEFT(V$,2%)) &
	\ GOTO 14220 IF D7%<1% OR D7%>12% &
	\ D7%=VAL(MID(V$,4%,2%)) &
	\ GOTO 14220 IF D7%<1% OR D7%>31% &
	\ D7%=VAL(RIGHT(V$,7%)) &
	\ GOTO 14220 IF D7%<0% &
	\ FND7%=0% &
	\ GOTO 14230
14220	  FND7%=-1% &
	\ RESUME 14230
14230	  ON ERROR GOTO 0 &
	\ FNEND
14260	  DEF FND7$(V$) &
	\ V$=V$+"."+RIGHT(DATE$(0%),8%) IF LEN(V$)<6% &
	\ V$="0"+V$ IF INSTR(1%,V$,".")=2% &
	\ V$=LEFT(V$,3%)+"0"+RIGHT(V$,4%) IF INSTR(4%,V$,".")=5% &
	\ FND7$=V$ &
	\ FNEND
14410	  DEF FNN4$(T) &
	\ FNN4$=CVT%$(INT(T/2.^8.))+CHR$(T-INT(T/2.^8.)*2.^8.)+CHR$((T-INT(T))*100.+6.8213185310482798e-15) &
	\ FNEND
14420	  DEF FNN4(V$) &
	\ FNN4=CVT$%(LEFT(V$,2%))*2.^8%+ASCII(MID(V$,3%,1%))+ASCII(MID(V$,4%,1%))/100. &
	\ FNEND
32767	  END
