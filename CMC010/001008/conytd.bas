10	  ! &
	  ! Program name: conytd		Compiled with SCALE 0 on V06.C &
	  ! Decompiled on 28-Nov-16 at 04:11 PM
15	  DIM M1$(24%), M$(24%), M2$(37%), M(37%), T(4%)
100	  !
105	  OPEN "KB:" AS FILE 1%, RECORDSIZE 512%
120	  INPUT "YEAR FOR MASTER FILE (79, 80, ETC)";F$ &
	\ F$="FL" IF F$="" &
	\ F$="MSTR"+F$+".DAT" &
	\ IF FNO%(2%,F$,"","") THEN  &
		  PRINT "MASTER FILE NOT FOUND !!!" &
		\ GOTO 10000
200	  !
220	  FIELD #1%, 6% AS M1$(1%),30% AS M1$(2%),30% AS M1$(3%),30% AS M1$(4%),30% AS M1$(5%),11% AS M1$(6%),8% AS M1$(7%),1% AS M1$(8%),2% AS M1$(9%),12% AS M1$(10%),2% AS M1$(11%)
221	  FIELD #1%, 152%+I%*10% AS E$,8% AS M2$(I%),2% AS M1$(I%+11%) FOR I%=1% TO 9% &
	\ FIELD #1%, 252% AS E$,8% AS M1$(21%),8% AS M1$(22%) &
	\ FIELD #1%, 188%+I%*8% AS E$,8% AS M2$(I%) FOR I%=10% TO 36% &
	\ FIELD #1%, 484% AS E$,2% AS M1$(23%)
290	  FIELD #1%, 512% AS T2$
1000	  !
1010	  INPUT "LENGTH OF FORM (8.5 OR 11)";L1 &
	\ L1%=L1*6% &
	\ L1%=66% IF L1%=0%
1020	  PRINT "PAPER SHOULD BE AT LEAST 80 COL BY 51 LINES" &
	\ GOTO 8000
2300	  !
2310	  LSET T2$=FNL$ &
	\ M$(I%)=M1$(I%)+"" FOR I%=1% TO 23% &
	\ M(I%)=CVT$F(M2$(I%)) FOR I%=1% TO 36% &
	\ RETURN
8000	  !
8010	  INPUT "SET PAGE",K$ &
	\ L%=L1%-6% &
	\ GOSUB 8100 &
	\ T1(I%)=0. FOR I%=1% TO 4% &
	\ V%=FNG%(2%,"") &
	\ A$="\"+SPACE$(20%)+" \ \     \" &
	\ B$="###,###.## #,###.## ##,###.## #,###.## \\"
8020	  GOSUB 2300 &
	\ GOTO 8050 IF M$(1%)="ZPAYRL" &
	\ PRINT USING A$, M$(2%),M$(1%); &
	\ T(I%)=0. FOR I%=1% TO 4%
8030	  FOR I%=1% TO 4% &
		\ T(1%)=T(1%)+INT(M(10%+I%)*100.+0.5) &
		\ T(2%)=T(2%)+INT(M(21%+I%)*100.+0.5) &
		\ T(3%)=T(3%)+INT(M(16%+I%)*100.+0.5) &
		\ T(4%)=T(4%)+INT(M(26%+I%)*100.+0.5) &
	\ NEXT I% &
	\ T1(I%)=T1(I%)+T(I%) FOR I%=1% TO 4%
8035	  STOP IF T(1%)<>INT(M(10%)*100.+0.5) &
	\ STOP IF T(2%)<>INT(M(21%)*100.+0.5) &
	\ STOP IF T(3%)<>INT(M(16%)*100.+0.5) &
	\ STOP IF T(4%)<>INT(M(26%)*100.+0.5)
8040	  PRINT USING B$, T(1%)/100.,T(2%)/100.,T(3%)/100.,T(4%)/100.,M$(24%) &
	\ GOSUB 8100
8050	  IF FNN%(2%) THEN &
		  GOTO 8060 &
	  ELSE &
		  GOTO 8020
8060	  B$="\           \ ##,###,###.##" &
	\ PRINT  &
	\ PRINT  &
	\ PRINT "TOTALS!!" &
	\ PRINT USING B$, "EARNINGS ",T1(1%)/100. &
	\ PRINT USING B$, "FICA",T1(2%)/100. &
	\ PRINT USING B$, "FEDERAL",T1(3%)/100. &
	\ PRINT USING B$, "STATE",T1(4%)/100. &
	\ GOTO 10000
8100	  !
8110	  IF L%<L1%-9% THEN  &
		  L%=L%+1% &
		\ RETURN
8120	  PRINT DATE$(0%),STRING$(L1%-L%,10%) &
	\ PRINT "NAME                       #     EARNINGS     FICA"; &
	\ PRINT "   FEDERAL    STATE CD" &
	\ L%=3% &
	\ RETURN
10000	  !
10005	  I%=32767%
10010	  V$=SYS(CHR$(7%)) &
	\ Q3$=CVT%$(8100%)+"!MENU.BAC"+V$ IF ASCII(V$)=255% &
	\ V%=FNC%(2%) &
	\ CLOSE 1% &
	\ V%=FNC%(0%) &
	\ CHAIN "!MENU" 0. IF ASCII(V$)=255%
10020	  GOTO 32767
32767	  END
