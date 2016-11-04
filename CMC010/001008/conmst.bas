10	! &
	! MSTRFL - CONSTRUCTION MASTER EMPLOYEE FILE &
	! &
	! 09/31/79 - Written by HA     ! 06/18/79 - BL for (1,3) system &
	! 01/02/80 - BL add YTD hours  ! 10/01/84 - BH paging now works &
	! &

50	  DIM M1$(23%),M$(24%),M2$(44%),M(44%),E$(67%),T(44%) &

125	  Y$="FL" &
	\ Y$=CVT$$(FNX$,-1%) IF FNX$<>"" &
	\ GOSUB 2515 &

130	  OPEN "KB:" AS FILE 1%, RECORDSIZE 512% &
	\ OPEN "KB:" FOR INPUT AS FILE 11% &

200	! &
	! FIELD STATEMENTS &
	! &

220	  FIELD#1%, 6% AS M1$(1%),30% AS M1$(2%),30% AS M1$(3%),30% AS M1$(4%), &
		  30% AS M1$(5%),11% AS M1$(6%), 8% AS M1$(7%), 1% AS M1$(8%), &
		  2% AS M1$(9%),12% AS M1$(10%),2% AS M1$(11%) &

221	  FIELD#1%,152%+I%*10% AS E$,8% AS M2$(I%),2% AS M1$(I%+ &
		  11%) FOR I%=1% TO 9% &
	\ FIELD#1%,252% AS E$,8% AS M1$(21%),8% AS M1$(22%) &
	\ FIELD#1%,188%+I%*8% AS E$,8% AS M2$(I%) FOR I%=10% TO 36% &
	\ FIELD#1%,484% AS E$,2% AS M1$(23%) &
	\ FIELD #1%,486%+I%*2% AS E$,2% AS M2$(I%+37%)FOR I%=0% TO 7% &
	\ FIELD#1%,512% AS T2$ &
	! MSTRFL.DAT &

310	  READ E$(I%) FOR I%=1% TO 67% &

320	DATA "EMPLOYEE # ","NAME       ","ADDRESS    ","CITY-STATE ", &
	"ZIP        ","SSAN       ","TELEPHONE  ","MARITAL    ", &
	"EXEMPTIONS ","TRADE      ","PAY PERIOD ","RATE 1     ", &
	"RATE 1 CD  ","RATE 2     ","RATE 2 CD  ","RATE 3     ", &
	"RATE 3 CD  ","VACATION   ","VAC CODE   ","INS RATE   ", &
	"INS CODE   ","OTHER 1    ","OTHER 1 CD ","OTHER 2    ", &
	"OTHER 2 CD ","OTHER 3    ","OTHER 3 CD ","OTHER 4    ", &
	"OTHER 4 CD ","START DATE ","QUIT DATE  ","EARN TOTAL ", &
	"EARN QTR 1 ","EARN QTR 2 ","EARN QTR 3 ","EARN QTR 4 ", &
	"EARN OTHER ","FED TOTAL  ","FED QTR 1  ","FED QTR 2  ", &
	"FED QTR 3  ","FED QTR 4  ","FICA TOTAL ","FICA QTR 1 ", &
	"FICA QTR 2 ","FICA QTR 3 ","FICA QTR 4 ","ST TOTAL   ", &
	"ST QTR 1   ","ST QTR 2   ","ST QTR 3   ","ST QTR 4   ", &
	"VAC TOTAL  ","INSUR TOTAL","TRAVEL TOT ","FRINGE TOT ", &
	"O. DED TOT ","NET PD TOT ","STATE (##) ", &
	"HRS QTR 1  ","HRS QTR 2  ","HRS QTR 3  ","HRS QTR 4 ", &
	"OVT QTR 1  ","OVT QTR 2  ","OVT QTR 3  ","OVT QTR 4  " &

1000	! &
	!  PROGRAM CONTROL SECTION &
	! &

1020	  PRINT &
	\ INPUT "Option ";K$ &
	\ K$=LEFT(K$,3%) &
	\ GOTO 1030 IF K$="" &
	\ GOTO 8000 IF K$="ENT" &
	\ GOTO 3000 IF K$="DEL" &
	\ GOTO 4000 IF K$="CHA" &
	\ GOTO 4500 IF K$="RAT" &
	\ GOTO 6000 IF K$="FIN" &
	\ GOTO 7000 IF K$="PRI" &
	\ GOTO 9600 IF K$="LIS" &
	\ GOTO 9700 IF K$="TOT" &
	\ GOTO 9600 IF K$="NAM" &
	\ GOTO 10000 IF K$="END" &
	\ GOTO 2480 IF K$="OTH" &
	\ GOTO 9000 IF K$="SWA" &
	\ PRINT &
	\ PRINT "TYPE <RETURN> FOR AN OPTIONS LIST." &
	\ GOTO 1020 &

1030	  PRINT &
	\ PRINT "OPTIONS:" &
	\ PRINT "   ENTER  NEW EMPLOYEES" &
	\ PRINT "   DELETE AN EMPLOYEE" &
	\ PRINT "   CHANGE AN EMPLOYEE" &
	\ PRINT "   RATE   CHANGE EMPLOYEE'S RATES" &
	\ PRINT "   PRINT  ALL EMPLOYEES (PERSONAL OR FINANCIAL FORMAT)" &
	\ PRINT "   FIND   ONE EMPLOYEE" &
	\ PRINT "   LIST   EMPLOYEE CODES AND NAMES" &
	\ PRINT "   TOTALS OF ALL FINANCIAL DATA" &
	\ PRINT "   NAME   AND ADDRESS LIST (SINGLE LINE OR LABEL FORMAT)" &
	\ PRINT "   OTHER  YEAR OPENED" &
	\ PRINT "   SWAP   THE CURRENT YEAR WITH ANOTHER YEAR" &
	\ PRINT "   END    PROGRAM" &
	\ GOTO 1020 &

2000	! &
	!  INPUT EMPLOYEE # &
	! &

2010	  PRINT &
	\ PRINT E$(1%); &
	\ INPUT #11%,A$ &
	\ A$=A$+SPACE$(6%-LEN(A$)) &
	\ RETURN &

2100	! &
	! PREPARE MASTER FILE &
	! &

2110	  LSET M1$(I%)=M$(I%) FOR I%=1% TO 23% &
	\ LSET M2$(I%)=CVTF$(M(I%)) FOR I%=1% TO 36% &
	\ LSET M2$(I%)=CVT%$(M(I%)*10.+0.51) FOR I%=37% TO 44% &
	\ RETURN &

2200	! &
	!  INPUT LINE STRING AND FORMAT FOR 30% &
	! &

2210	  INPUT LINE K$ &
	\ K$=CVT$$(K$,4%) &
	\ RETURN IF K$="" &
	\ K$=K$+SPACE$(30%-LEN(K$)) &
	\ RETURN &

2300	! &
	! SEPARATE MASTER FILE &
	! &

2310	  LSET T2$=FNL$ &
	\ M$(I%)=M1$(I%)+"" FOR I%=1% TO 23% &
	\ M(I%)=CVT$F(M2$(I%)) FOR I%=1% TO 36% &
	\ M(I%)=CVT$%(M2$(I%))/10. FOR I%=37% TO 44% &
	\ RETURN &

2480	  GOSUB 2500 &
	\ GOTO 1020 &

2500	! &
	! Ask for/open a different year for the masterfile &
	!
2510	  PRINT &
	\ INPUT "What year do you want to open (?) ";Y$ &
	\ IF Y$="?" &
	  THEN	  PRINT &
		\ PRINT "Press RETURN for current year, or enter the" &
		\ PRINT "year in the format '81','82',..." &
		\ GOTO 2510 &

2515	  Y$="FL" IF Y$="" &
	\ V%=FNC%(2%) &

2520	  IF FNO%(2%,"MSTR"+Y$+".DAT","","")=0% &
	  THEN	  GOTO 2560 &
	  ELSE	  PRINT "Unable to open master file for "; &
		\ PRINT "current year"; IF Y$="FL" &
		\ PRINT "19";Y$; IF Y$<>"FL" &
		\ PRINT "." &
		\ IF FNS%<>5% &
		  THEN	  PRINT "ERROR: ";RIGHT(SYS(CHR$(6%)+CHR$(9%)+ &
				  CHR$(FNS%)),3%);" (";FNS%;")" &
			\ PRINT "Please call CMC if there is a problem!" &
			\ GOTO 10000 &

2530	 PRINT "A master file does not yet exist, shall I create" &
	\ PRINT "it (Y or N) "; &
	\ INPUT K$ &
	\ K$=LEFT(CVT$$(K$,-1%),1%) &
	\ GOTO 10000 IF K$="N" &
	\ GOTO 2530 IF K$<>"Y" &
	\ IF FNO%(2%,"MSTR"+Y$+".DAT","/CR:8,512","") &
	  THEN	  PRINT "Unable to create MASTER file!" &
		\ PRINT "Please call CMC to correct any problem!" &
		\ GOTO 10000 &

2540	  Y1$=Y$ &
	\ IF Y1$="FL" &
	  THEN	  Y1$=RIGHT(DATE$(0%),8%) &
		\ PRINT "Please enter the current year <";Y1$;"> "; &
		\ INPUT K$ &
		\ Y1$=K$ IF K$<>"" &

2550	  IF FNA%(2%,"ZPAYRL"+SPACE$(30%)+Y1$) &
	  THEN	  PRINT "Unable to add ZPAYRL record to file!" &
		\ GOTO 10000 &

2560	  Y1$="Unknown" &
	\ IF FNG%(2%,"ZPAYRL") &
	  THEN	  PRINT "Unable to find ZPAYRL record!" &
	  ELSE	  Y1$=MID(FNL$,37%,2%) &
		\ Y1$="82" IF Y1$="  " &
		\ PRINT "Masterfile for ";Y1$; &
		\ PRINT " (current)"; IF Y$="FL" &
		\ PRINT " opened!" &

2570	  RETURN &

2990	  PRINT &
	\ PRINT "ENTER <RETURN> FOR THE CURRENT YEAR." &
	\ PRINT 'ENTER THE YEAR NUMBER (EG., "79"), FOR ANY OTHER YEAR.' &
	\ PRINT &
	\ GOTO 120 &

3000	! &
	!  DELETE A FILE RECORD &
	! &

3010	  GOSUB 2000 &
	\ GOTO 1020 IF A$="" &
	\ GOTO 3010 IF FNG%(2%,A$) &
	\ GOSUB 2300 &
	\ PRINT E$(1%),M$(1%) &
	\ PRINT E$(2%),M$(2%); &
	\ T=0. &
	\ T=T+M(I%) FOR I%=10% TO 36% &
	\ PRINT CHR$(10%);CHR$(13%); &
		  "There is current activity for this employee.";CHR$(10%); &
		  CHR$(13%); &
		  "This employee should not be deleted.  If you need help"; &
		  CHR$(10%);CHR$(13%);"please contact CMC." IF INT(T)<>0. &
	\ IF INT(T)<>0. &
	  THEN	  3010 &
	  ELSE	  INPUT "CONFIRM DELETION (Y/N) ";K$ &
		\ GOTO 3010 IF LEFT(K$,1%)<>"Y" &
		\ V%=FND%(2%,"") &
		\ PRINT "DELETED." UNLESS V% &
		\ GOTO 3010 &

4000	! &
	!  CHANGE FILE INFORMATION &
	! &

4010	  GOSUB 2000 &
	\ GOTO 10020 IF A$="" &
	\ GOTO 4010 IF FNG%(2%,A$) &
	\ GOSUB 2300 &
	\ PRINT E$(1%),M$(1%) &
	\ PRINT E$(2%),M$(2%) &

4020	  PRINT &
	\ PRINT "ITEM TO CHANGE (? FOR HELP)"; &
	\ GOSUB 2200 &
	\ GOTO 4010 IF K$="" &
	\ K$=LEFT(K$,11%) &
	\ GOTO 4040 IF K$=E$(I%) OR K$=NUM1$(I%) FOR I%=1% TO 67% &
	\ IF K$="?" &
	  THEN	  PRINT &
		\ PRINT "Enter either the number or the description of "; &
			  "the item to change." &
		\ PRINT "POSSIBLE CHOICES ARE:" &
		\ FOR I%=1% TO 67% &
			\ PRINT USING "\\ \         \ ",NUM1$(I%),E$(I%); &
			\ PRINT IF POS(0%)>60% &
		\ NEXT I% &
		\ PRINT &
		\ GOTO 4020 &

4030	  PRINT "ITEM NOT RECOGNIZED AS SPELLED." &
	\ GOTO 4020 &

4040	  IF I%<12% &
	  THEN	  L%=1% &
		\ K%=I% &
		\ GOTO 4080 &

4045	  IF I%=59% &
	  THEN	  L%=1% &
		\ K%=23% &
		\ GOTO 4080 &

4050	  IF I%>=32% &
	  THEN	  L%=2% &
		\ K%=I%-22% &
		\ K%=K%-1% IF K%>36% &
		\ GOTO 4080 &

4055	  IF I%=30% OR I%=31% &
	  THEN	  K%=I%-9% &
		\ L%=1% &
		\ GOTO 4080 &

4060	  IF I%=I%/2%*2% &
	  THEN	  L%=2% &
		\ K%=I%/2%-5% &
		\ GOTO 4080 &

4070	  L%=1% &
	\ K%=I%/2%+6% &

4080	  PRINT E$(I%), &
	\ IF L%=1% &
	  THEN	  PRINT CVT$$(M$(K%),128%);"  "; &
		\ INPUT LINE K$ &
		\ K$=CVT$$(K$,4%) &
		\ M$(K%)=K$ IF K$<>"" &
		\ M$(K%)="" IF K$="." &
		\ GOTO 4100 &

4090	  PRINT M(K%);"  "; &
	\ INPUT K$ &
	\ T(1%)=M(K%) &
	\ M(K%)=VAL(K$) IF K$<>"" &
	\ V%=INSTR(1%,E$(I%),"QTR ") &
	\ IF V% AND K$<>"" AND I%<60% &
	  THEN	  V%=VAL(RIGHT(E$(I%),V%+4%)) &
		\ M(K%-V%)=M(K%-V%)+VAL(K$)-T(1%) &
		\ PRINT "TOTAL AMOUNT ALSO ADJUSTED" &
	! CHANGE TOTAL IF QUARTER CHANGES &

4100	  IF I%<>1% &
	  THEN	  GOSUB 2100 &
		\ V%=FNU%(2%,T2$) &
		\ GOTO 4020 &

4120	  GOTO 4020 IF K$="" &
	\ T=0. &
	\ T=T+M(I%) FOR I%=10% TO 36% &
	\ IF T<>0. &
	  THEN	  IF LEFT(K$,1%)<>"*" &
		  THEN	  PRINT "Please call CMC and we will help you make"; &
				  " the employee code change" &
			\ M$(1%)=M1$(1%) &
			\ GOTO 4020 &

4125	  M$(1%)=RIGHT(K$,2%) IF LEFT(K$,1%)="*" &
	\ M$(1%)=M$(1%)+SPACE$(6%-LEN(M$(1%))) &
	\ IF FNG%(2%,M$(1%))=0% &
	  THEN	  PRINT "THAT EMPLOYEE # IS IN USE. REQUEST IS ABORTED." &
		\ GOTO 4020 &

4130	  GOSUB 2100 &
	\ STOP IF FNG%(2%,A$) &
	\ IF FNU%(-2%,T2$)=0% &
	  THEN	  4010 &
	  ELSE	  PRINT"Unable to make change to ";M$(1%);"." &
		\ PRINT "CALL CMC!!!!!" &
		\ K$=SYS(CHR$(5%)) &

4500	! &
	! CHANGE RATE ONE IN SERIES &
	! &

4505	  PRINT 'USE "END" TO END OPTION, USE "RETURN" TO LEAVE RATE UNCHANGED' &

4510	  INPUT "START WITH EMPLOYEE CODE :",A$ &
	\ GOTO 1020 IF A$="" &
	\ IF FNG%(2%,A$) &
	  THEN	  PRINT "EMPLOYEE NOT FOUND" &
		\ GOTO 4510 &

4520	  GOSUB 2300 &
	\ PRINT M$(1%);M$(2%); &
	\ PRINT USING " RATE = ###.### ",M(1%); &
	\ INPUT "NEW RATE ",A$ &
	\ GOTO 4510 IF A$="END" &
	\ GOTO 4540 IF A$="" &
	\ ON ERROR GOTO 4535 &
	\ M(1%)=VAL(A$) &
	\ GOSUB 2100 &
	\ STOP IF FNU%(2%,T2$) &
	\ GOTO 4540 &

4535	  PRINT "BAD INPUT TRY AGAIN " &
	\ RESUME 4520 &

4540	  GOTO 1020 IF FNN%(2%) &
	\ GOTO 4520 &

6000	! &
	!  FIND A SINGLE RECORD &
	! &

6010	  GOSUB 2000 &
	\ GOTO 1020 IF A$="" &
	\ V%=FNG%(2%,A$) &
	\ GOSUB 2300 &
	\ X1%=70% &
	\ PRINT &
	\ GOSUB 7220 &
	\ GOSUB 7100 &
	\ X1%=70% &
	\ X%=X%-1% &
	\ GOSUB 7700 &
	\ GOSUB 7600 &
	\ GOTO 6000 &

7000	! &
	!  PERSONAL DATA PRINTOUT CONTROL SECTION &
	! &

7005	  V%=FNG%(2%,"") &
	\ IF V% OR LEFT(FNL$,6%)="ZPAYRL" &
	  THEN	  PRINT "File is empty." &
		\ GOTO 1020 &

7010	  PRINT &
	\ INPUT "PRINT PERSONAL OR FINANCIAL DATA (P/F) ";K$ &
		  UNTIL INSTR(1%," PF",CVT$$(K$,-1%)) OR K$="" &
	\ GOTO 1020 IF K$="" &
	\ GOTO 7500 IF K$="F" &
	\ X5%=66% &
	\ X1%=68% &
	\ X%=0% &
	\ V%=FNG%(2%,"") &
	\ INPUT #11%,"SET PAGE.";K$ &

7030	  GOSUB 7200 IF X1%>X5%-5% &
	\ GOSUB 2300 &
	\ GOSUB 7100
7035	  IF FNN%(2%) &
	  THEN	  PRINT FOR I%=X1% TO 68% &
		\ GOTO 1020 &

7040	  GOTO 7030 UNLESS LEFT(FNL$,6%)="ZPAYRL" &
	\ GOTO 7035 IF LEFT(FNL$,6%)="ZPAYRL" &
	\ PRINT FOR I%=X1% TO 66% &
	\ GOTO 1020 &

7100	! &
	!  ROUTINE TO PRINT A PERSONAL DATA RECORD &
	! &

7110	  PRINT M$(1%);" ";M$(2%);"  ";M$(6%);"  ";M$(7%);"  ";M$(8%);M$(9%); &
		  " ";M$(10%) &
	\ PRINT "       ";M$(3%);"  PAY PERIODS ";M$(11%); &
		  "         STATE CODE ";M$(23%) &
	\ PRINT "       ";M$(4%);"  START DATE ";M$(21%); &
	\ PRINT "    QUIT DATE ";M$(22%) IF CVT$$(M$(22%),-1%)<>"" &
	\ PRINT IF CVT$$(M$(22%),-1%)="" &
	\ X1%=X1%+4% &
	\ PRINT "       ";M$(5%) &
	\ X1%=X1%+1% &
	\ PRINT &
	\ RETURN &

7200	! &
	!  PERSONAL DATA HEADER &
	! &

7210	  X%=X%+1% &
	\ PRINT FOR I%=X1% TO 69% &
	\ PRINT "PAGE";X% &

7220	  PRINT "EMP #  NAME";SPACE$(28%);"SSAN         PHONE    W/H  TRADE" &
	\ PRINT &
	\ X1%=7% &
	\ RETURN &

7490	! &
	!  PRINT FINANCIAL RECORD CONTROL SECTION &
	! &

7500	  X%=0% &
	\ X5%=51% &
	\ X1%=68% &
	\ V%=FNG%(2%,"") &
	\ INPUT #11%,"SET PAGE.";K$ &

7510	  GOSUB 2300 &
	\ GOSUB 7800 &
	\ GOSUB 7700 &
	\ GOSUB 7600
7515	  IF FNN%(2%) &
	  THEN	  PRINT FOR I%=X1% TO 53% &
		\ GOTO 1020 &

7530	  GOTO 7515 IF LEFT(FNL$,6%)="ZPAYRL" &
	\ GOTO 7510 &

7600	! &
	!  PRINT FINANCIAL DATA RECORDS &
	! &

7610	  PRINT USING "####.##\\ ###.##\\ ###.##\\ ##,###.## ##,###.## "+ &
		  "####.## ####.## OTH=##,###.##",M(1%),M$(12%),M(6%),M$(17%), &
		  M(5%),M$(16%),M(11%),M(17%),M(22%),M(27%),M(35%) &
	\ PRINT USING "####.##\\ ###.##\\          ##,###.## ##,###.## "+ &
		  "####.## ####.## INS=##,###.##",M(2%),M$(13%),M(7%),M$(18%), &
		  M(12%),M(18%),M(23%),M(28%),M(32%) &
	\ PRINT USING "####.##\\ ###.##\\    VAC   ##,###.## ##,###.## "+ &
		  "####.## ####.## VAC=##,###.##",M(3%),M$(14%),M(8%),M$(19%), &
		  M(13%),M(19%),M(24%),M(29%),M(31%) &
	\ PRINT USING "          ###.##\\ ###.##\\ ##,###.## ##,###.## "+ &
		  "####.## ####.## REG=##,###.##",M(9%),M$(20%),M(4%),M$(15%), &
		  M(14%),M(20%),M(25%),M(30%),M(37%)+M(38%)+M(39%)+ &
		  M(40%) &
	\ PRINT USING "         NON-TAXED EARNINGS=##,###.##"+SPACE$(27%)+ &
		  "OVT=##,###.##",M(15%),M(41%)+M(42%)+M(43%)+M(44%) &
	\ PRINT USING "         TOTALS             ##,###.## ##,###.## "+ &
		  "####.## ####.## NET=##,###.##",M(10%),M(16%),M(21%),M(26%), &
		  M(36%) &
	\ PRINT &
	\ X1%=X1%+11% &
	\ RETURN &

7700	! &
	!  FINANCIAL DATA PAGE HEADER &
	! &

7710	  PRINT "   RATE     OTHER   INSUR    EARNINGS   FEDERAL    FICA   STATE " &
	\ RETURN &

7800	! &
	!  PRINT FINANCIAL-PERSONAL DATA HEADER &
	! &

7810	  IF X1%>50% &
	  THEN	  X%=X%+1% &
		\ PRINT FOR I%=X1% TO 69% &
		\ PRINT "PAGE";X% &
		\ X1%=5% &

7820	  PRINT "EMP#   NAME                  SSN #        PP   STATUS   STATE CODE" &
	\ PRINT USING "\    \ \                   \ \         \  \\     \ \"+ &
		  "        \\",M$(1%),M$(2%),M$(6%),M$(11%),M$(8%)+M$(9%), &
		  M$(23%) &
	\ PRINT &
	\ RETURN &

8000	! &
	!  ADD NEW EMPLOYEE RECORDS &
	! &

8010	  GOSUB 2000 &
	\ GOTO 10020 IF A$="" &
	\ IF FNG%(2%,A$)=0% &
	  THEN	  PRINT "THAT EMPLOYEE # IS IN USE." &
		\ GOTO 8010 &

8020	  M$(1%)=A$ &
	\ PRINT E$(2%); &
	\ GOSUB 2200 &
	\ M$(2%)=K$ &
	\ PRINT E$(3%); &
	\ GOSUB 2200 &
	\ M$(3%)=K$ &
	\ PRINT E$(4%); &
	\ GOSUB 2200 &
	\ IF K$="" &
	  THEN	  M$(4%)="IDAHO FALLS, IDAHO  83401" &
		\ M$(5%)="" &
		\ GOTO 8040 &

8030	  M$(4%)=K$ &
	\ PRINT E$(5%); &
	\ GOSUB 2200 &
	\ M$(5%)=K$ &

8040	  FOR I%=6% TO 11% &
		\ PRINT E$(I%); &
		\ INPUT M$(I%) &
	\ NEXT I% &
	\ FOR I%=1% TO 12% &
		\ M$(I%+11%)="" &
		\ M(I%)=0. &
	\ NEXT I% &
	\ FOR I%=1% TO 3% &
		\ PRINT E$(I%*2%+10%); &
		\ INPUT M(I%) &
		\ IF M(I%)<>0. &
		  THEN	  PRINT E$(I%*2%+11%); &
			\ INPUT M$(I%+11%) &
			\ M$(I%+11%)="O" IF M$(I%+11%)="" &
		\ NEXT I% &

8055	  FOR I%=1% TO 6% &
		\ PRINT E$(I%*2%+16%); &
		\ INPUT M(I%+3%) &
		\ GOTO 8075 IF M(I%+3%)=0. AND I%>2% &
		\ PRINT E$(I%*2%+17%); &
		\ INPUT M$(I%+14%) &
	\ NEXT I% &

8075	  PRINT E$(30%); &
	\ INPUT M$(21%) &
	\ PRINT E$(31%); &
	\ INPUT M$(22%) &
	\ PRINT E$(59%); &
	\ INPUT M$(23%) &
	\ M$(23%)="00" IF M$(23%)="" &
	\ M(I%)=0. FOR I%=10% TO 44% &
	\ GOSUB 2100 &
	\ IF FNA%(2%,T2$) &
	  THEN	  PRINT "ERROR";FNS%;".  UNABLE TO ADD NEW RECORD." &
		\ GOTO 10020 &

8090	  GOTO 8010 &

9000	! &
	! Re-name current master file as MSTRFL.xx?, and replace &
	! an old master file as MSTRFL.DA? so that it can be used. &
	!
9005	  IF Y$<>"FL" &
	  THEN	  PRINT "Before swapping the current master file, you" &
		\ PRINT "must have the current file open, and not ";Y$;"." &
		\ GOTO 1000 &

9010	  PRINT "What year do you wish to use as the current master file." &
	\ PRINT "Currently, it is set for ";Y1$;" "; &
	\ INPUT Y2$ &
	\ GOTO 1000 IF Y2$="" &

9015	  GOTO 1000 IF Y2$=Y$ &

9020	  IF FNO%(4%,"MSTR"+Y2$+".DAT","","") &
	  THEN	  PRINT "That file is not currently on the system!" &
		\ GOTO 1000 &

9030	  IF FNG%(4%,"ZPAYRL") &
	  THEN	  PRINT "Unable to find ZPAYRL record, adding it!" &
		\ IF FNA%(4%,"ZPAYRL"+SPACE$(30%)+Y2$) &
		  THEN	  PRINT "Unable to add ZPAYRL record! Aborting!" &
			\ GOTO 10000 &

9040	  V%=FNC%(2%)+FNC%(4%) &
	\ NAME "MSTRFL.DAT" AS "MSTR"+Y1$+".DAT" &
	\ NAME "MSTRFL.DA1" AS "MSTR"+Y1$+".DA1" &
	\ NAME "MSTR"+Y2$+".DAT" AS "MSTRFL.DAT" &
	\ NAME "MSTR"+Y2$+".DA1" AS "MSTRFL.DA1" &

9050	  Y$="FL" &
	\ GOSUB 2520 &

9060	  PRINT "Swapping complete!" &
	\ GOTO 1000 &

9600	! &
	!  LIST & NAME OPTIONS &
	! &

9610	  V%=FNG%(2%,"") &
	\ INPUT #11%,"LABELS (Y OR N)";E$ IF K$="NAM" &
	\ INPUT #11%,"SET PAGE.";A$ &
	\ X1%=62% &
	\ X%=1% &

9620	  GOSUB 2300 &
	\ GOSUB 9650 IF E$<>"Y" OR K$="LIS" &
	\ IF E$<>"Y" AND K$="NAM" &
	  THEN	  PRINT LEFT(M$(2%),20%);TAB(22%);LEFT(M$(3%),20%);TAB(44%); &
		\ PRINT LEFT(M$(4%),20%);TAB(66%);LEFT(M$(5%),20%) &
		\ GOTO 9640 &

9630	  IF E$="Y" AND K$="NAM" &
	  THEN	  PRINT M$(2%) &
		\ PRINT M$(3%) &
		\ PRINT M$(4%) &
		\ PRINT M$(5%) &
		\ PRINT &
		\ PRINT &
		\ GOTO 9640 &

9635	  IF K$="LIS" &
	  THEN	  PRINT M$(1%);" . ";M$(2%) &

9640	  IF FNN%(2%) OR LEFT(FNL$,6%)="ZPAYRL" &
	  THEN	  1020 &
	  ELSE	  9620 &

9650	  X1%=X1%+1% &
	\ RETURN IF X1%<58% &
	\ PRINT STRING$(67%-X1%,10%);"PAGE ";X% &
	\ X%=X%+1% &
	\ X1%=2% &
	\ RETURN &

9700	! &
	!  SUM AND PRINT DATA TOTALS &
	! &

9710	  T(I%)=0. FOR I%=0% TO 37% &
	\ V%=FNG%(2%,"") &

9720	  GOSUB 2300 &
	\ IF M$(1%)<>"ZPAYRL" &
	  THEN	  T(I%)=T(I%)+M(I%) FOR I%=10% TO 36% &
		\ T(0%)=T(0%)+1. &
		\ IF FNN%(2%) &
		  THEN	  9730 &
		  ELSE	  9720 &

9725	  PRINT "LAST UPDATE WAS ";M$(2%) &
	\ IF FNN%(2%)=0% &
	  THEN	  9720 &

9730	  M$(I%)="" FOR I%=1% TO 24% &
	\ M(I%)=T(I%) FOR I%=0% TO 36% &
	\ K$="ALL STATES" &
	\ GOSUB 9735 &
	\ GOTO 1020 &

9735	  PRINT &
	\ PRINT "SUMMARY OF YEAR-TO-DATE TOTALS FOR ";K$;" PRINTED ON "; &
		  DATE$(0%) &
	\ PRINT &
	\ PRINT &
	\ PRINT "    EARNINGS     FED TAX        FICA       STATE" &
	\ A$="#,###,###.##  ###,###.##  ###,###.##  ###,###.##    "+ &
		  "\          \ #,###,###.##" &
	\ PRINT USING A$,M(11%),M(17%),M(22%),M(27%),"VACATION", M(31%) &
	\ PRINT USING A$,M(12%),M(18%),M(23%),M(28%),"INSURANCE", M(32%) &
	\ PRINT USING A$,M(13%),M(19%),M(24%),M(29%),"TRAVEL", M(33%) &
	\ PRINT USING A$,M(14%),M(20%),M(25%),M(30%),"FRINGE", M(34%) &
	\ PRINT USING "#,###,###.##"+SPACE$(40%)+"\          \ #,###,###.##", &
		  M(15%),"OTHER DEDUCT",M(35%) &
	\ PRINT USING A$,M(10%),M(16%),M(21%),M(26%),"NET PAID", M(36%) &
	\ PRINT &
	\ PRINT "TOTAL EMPLOYEES=";T(0%) &
	\ PRINT &
	\ RETURN &

10000	! &
	!  TERMINATE PROGRAM &
	! &

10010	  STOP IF FNX%("",0%,"") &

10020	  STOP IF FNX%("PP0:[1,8]CONMST.BAC",125%,Y$) &

32767 END
