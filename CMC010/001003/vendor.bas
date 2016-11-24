10	  ! &
	  ! Program name: vendor		Compiled with SCALE 0 on V08.0 &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
20	  OPEN "KB:" AS FILE 10% &
	\ FIELD #10%, 6% AS U$(1%),29% AS U$(2%),29% AS U$(3%),29% AS U$(4%),29% AS U$(5%),5% AS U$(6%),1% AS U$(7%) &
	\ FIELD #10%, 128% AS T3$
30	  V%=FNO%(2%,"VENDOR.DAT","/RW","") &
	\ IF V%<>0% THEN  &
		  PRINT "Unable to open Vendor file." IF V%<>5% &
		\ GOTO 10000 IF V%<>5% &
		\ INPUT "Vendor file doesn't exist.  Shall I create one<N>";K$ &
		\ IF LEFT(K$,1%)<>"Y" THEN &
			  GOTO 10000 &
		  ELSE &
			  V%=FNO%(2%,"VENDOR.DAT","/CR:8,128/RW","") &
			\ IF V%<>0% THEN  &
				  PRINT "Unable to open Vendor file." &
				\ PRINT "Please call CMC for help." &
				\ STOP
50	  V%=FNO%(4%,"PAHOLD.DAT","/RO","") &
	\ V%=FNO%(6%,"PAYABL.DAT","/RO","")
70	  S$(1%)="VENDOR # " &
	\ S$(2%)="NAME     " &
	\ S$(3%)="ADDRESS  " &
	\ S$(4%)="CITY, ST " &
	\ S$(5%)="ZIP CODE " &
	\ S$(6%)="DISCOUNT " &
	\ S$(7%)="PAY DATE "
80	  OPEN "KB:" FOR INPUT AS FILE 11%
1000	  !
1020	  PRINT  &
	\ INPUT "OPTION ";K$ &
	\ K$=LEFT(K$,3%) &
	\ GOTO 1030 IF K$="" &
	\ GOTO 2000 IF K$="1" OR K$="ENT" &
	\ GOTO 3000 IF K$="2" OR K$="DEL" &
	\ GOTO 4000 IF K$="3" OR K$="CHA" &
	\ GOTO 5000 IF K$="4" OR K$="EXA" &
	\ GOTO 6000 IF K$="5" OR K$="FIN" &
	\ GOTO 7000 IF K$="6" OR K$="PRI" &
	\ GOTO 8000 IF K$="7" OR K$="ENV" &
	\ GOTO 9000 IF K$="9" OR K$="SOR" &
	\ GOTO 8200 IF K$="8" OR K$="LAB" &
	\ GOTO 10000 IF K$="10" OR K$="END" &
	\ GOTO 1020
1030	  PRINT  &
	\ PRINT "OPTIONS:" &
	\ PRINT '          1.  "ENTER" NEW VENDORS' &
	\ PRINT '          2.  "DELETE" EXISTING VENDORS' &
	\ PRINT '          3.  "CHANGE" VENDOR INFORMATION' &
	\ PRINT '          4.  "EXAMINE" VENDORS' &
	\ PRINT '          5.  "FIND" A SINGLE VENDOR' &
	\ PRINT '          6.  "PRINT" ALL VENDORS' &
	\ PRINT '          7.  "ENVELOPE" PRINTER' &
	\ PRINT '          8.  "LABELS" PRINTED' &
	\ PRINT '          9.  "SORT" VENDOR FILE' &
	\ PRINT '          10. "END" PROGRAM AND UPDATE FILES' &
	\ GOTO 1020
2000	  !
2010	  PRINT  &
	\ PRINT "NEW VENDOR # "; &
	\ GOSUB 2200 &
	\ GOTO 1020 IF K$="" &
	\ W$(1%)=K$ &
	\ IF FNG%(2%,W$(1%))=0% THEN  &
		  PRINT "THAT NUMBER IS PRESENTLY IN USE." &
		\ GOTO 2010
2020	  FOR I%=2% TO 4% &
		\ PRINT S$(I%);"? "; &
		\ GOSUB 2200 &
		\ GOTO 2090 IF K$="-" &
		\ W$(I%)=K$ &
	\ NEXT I% &
	\ IF W$(4%)="" THEN  &
		  W$(4%)="IDAHO FALLS, IDAHO  83401" &
		\ W$(5%)="" &
		\ GOTO 2035
2030	  PRINT "ZIP CODE ? "; &
	\ GOSUB 2200 &
	\ GOTO 2090 IF K$="-" &
	\ W$(5%)=K$
2035	  PRINT "DISCOUNT % ? "; &
	\ GOSUB 2200 &
	\ GOTO 2090 IF K$="-" &
	\ W$(6%)=K$ &
	\ PRINT "PAY DATE ? "; &
	\ GOSUB 2200 &
	\ GOTO 2090 IF K$="-" &
	\ W(1%)=VAL(K$) &
	\ GOSUB 2100 &
	\ V%=FNA%(2%,T3$) &
	\ GOTO 2010
2090	  PRINT "THIS ITEM IS VOID !!!" &
	\ GOTO 2010
2100	  !
2110	  LSET U$(I%)=W$(I%) FOR I%=1% TO 6% &
	\ LSET U$(7%)=CHR$(W(1%)) &
	\ RETURN
2200	  !
2210	  INPUT LINE #11%, K$ &
	\ K$=CVT$$(K$,132%) &
	\ RETURN
2300	  !
2310	  LSET T3$=FNL$ &
	\ W$(I%)=U$(I%)+"" FOR I%=1% TO 6% &
	\ W(1%)=ASCII(U$(7%)) &
	\ RETURN
2400	  !
2410	  IF X1%>55% THEN  &
		  PRINT  FOR X1%=X1% TO 70% &
		\ PRINT TAB(10%);"VENDOR REGISTER ON ";DATE$(0%) &
		\ PRINT  &
		\ X1%=7%
2420	  PRINT W$(1%);"  ";W$(2%) &
	\ RETURN IF V1$="S" &
	\ FOR J%=3% TO 5% &
		\ PRINT "        ";W$(J%) &
	\ NEXT J% &
	\ PRINT "        DISCOUNT= ";W$(6%); &
	\ PRINT "        PAY DATE ";W(1%); IF W(1%)<>0. &
	\ PRINT  &
	\ PRINT  &
	\ X1%=X1%+6% &
	\ RETURN
3000	  !
3010	  PRINT  &
	\ PRINT "DELETE - VENDOR # "; &
	\ GOSUB 2200 &
	\ F%=0% &
	\ IF K$="" THEN &
		  GOTO 1020 &
	  ELSE &
		  IF LEFT(K$,1%)="*" THEN  &
			  F%=-1% &
			\ K$=RIGHT(K$,2%)
3020	  V%=FNG%(2%,K$) &
	\ GOTO 3030 IF V% &
	\ GOSUB 2300 &
	\ IF F%<>-1% THEN  &
		  IF FNG%(4%,W$(1%))=0% OR FNG%(6%,W$(1%))=0% THEN  &
			  PRINT "VENDOR HAS CURRENT TRANSACTIONS!  DELETION DENIED." &
			\ GOTO 3010
3025	  GOSUB 2400 &
	\ INPUT "CONFIRM (Y/N) ";K$ &
	\ V%=FND%(2%,"") IF LEFT(K$,1%)="Y" &
	\ GOTO 3010
3030	  PRINT "VENDOR # NOT FOUND !!!" &
	\ GOTO 3010
4000	  !
4010	  PRINT  &
	\ PRINT "CHANGE - VENDOR # "; &
	\ GOSUB 2200 &
	\ F%=0% &
	\ IF K$="" THEN &
		  GOTO 1020 &
	  ELSE &
		  IF LEFT(K$,1%)="*" THEN  &
			  F%=-1% &
			\ K$=RIGHT(K$,2%)
4020	  N8$=K$ &
	\ V%=FNG%(2%,K$) &
	\ GOTO 4040 IF V% &
	\ GOSUB 2300 &
	\ PRINT S$(1%);CVT$$(W$(1%),128%);" ? "; &
	\ GOSUB 2200 &
	\ GOTO 4050 IF K$="-" &
	\ N1$=K$ &
	\ N9$=K$ &
	\ FOR I%=2% TO 6% &
		\ PRINT S$(I%);CVT$$(W$(I%),128%);" ? "; &
		\ GOSUB 2200 &
		\ GOTO 4050 IF K$="-" &
		\ W$(I%)=K$ IF K$<>"" &
	\ NEXT I% &
	\ PRINT S$(7%);W(1%); &
	\ INPUT K$ &
	\ GOTO 4050 IF K$="-" &
	\ W(1%)=VAL(K$) IF K$<>"" &
	\ IF N1$="" THEN  &
		  W$(1%)=U$(1%)+"" &
		\ GOSUB 2100 &
		\ IF FNU%(2%,T3$) THEN &
			  STOP &
		  ELSE &
			  GOTO 4010
4025	  N1$=N1$+SPACE$(6%-LEN(N1$)) &
	\ V%=FNG%(2%,N1$) &
	\ IF V%=88% THEN  &
		  V%=FNG%(4%,N8$) &
		\ V1%=FNG%(6%,N8$) &
		\ V%,V1%=1% IF F%=-1% &
		\ PRINT "Vendor code has current transactions!  Change denied." IF V%=0% OR V1%=0% &
		\ GOTO 4010 IF V%=0% OR V1%=0% &
		\ STOP IF FNG%(2%,N8$) &
		\ GOTO 4030
4026	  STOP IF V% &
	\ GOSUB 2300 &
	\ PRINT N1$;" IS USED BY ";W$(2%);" CHANGE NOT MADE" &
	\ GOTO 4010
4030	  V%=FND%(2%,"") &
	\ W$(1%)=N9$+"" &
	\ GOSUB 2100 &
	\ V%=FNA%(2%,T3$) &
	\ GOTO 4010
4040	  PRINT "VENDOR NOT FOUND !!!! " &
	\ GOTO 4010
4050	  PRINT "Change VOID -- No changes made !!!" &
	\ GOTO 4010
5000	  !
5010	  PRINT  &
	\ PRINT "EXAMINE FROM - VENDOR # "; &
	\ GOSUB 2200 &
	\ GOTO 1020 IF K$="" &
	\ N9$=K$+"" &
	\ PRINT "          TO - VENDOR # "; &
	\ GOSUB 2200 &
	\ V%=FNG%(2%,"") IF FNG%(2%,N9$) &
	\ GOSUB 2300
5025	  IF U$(1%)<N9$ THEN  &
		  V%=FNN%(2%) &
		\ GOTO 5010 IF V% &
		\ GOSUB 2300 &
		\ GOTO 5025
5030	  IF U$(1%)>K$ THEN &
		  GOTO 5010 &
	  ELSE &
		  GOSUB 2400 &
		\ IF FNN%(2%) THEN  &
			  GOTO 5010
5040	  GOSUB 2300 &
	\ GOTO 5030
6000	  !
6010	  PRINT  &
	\ PRINT "FIND - VENDOR # "; &
	\ GOSUB 2200 &
	\ GOTO 1020 IF K$="" &
	\ IF LEFT(K$,1%)<>"+" THEN  &
		  GOTO 6030 IF LEFT(K$,1%)="-" &
		\ GOTO 6010 IF FNG%(2%,K$) &
		\ GOTO 6040
6020	  V%=FNN%(2%) &
	\ GOTO 6040
6030	  V%=FNN%(-2%)
6040	  GOSUB 2300 &
	\ GOSUB 2400 &
	\ GOTO 6010
7000	  !
7010	  GOTO 1020 IF FNG%(2%,"") &
	\ X1%=66%
7020	  PRINT  &
	\ INPUT "FORM (S/L) ";V1$ &
	\ V1$=LEFT(V1$,1%) &
	\ IF V1$<>"S" AND V1$<>"L" THEN  &
		  PRINT  &
		\ PRINT "TYPE 'S' FOR JUST VENDOR NUMBERS AND NAMES" &
		\ PRINT "TYPE 'L' FOR FULL PRINTOUT" &
		\ GOTO 7020
7040	  INPUT "SET PAGE ";I% &
	\ V%=FNG%(2%,"")
7060	  GOSUB 2300 &
	\ GOSUB 2400 &
	\ IF FNN%(2%) THEN &
		  GOTO 1020 &
	  ELSE &
		  GOTO 7060
8000	  !
8010	  K$=SYS(CHR$(3%))
8020	  PRINT TAB(20%); &
	\ INPUT #11%, K$ &
	\ IF K$<>"" THEN  &
		  GOTO 8020 IF FNG%(2%,K$) &
		\ GOSUB 2300 &
		\ PRINT TAB(20%);W$(I%) FOR I%=2% TO 5% &
		\ PRINT  &
		\ GOTO 8020
8040	  K$=SYS(CHR$(2%)) &
	\ GOTO 1020
8200	  !
8205	  INPUT "START AT VENDOR NUMBER";K$ &
	\ PRINT "CLOSE ENOUGH" IF FNG%(2%,K$)=88% &
	\ GOTO 1020 IF FNS%<>88% AND FNS%<>0% &
	\ PRINT "SET LABELS IN PLACE, PRESS 'P' AND RETURN" &
	\ V$=SYS(CHR$(3%)) &
	\ E$="" &
	\ WHILE E$<>"P" &
		\ PRINT CHR$(13%);"#"; &
		\ INPUT #10%, E$ &
	\ NEXT &
	\ V$=SYS(CHR$(2%))
8220	  GOSUB 2300 &
	\ PRINT  &
	\ PRINT TAB(3%);CVT$$(W$(I%),128%) FOR I%=2% TO 5% &
	\ PRINT  &
	\ GOTO 8220 UNLESS FNN%(2%) &
	\ PRINT ""; &
	\ INPUT #10%, K$ &
	\ GOTO 1020
9000	  V$=SYS(CHR$(7%)) &
	\ Q3$=CVT%$(9010%)+"!VENDOR.BAC" &
	\ Q3$=Q3$+V$ IF ASCII(V$)=255% &
	\ CLOSE 1% &
	\ V%=FNC%(2%)+FNC%(0%) &
	\ CHAIN "!VENDOR" 0%
9010	  V$=SYS(CHR$(7%)) &
	\ V$=RIGHT(V$,INSTR(1%,V$,".")+4%) &
	\ V$=SYS(CHR$(8%)+LEFT(V$,INSTR(1%,V$,CHR$(13%))-1%)) &
	\ GOTO 10
10000	  !
10010	  V%=FNC%(2%) &
	\ CLOSE 10% &
	\ CLOSE 11% &
	\ Q3$=CVT%$(8100%)+"!MENU.BAC"+SYS(CHR$(7%)) IF ASCII(SYS(CHR$(7%)))=255% &
	\ V%=FNC%(0%)
10020	  IF ASCII(SYS(CHR$(7%)))=255% THEN &
		  CHAIN "!MENU" 0. &
	  ELSE &
		  V$=SYS(CHR$(8%)) &
		\ GOTO 32767
32767	  END
