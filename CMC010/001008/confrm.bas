10	  ! &
	  ! Program name: confrm		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 28-Nov-16 at 04:11 PM
15	  DIM M1$(24%), M$(24%), M2$(36%), P$(37%), M(37%), E$(61%), T(37%), H$(14%)
110	  OPEN "KB:" AS FILE 1%, RECORDSIZE 512%+64%+128% &
	\ IF FNO%(2%,"MSTRFL.DAT","/RO","") THEN  &
		  PRINT "MSTRFL.DAT IS NOT READABLE " &
		\ GOTO 10000
200	  !
220	  FIELD #1%, 6% AS M1$(1%),30% AS M1$(2%),30% AS M1$(3%), &
		30% AS M1$(4%),30% AS M1$(5%),11% AS M1$(6%),8% AS M1$(7%), &
		1% AS M1$(8%),2% AS M1$(9%),12% AS M1$(10%),2% AS M1$(11%)
221	  FIELD #1%, 152%+I%*10% AS E$,8% AS M2$(I%),2% AS M1$(I%+11%) &
		FOR I%=1% TO 9% &
	\ FIELD #1%, 252% AS E$,8% AS M1$(21%),8% AS M1$(22%) &
	\ FIELD #1%, 188%+I%*8% AS E$,8% AS M2$(I%) FOR I%=10% TO 36% &
	\ FIELD #1%, 484% AS E$,2% AS M1$(23%) &
	\ FIELD #1%, 512% AS T2$
1000	  !
1010	  OPEN "SS0:UNIQUE.FIL" AS FILE 12% &
	\ DIM #12%, A0$(255%)=64% &
	\ W$=A0$(1%) &
	\ CLOSE 12% &
	\ GOTO 1030
1020	  GOTO 10000
1030	  INPUT "PAYROLL DATE ENDING :";D$ &
	\ INPUT "SET PAGE "X$ &
	\ GOSUB 7000 &
	\ X1%=199% &
	\ GOTO 10000 IF FNG%(2%,"")
1040	  GOSUB 2300 &
	\ GOSUB 1200 &
	\ GOSUB 5000 &
	\ GOTO 1040 IF FNN%(2%)=0% &
	\ GOTO 10000
1200	  RETURN IF X1%<62% &
	\ PRINT STRING$(68%-X1%,10%) &
	\ GOSUB 6000 &
	\ X1%=6% &
	\ RETURN
2300	  !
2310	  LSET T2$=FNL$ &
	\ M$(I%)=M1$(I%)+"" FOR I%=1% TO 23% &
	\ M(I%)=CVT$F(M2$(I%)) FOR I%=1% TO 36% &
	\ RETURN
5000	  !
5010	  RETURN IF LEFT(M$(1%),4%)="ZPAY" &
	\ RETURN IF M$(22%)<>"" &
	\ X1%=X1%+2% &
	\ PRINT  &
	\ PRINT USING "\    \\"+SPACE$(20%)+"\\    \####.## ", M$(1%),M$(2%),M$(10%),M(1%);
5015	  PRINT ">        O                 E               D       -   D      ";" -   D       -   D       -" &
	\ RETURN
6000	  !
6010	  PRINT USING "\"+SPACE$(24%)+"\", W$; &
	\ PRINT "    PAYROLL TRANSMITTAL FOR THE PAY";" PERIOD ENDING  ";D$;TAB(110%);"PAGE ";X%+1% &
	\ X%=X%+1% &
	\ PRINT TAB(100%);"MISCELLANEOUS DEDUCTIONS"
6020	  PRINT "EMP#       NAME               DEPT   RATE  REG HRS  OVT HRS";"           OTR EARN       AMOUNT CODE AMOUNT CODE AMOUNT CODE ";"AMOUNT CODE"
6030	  RETURN
7000	  PRINT  FOR X%=1% TO 3% &
	\ PRINT W$;TAB(25%);"PAYROLL TRANSMITTAL FOR THE"+" PAY PERIOD ENDING ";D$;TAB(110%);"PAGE 1" &
	\ X%=1%
7100	  READ H$(L%) FOR L%=1% TO 12% &
	\ PRINT  FOR X2%=1% TO 10% &
	\ PRINT TAB(25%);"DEDUCTIONS";TAB(45%);"YES/NO";TAB(95%);"TOTALS" &
	\ PRINT 
7200	  FOR L%=1% TO 11% STEP 2% &
		\ PRINT TAB(25%);H$(L%);TAB(45%);STRING$(15%,95%);TAB(70%);H$(L%+1%);TAB(90%);STRING$(15%,95%) &
		\ PRINT  &
		\ PRINT  &
	\ NEXT L%
7300	  PRINT TAB(46%);"MM/DD/YY"
7400	  PRINT  FOR L%=1% TO 5% &
	\ PRINT TAB(60%);STRING$(30%,95%) &
	\ PRINT TAB(65%);"AUTHORIZED SIGNATURE"
7500	  PRINT  FOR L%=1% TO 26% &
	\ RETURN
10000	  !
10005	  V%=FNC%(2%) &
	\ CLOSE 1% &
	\ IF ASCII(SYS(CHR$(7%)))=255% THEN &
		  CHAIN "!MENU" 0. &
	  ELSE &
		  GOTO 32767
29000	  DATA	"INSURANCE","REGULAR HOURS","MISCELLANEOUS ONE", &
		"OVERTIME HOURS","MISCELLANEOUS TWO","VACATION HOURS", &
		"MISCELLANEOUS THREE","","MISCELLANEOUS FOUR","", &
		"DATE OF CHECKS",""

32767	  END
