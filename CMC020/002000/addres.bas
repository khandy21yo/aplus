10	  ! &
	  ! Program name: addres		Compiled with SCALE 0 on V08.0 &
	  ! Decompiled on 28-Nov-16 at 04:11 PM
20	  OPEN "KB:" AS FILE 1%, RECORDSIZE 128% &
	\ FIELD #1%, 6% AS U$(1%),30% AS U$(2%),30% AS U$(3%),30% AS U$(4%),29% AS U$(5%),1% AS U$(7%),2% AS U$(6%) &
	\ FIELD #1%, 128% AS T3$
22	  CSI$=CHR$(155%)+"[" &
	\ PASS.THRU.ON$=CSI$+"5i" &
	\ PASS.THRU.OFF$=CSI$+"4i"
25	  GOTO 30
27	  PRINT "TO SET UP A FILE, CHOOSE ANY CODE UP TO FOUR CHARACTERS LONG." &
	\ PRINT "ONCE ESTABLISHED, THE FILE MAY BE RECALLED AT ANY TIME BY" &
	\ PRINT "ENTERING THE CODE.  THE STANDARD CODE IS 'DRES' AND IS CALLED" &
	\ PRINT "BY ENTERING A CARRIAGE RETURN (<CR>)."
30	  INPUT "Name of file, <CR> for standard (? for help)";F$ &
	\ GOTO 27 IF F$="?" &
	\ F$="DRES" IF F$="" &
	\ F$="AD"+F$+".DAT" &
	\ GOTO 70 UNLESS FNO%(2%,F$,"","") &
	\ IF FNS%<>5% THEN  &
		  PRINT "ERROR";FNS%;"IN OPENING FILE.  ABORTED." &
		\ GOTO 10000
40	  INPUT "THAT FILE DOES NOT EXIST.  SHOULD I CREATE IT (Y/N)";K$ &
	\ GOTO 10000 IF K$<>"Y" &
	\ IF FNO%(2%,F$,"/CR:8,128","") THEN  &
		  PRINT "ERROR";FNS%;"IN CREATING FILE.  ABORTED." &
		\ GOTO 10000
70	  S$(1%)="CODE " &
	\ S$(2%)="NAME     " &
	\ S$(3%)="ADDRESS  " &
	\ S$(4%)="CITY, ST " &
	\ S$(5%)="ZIP CODE " &
	\ S$(6%)="TYPE " &
	\ S$(7%)="SALUTATION CODE "
130	  ON ERROR GOTO 150
140	  B%=0% &
	\ OPEN "SS0:UNIQUE.FIL" FOR INPUT AS FILE 12% &
	\ DIM #12%, A0$(255%)=64% &
	\ ON ERROR GOTO 0 &
	\ A$=CVT$$(RIGHT(A0$(1%),2%),128%) &
	\ A1$=CVT$$(RIGHT(A0$(4%),2%),128%) &
	\ GOTO 160
150	  IF ERR=5% THEN  &
		  B%=-1% &
		\ PRINT "PLEASE ENTER COMPANY NAME"; &
		\ INPUT LINE A$ &
		\ A$=CVT$$(A$,132%) &
		\ RESUME 160
160	  CLOSE 12% &
	\ ON ERROR GOTO 0 &
	\ OPEN "KB:" FOR INPUT AS FILE 12%
1000	  !
1020	  V1$="N" &
	\ Y%=0% &
	\ PRINT  &
	\ INPUT "SELECTION ";K$ &
	\ K$=CVT$$(LEFT(K$,3%),32%) &
	\ GOTO 1030 IF K$="" &
	\ GOTO 2000 IF K$="ENT" &
	\ GOTO 3000 IF K$="DEL" &
	\ GOTO 4000 IF K$="CHA" &
	\ GOTO 5000 IF K$="EXA" &
	\ GOTO 6000 IF K$="FIN" &
	\ GOTO 7000 IF K$="PRI" &
	\ GOTO 8000 IF K$="LAB" &
	\ GOTO 10000 IF K$="END" &
	\ GOTO 1020
1030	  PRINT  &
	\ PRINT "OPTIONS:    ENTER    NEW NAMES" &
	\ PRINT "            DELETE   EXISTING NAMES" &
	\ PRINT "            CHANGE   NAME INFORMATION" &
	\ PRINT "            EXAMINE  NAMES" &
	\ PRINT "            FIND     A SINGLE NAME" &
	\ PRINT "            PRINT    ALL NAMES" &
	\ PRINT "            LABELS   PRINTED" &
	\ PRINT "            END      PROGRAM AND UPDATE FILES" &
	\ GOTO 1020
2000	  !
2010	  PRINT  &
	\ PRINT "NEW CODE "; &
	\ K$=FNI$(K$) &
	\ GOTO 10020 IF K$="" &
	\ W$(1%)=K$+"" &
	\ UNLESS FNG%(2%,W$(1%)) THEN  &
		  PRINT "THAT # IS PRESENTLY IN USE." &
		\ GOTO 2010
2021	  FOR I%=2% TO 6% &
		\ PRINT S$(I%);TAB(20%); &
		\ K$=FNI$(K$) &
		\ W$(I%)=K$+"" &
		\ IF I%=4% AND W$(4%)="" THEN  &
			  W$(4%)=A1$+"" IF B%=0% &
			\ W$(4%)="IDAHO FALLS, IDAHO 83401 IF B% &
			\ W$(5%)="" &
			\ I%=I%+1%
2023			  NEXT I% &
	\ PRINT S$(7%);" (0-NONE, 1-MR., 2-MISS, 3-MRS., 4-MS.)"; &
	\ W$(7%)=FNI$(K$)
2040	  GOSUB 2100 &
	\ V%=FNA%(2%,T3$) &
	\ GOTO 2010
2100	  !
2110	  LSET U$(I%)=W$(I%) FOR I%=1% TO 7% &
	\ RETURN
2200	  !
2210	  INPUT LINE K$ &
	\ K$=CVT$$(K$,132%) &
	\ RETURN
2300	  !
2310	  LSET T3$=FNL$ &
	\ W$(I%)=CVT$$(U$(I%),128%) FOR I%=1% TO 7% &
	\ RETURN
2400	  !
2410	  PRINT W$(1%);TAB(9%);W$(2%) &
	\ X%=X%+1% &
	\ RETURN IF V1$<>"N" &
	\ FOR J%=3% TO 5% &
		\ IF W$(J%)<>"" THEN  &
			  PRINT TAB(9%);CVT$$(W$(J%),128%) &
			\ X%=X%+1%
2420			  NEXT J% &
	\ PRINT TAB(9%);S$(6%);"= ";W$(6%) IF W$(6%)<>"" &
	\ PRINT TAB(9%);S$(7%);"= ";MID("     MR.  MISS MRS. MS.  ",VAL(W$(7%))*5%+1%,4%) IF W$(7%)<>"0" &
	\ PRINT  &
	\ X%=X%+1%-(W$(6%)<>"")-(W$(7%)<>"0") &
	\ RETURN
3000	  !
3010	  PRINT  &
	\ PRINT "DELETE - CODE "; &
	\ K$=FNI$(K$) &
	\ GOTO 1020 IF K$=""
3020	  V%=FNG%(2%,K$) &
	\ GOTO 3030 IF V% &
	\ GOSUB 2300 &
	\ GOSUB 2400 &
	\ INPUT "CONFIRM (Y/N) ";K$ &
	\ V%=FND%(2%,"") IF LEFT(K$,1%)="Y" &
	\ GOTO 3010
3030	  PRINT "CODE NOT FOUND!" &
	\ GOTO 3010
4000	  !
4010	  PRINT  &
	\ PRINT "CHANGE - CODE "; &
	\ K$=FNI$(K$) &
	\ N8$=K$+"" &
	\ GOTO 10020 IF K$=""
4020	  V%=FNG%(2%,K$) &
	\ GOTO 4040 IF V% &
	\ GOSUB 2300 &
	\ PRINT S$(1%);W$(1%); &
	\ K$=FNI$(K$) &
	\ N1$=K$+"" &
	\ N9$=K$+"" &
	\ FOR I%=2% TO 6% &
		\ PRINT S$(I%);W$(I%); &
		\ K$=FNI$(K$) &
		\ W$(I%)=K$+"" IF K$<>"" &
	\ NEXT I% &
	\ PRINT S$(7%);" (0-NONE, 1-MR., 2-MISS, 3-MRS., 4-MS.)  ";W$(7%); &
	\ K$=FNI$(K$) &
	\ W$(7%)=K$+"" IF K$<>"" &
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
		  STOP IF FNG%(2%,N8$) &
		\ GOTO 4030
4026	  STOP IF V% &
	\ GOSUB 2300 &
	\ PRINT N1$;" IS USED BY ";W$(2%);" CHANGE NOT MADE " &
	\ GOTO 4010
4030	  V%=FND%(2%,"") &
	\ W$(1%)=N9$+"" &
	\ GOSUB 2100 &
	\ V%=FNA%(2%,T3$) &
	\ GOTO 4010
4040	  PRINT "NAME NOT FOUND!" &
	\ GOTO 4010
5000	  !
5010	  PRINT  &
	\ PRINT "EXAMINE FROM - CODE "; &
	\ K$=FNI$(K$) &
	\ GOTO 1020 IF K$=""
5020	  N9$=K$+"" &
	\ PRINT "          TO - CODE "; &
	\ K$=FNI$(K$) &
	\ V%=FNG%(2%,N9$) &
	\ GOSUB 2300
5022	  PRINT "EXAMINE TYPES (RETURN FOR ALL)"; &
	\ INPUT LINE T$ &
	\ T$=CVT$$(T$,4%) &
	\ T$=","+T$+"," IF T$<>""
5030	  IF W$(1%)>K$ THEN &
		  GOTO 5010 &
	  ELSE &
		  GOSUB 2400 IF T$="" OR INSTR(1%,T$,","+CVT$$(W$(6%),2%)+",") &
		\ IF FNN%(2%) THEN  &
			  GOTO 5010
5040	  GOSUB 2300 &
	\ GOTO 5030
6000	  !
6010	  PRINT  &
	\ PRINT "FIND - CODE "; &
	\ K$=FNI$(K$) &
	\ GOTO 1020 IF K$="" &
	\ GOTO 6020 IF LEFT(K$,1%)="+" &
	\ GOTO 6030 IF LEFT(K$,1%)="-" &
	\ GOTO 6010 IF FNG%(2%,K$) &
	\ GOTO 6040
6020	  V%=FNN%(2%) &
	\ GOTO 6040
6030	  V%=FNN%(-2%)
6040	  GOSUB 2300 &
	\ GOSUB 2400 &
	\ GOTO 6010
7000	  !
7010	  X1%=66% &
	\ INPUT "TOTALS ONLY (Y/N)";N1$ &
	\ N1$="N" UNLESS N1$="Y" &
	\ V1$="N" &
	\ IF N1$="N" THEN  &
		  INPUT "SHORT FORM (Y/N) ";V1$ &
		\ V1$="N" UNLESS V1$="Y"
7015	  INPUT "TYPE ('0' FOR ALL)";T$ &
	\ IF FNG%(2%,"") THEN  &
		  PRINT "ERROR";FNS%;"IN GETTING FIRST RECORD.  ABORTED." &
		\ GOTO 10000
7017	  IF N1$="N" THEN  &
		  INPUT "SET PAGE ";I% &
		\ PRINT PASS.THRU.ON$; &
		\ GOSUB 7100
7020	  GOSUB 2300 &
	\ IF X%>60% THEN  &
		  PRINT STRING$(66%-X%,10%); &
		\ GOSUB 7100
7030	  GOSUB 2400 IF (T$=CVT$$(W$(6%),2%) OR T$="0") AND N1$="N" &
	\ Y%=Y%+1% IF T$=CVT$$(W$(6%),2%) OR T$="0" &
	\ GOTO 7020 UNLESS FNN%(2%) &
	\ PRINT  &
	\ PRINT "TOTAL=";Y% &
	\ PRINT PASS.THRU.OFF$; &
	\ GOTO 1020
7100	  !
7110	  PRINT STRING$(5%,10%); &
	\ PRINT TAB(9%);A$ &
	\ PRINT TAB(9%);"FOR FILE ";MID(F$,3%,INSTR(1%,F$,".")-3%) &
	\ PRINT TAB(9%);DATE$(0%) &
	\ PRINT  &
	\ PRINT  &
	\ X%=10% &
	\ RETURN
8000	  !
8010	  PRINT "TYPE '1' TO PRINT REGULAR LABELS" &
	\ PRINT "     '2' TO PRINT RODEX CARDS" &
	\ PRINT "     '3' TO PRINT LABLES WITH 9 LINES" &
	\ INPUT "PLEASE SELECT (1,2, OR 3) ";T9$ &
	\ T9$=LEFT(T9$,1%)
8020	  V1$="N" &
	\ INPUT "TYPE ('0' FOR ALL)";T$ &
	\ V$=SYS(CHR$(3%)) &
	\ PRINT "SET PAGE, AND PRESS 'P' TO START" &
	\ PRINT PASS.THRU.ON$; &
	\ K$="" &
	\ UNTIL K$="P" &
		\ PRINT CHR$(13%);"#"; &
		\ INPUT #12%, K$ &
	\ NEXT &
	\ PRINT  &
	\ V%=FNG%(2%,"")
8023	  GOSUB 2300 &
	\ GOSUB 8100 IF T$=CVT$$(W$(6%),2%) OR T$="0" &
	\ Y%=Y%+1% IF T$=CVT$$(W$(6%),2%) OR T$="0" &
	\ GOTO 8023 UNLESS FNN%(2%) &
	\ PRINT ""; &
	\ INPUT #12%, K$ &
	\ V$=SYS(CHR$(2%)) &
	\ PRINT PASS.THRU.OFF$; &
	\ PRINT "TOTAL=";Y% &
	\ GOTO 1020
8100	  PRINT STRING$(7%,10%); IF T9$="2" &
	\ PRINT STRING$(3%,10%); IF T9$="3"
8110	  FOR J%=2% TO 5% &
		\ IF W$(J%)<>"" THEN  &
			  PRINT CVT$$(W$(J%),128%) &
			\ X%=X%+1%
8120			  NEXT J% &
	\ PRINT STRING$(6%-X%,10%); &
	\ X%=0% &
	\ RETURN
10000	  !
10005	  V$="" &
	\ I%=0%
10010	  CLOSE 1% &
	\ CLOSE 12% &
	\ V%=FNX%(V$,I%,"")
10020	  V$="[1,3]ADDRES.BAC" &
	\ I%=10% &
	\ GOTO 10010
14000	  DEF FNI$(K$) &
	\ INPUT LINE K$ &
	\ FNI$=CVT$$(K$,132%) &
	\ FNEND
19000	  !
32767	  END
