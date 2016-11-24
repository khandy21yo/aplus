10	  ! &
	  ! Program name: gl		Compiled with SCALE 0 on V08.0 &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
35	  !
36	  Y$=SYS(CHR$(12%)) &
	\ CHANGE Y$ TO Y% &
	\ PRGNUM$="["+NUM1$(Y%(6%))+","+NUM1$(Y%(5%))+"]" &
	\ PRGNAM$=RAD$(Y%(7%)+SWAP%(Y%(8%)))+RAD$(Y%(9%)+SWAP%(Y%(10%)))+"."+RAD$(Y%(11%)+SWAP%(Y%(12%))) &
	\ DEVNAM$=CHR$(Y%(23%))+CHR$(Y%(24%))+NUM1$(Y%(25%))+":"
100	  !
102	  IF FNO%(4%,"CHART.DAT","/RW","") THEN  &
		  PRINT "Error ";FNS%;" has occurred while attempting to open"+" the chart of accounts." &
		\ PRINT "Please check error list.  Aborting. . . "; &
		\ V%=FNX%("",-1%,"")
104	  IF FNG%(4%,"ZZZZZ[1]") THEN  &
		  PRINT "Better run the ZZZ option in CHART first." &
		\ GOTO 10000
106	  FIND%=CVT$%(MID(FNL$,11%,2%)) &
	\ YEAR%=CVT$%(MID(FNL$,21%,2%)) &
	\ YEAR$=RIGHT(NUM1$(100%+YEAR%),2%) &
	\ LAST%=CVT$%(MID(FNL$,9%,2%)) &
	\ IF YEAR%>99% THEN  &
		  INPUT "Year of last month closed ";Y$ &
		\ Y$=RIGHT(Y$,3%) IF LEN(Y$)=4% &
		\ YEAR%=VAL(Y$) &
		\ IF FNU%(4%,LEFT(FNL$,20%)+CVT%$(YEAR%)+RIGHT(FNL$,23%))=0% THEN &
			  GOTO 106 &
		  ELSE &
			  PRINT "Unable to change flag record in the chart file." &
			\ PRINT "Aborting this routine." &
			\ GOTO 10000
108	  GOTO 120 IF FNO%(6%,"TT0:GL.DAS","/SF","") &
	\ GOTO 120 IF FNG%(6%,"") &
	\ FILE$=MID(FNL$,2%,10%) &
	\ SIZE%=CVT$%(MID(FNL$,12%,2%)) &
	\ ON ERROR GOTO 19000 &
	\ OPEN FILE$ FOR INPUT AS FILE 2% &
	\ GET #2% &
	\ FIELD #2%, 2% AS N$ &
	\ N%=CVT$%(N$) &
	\ CLOSE 2% &
	\ GOSUB 21999 &
	\ IF N%=SIZE% THEN  &
		  IF FIND% THEN &
			  CHAIN DEVNAM$+PRGNUM$+"GLPRNT.BAC" 0% &
		  ELSE &
			  PRINT "There are undefined account numbers in the G/L index file." &
			\ PRINT "Do you want to re-create the index file <Y> "; &
			\ INPUT LINE K$ &
			\ IF CVT$$(LEFT(K$,1%),-1%)="N" THEN  &
				  CHAIN DEVNAM$+PRGNUM$+"GLPRNT.BAC" 0%
120	  !
122	  V%=FNC%(6%) &
	\ ON ERROR GOTO 130 &
	\ K$=NUM1$(LAST%) &
	\ K$="0"+K$ IF LEN(K$)=1% &
	\ OPEN "CKJ"+K$+".DAT" FOR INPUT AS FILE 2% &
	\ FIELD #2%, 2% AS N$ &
	\ GET #2% &
	\ SIZE%=CVT$%(N$) &
	\ CLOSE 2% &
	\ FILE$="CKJ"+K$+".DA1" &
	\ RESUME 200
130	  RESUME 131
131	  ON ERROR GOTO 132 &
	\ K$=NUM1$(LAST%) &
	\ IF LEN(K$)>1% THEN &
		  GOTO 135 &
	  ELSE &
		  OPEN "CKJ"+K$+".DAT" FOR INPUT AS FILE 2% &
		\ FIELD #2%, 2% AS N$ &
		\ GET #2% &
		\ SIZE%=CVT$%(N$) &
		\ CLOSE 2% &
		\ FILE$="CKJ"+K$+".DA1" &
		\ RESUME 200
132	  RESUME 135
135	  ON ERROR GOTO 140 &
	\ NEX%=LAST%+1% &
	\ NEX%=1% IF NEX%>12% &
	\ FILE$="CK"+FNM1$(NEX%)+".DA1" &
	\ OPEN FILE$ FOR INPUT AS FILE 2% &
	\ FIELD #2%, 2% AS N$ &
	\ GET #2% &
	\ SIZE%=CVT$%(N$) &
	\ CLOSE 2% &
	\ RESUME 137
137	  ON ERROR GOTO 0 &
	\ GOTO 200
140	  RESUME 142
142	  PRINT FILE$;" not found - ledger cannot be run." &
	\ GOTO 10000
200	  !
204	  REC.SIZE%=6000% &
	\ OPEN "NL:" AS FILE 1%, RECORDSIZE REC.SIZE% &
	\ IF FNO%(2%,FILE$,"/SF/NS","") THEN  &
		  PRINT "Error";FNS%;"has occurred at line 204 while opening ";FILE$;"." &
		\ PRINT "Aborting this routine." &
		\ GOTO 10000
206	  IF FNG%(2%,"") THEN  &
		  PRINT FILE$;" is empty.  Aborting this"+" routine." &
		\ GOTO 10000
207	  PRINT "Creating a ledger file for ";FILE$;"." &
	\ S$="/CR:16/SF/EX:"+NUM1$(FNT) &
	\ IF FNO%(6%,"TT0:GL.DAS",S$,"") THEN  &
		  PRINT "Error in opening the temporary file." &
		\ PRINT "Aborting this routine." &
		\ GOTO 10000
208	  IF FNA%(6%,CHR$(0%)+SPACE$(10%-LEN(FILE$))+FILE$+CVT%$(SIZE%)) THEN  &
		  PRINT "Unable to add flag record to temporary file." &
		\ PRINT "Aborting this routine." &
		\ GOTO 10000
209	  T=0. &
	\ A%,T%=0% &
	\ FIELD #1%, 1% AS G$ &
	\ LSET G$="!" &
	\ V%=FNG%(2%,"") &
	\ CPU=TIME(1%) &
	\ ELAPSE=TIME(0%) &
	\ PRINT "This is going to take a little while so please be"+" patient." &
	\ FIND%=-1% &
	\ PRINT  &
	\ PRINT "Bad general ledger numbers: " &
	\ PRINT  &
	\ PRINT " Check#   Acct #  Description                 "+"   Date          Amount  Record" &
	\ PRINT 
210	  FIELD #2%, FNL% AS D$,6% AS R$,8% AS R1$,2% AS R2$,8% AS R3$,2% AS R4$,2% AS R5$,2% AS R6$,28% AS R7$ &
	\ IF R$="DDDDDD" THEN &
		  GOTO 230 &
	  ELSE &
		  T%=T%+1% &
		\ RSET R1$=CVT$$(R1$,-1%) &
		\ IF FNA%(6%,R1$+R4$+R5$+CVT%$(FNR(2%))) THEN  &
			  PRINT "Unable to add to temporary file." &
			\ PRINT "Aborting this routine." &
			\ GOTO 10000
220	  A=CVT$F(R3$) &
	\ T=T+A &
	\ IF INSTR(1%,G$,"!"+R1$+"!")=0% THEN  &
		  SEARCH.LEN%=LEN(G$) &
		\ SEARCH.LEN%=SEARCH.LEN%-9% IF SEARCH.LEN%>REC.SIZE%-9% &
		\ FIELD #1%, SEARCH.LEN% AS E$,9% AS E$ &
		\ LSET E$=R1$+"!" &
		\ IF FNG%(4%,R1$)=0% THEN &
			  FIELD #1%, SEARCH.LEN%+9% AS G$ &
		  ELSE &
			  PRINT USING " \    \  \      \ \"+SPACE$(26%)+"\ ##/##/##"+" ##,###,###.## (#####)", R$,R1$,R7$,CVT$%(R4$),CVT$%(R5$),CVT$%(R6$),A,FNR(2%) &
			\ FIND%=0%
230	  IF FNN%(2%)=0% THEN &
		  GOTO 210 &
	  ELSE &
		  IF FNG%(4%,"ZZZZZ[1]") THEN  &
			  PRINT "Unable to find the"+" flag record in chart." &
			\ PRINT "Aborting this routine." &
			\ GOTO 10000
240	  IF FNU%(4%,LEFT(FNL$,10%)+CVT%$(FIND%)+RIGHT(FNL$,13%)) THEN  &
		  PRINT "Error";FNS%;" has occurred at line 240." &
		\ PRINT "Aborting this routine." &
		\ GOTO 10000
250	  A%=(LEN(G$)-1%)/(LEN(R1$)+1%) &
	\ A%=-1% IF LEN(G$)>REC.SIZE%-9% &
	\ PRINT  &
	\ PRINT USING SPACE$(31%)+"TOTAL (all transactions) ##,###,###.##", INT(T*100.+6.8213185310482798e-15)/100. &
	\ PRINT DATE$(0%) &
	\ PRINT "Total elapse time in seconds ";TIME(0%)-ELAPSE &
	\ PRINT "CPU time in seconds ";(TIME(1%)-CPU)/10. &
	\ PRINT "Active accounts";A% IF A%<>-1% &
	\ PRINT "Active accounts  *cannot be determined" IF A%=-1% &
	\ PRINT "Total transactions";T% &
	\ PRINT  &
	\ IF FIND% THEN  &
		  PRINT "All accounts confirmed."
260	  V%=FNX%(PRGNUM$+"GLPRNT.BAC",0%,"")
10000	  !
10010	  ON ERROR GOTO 10020 &
	\ KILL "TT0:GL.DAS"
10020	  RESUME 10030
10030	  CLOSE 1% &
	\ V%=FNX%("",0%,"")
14010	  DEF FNM1$(ARG%) &
	\ FNM1$=MID("JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC",ARG%*3%-2%,3%) &
	\ FNEND
15900	  DIM Y%(32%), Y1%(32%), Y$(32%)
19000	  !
19040	  IF ERL=108% THEN  &
		  PRINT "Unable to find the file named ";FILE$ &
		\ PRINT "That must be retored before this routine can ";"continue. . . ";FNX%("",0%,"")
19999	  ON ERROR GOTO 0
32767	  END
