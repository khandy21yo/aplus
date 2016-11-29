10	  ! &
	  ! Program name: conent		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 28-Nov-16 at 04:11 PM
50	  DIM M1$(23%), M$(24%), M2$(36%), M(37%), E$(59%), T(37%)
110	  OPEN "KB:" AS FILE 1%, RECORDSIZE 512%+64%+128%
120	  INPUT "WHICH YEAR ";E$ &
	\ E$="FL" IF E$="" &
	\ GOTO 2990 IF E$="?" &
	\ E$=RIGHT(E$,LEN(E$)-1%)
130	  V%=FNO%(2%,"MSTR"+E$+".DAT","","") &
	\ IF V%=5% THEN &
		  GOSUB 2400 &
	  ELSE &
		  IF V%<>0% THEN  &
			  PRINT "ERROR ";V%;" IN OPENING MASTER FILE." &
			\ GOTO 10000
140	  OPEN "KB:" FOR INPUT AS FILE 11%
200	  !
220	  FIELD #1%, 6% AS M1$(1%),30% AS M1$(2%),30% AS M1$(3%),30% AS M1$(4%),30% AS M1$(5%),11% AS M1$(6%),8% AS M1$(7%),1% AS M1$(8%),2% AS M1$(9%),12% AS M1$(10%),2% AS M1$(11%)
221	  FIELD #1%, 152%+I%*10% AS E$,8% AS M2$(I%),2% AS M1$(I%+11%) FOR I%=1% TO 9% &
	\ FIELD #1%, 252% AS E$,8% AS M1$(21%),8% AS M1$(22%) &
	\ FIELD #1%, 188%+I%*8% AS E$,8% AS M2$(I%) FOR I%=10% TO 36% &
	\ FIELD #1%, 484% AS E$,2% AS M1$(23%) &
	\ FIELD #1%, 512% AS T2$
310	  READ E$(I%) FOR I%=1% TO 59%
320	  DATA	"EMPLOYEE # ","NAME       ","ADDRESS    ","CITY       ", &
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
		"O. DED TOT ","NET PD TOT ","STATE (##) "

350	  INPUT "# OF QUARTERS TO ENTER (1-4)";N1% &
	\ GOTO 350 IF N1%<1% OR N1%>4%
360	  N1%(1%)=10% &
	\ N1%(2%)=16% &
	\ N1%(3%)=21% &
	\ N1%(4%)=26% &
	\ N2%(1%)=32% &
	\ N2%(2%)=38% &
	\ N2%(3%)=43% &
	\ N2%(4%)=48% &
	\ DIM N1%(4%), N2%(4%)
1000	  GOTO 3000
2000	  !
2010	  PRINT  &
	\ PRINT E$(1%); &
	\ INPUT #11%, A$ &
	\ A$=A$+SPACE$(6%-LEN(A$)) &
	\ RETURN
2100	  !
2110	  LSET M1$(I%)=M$(I%) FOR I%=1% TO 23% &
	\ LSET M2$(I%)=CVTF$(M(I%)) FOR I%=1% TO 36% &
	\ RETURN
2200	  !
2210	  INPUT LINE K$ &
	\ K$=CVT$$(K$,4%) &
	\ RETURN IF K$="" &
	\ K$=K$+SPACE$(30%-LEN(K$)) &
	\ RETURN
2300	  !
2310	  LSET T2$=FNL$ &
	\ M$(I%)=M1$(I%)+"" FOR I%=1% TO 23% &
	\ M(I%)=CVT$F(M2$(I%)) FOR I%=1% TO 36% &
	\ RETURN
2400	  !
2410	  PRINT "A MASTER FILE FOR THAT YEAR DOES NOT EXIST. SHALL I CREATE ONE "; &
	\ INPUT K$ &
	\ GOTO 10000 IF LEFT(K$,1%)<>"Y" &
	\ OPEN "MSTR"+E$+".DAT" FOR OUTPUT AS FILE 2% &
	\ PRINT #2%, CVT%$(0%)+CVT%$(8%)+"S"+CHR$(128%); &
	\ CLOSE 2% &
	\ OPEN "MSTR"+E$+".DA1" FOR OUTPUT AS FILE 2% &
	\ PRINT #2%, CVT%$(0%)+CVT%$(512%)+"S"+CHR$(128%); &
	\ CLOSE 2% &
	\ IF FNO%(2%,"MSTR"+E$+".DAT","","") THEN  &
		  PRINT "ERROR";FNS%;"IN OPENING MSTR"+E$+".DAT" &
		\ GOTO 10000
2440	  STOP IF FNA%(2%,"ZPAYRL") &
	\ PRINT "CREATED." &
	\ PRINT  &
	\ RETURN
2990	  PRINT  &
	\ PRINT "ENTER <RETURN> FOR THE CURRENT YEAR." &
	\ PRINT "ENTER THE YEAR NUMBER, FOR ANY OTHER YEAR." &
	\ PRINT  &
	\ GOTO 120
3000	  !
3010	  S$=SYS(CHR$(2%)) &
	\ INPUT "EMPLOYEE #";S$ &
	\ GOTO 9000 IF S$="" &
	\ S$=S$+SPACE$(6%-LEN(S$)) &
	\ V%=FNG%(2%,S$) &
	\ GOTO 3010 IF V%
3100	  GOSUB 2300
3105	  PRINT M$(1%);"  ";M$(2%)
3110	  ON ERROR GOTO 9000 &
	\ V$=SYS(CHR$(3%))
3160	  FOR I%=1% TO 4% &
		\ FOR J%=1% TO N1% &
			\ PRINT E$(N2%(I%)+J%); &
			\ PRINT USING " ##,###.## ", M(N1%(I%)+J%); &
			\ INPUT N$ &
			\ M(N1%(I%)+J%)=VAL(N$)/100. IF N$<>"" &
			\ PRINT USING " ##,###.##", M(N1%(I%)+J%) &
		\ NEXT J%
3170		  T=0. &
		\ T=T+M(J%) FOR J%=N1%(I%)+1% TO N1%(I%)+4% &
		\ M(N1%(I%))=T &
	\ NEXT I%
3175	  !
3180	  !
3210	  PRINT  &
	\ PRINT  &
	\ PRINT 
3300	  !
3310	  GOSUB 2100 &
	\ STOP IF FNU%(2%,T2$) &
	\ GOTO 3000
9000	  V$=SYS(CHR$(2%)) &
	\ RESUME 10000
10000	  !
10005	  I%=32767%
10010	  V%=FNC%(2%) &
	\ CLOSE 1% &
	\ CHAIN "!MENU" 0. IF ASCII(SYS(CHR$(7%)))=255% &
	\ GOTO 32767
32767	  END
