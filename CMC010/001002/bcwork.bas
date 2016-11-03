
10	  ! &
	  !  BCWORK--CMC BILLING SYSTEM &
	  ! &
	!	12/12/91 - Kevin Handy &
	!		Modified to put "BAD" files in not accessed &
	!		category. (Bad disk blocks) &

35	  EXTEND &

50	  DIM B1$(19%), B$(5%), B(13%), T(13%), B%(12%), B1%(12%), A%(30%), &
		  A1%(30%), X$(11%), PTR$(60%,4%), PTR%(4%) &

110	  OPEN "KB:" AS FILE 1%, RECORDSIZE 256% &

120	  IF FNO%(2%,"[30,1]BCACC.DAT" ,"" ,"" ) &
	  THEN	  PRINT "FILE BCACC NOT FOUND!!!" &
		\ GOTO 10000 &

130	  OPEN "MONEY.ACT" FOR INPUT AS FILE 4%, MODE 1% &
	\ OPEN "MONEY.MST" FOR INPUT AS FILE 5%, MODE 1% &
	\ GET #4%, RECORD 1% &
	\ B%(4%)=1% &
	\ GET #5%, RECORD 1% &
	\ B%(5%)=1% &
	\ FIELD #4%, 2% AS E$ &
	\ B1%(4%)=CVT$%(E$) &
	\ FIELD #5%, 2% AS E$ &
	\ B1%(5%)=CVT$%(E$) &

210	  FIELD #1%, 6% AS B1$(1%),2% AS B1$(2%),2% AS B1$(3%),40% AS B1$(4%), &
		  1% AS B1$(5%) &
	\ FIELD #1%, 51%+I%*8% AS E$,8% AS B1$(I%+6%) FOR I%=0% TO 13% &
	\ FIELD #1%, 256% AS T2$ &

1000	  ! &
	  !  CONTROL &
	  ! &

1020	  INPUT "Month to bill for (JAN,FEB,ETC.)";MONTH$ &
	\ MONTH$=CVT$$(LEFT(MONTH$,3%),-1%) &
	\ MONTH%=INSTR(1%,"!!!JAN!FEB!MAR!APR!MAY!JUN!JUL!AUG!SEP!OCT"+ &
		  "!NOV!DEC" ,MONTH$)/4% &
	\ UNLESS MONTH% &
	  THEN	  PRINT "Illegal month name !!"+STRING$(3%,7%) &
		\ GOTO 1020 &

1030	  INPUT "YEAR TO BILL FOR (YY) ",YEAR% &
	\ FIRST.DATE$=FND7$(NUM1$(MONTH%)+".01."+NUM1$(YEAR%)) &
	\ FIRST%=FNDATE%(FIRST.DATE$) &

1040	  INPUT "Set page (12 char/inch)";K$ &
	\ X1%=66% &
	\ GOTO 3000 &

2000	  ! &
	  !  SEPARATE BCACC.DAT &
	  ! &

2010	  LSET T2$=FNL$ &
	\ B$(I%)=B1$(I%)+"" FOR I%=1% TO 5% &
	\ B(I%)=CVT$F(B1$(I%+6%)) FOR I%=0% TO 13% &
	\ RETURN &

2050	  ! &
	  !  PREPARE BCACC.DAT &
	  ! &

2060	  LSET B1$(I%)=B$(I%) FOR I%=1% TO 5% &
	\ LSET B1$(I%+6%)=CVTF$(B(I%)) FOR I%=0% TO 13% &
	\ RETURN &

2500	  ! &
	  !  HEADER &
	  ! &

2510	  PRINT FOR X1%=X1% TO 65% &
	\ PRINT CHR$(12%) &
	\ PRINT &
	\ PRINT &
	\ PRINT "ACCOUNT ";FNA1$(B$(3%));" ";B$(4%) &
	\ PRINT &
	\ PRINT "BASIC             COMPILED          DATA              TEMP"+ &
		  "              NOT ACCESSED    * LAST" &
	\ PRINT &
	\ FOR IX% = 1% TO 60% &
		\ TEXT$ = "" &
		\ FOR IY% = 0% TO 4% &
			\ IF PTR%(IY%) >= IX% &
			  THEN	  TEXT$ = TEXT$ + PTR$(IX%,IY%) + "   " &
			  ELSE	  TEXT$ = TEXT$ + SPACE$(18%) &

2520		  NEXT IY% &
		\ PRINT CVT$$(TEXT$, 128%) &
	\ NEXT IX% &
	\ PTR%(IY%) = 0% FOR IY% = 0% TO 4% &
	\ RETURN &

3000	  ! &
	  !  BODY--FIND EACH "A" RECORD AND PRINT ITS FILE STORAGE INFO. &
	  ! &

3010	  GOTO 1000 IF FNG%(2%,"" ) &

3020	  GOSUB 2000 &
	\ IF B$(5%)="A" &
	  THEN	  GOSUB 4000 &

3030	  GOTO 3020 UNLESS FNN%(2%) &
	\ GOTO 10000 &

4000	  ! &
	  !  NEW ACCOUNT # &
	  ! &

4010	  GOSUB 5000 &
	\ A$=FNA1$(B$(3%))+"*.*" &
	\ GOSUB 6000 &
	\ GOSUB 7000 &
	\ RETURN &

5000	  ! &
	  !  GET ACCT # FROM MONEY.ACT (#4) & DIRECTORY FROM MONEY.MST (#5) &
	  ! &

5010	  T(10%),T(11%)=0. &
	\ FOR I%=1% TO B1%(4%) &
		\ FIELD #4%, FNC1%(4%,I%,4%) AS E$,2% AS E$,2% AS E1$ &
		\ GOTO 5020 IF E$=B$(3%) &
	\ NEXT I% &
	\ PRINT "ACCOUNT ";FNA1$(B$(3%));" NOT FOUND !!!" &
	\ B$(4%) = "NOT FOUND" &
	\ X1%=X1%+1% &
	\ RETURN &

5020	  FIELD #5%, FNC1%(5%,CVT$%(E1$),128%) AS E$,1% AS X$(1%),2% AS X$(2%), &
		  2% AS X$(3%),8% AS X$(4%),8% AS X$(5%),2% AS X$(6%),2% AS X$(7%), &
		  4% AS X$(8%),2% AS X$(9%),2% AS X$(10%),1% AS X$(11%) &

5030	  T(10%)=FIX(CVT$%(X$(6%))/60.+.5) &
	\ T(11%)=CVT$F(X$(5%)) &
	\ RETURN &

6000	  ! &
	  !  SCAN MONEY.MST AND SPREAD ITEMS INTO PRINT COLUMNS &
	  ! &

6010	  A%=0% &
	\ CHANGE SYS(CHR$(6%)+CHR$(-23%)+A$) TO A% &
	\ ON ERROR GOTO 19000 &
	\ T(I%)=0. FOR I%=1% TO 5% &

6020	  A%(1%)=6% &
	\ A%(2%)=17% &
	\ A%(3%)=A% &
	\ A%(4%)=SWAP%(A%) &

6030	  CHANGE A% TO A1$ &
	\ A1$=SYS(A1$) &
	\ CHANGE A1$ TO A1% &
	\ LAST%=SWAP%(CVT$%(RIGHT(A1$,17%))) &
	\ P$=RAD$(A1%(7%)+SWAP%(A1%(8%)))+RAD$(A1%(9%)+SWAP%(A1%(10%)))+"."+ &
		  RAD$(A1%(11%)+SWAP%(A1%(12%))) &
	\ S%=A1%(13%)+SWAP%(A1%(14%)) &

6040	  S1%=0% &
	\ E$=RIGHT(P$,8%) &
	\ S1%=1% IF E$="BAS" OR E$="B2S" &
	\ S1%=2% IF E$="BAC" OR E$="SAV" OR E$="TSK" &
	\ S1%=4% IF LEFT(E$,1%)="T" AND S1%=0% &
	\ S1%=3% IF (RIGHT(E$,3%)="T" OR RIGHT(E$,3%)="1" ) AND S1%=0% &
	\ S1%=3% IF (RIGHT(E$,3%)="S") AND S1%=0% &
	\ S1%=4% IF S1%=0% &
	\ S1%=5% IF ((LAST%<FIRST%) AND ((S1% = 3%) OR (S1% = 4%))) OR E$ = "BAD" &

6045 !	  E%=(S1%-1%)*18% &
 !	\ IF POS(0%)>E% &
 !	  THEN	  PRINT &
 !		\ X1%=X1%+1% &
 !		\ GOSUB 2500 IF X1%>60% &
	  GOSUB 2500 IF PTR%(S1%-1%) >= 55% &
	\ PTR%(S1%-1%)=PTR%(S1%-1%)+1% &
	\ SS$ = NUM1$(S%) &
	\ SS$ = SPACE$(5%-LEN(SS$))+SS$ &
	\ IF S1% = 5% &
	  THEN	PTR$(PTR%(S1%-1%),S1%-1%) = P$ + SS$ + " " + DATE$(LAST%) &
	  ELSE	PTR$(PTR%(S1%-1%),S1%-1%) = P$ + SS$ &

6046 !	  PRINT TAB(E%); &
 !	\ PRINT USING "\        \ #### " , P$,S%; IF S1%<>5% &
 !	\ PRINT USING "\        \ #### * \        \" , P$,S%,DATE$(LAST%); &
 !		  IF S1%=5% &

6050	  T(S1%)=T(S1%)+S% &

6060	  A%=A%+1% &
	\ GOTO 6020 &

6090	  ON ERROR GOTO 0 &
	\ GOSUB 2500 &
	\ PRINT &
	\ PRINT USING "###############   " , T(I%); FOR I%=1% TO 5% &
	\ PRINT &
	\ X1%=X1%+2% &
	\ RETURN &

7000	  ! &
	  !  UPDATE BCACC.DAT RECORD &
	  ! &

7010	  B(2%)=T(10%) &
	\ B(4%)=T(11%) &
	\ B(I%*2%+4%)=T(I%) FOR I%=1% TO 4% &
	\ GOSUB 2050 &
	\ STOP IF FNU%(2%,T2$) &
	\ RETURN &

9000	  ! &
	  !  SET UP ACCOUNT # STRING &
	  ! &

9010	  DEF FNA1$(K$) &
		\ E$="("+NUM1$(ASCII(K$))+","+NUM1$(ASCII(RIGHT(K$,2%)))+")" &
		\ E$=SPACE$(5%-INSTR(1%,E$,"," ))+E$ &
		\ E$=LEFT(E$+SPACE$(9%),9%) &
		\ E$=SPACE$(9%) IF CVT$%(K$)=0% &

9015		  FNA1$=E$ &
	\ FNEND &

9020	  ! &
	  !  UNUSED FUNCTION? &
	  ! &

9030	  DEF FNA2$(K$) &
		\ E$=CVT$$(K$,-1%) &
		\ E$=RIGHT(E$,2%) IF LEFT(E$,1%)="(" &
		\ E$=LEFT(E$,LEN(E$)-1%) IF RIGHT(E$,LEN(E$))=")" &
		\ I%=INSTR(1%,E$+"," ,"," ) &
		\ E$=CHR$(VAL(LEFT("0"+E$,I%)))+CHR$(VAL("0"+RIGHT(E$,I%+1%))) &

9035		  FNA2$=E$ &
	\ FNEND &

9040	  ! &
	  !  BREAK OUT RECORD &
	  ! &

9045	  DEF FNC1%(C%,I%,R%) &
		\ E%=I%/(512%/R%)+1% &
		\ GET #C%, RECORD E% IF E%<>B%(C%) &
		\ B%(C%)=E% &

9047		  FNC1%=(I%-(E%-1%)*(512%/R%))*R% &
	\ FNEND &

10000	  ! &
	  !  END &
	  ! &

10010	  CLOSE #1% &
	\ I%=FNC%(2%)+FNX%("" ,0%,"" ) &

14000	  !	********************************************************* &
	  !		S T A N D A R D   F U N C T I O N S &
	  !	********************************************************* &
	&

14015	  DEF FNDATE%(Y0$) = VAL(MID(Y0$,4%,2%))+(VAL(LEFT(Y0$,2%))-1%)*31%+(- &
		  INT(VAL(LEFT(Y0$,2%))*.4+2.3)+ABS(VAL(RIGHT(Y0$, &
		  7%))/4.=INT(VAL(RIGHT(Y0$,7%))/4%)))*-(VAL(LEFT(Y0$,2%))>2%)+ &
		  (VAL(RIGHT(Y0$,7%))-70%)*1000% &

14045	  DEF FNN4$(Y)=CVT%$(INT(Y/65536.))+ CVT%$(Y-INT(Y/65536.)*65536.- &
		  32768.) &

14055	  DEF FNN4(Y0$)=CVT$%(LEFT(Y0$,2%))*65536. + CVT$%(RIGHT(Y0$,3%)) + &
		  32768. &

14200	  !	********************************************************* &
	  !		Format date entered to MM/DD/YY format &
	  !	********************************************************* &

14210	  DEF FND7$(Y0$) &
		\ Y1%=0% &
		\ Y2$="" &
		\ FOR Y3%=1% TO LEN(Y0$)+1% &
			\ Y4%=ASCII(MID(Y0$,Y3%,1%)) &
			\ IF Y4%<48% OR Y4%>57% &
			  THEN	  GOTO 14230 &
			  ELSE	  GOTO 14230 IF Y1%>1% &
				\ Y1%=Y1%+1% &

14220		  NEXT Y3% &
		\ Y0$=LEFT(Y2$,8%) &
		\ Y0$=Y0$+RIGHT(DATE$(0%),8%) IF LEN(Y0$)<7% &
		\ Y0$="-" IF LEN(Y0$)<8% &
		\ GOTO 14240 &

14230		  GOTO 14220 IF Y1%=0% &
		\ Y0%=VAL(MID(Y0$,Y3%-Y1%,Y1%)) &
		\ Y0$="-" IF Y0%>31% AND LEN(Y2$)=3% OR Y0%>12% AND LEN(Y2$)=0% OR Y0%<1% &
		\ IF Y0$<>"-" &
		  THEN	  Y2$=Y2$+RIGHT(NUM1$(Y0%+100%),2%)+"/" &
			\ Y1%=0% IF Y1%<=1% OR Y4%<48% OR Y4%>57% &
			\ Y1%=1% IF Y1%>1% &
			\ GOTO 14220 &

14240		  FND7$=Y0$ &
	\ FNEND &

19000	  ! &
	  !  ERROR TRAP &
	  ! &

19010	  IF ERL=6030% &
	  THEN	  RESUME 6090 &

19990	  ON ERROR GOTO 0 &

32767	  END &

