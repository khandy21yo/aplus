10	  ! &
	  ! Program name: layout		Compiled with SCALE 0 on V06.C &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
50	  DIM C1$(32%), L$(200%,10%)
100	  !
110	  OPEN "KB:" AS FILE 1%
200	  !
210	  FIELD #1%, I%-1% AS E$,1% AS C1$(I%) FOR I%=1% TO 32%
300	  !
310	  L%(1%)=3% &
	\ L1$(1%)="  #" &
	\ L%(2%)=26% &
	\ L1$(2%)=" NAME" &
	\ L%(3%)=11% &
	\ L1$(3%)="SIZE" &
	\ L%(4%)=11% &
	\ L1$(4%)="VARIABLE" &
	\ L%(5%)=20% &
	\ L1$(5%)=" REMARK" &
	\ L1$(I%)=LEFT(L1$(I%)+SPACE$(L%(I%)),L%(I%)+1%) FOR I%=1% TO 5%
320	  B%=-1% &
	\ T1%=4%
1000	  !
1020	  PRINT  &
	\ INPUT "SELECTION ";K$ &
	\ K$=LEFT(CVT$$(K$,-1%),3%)
1030	  GOTO 3000 IF K$="ENT" &
	\ GOTO 10000 IF K$="END" &
	\ GOTO 4000 IF K$="CHA"
1035	  PRINT  &
	\ PRINT "OPTIONS : ENTER A LAYOUT" &
	\ PRINT "          CHANGE AN EXISTING LAYOUT" &
	\ PRINT "          END PROGRAM" &
	\ GOTO 1020
1040	  GOTO 1020
3000	  !
3010	  GOSUB 8000 &
	\ T%=0% &
	\ GOSUB 8100 &
	\ GOSUB 8500 &
	\ GOTO 1020
4000	  !
4010	  PRINT "THIS CHANGE OPTION WILL ONLY CHANGE A STANDARD FORMAT LAYOUT" &
	\ L1$="#NSVR" &
	\ T%=0% &
	\ ON ERROR GOTO 19000
4020	  INPUT "FILE NAME";F$(1%) &
	\ F$(1%)=F$(1%)+".LAY" IF INSTR(1%,F$(1%),".")=0% &
	\ OPEN F$(1%) FOR INPUT AS FILE 4%
4030	  INPUT LINE #4%, K$ &
	\ K$=CVT$$(K$,4%) &
	\ GOTO 4030 IF K$="" &
	\ IF LEFT(CVT$$(K$,2%),10%)<>"----------" THEN  &
		  F$(4%)=K$ &
		\ INPUT LINE #4%, K$ &
		\ IF LEN(F$(4%))>55% THEN  &
			  I%=LEN(F$(4%)) &
			\ I%=I%-1% UNTIL MID(F$(4%),I%,4%)=="    " OR I%=0% &
			\ IF I%>2% THEN  &
				  F$(4%)=CVT$$(LEFT(F$(4%),I%),128%)
4040	  INPUT LINE #4%, K$ UNTIL INSTR(1%,K$,":") &
	\ F$(2%)=CVT$$(RIGHT(K$,INSTR(1%,K$,":")+1%),4%)
4050	  K$="" &
	\ INPUT LINE #4%, K$ UNTIL INSTR(1%,K$,":") &
	\ K$=RIGHT(K$,INSTR(1%,K$,":")+1%) &
	\ F$(3%)=CVT$$(LEFT(K$,20%),128%) &
	\ I%=INSTR(1%,K$,"=") &
	\ F$(5%)=CVT$$(MID(K$,I%+1%,INSTR(1%,K$,"+")-I%-1%),2%) &
	\ F$(6%)=CVT$$(RIGHT(K$,INSTR(I%+1%,K$,"=")+1%),6%) &
	\ F$(I%)=CVT$$(F$(I%),8%+128%) FOR I%=1% TO 6%
4060	  K$="" &
	\ INPUT LINE #4%, K$ UNTIL INSTR(1%,K$," _") &
	\ T%=T%+1%
4070	  I%=60% &
	\ I%=I%-1% UNTIL MID(K$,I%,1%)=":" &
	\ L$(T%,5%)=RIGHT(K$,I%+2%) &
	\ K$=LEFT(K$,I%-1%)
4080	  I%=I%-1% UNTIL MID(K$,I%,2%)=" _" &
	\ L$(T%,4%)=RIGHT(K$,I%+1%) &
	\ K$=LEFT(K$,I%)
4090	  I%=I%-1% UNTIL MID(K$,I%,2%)=" _" &
	\ K1$=RIGHT(K$,I%+1%) &
	\ L$(T%,3%)=LEFT(K1$,INSTR(1%,K1$+"/","/")-1%) &
	\ K$=LEFT(K$,I%-1%)
4100	  I%=INSTR(1%,K$,"_") &
	\ L$(T%,2%)=RIGHT(K$,I%+1%)
4110	  L$(T%,2%)=CVT$$(L$(T%,2%),128%+8%) &
	\ FOR I%=3% TO 5% &
		\ L$(T%,I%)=CVT$$(L$(T%,I%),4%+8%+128%) &
		\ L$(T%,I%)=RIGHT(L$(T%,I%),2%) WHILE LEFT(L$(T%,I%),1%)="_" &
	\ NEXT I% &
	\ FOR I%=2% TO 5% &
		\ L$(T%,I%)=LEFT(L$(T%,I%),LEN(L$(T%,I%))-1%) WHILE RIGHT(L$(T%,I%),LEN(L$(T%,I%)))="_" &
	\ NEXT I% &
	\ GOTO 4060
4200	  !
4210	  PRINT "READ FROM FILE ";F$(1%) &
	\ PRINT "TOTAL OF";T%;"LINES" &
	\ PRINT "PROGRAM : ";F$(2%);"  KEY : ";F$(3%);"  KEY LENGTH ";F$(5%);"  DATA LENGTH ";F$(6%) &
	\ PRINT "DESCRIPTION : ";F$(4%) &
	\ PRINT  &
	\ PRINT 
4220	  PRINT  &
	\ INPUT "CHANGE OPTION ";K$ &
	\ K$=LEFT(CVT$$(K$,-1%),3%) &
	\ GOTO 4400 IF K$="CHA" &
	\ GOTO 4800 IF K$="END" &
	\ GOTO 10000 IF K$="VOI" &
	\ GOTO 4500 IF K$="HEA" &
	\ PRINT "CHA OPTIONS : CHANGE ITEM" &
	\ PRINT "              HEADER CHANGE" &
	\ PRINT "              VOID ALL CHANGES (LEAVE FILE ALONE)" &
	\ PRINT "              END CHANGE, REPLACE FILE" &
	\ GOTO 4220
4400	  !
4410	  INPUT "LINE TO CHANGE ";T0% &
	\ GOTO 4220 IF T0%=0% &
	\ IF T0%>T% THEN  &
		  PRINT "NOT THAT MANY LINES !!!" &
		\ GOTO 4410
4420	  INPUT "ITEM (#NSVR)";K$ &
	\ GOTO 4410 IF K$="" &
	\ I%=INSTR(1%,"#NSVR",K$) &
	\ GOTO 4420 IF K$=""
4430	  PRINT "OLD = ";L$(T0%,I%);" NEW : "; &
	\ INPUT LINE K$ &
	\ K$=CVT$$(K$,4%) &
	\ L$(T0%,I%)=K$ IF K$<>"" &
	\ GOTO 4420
4500	  !
4510	  INPUT "OUTPUT FILE : ";K$ &
	\ IF K$<>"" THEN  &
		  K$=K$+".LAY" IF INSTR(1%,K$,".")=0% AND INSTR(1%,K$,":")=0% &
		\ F$(1%)=K$
4590	  GOTO 4220
4800	  !
4810	  GOSUB 8500 &
	\ GOTO 1020
8000	  !
8010	  INPUT "OUTPUT TO";F$(1%) &
	\ F$(1%)="KB:" IF F$(1%)="" &
	\ F$(1%)=F$(1%)+".LAY" IF INSTR(1%,F$(1%),".")=0% AND INSTR(1%,F$(1%),":")=0%
8020	  PRINT "ACCOUNT DESCRIPTION"; &
	\ INPUT LINE C$ &
	\ C$=CVT$$(C$,4%) &
	\ F$(4%)=C$
8030	  INPUT "PROGRAM NAME";F$(2%),"FILE NAME";F$(3%) &
	\ INPUT "KEY LANGTH";F$(5%),"DATA LENGTH";F$(6%)
8040	  INPUT "LAYOUT FORMAT (<CR> FOR STANDARD)";L1$ &
	\ L1$="#NSVR" IF L1$=""
8090	  RETURN
8100	  !
8110	  V$=SYS(CHR$(3%)) &
	\ GOSUB 8140 FOR I%=1% TO LEN(L1$) &
	\ PRINT 
8120	  T%=T%+1% &
	\ N%=0% &
	\ FOR I1%=1% TO LEN(L1$) &
		\ J%=FNT%(L1$,I1%) &
		\ IF J%=1% THEN &
			  GOSUB 8150 &
		  ELSE &
			  GOSUB 8160
8130		  RETURN IF N%=99% &
	\ NEXT I1% &
	\ PRINT  &
	\ GOTO 8120
8140	  PRINT L1$(FNT%(L1$,I%)); &
	\ RETURN
8150	  PRINT USING "### ", T%; &
	\ RETURN
8160	  N%=N%+1% &
	\ E$=FNC1$(L%(J%),I1%,T%) &
	\ GOTO 8400 IF N%=1% AND E$="." &
	\ IF E%=27% THEN  &
		  GOTO 8200 IF N%=1% &
		\ PRINT "LINE VOIDED!!!" &
		\ N%=0% &
		\ I1%=0% &
		\ RETURN
8170	  L$(T%,I1%)=E$ &
	\ RETURN
8200	  !
8210	  PRINT "OPTION : "; &
	\ E$=FNC1$(4%,0%,0%) &
	\ GOTO 8400 IF E$="END" &
	\ PRINT 
8316	  I3%=VAL(E$) UNLESS E$="" &
	\ T%=I3% &
	\ N%=0% &
	\ I1%=0% &
	\ RETURN
8400	  !
8410	  PRINT  &
	\ V$=SYS(CHR$(2%)) &
	\ PRINT  &
	\ T%=T%-1% &
	\ N%=99% &
	\ RETURN
8500	  !
8510	  INPUT "SET PAGE";K$ IF F$(1%)="KB:" &
	\ OPEN F$(1%) FOR OUTPUT AS FILE 4% &
	\ X3%=0% &
	\ GOSUB 8760 &
	\ V%=0%
8520	  FOR I1%=1% TO T% &
		\ GOSUB 8700 IF X1%>=40% &
		\ PRINT #4%, TAB(T1%); &
		\ FOR I%=1% TO LEN(L1$) &
			\ E1$=L$(I1%,I%) &
			\ J%=FNT%(L1$,I%) &
			\ ON J% GOSUB 8540,8550,8560,8570,8580
8525				  NEXT I% &
		\ PRINT #4% &
		\ PRINT #4% &
		\ X1%=X1%+2% &
	\ NEXT I1% &
	\ GOSUB 8740 &
	\ CLOSE 4% &
	\ RETURN
8540	  PRINT #4%, USING "### ", I1%; &
	\ RETURN
8550	  J%=1% &
	\ J%=J%+1% WHILE MID(E1$,J%,1%)==" " &
	\ E1$=STRING$(J%,ASCII("_"))+RIGHT(E1$,J%) &
	\ E1$=E1$+STRING$(L%(2%)-LEN(E1$)-1%,ASCII("_"))+" " &
	\ PRINT #4%, E1$; &
	\ RETURN
8560	  E1$="8F" IF E1$="F" &
	\ E1$="2I" IF E1$="I" &
	\ E1$="1C" IF E1$="C" &
	\ E1$="1A" IF E1$="A" &
	\ E1$=E1$+"A" IF RIGHT(E1$,LEN(E1$))<="9" AND E1$<>"" &
	\ V1%=VAL(LEFT(E1$,LEN(E1$)-1%)) &
	\ E1$=" "+STRING$(4%-LEN(E1$),ASCII("_"))+E1$ &
	\ E1$=E1$+"/"+NUM1$(V%+1%) UNLESS V1%=0% &
	\ E1$=E1$+STRING$(11%-LEN(E1$),ASCII("_"))+" " &
	\ V%=V%+V1% &
	\ PRINT #4%, E1$; &
	\ RETURN
8570	  E1$=E1$+")" IF INSTR(1%,E1$,"(") AND INSTR(1%,E1$,")")=0% &
	\ J%=INSTR(1%,E1$,"(") &
	\ IF J%=0% THEN &
		  E1$=E1$+"____" &
	  ELSE &
		  E1$=LEFT(E1$,J%)+SPACE$(3%-LEN(E1$)+J%)+RIGHT(E1$,J%+1%)
8572	  E1$=" "+STRING$(8%-LEN(E1$),ASCII("_"))+E1$+"_ " &
	\ PRINT #4%, E1$; &
	\ RETURN
8580	  E1$=": "+E1$ &
	\ E1$=E1$+SPACE$(L%(J%)-LEN(E1$)) UNLESS I%=LEN(L1$) &
	\ PRINT #4%, E1$; &
	\ RETURN
8700	  !
8710	  GOSUB 8720 &
	\ GOSUB 8760 &
	\ RETURN
8720	  !
8730	  PRINT #4%, TAB(T1%);STRING$(75%,45%); &
	\ PRINT #4% FOR X1%=X1% TO 39%+12% &
	\ RETURN
8740	  !
8750	  PRINT #4%, TAB(T1%);STRING$(75%,45%) &
	\ PRINT #4%, USING SPACE$(12%+T1%)+"TOTAL CHARACTERS USED =\ \, FROM "+"\ \, FREE =\ \", FNN$(V%),FNN$(VAL(F$(6%))),FNN$(VAL(F$(6%))-V%) &
	\ PRINT #4% FOR X1%=X1% TO 39%+10% &
	\ RETURN
8760	  !
8770	  X3%=X3%+1% &
	\ PRINT #4% FOR I%=1% TO 5% &
	\ PRINT #4%, TAB(T1%+7%);F$(4%); &
	\ PRINT #4%, TAB(64%);"PAGE";X3%;"OF";(T%-1%)/20%+1%; IF T%>20% &
	\ PRINT #4% &
	\ PRINT #4%, TAB(T1%+7%);STRING$(68%,45%) &
	\ PRINT #4% &
	\ PRINT #4%, TAB(T1%);"PROGRAM NAME : ";F$(2%) &
	\ PRINT #4% &
	\ PRINT #4%, TAB(T1%);"KEY FILE NAME : ";F$(3%);TAB(40%+T1%);"KEY = ";F$(5%);" + 2   DATA = ";F$(6%) &
	\ PRINT #4% &
	\ PRINT #4%, TAB(T1%); &
	\ PRINT #4%, L1$(FNT%(L1$,I%)); FOR I%=1% TO LEN(L1$) &
	\ PRINT #4% &
	\ PRINT #4% &
	\ X1%=0% &
	\ RETURN
9000	  !
9010	  !
9012	  DEF FNC$ &
	\ IF C1%=C2% THEN  &
		  V$=SYS(CHR$(4%)) &
		\ GET #1% &
		\ C1%=0% &
		\ C2%=RECOUNT
9014	  C1%=C1%+1% &
	\ FNC$=C1$(C1%) &
	\ FNEND
9100	  !
9110	  DEF FNC1$(X%,X1%,X2%) &
	\ C$=FNC$ &
	\ C$=L$(X2%+B%,X1%) IF C$="/" &
	\ GOSUB 9170 IF C$="+" OR C$="-" &
	\ PRINT LEFT(C$,LEN(C$)-1%); &
	\ E%=ASCII(RIGHT(C$,LEN(C$))) AND 127% &
	\ C$=LEFT(C$,LEN(C$)-1%) &
	\ GOTO 9130
9120	  E%=ASCII(FNC$) AND 127%
9130	  GOTO 9150 IF E%=10% OR E%=13% OR E%=27% &
	\ IF E%=127% THEN  &
		  GOTO 9120 IF C$=="" &
		\ PRINT CHR$(8%);"\";CHR$(8%); &
		\ C$=LEFT(C$,LEN(C$)-1%) &
		\ GOTO 9120
9140	  IF LEN(C$)>=X% OR E%<32% THEN &
		  GOTO 9120 &
	  ELSE &
		  C$=C$+CHR$(E%) &
		\ PRINT CHR$(E%); &
		\ GOTO 9120
9150	  E$=FNC$ IF E%=13% &
	\ IF E%<>27% THEN  &
		  FNC1$=C$ &
		\ PRINT SPACE$(X%+1%-LEN(C$));
9160	  FNEND
9170	  A%=1% &
	\ A%=-1% IF C$="-" &
	\ C$=CVT$$(L$(X2%+B%,X1%),128%) &
	\ FOR I2%=1% TO LEN(C$) &
		\ GOSUB 9180 IF MID(C$,I2%,1%)>="0" AND MID(C$,I2%,1%)<="9" &
	\ NEXT I2% &
	\ RETURN
9180	  I3%=I2% &
	\ I3%=I3%+1% WHILE MID(C$,I3%,1%)>="0" AND MID(C$,I3%,1%)<="9" &
	\ C$=LEFT(C$,I2%-1%)+NUM1$(VAL(MID(C$,I2%,I3%-I2%))+A%)+RIGHT(C$,I3%) IF INSTR(1%," #(-",MID(C$,I2%-1%,1%))<>0% OR I2%=1% &
	\ I2%=I3% &
	\ RETURN
9200	  !
9210	  DEF FNT%(X$,X%) &
	\ FNT%=INSTR(1%,"#NSVR",MID(X$,X%,1%)) &
	\ FNEND
9300	  !
9310	  DEF FNN$(X%) &
	\ E$="   "+"" &
	\ RSET E$=NUM1$(X%) IF X%<>0% &
	\ FNN$=E$ &
	\ FNEND
10000	  !
10010	  CLOSE 4%
10020	  GOTO 32767
19000	  !
19010	  IF ERL=4060% THEN  &
		  PRINT  &
		\ RESUME 4200
32767	  END
