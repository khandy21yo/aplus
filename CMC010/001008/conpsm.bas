10	  ! &
	  ! Program name: conpsm		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 28-Nov-16 at 04:11 PM
50	  DIM P1$(30%), P$(9%), P%(17%), P(5%)
60	  DIM R$(9%,20%), R%(15%,20%), R(5%,20%)
100	  !
110	  OPEN "KB:" AS FILE 1%, RECORDSIZE 64%
230	  FIELD #1%, 6% AS P1$(1%),6% AS P1$(2%),1% AS P1$(3%),1% AS P1$(4%),2% AS P1$(5%),2% AS P1$(6%),2% AS P1$(7%),7% AS P1$(8%),2% AS P1$(9%) &
	\ FIELD #1%, 19%+I% AS E$,1% AS P1$(I%) FOR I%=10% TO 24% &
	\ FIELD #1%, 44% AS E$,8% AS P1$(25%),3% AS P1$(26%),3% AS P1$(27%),3% AS P1$(28%),3% AS P1$(29%)
250	  FIELD #1%, 64% AS T2$
300	  !
305	  GOTO 3200 IF F0%=-1%
310	  INPUT "BASE FILE (MM.DD.YY)";K$ &
	\ GOTO 10000 IF K$="" &
	\ GOSUB 2600 &
	\ B$=K$ &
	\ Z1$="PR"+LEFT(B$,2%)+RIGHT(B$,4%)+"T" &
	\ IF FNO%(2%,Z1$,"U","R")=0% THEN  &
		  PRINT "FILE EXISTS . . . "; &
		\ INPUT "CONFIRM COMBINING NEW FILES (Y OR N)";K$ &
		\ GOTO 10000 UNLESS LEFT(K$,1%)="Y" &
		\ GOTO 330
320	  INPUT "VERIFY OPENING NEW FILE ";K$ &
	\ GOTO 10000 UNLESS LEFT(K$,1%)="Y" &
	\ OPEN Z1$ FOR OUTPUT AS FILE 4% &
	\ PRINT #4%, CVT%$(0%)+CVT%$(14%)+"S"+CHR$(128%) &
	\ CLOSE 4% &
	\ OPEN LEFT(Z1$,LEN(Z1$)-1%)+"1" FOR OUTPUT AS FILE 4% &
	\ PRINT #4%, CVT%$(0%)+CVT%$(64%)+"S"+CHR$(128%) &
	\ CLOSE 4%
325	  STOP IF FNO%(2%,Z1$,"/RW","")
330	  !
1000	  !
1010	  INPUT "FILE TO COMBINE (MM.DD.YY) ";K$ &
	\ GOTO 3100 IF K$="" &
	\ GOSUB 2600 &
	\ C$=K$ &
	\ Z$="PR"+LEFT(K$,2%)+RIGHT(K$,4%)+"T" &
	\ IF FNO%(4%,Z$,"/RO","") THEN  &
		  PRINT "FILE DOES NOT EXIST . . ." &
		\ GOTO 1010
1020	  PRINT "VERIFY COMBINING ";C$;" TO ";B$;" "; &
	\ INPUT K$ &
	\ IF LEFT(K$,1%)<>"Y" THEN  &
		  STOP IF FNC%(4%) &
		\ PRINT "NOT ADDED TO BASE FILE . . ." &
		\ GOTO 1010
1030	  GOSUB 3000 &
	\ GOTO 1010
2200	  !
2210	  LSET P1$(I%)=P$(I%) FOR I%=1% TO 8% &
	\ LSET P1$(9%)=CVT%$(FND9%(P$(9%))) &
	\ LSET P1$(I%+9%)=CHR$(P%(I%)) FOR I%=1% TO 15% &
	\ LSET P1$(25%)=CVTF$(P(1%)) &
	\ LSET P1$(I%+24%)=FNN3$(P(I%)) FOR I%=2% TO 5% &
	\ RETURN
2400	  !
2410	  LSET T2$=FNL$ &
	\ P$(I%)=P1$(I%)+"" FOR I%=1% TO 8% &
	\ P%(I%)=ASCII(P1$(I%+9%)) FOR I%=1% TO 15% &
	\ P(1%)=CVT$F(P1$(25%)) &
	\ P(I%)=FNN3(P1$(I%+24%)) FOR I%=2% TO 5% &
	\ P%(0%)=CVT$%(P1$(9%)) &
	\ P$(9%)=FND9$(P%(0%)) &
	\ P%(16%),P%(17%)=0% &
	\ P%(16%)=P%(16%)+P%(I%) FOR I%=1% TO 7% &
	\ P%(17%)=P%(17%)+P%(I%) FOR I%=8% TO 14% &
	\ RETURN
2600	  !
2610	  K$="0"+K$ IF INSTR(1%,K$,".")=2% &
	\ K$=LEFT(K$,3%)+"0"+RIGHT(K$,4%) IF INSTR(4%,K$,".")=5% &
	\ RETURN
3000	  !
3010	  IF FNG%(4%,"") THEN  &
		  GOTO 3090
3020	  X1%=X1%+1% &
	\ PRINT "!"; IF X1%/10%*10%=X1% &
	\ PRINT X1% IF POS(0%)>49% &
	\ STOP IF FNA%(2%,FNL$) &
	\ GOTO 3020 UNLESS FNN%(4%) &
	\ GOTO 3090
3090	  STOP IF FNC%(4%) &
	\ RETURN
3100	  !
3110	  STOP IF FNX%("[1,8]CONPSM.BAC",3120%,Z1$) &
	\ STOP
3120	  Z1$=FNX$ &
	\ F0%=-1% &
	\ GOTO 10
3200	  !
3210	  IF FNO%(2%,Z1$,"","") THEN  &
		  PRINT "PAYROLL FILE NAME NOT RETURNED THROUGH SORT!!!" &
		\ PRINT "PLEASE HANG UP THE PHONE AND CALL CMC !!!!!!!!" &
		\ STOP
3220	  STOP IF FNG%(2%,"") &
	\ N%=0% &
	\ J$=""
3300	  GOSUB 2400 &
	\ IF P$(1%)+P$(2%)<>J$ THEN  &
		  J$=P$(1%)+P$(2%) &
		\ N%=0%
3301	  X1%=X1%+1% &
	\ PRINT "*"; IF X1%/10%*10%=X1% &
	\ PRINT X1% IF POS(0%)>49%
3310	  R$(J%,N%+1%)=P$(J%) FOR J%=1% TO 9% &
	\ R(J%,N%+1%)=P(J%) FOR J%=1% TO 5% &
	\ R%(J%,N%+1%)=P%(J%) FOR J%=1% TO 15% &
	\ GOTO 3330 IF P$(8%)<>""
3315	  FOR I1%=1% TO N% &
		\ GOTO 3320 IF R%(0%,I1%) OR P$(4%)<>R$(4%,I1%) OR P(1%)<>R(1%,I1%) &
		\ GOTO 3320 IF P(J%)<>0. AND R(J%,I1%)<>0. FOR J%=2% TO 5% &
		\ GOTO 3320 IF P%(J%)+R%(J%,I1%)>255% FOR J%=1% TO 14% &
		\ GOTO 3400
3320			  NEXT I1%
3330	  N%=N%+1% &
	\ R%(0%,N%)=0% &
	\ R%(0%,N%)=-1% IF R$(8%,N%)<>""
3340	  GOTO 3300 UNLESS FNN%(2%) &
	\ PRINT "COMBINE COMPLETE . . ." &
	\ GOTO 10000
3400	  !
3410	  STOP IF FND%(2%,"") &
	\ STOP IF FNN%(-2%) FOR J%=I1% TO N% &
	\ GOSUB 2400
3415	  STOP IF P$(J%)<>R$(J%,I1%) FOR J%=1% TO 9% &
	\ STOP IF P%(J%)<>R%(J%,I1%) FOR J%=1% TO 15% &
	\ STOP IF P(J%)<>R(J%,I1%) FOR J%=1% TO 5%
3420	  N0%=N%+1% &
	\ P$(3%)=R$(3%,N0%) IF P(2%)=0. &
	\ P$(J%)=R$(J%,N0%) IF P(J%-2%)<>0. FOR J%=3% TO 5% &
	\ P(J%)=R(J%,N0%) IF P(J%)=0. FOR J%=2% TO 5% &
	\ P%(J%)=P%(J%)+R%(J%,N0%) FOR J%=1% TO 14% &
	\ GOSUB 2200 &
	\ STOP IF FNU%(2%,T2$)
3430	  R$(J%,I1%)=P$(J%) FOR J%=1% TO 8% &
	\ R%(J%,I1%)=P%(J%) FOR J%=1% TO 15% &
	\ R(J%,I1%)=P(J%) FOR J%=1% TO 5%
3440	  STOP IF FNN%(2%) FOR J%=I1% TO N%-1% &
	\ GOTO 3340
10000	  !
10010	  STOP IF FNX%("",0%,"") &
	\ STOP
14000	  !
14010	  DEF FND9%(D9$) &
	\ D9$="0"+D9$ IF INSTR(1%,D9$,".")=2% &
	\ D9$=LEFT(D9$,3%)+"0"+RIGHT(D9$,4%) IF INSTR(4%,D9$,".")=5% &
	\ FND9%=VAL(LEFT(D9$,2%))+VAL(MID(D9$,4%,2%))*16%+FND8%(VAL(RIGHT(D9$,7%)))*512% &
	\ FNEND
14020	  DEF FND9$(D9%) &
	\ FND9$=RIGHT(NUM1$((D9% AND 15%)+100%),2%)+"."+RIGHT(NUM1$((D9% AND 31%*16%)/16%+100%),2%)+"."+RIGHT(NUM1$((SWAP%(D9%) AND 254%)/2%+100%),2%) &
	\ FNEND
14030	  DEF FND8%(D8) &
	\ FND8%=D8 &
	\ FNEND
14100	  !
14110	  DEF FNA8$(X$) &
	\ FNA8$=CVT%$(VAL(LEFT(X$,3%)))+CHR$(VAL(RIGHT(X$,5%))) &
	\ FNEND
14120	  DEF FNA7$(X$) &
	\ FNA7$=RIGHT(NUM1$(1000%+CVT$%(X$)),2%)+"."+RIGHT(NUM1$(100%+ASCII(RIGHT(X$,3%))),2%) &
	\ FNEND
14200	  !
14210	  DEF FND7%(D7$) &
	\ ON ERROR GOTO 14220 &
	\ GOTO 14220 IF INSTR(1%,D7$,".")<>3% OR INSTR(4%,D7$,".")<>6% OR INSTR(7%,D7$,".")<>0% OR LEN(D7$)<>8% &
	\ D7%=VAL(LEFT(D7$,2%)) &
	\ GOTO 14220 IF D7%<1% OR D7%>12% &
	\ D7%=VAL(MID(D7$,4%,2%)) &
	\ GOTO 14220 IF D7%<1% OR D7%>31% &
	\ D7%=VAL(RIGHT(D7$,7%)) &
	\ GOTO 14220 IF D7%<0% &
	\ FND7%=0% &
	\ GOTO 14230
14220	  FND7%=-1% &
	\ RESUME 14230
14230	  ON ERROR GOTO 0 &
	\ FNEND
14250	  !
14260	  DEF FND7$(D7$) &
	\ D7$=D7$+"."+RIGHT(DATE$(0%),8%) IF LEN(D7$)<6% &
	\ D7$="0"+D7$ IF INSTR(1%,D7$,".")=2% &
	\ D7$=LEFT(D7$,3%)+"0"+RIGHT(D7$,4%) IF INSTR(4%,D7$,".")=5% &
	\ FND7$=D7$ &
	\ FNEND
14300	  !
14310	  DEF FNN3$(N3) &
	\ FNN3$=CVT%$(INT(N3))+CHR$((N3-INT(N3))*100.+6.8213185310482798e-15) &
	\ FNEND
14350	  DEF FNN3(N3$) &
	\ FNN3=CVT$%(LEFT(N3$,2%))+ASCII(MID(N3$,3%,1%))/100. &
	\ FNEND
32767	  END
