10	! Play a game of football against RSTS/E &
	! &
	! Modified by Don Emerson and Carolyn Schipper, &
	! Project DELTA, 14-Aug-74 &
	! &
	! Modified 30-Dec-75 by Carolyn Schipper &
	! &
	! Modified for a VT100 terminal, 02-Jan-80, &
	! by John Rich, Project DIRECT &
	! &
	! Modified to let user see end of game, 02-Jun-85, &
	! by Brian Handy &
	!
15	C$=Sys(Chr$(6%)+Chr$(16%)+Chr$(0%)+Chr$(255%)+Chr$(132%)+ &
		String$(6%,0%)+Chr$(128%)+String$(18%,0%)) &
	! Sets terminal width to 132, sets No Lower Case Input &

20	ON ERROR GOTO 32767 &
	\ A$=SYS(CHR$(6%)+CHR$(-7%)) &
	! ^C TRAP.
40	PRINT "YOUR TEAM'S NAME"; &
	\ INPUT LINE A$ &
	\ A$=CVT$$(A$,4%) &
	\ IF LEN(A$)=0% THEN A$="NONAME" ELSE &
		IF LEN(A$)>9% THEN &
		PRINT \ PRINT &
		"LENGTH OF NAME MUST BE LESS THAN 10 CHARACTERS." &
		\ PRINT \ GOTO 40
50	S$=CHR$(155%) \ &
	PRINT S$;"<";S$;"(0";S$;")0";S$;"[H";S$;"[2J";S$;"[?3h";S$;"[?4l"; \ &
	! &
	!	Sets ANSI mode, enables special graphics characters, &
	!	clears screen, sets column mode to 132, and sets &
	!	scroll mode to JUMP. &
	!
60	! Lines 70-90 print the football field. &
	!
70	PRINT S$;"[4;1H";STRING$(111%,115%);S$;"[14;1H";STRING$(111%,111%); &
	\ FOR X%=1% TO 111% STEP 110% &
	\ GOSUB 1372 &
	\ NEXT X% &
	\ FOR X%=6% TO 106% STEP 10% &
	\ GOSUB 1372 &
	\ NEXT X%
80	PRINT S$;"[1;6H";"PERIOD: 1";S$;"[1;45H";"TIME TO GO:"; &
	S$;"[2;114H";"YOU'RE ON";S$;"[17;1H","DOWN:","YARDS TO GO:"; &
	S$;"[17;53H";"BALL:";S$;"[17;71H";"SCORE   ME: 0     YOU: 0";
90	FOR V%=10% TO 40% STEP 10% &
	\ P$=NUM1$(V%) &
	\ T%=V%+6% &
	\ GOSUB 1376 &
	\ T%=(100%-V%)+6% &
	\ GOSUB 1376 &
	\ NEXT V% &
	\ T%=6% &
	\ P$="G" &
	\ GOSUB 1376 &
	\ T%=106% &
	\ GOSUB 1376 &
	\ P$="50" &
	\ T%=56% &
	\ GOSUB 1376
110	B$=FNV$(108%,"PDP") \ &
	B$=FNV$(3%,A$) \ &
	! &
	!	Prints team names in endzones. Computer's &
	!	name can be changed by changing string in &
	!	first function. &
	!
120	PRINT S$;"[19;24r";S$;"[19;1H"; &
	! Sets scrolling region, positions &
	! cursor at beginning of scrolling region. &

130	DIM R(17%),P(2),D(2%,5%),Z(5%,3%),D$(4%),R$(17%) &
	\ READ R(0%) \ MAT READ R \ READ R$(0%) \ MAT READ R$, D$
170 DATA 9,13,100,0,9,10,12,11,12,0,1,5,3,4,6,2,8,7
180 DATA NO,YES,TIMEOUT,""
190 DATA NORMAL,HOLD,BLITZ,INTERCEPT,BLOCK,""
200 DATA RUN,PASS,SWEEP,SCREEN,BOMB,DRAW,PUNT,FIELDGOAL
210 DATA 1ST,2ND,3RD,4TH
380	PRINT \ RANDOM &
	\ FOR A=0 TO 5 &
	\ READ Z(A,B),D(B,A) FOR B=0 TO 2 &
	\ READ Z(A,3) &
	\ NEXT A
400	INPUT "DO YOU WANT TO RECIEVE";A$ : S=2 : X%=1% : &
	S=1 IF ASCII(CVT$$(A$,-1%))=78%
410 K=S : U,U2=3 : C=900
420	PRINT \ M$="YOU" \ M$="I" IF S=2 &
	\ PRINT M$;" KICKOFF." \ F2=.06 \ Z2=0 &
	\ IF S=1 THEN &
		INPUT "     ON-SIDE";A$ &
		\ IF LEFT(A$,1%)="Y" THEN F2=.15 \ Z2,O=1
450	F1=50 \ PRINT "     KICK OFF ";
460	B=INT(F1+O*20*RND(0)+(1-O)*29*(2-RND(0)^7-RND(0)^(3-Z2))) &
	\ E=B+6 UNLESS D7% &
	\ D7%=1% &
	\ O,L=0 \ Z9=8 \ C=FNC &
	\ IF B>99 THEN &
		PRINT "IS A TOUCHBACK." &
		\ B=20 \ L=0 \ GOTO 570
490	B=100-B \ PRINT "RECIEVED AT " \ GOSUB 1230 &
	\ GOTO 560 IF Z2*RND>Z3 &
	\ IF RND<F2 THEN &
		G=0 \ PRINT "    ** FUMBLE **" &
		\ GOTO 1080
520	G=5+INT(100*RND) &
	\ IF RND>=.15 THEN &
		G=INT(24*(1-RND**2)) &
		\ GOTO 560 IF G=0
540	B=B+G \ PRINT "       AND RUN BACK..."; \ L=1 \ &
	GOTO 1040 IF B>99 &
	\ PRINT " TO "; \ GOSUB 1230
560	IF RND<=F2 THEN G=0 &
	\ PRINT "    ** FUMBLE **" &
	\ GOTO 1080
570	PRINT \ M$="YOUR" \ M$="MY" IF S=1 &
	\ PRINT M$;" BALL ON "; \ GOSUB 1230 &
	\ GOSUB 1600 \ D=0 &
	\ B1=B+10 \ B1=100 IF B1>99
610	IF C<=0 THEN 1200 ELSE IF C<=120*F THEN PRINT &
	\ PRINT \ L,F=0 \ PRINT FNT$ &
	\ C=120
620	D=D+1 \ IF D>4 THEN &
		S=3-S \ B=100-B \ GOTO 570
635	PRINT S$;"[17;16H";D$(D);S$;"[17;37H";"  "; &
		S$;"[17;37H";NUM1$(B1-B); &
	\ F2=.03 \ O=0 &
	\ IF S=2 THEN &
		Y=INT(1+3.5*RND) \ GOTO 700
660	M=INT(55*RND/10.5)+1 &
	\ M=INT(6-4*RND**2) IF (B1-B)/(5-D)>=3 &
	\ IF L*(1%-(X% AND 1%))*60%>=C AND P(2)>P(1) AND U2>0 THEN &
		PRINT "TIME OUT" \ U2=U2-1 \ L=0 &
		\ PRINT FNT$ \ PRINT
690 IF D>=4 THEN IF B<55 THEN M=8 ELSE IF B1-B>=4*RND THEN M=7
700	Z2=1 \ Z3=.3 &
	\ M$=NUM1$(INT(C/60)) &
	\ H$=NUM1$(C-INT(C/60)*60) &
	\ M$=" "+M$ IF LEN(M$)=1% &
	\ H$="0"+H$ IF LEN(H$)=1% &
	\ PRINT S$;"[1;57H";M$+":"+H$; &
	\ GOSUB 1720
710	! &
	!	PRINTS TIME IN TOP OF FIELD &
	!
720 PRINT"YOUR PLAY"; : GOSUB 1270 : IF Q=100 THEN 720
730 IF S=1 AND (Q<9 OR Q>12) THEN PRINT".....PICK A DEFENSE, "; : GOTO 720
740 IF S=2 AND Q>8 THEN PRINT".....PICK AN OFFENSE, "; : GOTO 720
750 IF S=1 THEN Y=Q-8 ELSE M=Q
760 C=INT(C-L*(5+23*RND)) : L=1
770	GOTO 820 IF M<>7 &
	\ PRINT "     I'LL TRY FOR A FIELDGOAL" IF S=1
780	IF RND<.06 THEN PRINT "     ** BLOCKED ** RECOVERED..." &
	\ G=-INT(14*RND) : GOTO 1080
790 B=B+30+INT(15*(1-RND**3)) : Z9=2 : C=FNC : IF B<=99 THEN 810 ELSE &
	PRINT"     THE KICK IS "; : IF RND>=(B/132)**4 THEN &
	PRINT"NO GOOD" : S=3-S : PRINT"     **  TOUCHBACK  **" : &
	B=20 : L=0 : GOTO 570
800 PRINT"GOOD" : P(S)=P(S)+3 : GOSUB 1070 : S=3-S : GOTO 420
810 PRINT"     THE KICK IS RECEIVED AT "; : Z2=0 : S=3-S : B=100-B : GOTO 1120
820	GOTO 850 IF M<>8 &
	\ PRINT "     I'LL PUNT" IF S=1
830	PRINT "    THE PUNT "; : IF RND<.06 THEN &
	PRINT "IS  ** BLOCKED ** RECOVERED..." : G=-INT(14*RND) : GOTO 1080
840 F1=B : Z2=1 : S=3-S : Z3=.65 : GOTO 460
850 GOTO 930 IF RND>Z(M-1,Y-1) : A=2 : GOSUB 1210
860 IF M<4 THEN PRINT"     "; ELSE &
	PRINT"     PASS COMPLETE... "; : L=SGN(INT(4*RND))
870	IF RND<F2 THEN PRINT "   ** FUMBLE **   " : GOTO 1080
880	IF G>0 THEN PRINT "GAIN OF "G; IF B+G<=99 \ &
	IF B+G<=99 THEN 980 ELSE 1040
890 IF G=0 THEN PRINT"NO GAIN" : Z9=2 : C=FNC : GOTO 1000
900 IF G+B>=1 THEN PRINT"LOSS OF ";-G; : GOTO 980
910 PRINT"** SAFETY **" : P(3-S)=P(3-S)+2 : F1=30 : S=3-S : &
	GOSUB 1070 : PRINT : IF S=2 THEN PRINT"I"; ELSE PRINT"YOU";
920 PRINT" KICK OFF FROM THE 20." : F2=.06 : Z2=0 : GOTO 460
930 IF M<=3 THEN A=1 : GOSUB 1210 : PRINT"     "; : GOTO 880
940	IF RND<.06*(2-SGN(Y-3)) THEN PRINT "     PASS ** INTERCEPTED **  " : &
	A=1 : GOSUB 1210 : G=20*(1+SGN(G)) IF B>=40 : GOTO 1080
950 IF RND>=.05+INT(Y/3.5)/5 THEN Z9=6 : C=FNC : L,G=0 : &
	PRINT"     PASS INCOMPLETE" : GOTO 610
960 G=-(5+INT(10*RND)) : PRINT"     THE QUARTERBACK "; : &
	IF RND<=.5 THEN PRINT"IS THROWN FOR A "; : GOTO 870
970 G=-(2*G+8) : M=5 : A=1 : PRINT"SCRAMBLES FOR A "; : GOTO 870
980 B=B+G : PRINT"TO "; : GOSUB 1230
990	IF B>99 THEN 1040
1000 IF B1>B THEN 610
1010 IF B1=B AND RND<.5 THEN PRINT : PRINT"** MEASUREMENT **" : L=0 : &
	PRINT FNT$ : PRINT : IF RND<.5 THEN 610
1020 IF B+10<=100 THEN B1=B+10 ELSE B1=100
1030 PRINT : D=0 : GOTO 610
1040 PRINT : PRINT : PRINT"**  TOUCHDOWN  **" : P(S)=P(S)+6
1050 PRINT"     THE KICK IS "; : IF RND>=.94 THEN PRINT"NO "; ELSE P(S)=P(S)+1
1060	PRINT "GOOD" \ GOSUB 1070 \ S=3-S \ GOTO 420
1070	GOSUB 7000 UNLESS C=900 &
	\ GOSUB 1720 &
	\ S3$="ME: "+NUM1$(P(1))+"      YOU: "+NUM1$(P(2)) &
	\ PRINT "     SCORE:    ";S3$ &
	\ IF X%<5% OR P(1)=P(2) THEN 1072 ELSE &
		PRINT "...... THE GAME IS OVER." & 
\	SLEEP 5 &
\	GOTO 32767
1072	PRINT S$;"[17;79H";S3$; &
	\ GOTO 1720
1080	GOSUB 7000 &
	\ B=100-G-B &
	\ S=3-S \ Z9=9 \ C=FNC
1090 IF B<1 THEN PRINT"A TOUCHBACK" : B=20 : L=0 : GOTO 570
1100	IF B>99 THEN 1040
1110 PRINT"AT ";
1120 GOSUB 1230
1130 IF Z2*RND>Z3 THEN 1180 ELSE IF RND<F2 THEN 1190 ELSE G=5+INT(100*RND)
1140 IF RND>=.15 THEN G=INT(24*(1-RND**2)) : IF G=0 THEN 1180
1150 B=B+G : PRINT"     AND RUN BACK..."; : L=1
1160	IF B>99 THEN 1040
1170 PRINT" TO "; : GOSUB 1230
1180 IF RND>F2 THEN 570
1190 G=0 : PRINT"     **  FUMBLE  ** "; : GOTO 1080
1200	PRINT \ PRINT "END OF PERIOD"X% \ F=X% AND 1% \ X%=X%+1% &
	\ PRINT S$;"[1;14H";NUM1$(X%); UNLESS X%>4% \ C=900 &
	\ GOSUB 1070 \ L=0 \ PRINT FNT$ \ PRINT &
	\ IF F<>0 THEN 610 ELSE &
		S=3-K \ GOTO 410
1210 Q=1.3*(A*RND-1)-.06 : A=1 : IF Q<0 THEN A=0 : Q=-Q
1220 M1=M-1 : &
	G=INT(D(A,M1)+TAN(Q)*(D(2,M1)-D(A,M1))/3.5+INT(RND+.02)*A*100*RND) : &
	RETURN
1230	IF B=50 THEN PRINT "THE 50" &
	\ GOTO 1265
1240 C=C-INT(7+3*RND)
1250 IF (S=1 AND B<50) OR (S=2 AND B>50) THEN PRINT"MY "; ELSE PRINT"YOUR ";
1260	PRINT 50-ABS(B-50)
1265	GOSUB 1700 &
	\ GOTO 1720
1270	INPUT A$ \ GOTO 1290 IF LEFT(A$,4-S)=LEFT(R$(A),4-S) FOR A=0 TO 17
1280 PRINT".....WRONG, TRY AGAIN"; : GOTO 1270
1290 Q=R(A) : IF Q=0 THEN 1280 ELSE IF Q<>100% THEN RETURN
1300 IF U=0 THEN PRINT"YOU HAVE NO TIMEOUTS LEFT." : GOTO 1280
1310 PRINT FNT$ : PRINT : U=U-1 : L=0 : RETURN
1320 DATA .5,-2,.25,4,.5,13,.55
1330 DATA .4,-2,.3,7,.65,15,.75
1340 DATA .4,-2,.3,6,.6,15,.35
1350 DATA .65,-2,.65,6,.6,17,.9
1360 DATA .4,2,.7,10,.4,27,.2
1370 DATA .1,19,.4,35,.2,100,.1
1372	PRINT S$;"["+NUM1$(Y%)+";"+NUM1$(X%)+"H";CHR$(120%); FOR Y%=5% TO 13% &
	\ RETURN
1376	PRINT S$;"[3;"+NUM1$(T%)+"H";P$;S$;"[15;"+NUM1$(T%)+"H";P$;
1377	RETURN
1400	PRINT S$;"["+NUM1$(H%)+";120H";R$(H%); FOR H%=4% TO 8% &
	\ GOTO 1720
1500	PRINT S$;"["+NUM1$(H%-6%)+";120H";R$(H%); FOR H%=10% TO 17% &
	\ GOTO 1720
1600	IF S<>2 THEN Z2%=8% ELSE Z2%=5%
1605	PRINT S$;"[4;120H";
1620	PRINT S$;"[K";S$;"D"; FOR H%=1% TO Z2%
1640	IF Z2%=8% THEN &
		W$="DE" &
		\ GOSUB 2100 &
		\ GOTO 1400
1650	W$="OF" &
	\ GOSUB 2100 &
	\ GOTO 1500
1700	PRINT S$;"[17;61H";"  ";S$;"[17;61H";NUM1$(50-ABS(B-50)); &
	\ GOSUB 7000
1707	IF S=1 THEN GOTO 8000
1710	E=B+6 &
	\ PRINT S$;"[1;5m";S$;"[9;"+NUM1$(B+6)+"H";"`";S$;"[0m";
1720	PRINT S$;"[24;1H"; &
	\ RETURN
1800	PRINT S$;"[13;61H";NUM1$(50-ABS(B-50)); &
	\ RETURN
1810	PRINT S$;"[24;1H"; &
	\ RETURN
2100	PRINT S$;"[2;124H";W$+"FENSE"; &
	\ RETURN
7000	PRINT S$;"[9;"+NUM1$(E)+"H";" "; &
	\ IF (E-6)/10=INT((E-6)/10) THEN PRINT &
		S$;"[9;"+NUM1$(E)+"H";CHR$(120%);
7005	GOTO 1720
8000	E=100-B+6 &
	\ PRINT S$;"[1;5m";S$;"[9;"+NUM1$(E)+"H";"`";S$;"[0m"; &
	\ GOTO 1720
11000	A$=SYS(CHR$(6%)+CHR$(16%)+CHR$(0%)+CHR$(255%)+CHR$(80%)+ &
		STRING$(6%,0%)+CHR$(255%)+STRING$(18%,0%)) &
	\ PRINT S$;"(B";S$;")B";S$;"[?3l"; &
	! &
	! Resets screen width, scrolling region and column mode. &
	!
11099	RETURN
15000	DEF FNV$(W%,V$) \ &
	CHANGE V$ TO E% \ &
	Q1%=E%(0%)	\ &
	Y1%=9-(Q1%/2%)	\ &
	Y2%=Y1%+Q1%	\ &
	PRINT S$;"["+NUM1$(X%)+";"+NUM1$(W%)+"H";CHR$(E%(X%-Y1%+1%)); &
		FOR X%=Y1% TO Y2% \ &
	FNEND
15010	DEF FNP$(X%,Y%)=S$+"["+NUM1$(X%)+";"+NUM1$(Y%)+"H"
15020	DEF FNC=C-INT(4+Z9/2*(1+RND)) &

15030	DEF FNT$= "TIMEOUT CALLED AT "+TIME$(0%) &

20000 DATA 9,13,100,0,9,10,12,11,12,0,1,5,3,4,6,2,8,7
20010 DATA NO,YES,TIMEOUT,""
20020 DATA NORMAL,HOLD,BLITZ,INTERCEPT,BLOCK,""
20030 DATA RUN,PASS,SWEEP,SCREEN,BOMB,DRAW,PUNT,FIELDGOAL
20040 DATA 1ST,2ND,3RD,4TH
20050 DATA .5,-2,.25,4,.5,13,.55
20060 DATA .4,-2,.3,7,.65,15,.75
20070 DATA .4,-2,.3,6,.6,15,.35
20080 DATA .65,-2,.65,6,.6,17,.9
20090 DATA .4,2,.7,10,.4,27,.2
20100 DATA .1,19,.4,35,.2,100,.1
32767	GOSUB 11000 &
	\ END
