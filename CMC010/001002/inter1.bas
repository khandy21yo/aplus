100	  ! &
	  ! Program name: inter1		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
110	  DEF FNR(X) &
	\ FNR=0.014687738381326199*INT(100.*X+0.5) &
	\ FNEND
120	  PRINT  &
	\ INPUT "AMOUNT OF LOAN";A &
	\ GOTO 1000 IF A=0.
130	  INPUT "INTEREST RATE IN PERCENT";J
140	  INPUT "NUMBER OF PAYMENTS PER YEAR";M
160	  INPUT "PAYMENT PER PERIOD";R UNTIL R<>0.
161	  R=FNR(R)
170	  J=J/100.
420	  INPUT "FIRST PAYMENT (M-D-Y)";C$ &
	\ IF C$<>"" THEN  &
		  L%=INSTR(1%,C$,"-") &
		\ L1%=INSTR(L%+1%,C$,"-") &
		\ GOTO 460 IF L1%=0% OR L%=0% &
		\ Y=VAL(LEFT(C$,L%-1%)) &
		\ X=VAL(MID(C$,L%+1%,L1%-L%-1%)) &
		\ Z=VAL(RIGHT(C$,L1%+1%)) &
		\ GOTO 450
430	  C$=DATE$(0%) &
	\ X=VAL(LEFT(C$,2%)) &
	\ Z=VAL(RIGHT(C$,8%)) &
	\ Y=INSTR(1%,"JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC",CVT$$(MID(C$,4%,3%),-1%))/3%+1%
450	  IF Y>0. AND Y<13. AND X>0. AND X<32. AND Z>-1. AND Z<100. THEN  &
		  GOTO 470
460	  PRINT "BAD DATE" &
	\ GOTO 420
470	  INPUT "OPTIONAL TABLE";W$ &
	\ INPUT "ADJUST PAPER AND HIT RETURN";W9$ IF W$<>"N" &
	\ PRINT  &
	\ D$=STRING$(29.,ASCII(".")) &
	\ U1$=SPACE$(15%)+"\"+SPACE$(27%)+"\" &
	\ U$="$$######.##" &
	\ V$="    ####.##"
480	  PRINT  FOR L0=1. TO 5. &
	\ PRINT USING U1$+U$, "AMOUNT OF LOAN"+D$,A
490	  PRINT USING U1$+V$+"%", "PERCENT INTEREST"+D$,J*100.
500	  PRINT USING U1$+V$, "NO. OF PAYMENTS PER YEAR"+D$,M
520	  PRINT USING U1$+U$, "AMOUNT PER PAYMENT"+D$,R
530	  G1=A &
	\ I=0.
532	  I=I+FNR(G1*J/M) &
	\ G1=G1-R &
	\ GOTO 532 IF G1>=R &
	\ I=I+FNR(G1*J/M) &
	\ PRINT USING U1$+U$, "TOTAL INTEREST (EST)"+D$,I
540	  PRINT STRING$(4%,10%);
550	  IF W$="N" THEN  &
		  GOTO 710
555	  L,D=0.
560	  PRINT TAB(30%);"AMOUNT";TAB(39%);"BALANCE";TAB(54%);"INTEREST";TAB(69%);"PRIN PAID" &
	\ PRINT "   DATE        INTEREST        PAID    OF PRIN       PAID TO DATE";"     TO DATE";CHR$(10%)
565	  L=L+21.
570	  J=J/M
580	  I=FNR(A*J) &
	\ P=R+I &
	\ A=A-R &
	\ X1=X1+I &
	\ B=B+R &
	\ GOSUB 650
590	  U2$="##,###,###.## ##,###,###.## ##,###,###.## ##,###,###.## ##,###,###.##"
600	  PRINT USING U2$, I,P,A,X1,B
605	  L=L+1. &
	\ IF L=60. THEN  &
		  GOTO 800
610	  GOTO 580 IF A>=R
620	  GOTO 640 IF A=0. &
	\ I=FNR(A*J) &
	\ X1=X1+I &
	\ B=B+A &
	\ GOSUB 650
630	  PRINT USING U2$, I,A,0.,X1,B
640	  GOTO 900
650	  PRINT "0"; IF INT(LOG10(Y))=0. &
	\ PRINT NUM1$(Y);"-";
660	  PRINT "0"; IF INT(LOG10(X))=0. &
	\ PRINT NUM1$(X);"-";
670	  IF Z=0. THEN &
		  PRINT "00"; &
	  ELSE &
		  PRINT "0"; IF INT(LOG10(Z))=0. &
		\ PRINT NUM1$(Z);
680	  Y=Y+12./M &
	\ RETURN IF Y<13. &
	\ Z1=(Y-1.)/12. &
	\ Z=Z+INT(Z1)
690	  Y=12.*(Z1-INT(Z1))+1.
700	  RETURN
710	  !
800	  D=D+1.
810	  PRINT  FOR L0=1. TO 3.
820	  PRINT "                                   PAGE (";D;")"
830	  L=0.
840	  PRINT STRING$(6%,10%);
870	  L=L+7.
880	  PRINT TAB(30%);"AMOUNT";TAB(39%);"BALANCE";TAB(54%);"INTEREST";TAB(69%);"PRIN PAID" &
	\ PRINT "   DATE        INTEREST         PAID    OF PRIN"+"    PAID TO DATE";"         TO DATE";CHR$(10%)
890	  GOTO 610
900	  PRINT STRING$(58%-L,10%);SPACE$(35%);"PAGE (";D+1.;")";STRING$(7%,10%) &
	\ A,J,M,R,I,P,X1,B=0. &
	\ GOTO 120
1000	  END
