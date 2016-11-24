2	  ! &
	  ! Program name: ismsrt		Compiled with SCALE 0 on V06.C &
	  ! Decompiled on 24-Nov-16 at 03:11 AM
5	  !
8	  V$=SYS(CHR$(6%)+CHR$(-21%)) &
	\ D9$=""
10	  W0%=1% &
	\ X0%=2% &
	\ V0%=0% &
	\ ON ERROR GOTO 100 &
	\ V$=SYS(CHR$(7%)) &
	\ L1%=INSTR(ASCII(V$)+1%,V$,CHR$(13%)) &
	\ GOTO 50 IF L1%=0% &
	\ V$=CHR$(L1%)+RIGHT(V$,2%) &
	\ V1$=SYS(CHR$(8%)+V$) &
	\ L2%=INSTR(L1%+1%,V$,CHR$(13%)) &
	\ L2%=LEN(V$)+1% IF L2%=0% &
	\ D9$=MID(V$,L1%+1%,L2%-L1%-1%) &
	\ R1%=10240% &
	\ OPEN D9$ FOR INPUT AS FILE W0%, RECORDSIZE R1% &
	\ GOTO 105 IF STATUS AND 1024% &
	\ L%=INSTR(1%,D9$,":") &
	\ D8$=LEFT(D9$,L%)+"SORT"+NUM1$(ASCII(SYS(CHR$(6%)+CHR$(9%)))/2%) &
	\ GET #1% &
	\ R9%=RECOUNT
20	  FIELD #W0%, X0% AS L$,X0% AS Z$,1% AS E$,1% AS E1$ &
	\ LSET E$="S" &
	\ PUT #1%, RECORD 1%, COUNT R9% &
	\ P%=ASCII(E1$) AND (NOT 128%) &
	\ N%=CVT$%(L$)+P% &
	\ Z%=2%^(INT(LOG10(CVT$%(Z$))/-1.6593168083046256e-13)+1%) &
	\ OPEN D8$ AS FILE X0% &
	\ DIM #2%, C%(32767%) &
	\ Q%=1% &
	\ C%(W0%)=P%+W0% &
	\ L3$=SPACE$(Z%) &
	\ DIM S%(200%) &
	\ L%=10% &
	\ S%(V0%)=W0%+P% &
	\ G1%=P% &
	\ P%=R1%/Z% &
	\ R%=-W0% &
	\ M$=SPACE$(Z%)
30	  R%=FNM%(N%,R%+P%) &
	\ S0%=W0% &
	\ S%(S0%)=R%-(Q%-W0%)*P%+W0% &
	\ GOSUB 10000 &
	\ PUT #W0%, RECORD (Q%-W0%)*(R1%/512%)+W0%, COUNT R9% &
	\ GOTO 40 IF R%=N% &
	\ GET #W0%, RECORD Q%*(R1%/512%)+W0% &
	\ R9%=RECOUNT
35	  Q%=Q%+W0% &
	\ S%(V0%)=V0% &
	\ C%(Q%)=R%+W0% &
	\ GOTO 30
40	  C%(V0%)=Q% &
	\ C%(Q%+X0%)=G1% &
	\ PRINT "SORT COMPLETE AT ";TIME$(V0%) UNLESS F9% &
	\ CLOSE W0% &
	\ CLOSE X0% &
	\ IF Q%>W0% THEN &
		  CHAIN "(1,6)ISMMRG" F9% &
	  ELSE &
		  KILL D8$ &
		\ GOTO 8
50	  L1%=CVT$%(RIGHT(V$,2%)) &
	\ D9$=MID(V$,4%,INSTR(4%,V$,CHR$(13%))-4%) &
	\ CHAIN D9$ L1% UNLESS D9$="" &
	\ GOTO 32000
100	  IF ERR=11% THEN &
		  IF ERL=10% THEN &
			  RESUME 20 &
		  ELSE &
			  RESUME 35 &
	  ELSE &
		  IF ERR=55% AND ERL=10410% THEN &
			  RESUME 10250 &
		  ELSE &
			  IF ERR=5% THEN  &
				  IF ERL=10% THEN  &
					  PRINT "File: ";D9$;" not found!!" &
					\ RESUME 8
102	  IF ERR=5% AND ERL=50% THEN  &
		  PRINT "Program: ";D9$;" not found for CHAIN. You are now in MONITOR!" &
		\ GOTO 32000
104	  ON ERROR GOTO 0
105	  PRINT "Protection Violation, Write Priviliges cannot be" &
	\ PRINT "Established for ";D9$ &
	\ GOTO 8
110	  ON ERROR GOTO 0
200	  F9%=200% &
	\ GOTO 8
1000	  DEF FNG$(I%) &
	\ FIELD #W0%, I%*Z% AS E$,Z% AS L$ &
	\ FNG$=L$ &
	\ FNEND
1010	  DEF FNB1%(X%) &
	\ FOR G%=9% STEP -1% UNTIL 2%^G%<X% &
	\ NEXT G% &
	\ FNB1%=2%^(G%+1%) &
	\ FNEND
10000	  RETURN IF S0%=V0% &
	\ X%=S%(S0%-W0%) &
	\ Y%=S%(S0%) &
	\ X1%=Z%*X% &
	\ Y1%=Z%*(Y%-W0%) &
	\ GOTO 10300 IF Y%-X%>L%
10100	  FOR K%=X1%+Z% TO Y1% STEP Z% &
		\ FIELD #W0%, K% AS L$,Z% AS L$ &
		\ LSET L3$=L$ &
		\ FOR J%=K%-Z% TO X1% STEP -Z% &
			\ L2$=L$ &
			\ FIELD #W0%, J% AS E$,Z% AS L$ &
			\ IF L$>L3$ THEN  &
				  LSET L2$=L$ &
			\ NEXT J% &
			\ L2$=L$
10110		  LSET L2$=L3$ &
	\ NEXT K%
10200	  S0%=S0%-W0% &
	\ GOTO 10000
10250	  X%=S%(S0%-W0%) &
	\ Y%=S%(S0%) &
	\ X1%=X%*Z% &
	\ Y1%=Z%*(Y%-W0%) &
	\ GOTO 10100
10300	  FIELD #W0%, X1% AS L0$,Z% AS L0$ &
	\ FIELD #W0%, Z%*(X%+(Y%-X%)/X0%) AS L1$,Z% AS L1$ &
	\ FIELD #W0%, Y1% AS L2$,Z% AS L2$ &
	\ IF L2$<=L0$ THEN &
		  T%=-W0% &
	  ELSE &
		  T%=V0%
10310	  IF T% EQV L0$<=L1$ THEN &
		  LSET M$=L0$ &
	  ELSE &
		  IF T% EQV L1$<=L2$ THEN &
			  LSET M$=L2$ &
		  ELSE &
			  LSET M$=L1$
10400	  FOR X%=X% TO Y%-W0% &
		\ FIELD #W0%, X1% AS L$,Z% AS L$ &
		\ X1%=X1%+Z% &
		\ GOTO 10420 IF L$>=M$ &
	\ NEXT X%
10410	  GOTO 10200 IF Y%=S%(S0%) &
	\ GOTO 10250 IF X%=S%(S0%-W0%) &
	\ S%(S0%+W0%)=S%(S0%) &
	\ S%(S0%)=Y% &
	\ S0%=S0%+W0% &
	\ GOTO 10000
10420	  L1$=L$ &
	\ FOR Y%=Y%-W0% TO X% STEP -W0% &
		\ FIELD #W0%, Y1% AS L$,Z% AS L$ &
		\ Y1%=Y1%-Z% &
		\ GOTO 10430 IF L$<M$ &
	\ NEXT Y% &
	\ GOTO 10410
10430	  LSET L3$=L$ &
	\ LSET L$=L1$ &
	\ LSET L1$=L3$ &
	\ X%=X%+W0% &
	\ GOTO 10400
30000	  DEF FNM%(V1%,V2%) &
	\ IF V2%>V0% AND V2%<V1% THEN &
		  FNM%=V2% &
	  ELSE &
		  FNM%=V1%
30010	  FNEND
32000	  V$=SYS(CHR$(9%)) &
	\ END
