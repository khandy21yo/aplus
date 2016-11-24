2	  ! &
	  ! Program name: sort		Compiled with SCALE 0 on V07.0 &
	  ! Decompiled on 24-Nov-16 at 02:11 AM
5	  !
8	  V$=SYS(CHR$(6%)+CHR$(-21%)) &
	\ D9$=""
10	  ON ERROR GOTO 100 &
	\ V$=SYS(CHR$(7%)) &
	\ L1%=INSTR(ASCII(V$)+1%,V$,CHR$(13%)) &
	\ GOTO 50 IF L1%=0% &
	\ V$=CHR$(L1%)+RIGHT(V$,2%) &
	\ V1$=SYS(CHR$(8%)+V$) &
	\ L2%=INSTR(L1%+1%,V$,CHR$(13%)) &
	\ L2%=LEN(V$)+1% IF L2%=0% &
	\ D9$=MID(V$,L1%+1%,L2%-L1%-1%) &
	\ GOTO 50 IF D9$="" &
	\ R1%=10240% &
	\ OPEN D9$ AS FILE 1%, RECORDSIZE R1% &
	\ GOTO 200 IF STATUS AND 1024% &
	\ L%=INSTR(1%,D9$,"]") &
	\ GET #1% &
	\ R9%=RECOUNT
20	  S0=TIME(1%) &
	\ PRINT LEFT(D9$,INSTR(1%,D9$+".",".")-1%);TAB(8%);"SORT ";TIME$(0%);" - "; &
	\ FIELD #1%, 2% AS L$,2% AS Z$,1% AS E$,1% AS E1$ &
	\ LSET E$="S" &
	\ PUT #1%, RECORD 1%, COUNT R9% &
	\ P%=ASCII(E1$) AND (NOT 128%) &
	\ N%=CVT$%(L$)+P% &
	\ Z%=2%^(INT(LOG10(CVT$%(Z$))/-1.6593168083046256e-13)+1%) &
	\ S8%=1% &
	\ S7%=0%
25	  Q%=1% &
	\ L3$=SPACE$(Z%) &
	\ DIM S%(200%) &
	\ L%=10% &
	\ S%(0%)=1%+P% &
	\ P%=R1%/Z% &
	\ R%=-1% &
	\ M$=SPACE$(Z%)
30	  R%=FNM%(N%,R%+P%) &
	\ S0%=1% &
	\ S%(S0%)=R%-(Q%-1%)*P%+1% &
	\ GOSUB 9000 &
	\ GOSUB 10000 UNLESS S9% &
	\ PUT #1%, RECORD (Q%-1%)*(R1%/512%)+1%, COUNT R9% UNLESS S9% &
	\ GOTO 40 IF R%=N% &
	\ GET #1%, RECORD Q%*(R1%/512%)+1% &
	\ R9%=RECOUNT
35	  Q%=Q%+1% &
	\ S%(0%)=0% &
	\ GOTO 30
40	  CLOSE 1% &
	\ PRINT TIME$(0%);" (";NUM1$(TIME(1%)-S0);")" &
	\ IF Q%>1% THEN &
		  CHAIN "!MERGE.BAC" F9% &
	  ELSE &
		  GOTO 8
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
		  IF ERR=55% AND ERL=10410% THEN  &
			  RESUME 10250
105	  IF ERR=4% THEN  &
		  IF X1% THEN  &
			  X1%=0% &
			\ RESUME 
110	  IF ERR=5% AND ERL=50% THEN  &
		  PRINT "Program ";D9$;" not found for chain . . You are now at 'Ready'" &
		\ GOTO 32000
150	  ON ERROR GOTO 0
200	  PRINT "Write priviliges cannot be established for ";D9$ &
	\ GOTO 8
1000	  DEF FNG$(I%) &
	\ FIELD #1%, I%*Z% AS E$,Z% AS L$ &
	\ FNG$=L$ &
	\ FNEND
9000	  S7%=S7%+P% &
	\ IF S7%>N% THEN &
		  S6%=N%-S7%+P% &
	  ELSE &
		  S6%=P%-1%
9010	  FOR S9%=S6%-1% TO S8% STEP -1% &
		\ FIELD #1%, Z%*S9% AS S0$,Z% AS S0$,Z% AS S1$ &
		\ IF S1$<S0$ THEN  &
			  S8%,S9%=0% &
			\ RETURN
9020			  NEXT S9% &
	\ S8%=0% &
	\ S9%=-1% &
	\ RETURN
10000	  RETURN IF S0%=0% &
	\ X%=S%(S0%-1%) &
	\ Y%=S%(S0%) &
	\ X1%=Z%*X% &
	\ Y1%=Z%*(Y%-1%) &
	\ GOTO 10300 IF Y%-X%>L%
10100	  FOR K%=X1%+Z% TO Y1% STEP Z% &
		\ FIELD #1%, K% AS L$,Z% AS L$ &
		\ LSET L3$=L$ &
		\ FOR J%=K%-Z% TO X1% STEP -Z% &
			\ L2$=L$ &
			\ FIELD #1%, J% AS E$,Z% AS L$ &
			\ IF L$>L3$ THEN  &
				  LSET L2$=L$ &
			\ NEXT J% &
			\ L2$=L$
10110		  LSET L2$=L3$ &
	\ NEXT K%
10200	  S0%=S0%-1% &
	\ GOTO 10000
10250	  X%=S%(S0%-1%) &
	\ Y%=S%(S0%) &
	\ X1%=X%*Z% &
	\ Y1%=Z%*(Y%-1%) &
	\ GOTO 10100
10300	  FIELD #1%, X1% AS L0$,Z% AS L0$ &
	\ FIELD #1%, Z%*(X%+(Y%-X%)/2%) AS L1$,Z% AS L1$ &
	\ FIELD #1%, Y1% AS L2$,Z% AS L2$ &
	\ IF L2$<=L0$ THEN &
		  T%=-1% &
	  ELSE &
		  T%=0%
10310	  IF T% EQV L0$<=L1$ THEN &
		  LSET M$=L0$ &
	  ELSE &
		  IF T% EQV L1$<=L2$ THEN &
			  LSET M$=L2$ &
		  ELSE &
			  LSET M$=L1$
10400	  FOR X%=X% TO Y%-1% &
		\ FIELD #1%, X1% AS L$,Z% AS L$ &
		\ X1%=X1%+Z% &
		\ GOTO 10420 IF L$>=M$ &
	\ NEXT X%
10410	  GOTO 10200 IF Y%=S%(S0%) &
	\ GOTO 10250 IF X%=S%(S0%-1%) &
	\ S%(S0%+1%)=S%(S0%) &
	\ S%(S0%)=Y% &
	\ S0%=S0%+1% &
	\ GOTO 10000
10420	  L1$=L$ &
	\ FOR Y%=Y%-1% TO X% STEP -1% &
		\ FIELD #1%, Y1% AS L$,Z% AS L$ &
		\ Y1%=Y1%-Z% &
		\ GOTO 10430 IF L$<M$ &
	\ NEXT Y% &
	\ GOTO 10410
10430	  LSET L3$=L$ &
	\ LSET L$=L1$ &
	\ LSET L1$=L3$ &
	\ X%=X%+1% &
	\ GOTO 10400
30000	  DEF FNM%(V1%,V2%) &
	\ IF V2%>0% AND V2%<V1% THEN &
		  FNM%=V2% &
	  ELSE &
		  FNM%=V1%
30010	  FNEND
32000	  END
