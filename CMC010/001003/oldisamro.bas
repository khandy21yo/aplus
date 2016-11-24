21000	  ! An older version of ISAM.
21001	  ! From a decompiled program (cktota.bac)
21010	  ON ERROR GOTO 21990 &
	\ Q9%=0% &
	\ DIM Q%(12%,7%) &
	\ ON Q0% GOTO 21100,21200,21300,21400
21100	  IF Q%(0%,0%) AND 2%^C% THEN  &
		  Q9%=7% &
		\ GOTO 21999
21110	  GOTO 21510 IF C1%<1% OR C1%>20% &
	\ OPEN K$+"/RO" FOR INPUT AS FILE C%, RECORDSIZE 512%*C1% &
	\ GET #C% &
	\ FIELD #C%, 2% AS Q0$,2% AS Q1$,1% AS Q2$,1% AS Q2$ &
	\ Q%(C%,0%)=CVT$%(Q0$) &
	\ Q%(C%,1%)=(ASCII(Q2$) AND (NOT 128%))+1% &
	\ Q%(C%,2%)=Q%(C%,0%)+Q%(C%,1%)-1% &
	\ Q%(C%,3%)=2%^(INT(LOG10(CVT$%(Q1$))/-1.6593168083046256e-13)+1%) &
	\ Q%(C%,4%)=512%/Q%(C%,3%) &
	\ Q%(C%,5%)=0% &
	\ Q%(C%,7%)=C1% &
	\ Q%(0%,0%)=Q%(0%,0%) OR 2%^C% &
	\ GOTO 21999
21200	  GOTO 21500 IF FNQ0%(C%) &
	\ Q1%=Q%(C%,1%) &
	\ Q2%=Q%(C%,2%)+1% &
	\ Q1$=LEFT(K$,Q%(C%,3%)) &
	\ Q4%=LEN(Q1$)
21210	  Q1=Q1% &
	\ Q2=Q2% &
	\ Q3%=(Q1+Q2)/2. &
	\ FIELD #C%, FNQ1%(C%,Q3%) AS Q0$,Q4% AS Q0$ &
	\ GOTO 21230 IF Q0$==Q1$ &
	\ IF Q3%=Q1% THEN &
		  GOTO 21240 &
	  ELSE &
		  IF Q0$>Q1$ THEN &
			  Q2%=Q3% &
		  ELSE &
			  Q1%=Q3%
21220	  GOTO 21210
21230	  Q8%=FNQ1%(C%,Q3%) &
	\ GOTO 21999
21240	  Q9%=-1% &
	\ GOTO 21230
21300	  GOTO 21500 IF FNQ0%(C%) &
	\ Q1%=C1% &
	\ IF C1%<0% THEN  &
		  IF Q%(C%,5%)+C1%>=Q%(C%,1%) THEN &
			  Q%(C%,5%)=Q%(C%,5%)+C1% &
		  ELSE &
			  Q%(C%,5%)=Q%(C%,1%) &
			\ Q9%=-1%
21310	  IF C1%>0% THEN  &
		  IF Q%(C%,5%)+C1%<=Q%(C%,2%) THEN &
			  Q%(C%,5%)=Q%(C%,5%)+C1% &
		  ELSE &
			  Q%(C%,5%)=Q%(C%,2%) &
			\ Q9%=-1%
21320	  Q8%=FNQ1%(C%,Q%(C%,5%)) &
	\ GOTO 21999
21400	  CLOSE C% &
	\ Q%(0%,0%)=Q%(0%,0%) AND (NOT 2%^C%) &
	\ GOTO 21999
21500	  Q9%=46% &
	\ GOTO 21999
21510	  Q9%=126% &
	\ GOTO 21999
21700	  DEF FNO%(C%,K$,C1%) &
	\ Q0%=1% &
	\ GOSUB 21000 &
	\ FNO%=Q9% &
	\ FNEND
21710	  DEF FNB%(C%,K$) &
	\ Q0%=2% &
	\ GOSUB 21000 &
	\ FNB%=Q8% &
	\ FNEND
21720	  DEF FNN%(C%,C1%) &
	\ Q0%=3% &
	\ GOSUB 21000 &
	\ FNN%=Q8% &
	\ FNEND
21730	  DEF FNC%(C%) &
	\ Q0%=4% &
	\ GOSUB 21000 &
	\ FNEND
21740	  DEF FNF%(C%,R%) &
	\ FNF%=FNQ1%(C%,ABS(R%)) &
	\ FNEND
21750	  DEF FNR%(C%) &
	\ FNR%=Q%(C%,5%) &
	\ FNEND
21760	  DEF FNS% &
	\ FNS%=Q9% &
	\ FNEND
21800	  DEF FNQ0%(C%) &
	\ FNQ0%=0% &
	\ IF C%<1% OR C%>12% THEN &
		  FNQ0%=-1% &
	  ELSE &
		  IF (Q%(0%,0%) AND 2%^C%)=0% THEN  &
			  FNQ0%=-1%
21810	  FNEND
21820	  DEF FNQ1%(C%,C1%) &
	\ Q%(C%,5%)=C1% &
	\ Q7%=C1%/(Q%(C%,4%)*Q%(C%,7%))*Q%(C%,7%)+1% &
	\ IF Q7%<>Q%(C%,6%) THEN  &
		  Q%(C%,6%)=Q7% &
		\ GET #C%, RECORD Q7%
21830	  FNQ1%=(C1%-(Q7%-1%)*Q%(C%,4%))*Q%(C%,3%) &
	\ FNEND
21990	  IF ERR=11% AND ERL=21100% THEN  &
		  Q9%=1% &
		\ RESUME 21999
21992	  IF ERR=11% AND ERL=21820% THEN  &
		  Q9%=-1% &
		\ RESUME 21830
21994	  Q9%=ERR &
	\ Q8%=0% &
	\ RESUME 21999
21999	  ON ERROR GOTO 0 &
	\ RETURN
