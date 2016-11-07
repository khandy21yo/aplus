1	  EXTEND
10	! &
	! LOAD DATA FROM MAGTAPE &
	! &
	  DIM T$(300%) &
	\ GOSUB 9000 &
	\ P9$="" &
	\ L1%=0% &
	\ K9$="" &
	\ INPUT "BLOCKING FACTOR"; BLOCK.FACTOR% &
		"RECORD LENGTH  "; RECL% &
		"TAPE DENSITY (800/1600) <1600>"; DEN% &
		"ASCII/EBCDIC   "; FORMAT.TYPE$ &
		"FILE TO SAVE TO"; V4$ &
	\ NUM.REC%=BLOCK.FACTOR%/RECL% &
	\ CONVERT% = 0% &
	\ CONVERT% = -1% IF LEFT(FORMAT.TYPE$, 2%) = "EB" &
	\ D%=256% &
	\ D% = 12% IF DEN% = 800% &
	\ P%=0% &
	\ S%=0% &
	! SET UP MAGETAPE MODE VARIABLES FOR NON FILE STRUCTURED &
	! D% = 12% FOR 800 BPI OR D%=256% FOR 1600 BPI &
	! P% = 0% FOR ODD PARITY OR =1% FOR EVEN PARITY &
	! S% = 0% for no stay of MODE or =8192% if retained after CLOSE &
	\ OPEN "MM0:" AS FILE 1%, RECORDSIZE BLOCK.FACTOR%, &
			  MODE D%+P%+S% &
		\ I%=MAGTAPE (3%, 0%, 1%) &
		\ GOTO 12000 IF FNO%(2%, V4$, "/NS/SF/CR:"+NUM1$(RECL%), "") &
	\ OPEN "NL:" AS FILE 12%, RECORDSIZE RECL% &
		\ FIELD #12%, RECL% AS V$ &
	\ FIELD #1%, (I%-1%)*RECL% AS D$, RECL% AS T$(I%) &
		  FOR I%=1% TO NUM.REC% &
	! &
	! GET PAST ANSI HEADER RECORDS &
	! &
	\ ON ERROR GOTO 127 &
	\ GET #1% UNTIL 1%=0% &

127	  RESUME 130 &

130	  FOR T%=1% TO 32767% &
		\ ON ERROR GOTO 2000 &
		\ GET #1% &
		\ ON ERROR GOTO 0 &
		\ THIS.NUM.REC% = RECOUNT/RECL% &
		\ FOR I%=1% TO THIS.NUM.REC% &
			\ LSET V$=XLATE(T$(I%), X2$)+"" IF CONVERT% &
			\ LSET V$=T$(I%)+"" UNLESS CONVERT% &
			\ GOTO 12200 IF FNA%(2%,V$) &
		\ NEXT I% &
	\ NEXT T% &
	\ PRINT "ENTERED"; FNR(2%); &
			  "RECORDS AND STILL NOT AT END OF MAGTAPE FILE." &
	\ GOTO 4000 &

2000	  IF ERR=11% &
	  THEN	  RESUME 4000 &
	  ELSE	  ON ERROR GOTO 0 &
		\ GOTO 4000 &

4000	STOP IF FNX%(P9$,L1%,K9$) &
	\ GOTO 32767 &
9000	! XLATE table for EBCDIC to ASCII conversion. &
	  X2$=CHR$(0%)+CHR$(1%)+CHR$(2%)+CHR$(3%)+CHR$(0%)+CHR$(9%)+ &
		CHR$(0%)+CHR$(127%)+STRING$(3%,0%)+CHR$(11%)+CHR$(12%)+	&
		CHR$(3%)+CHR$(14%)+CHR$(15%)+CHR$(16%)+CHR$(17%)+CHR$(18%)+ &
		STRING$(3%,0%)+CHR$(8%)+CHR$(0%)+CHR$(24%)+CHR$(25%)+ &
		STRING$(3%,0%)+CHR$(29%)+CHR$(0%)+CHR$(31%)+CHR$(19%)+ &
		CHR$(0%)+CHR$(28%)+STRING$(2%,0%)+CHR$(10%)+CHR$(23%) &
	\ X2$=X2$+CHR$(27%)+STRING$(5%,0%)+CHR$(5%)+CHR$(6%)+ &
		CHR$(7%)+STRING$(2%,0%)+CHR$(22%)+STRING$(2%,0%)+CHR$(30%)+ &
		CHR$(0%)+CHR$(4%)+STRING$(4%,0%)+CHR$(20%)+CHR$(21%)+CHR$(0%)+ &
		CHR$(26%)+CHR$(32%)+STRING$(10%,0%)+CHR$(46%)+CHR$(60%)+ &
		CHR$(40%)+CHR$(43%)+CHR$(0%)+CHR$(38%)+STRING$(9%,0%)+ &
		CHR$(33%)+CHR$(36%)+CHR$(42%)+CHR$(41%)+CHR$(59%)+CHR$(0%) &
	\ X2$=X2$+CHR$(45%)+CHR$(47%)+STRING$(9%,0%)+CHR$(44%)+ &
		CHR$(37%)+CHR$(95%)+CHR$(62%)+CHR$(63%)+STRING$(10%,0%)+ &
		CHR$(58%)+CHR$(35%)+CHR$(64%)+CHR$(39%)+CHR$(61%)+CHR$(34%)+ &
		CHR$(0%) &
	\ X2$=X2$+CHR$(X%) FOR X%=97% TO 105% &
	\ X2$=X2$+STRING$(7%,0%) &
	\ X2$=X2$+CHR$(X%) FOR X%=106% TO 114% &
	\ X2$=X2$+STRING$(7%,0%)+CHR$(126%) &
	\ X2$=X2$+CHR$(X%) FOR X%=115% TO 122% &
	\ X2$=X2$+STRING$(22%,0%)+CHR$(123%) &
	\ X2$=X2$+CHR$(X%) FOR X%=65% TO 73% &
	\ X2$=X2$+STRING$(6%,0%)+CHR$(125%) &
	\ X2$=X2$+CHR$(X%) FOR X%=74% TO 82% &
	\ X2$=X2$+STRING$(6%,0%)+CHR$(92%)+CHR$(0%) &
	\ X2$=X2$+CHR$(X%) FOR X%=83% TO 90% &
	\ X2$=X2$+STRING$(6%,0%) &
	\ X2$=X2$+CHR$(X%) FOR X%=48% TO 57% &
	\ X2$=X2$+STRING$(6%,0%) &
	\ RETURN &

12000	PRINT "PROBLEM IN OPENING MAGTAPE FILE.  HALTED" &
	\ STOP &

12200	PRINT "PROBLEM IN ADDING TO ISAM FILE.  ERROR =";FNS% &
	\ GOTO 4000 &

21000	!!! ISAM (KEVIN'S BASE; BILL LORTZ' SORT--SEP. 16, 1980) &
	!!! &
	!!! MODIFIED JAN. 6, 1982 -- DLH &
	!!!	1.  Return an error #8 on FNO%() if file is being sorted &
	!!!	2.  Return an error #11 if attempt is made to delete header &
	!!!	3.  FNT now works after any ISAM call, not just FNG%() &
	!!!	4.  FNG% returns 29 if key and data file have different &
	!!!	    number of records &
	!!! &

21004	  PRINT "Fall through to ISAM!" &
	: STOP &

21005	  DIM Q$(12%),Q%(12%,11%) &

21010	  DEF FNO%(C%,C$,C1$,C2$) &
	: ON ERROR GOTO 21900 &
	: Q9%=0% &
	: Q1%=INSTR(1%,C$,"<") &
	: Q1$="" &
	: Q1$=RIGHT(C$,Q1%) IF Q1% &
	: C$=LEFT(C$,Q1%-1%) IF Q1% &
	: IF INSTR(1%,C1$,"/CR") THEN Q1%=INSTR(1%,C1$,"/CR") &
		: Q2%=INSTR(Q1%,C1$,",") &
		: Q2%=LEN(C1$)+1% UNLESS Q2% &
		: Q3%=INSTR(Q1%+1%,C1$,"/") &
		: Q3%=LEN(C1$)+1% UNLESS Q3% &
		: Q2%=Q3% IF Q3%<Q2% &
		: Q4%=VAL(MID(C1$,Q1%+4%,Q2%-Q1%-4%)) &
		: GOSUB 21070 &
		: OPEN C$+Q1$ FOR OUTPUT AS FILE C%,CLUSTERSIZE Q6%,FILESIZE Q7% &
		: PRINT #C%,CVT%$(0%)+CVT%$(Q4%)+"S"+CHR$(128%-[Q4%<=4%]); &
		: CLOSE #C% &
		: UNLESS INSTR(1%,C1$,"/SF") THEN Q9%=[Q2%=Q3%] AND 59% &
			: UNLESS Q9% THEN Q4%=VAL(MID(C1$,Q2%+1%,Q3%-Q2%-1%)) &
				: GOSUB 21070 &
				: OPEN LEFT(C$,LEN(C$)-1%)+"1"+Q1$ FOR OUTPUT AS FILE C%+1%, &
					CLUSTERSIZE Q6%,FILESIZE Q7% &
				: PRINT #C%+1%,CVT%$(0%)+CVT%$(Q4%)+"S"+CHR$(128%-[Q4%<=4%]); &
				: CLOSE #C%+1% &

21020	  Q9%=46% IF C%<1% OR C%>11% &
	: UNLESS Q9% THEN Q8%=INSTR(1%,C1$,"/SF")<>0% &
		: Q9%=7% IF (Q%(C%+1%,0%) AND NOT(Q8%)) OR Q%(C%,0%) &
		: UNLESS Q9% THEN Q%=-8192%*(INSTR(1%,C1$,"/RO")<>0%) &
			: Q%(C%,0%)=Q%(C%,0%) OR 16% IF INSTR(1%,C1$,"/NS") &
			: Q$=C$ &
			: Q0%,Q6%=C% &
			: GOSUB 21040 &
			: CLOSE C% IF Q9% &
			: UNLESS Q9% THEN Q%(C%,0%)=Q%(C%,0%) OR 1% &
				: IF Q8% THEN Q%(Q0%,0%)=Q%(Q0%,0%) OR 2% ELSE &
					 Q$=LEFT(C$,LEN(C$)-1%)+"1" &
					: Q0%=C%+1% &
					: GOSUB 21040 &
					: Q9%=Q9% OR (Q%(C%,1%)<>Q%(Q0%,1%) AND Q9%=0%)*-29% &
					: UNLESS Q9%=10% THEN GOTO 21899 IF Q9% AND Q9%<>29% &
						: Q%(Q0%,0%)=Q%(Q0%,0%) OR 2% &

21030	  GOSUB 21999 &
	: FNO%=Q9% &
	: FNEND &
21040	  OPEN Q$ FOR INPUT AS FILE Q0%, MODE Q% &
	: IF STATUS AND 1024% THEN IF INSTR(1%,C1$,"/RW") THEN Q9%=10% &
		: RETURN &

21045	  IF STATUS AND 1024% THEN &
		  PRINT "WARNING: WRITE PRIVILEGES ARE NOT ESTABLISHED FOR ";Q$ &
		  IF Q%<>8192% AND C%=Q0% &
		: Q%(Q0%,0%)=Q%(Q0%,0%) OR 4% &

21050	  GET #Q0%, RECORD 1% &
	: Q$(Q0%)=Q$ &
	: Q%(Q0%,6%)=1% &
	: Q%(Q0%,7%)=0% &
	: FIELD #Q0%, 2% AS Q0$, 2% AS Q1$, 1% AS Q2$, 1% AS Q3$ &
	: Q%(Q0%,1%),Q%(Q0%,5%)=CVT$%(Q0$) &
	: GOTO 21899 IF Q2$<>"U" AND Q2$<>"S" AND Q2$<>"X" &
	: Q9%=8% IF Q2$="X" &
	: Q%(Q0%,2%)=CVT$%(Q1$) &
	: Q%(Q0%,3%)=(Q2$="U") &
	: Q%(Q0%,4%)=(ASCII(Q3$) AND 127%) &
	: Q3%=1% &
	: Q3%=Q3%*2% UNTIL Q3%>=Q%(Q0%,2%) &
	: Q%(Q0%,8%)=Q3% &
	: Q%(Q0%,9%)=512%/Q%(Q0%,8%) &
	: IF Q%(Q0%,3%) THEN UNLESS C%<>Q0% OR INSTR(1%,Q9$,Q$) THEN &
		  PRINT "File ";Q$;" is not sorted"; UNLESS INSTR(1%,C1$,"/NS") &
		: PRINT " -- will sort on end."; UNLESS &
			INSTR(1%,C1$,"/RO") OR INSTR(1%,C1$,"/NS") &
		: PRINT UNLESS INSTR(1%,C1$,"/NS") &
		: Q9$=Q9$+CHR$(13%)+Q$ UNLESS &
			INSTR(1%,C1$,"/RO") OR INSTR(1%,C1$,"/NS") &
		: Q%(0%,5%)=Q%(0%,1%)/10%*9% &

21060	  RETURN &

21070	  Q6%=8% &
	: Q7%=0% &
	: Q8%=INSTR(1%,C1$,"/EX:") &
	: RETURN UNLESS Q8% &
	: Q9%=INSTR(Q8%+1%,C1$,"/") &
	: Q9%=LEN(C1$)+1% UNLESS Q9% &
	: Q7%=VAL(MID(C1$,Q8%+4%,Q9%-Q8%-4%)) &
	: Q9%=2%^(INT(LOG10(Q4%)/.30103)+1%) &
	: Q7%=Q7%/(512%/Q9%) &
	: Q8%=(Q7%+6%)/7% &
	: Q8%=1% IF Q8%=0% &
	: Q6%=-(2%^(1%+INT(LOG10(Q8%)/LOG(2)))) &
	: Q6%=-256% IF Q6%<-256% &
	: Q9%=0% &
	: RETURN &
21200	  DEF FNC%(C%) &
	: STOP IF FNX%("",0%,"") IF C%=0% &
	: Q0%=C% &
	: GOSUB 21440 &
	: UNLESS Q9% THEN GOSUB 21430 IF Q%(C%,11%) &
		: PUT #C%, RECORD Q%(C%,6%) IF Q%(C%,7%) &
		: CLOSE C% &
		: Q%(C%,0%)=0% &
		: Q0%=C%+1% &
		: UNLESS (Q%(Q0%,0%) AND 3%)<>2% THEN GOSUB 21430 IF Q%(Q0%,11%) &
			: PUT #Q0%, RECORD Q%(Q0%,6%) IF Q%(Q0%,7%) &
			: CLOSE Q0% &
			: Q%(Q0%,0%)=0% &

21210	  GOSUB 21999 &
	: FNC%=Q9% &
	: FNEND &

21370	  DEF FNA%(C%,C$) &
	: Q0%=C%* [(C%<0%)-(C%>0%)] &
	: Q%(Q0%,0%)=Q%(Q0%,0%) AND NOT 8% &
	: Q%(Q0%,0%)=Q%(Q0%,0%) OR 8% IF C%<0% &
	: C%=Q0% &
	: GOSUB 21440 &
		: UNLESS Q9% THEN Q9%=10% IF Q%(C%,0%) AND 4% &
			: UNLESS Q9% THEN Q3%,Q%(C%,1%)=Q%(C%,1%)+1% &
				: FIELD #C%, FNQ%(C%,Q3%) AS Q$, Q%(C%,2%)-2% AS Q$, 2% AS Q1$ &
				: Q9%=0% IF Q9%=11% &
				: LSET Q$=C$ &
				: LSET Q1$=CVT%$(Q3%) &
				: Q9$=Q9$+CHR$(13%)+Q$(C%) IF Q%(C%,3%)=0% UNLESS Q%(C%,0%) AND 16% &
				: Q%(C%,7%),Q%(C%,3%),Q%(C%,11%)=-1% &
				: GOSUB 21430 IF (Q%(C%,0%) AND 8%)<>0% &
				: UNLESS Q%(C%,0%) AND 2% THEN Q0%=C%+1% &
					: Q%(Q0%,1%)=Q3% &
					: FIELD #Q0%, FNQ%(Q0%,Q3%) AS Q$, Q%(Q0%,2%) AS Q$ &
					: Q9%=0% IF Q9%=11% &
					: LSET Q$=C$ &
					: Q%(Q0%,7%),Q%(Q0%,11%)=-1% &
					: GOSUB 21430 IF (Q%(Q0%,0%) AND 8%)<>0% &

21380	  GOSUB 21999 &
	: FNA%=Q9% &
	: FNEND &

21430	  FIELD #Q0%, FNQ%(Q0%,0%)+2% AS Q$, 2% AS Q1$, 1% AS Q1$ &
	: LSET Q$=CVT%$(Q%(Q0%,1%)) &
	: LSET Q1$=MID("US",Q%(Q0%,3%)+2%,1%) &
	: Q%(Q0%,7%)=-1% &
	: Q%(Q0%,11%)=0% &
	: RETURN &

21440	  Q6%=C%*[(C%<0%)-(C%>0%)] &
	: Q9%=0% &
	: Q9%=46% IF Q0%<1% OR Q0%>11% &
	: RETURN IF Q9% &
	: Q9%=9% IF (Q%(Q0%,0%) AND 3%)=0% &
	: RETURN IF Q9% &
	: ON ERROR GOTO 21900 &
	: RETURN &

21660	  DEF FNR(C%)=Q%(C%,10%) &
21700	  DEF FNX%(C$,C%,C1$) &
	: Q0$=SYS(CHR$(7%)) &
	: Q5%=INSTR(1%,Q0$,CHR$(255%)) &
	: Q2%=INSTR(1%,Q0$,CHR$(14%)) : Q2%=Q5%+12% IF Q2%=0% &
	: Q0$=MID(Q0$+SPACE$(12%),Q5%,Q2%-Q5%) IF Q5% &
	: Q0$="" IF Q5%=0% &
	: IF C%>=0% THEN Q2%=FNC%(Q1%) IF (Q%(Q1%,0%) AND 1%) FOR Q1%=1% TO 12% &
		: IF Q9$<>"" THEN Q3$=CVT%$(C%)+C$ IF C$<>"" &
			: Q3$=CVT%$(8100%)+"!MENU.BAC" IF C$="" AND Q5% &
			: Q3$=Q3$+"  " &
			: Q9$=Q9$+CHR$(13%)+CHR$(13%)+Q0$ &
			: Q9$=Q9$+CHR$(14%)+C1$ IF C1$<>"" &
			: Q$=LEFT(CHR$(LEN(Q3$))+Q3$+Q9$,127%) &
			: Q$=SYS(CHR$(8%)+Q$) &
			: PRINT "<->FILES SECURED<->" &
			: CHAIN "[1,6]FSTSRS.TSK" &

21710	  ON ERROR GOTO 21900 &
	: CLOSE Q1% FOR Q1%=1% TO 12% IF C%<0% &
	: C%=-C% IF C%<0% &
	: CHAIN "!MENU.BAC" 8100  IF Q5% AND C$="" &
	: V$=SYS(CHR$(8%)+Q0$+"   "+CHR$(14%)+C1$) &
	: CHAIN C$ C% IF C$<>"" &
	: GOTO 32767 &
	: FNEND &

21800	  DEF FNQ%(C%,R%) &
	: R%=R%+Q%(C%,4%) IF R% &
	: Q7%=R%/Q%(C%,9%)+1% &
	: GOTO 21820 IF Q7%=Q%(C%,6%) &
	: PUT #C%, RECORD Q%(C%,6%) IF Q%(C%,7%) &
	: Q%(C%,7%)=0% &

21810	  GET #C%, RECORD Q7% &
	: Q%(C%,6%)=Q7% &

21820	  FNQ%=R%*Q%(C%,8%) AND 511% &
	: FNEND &
21899	  PRINT C$;" HAS A FILE HEADER PROBLEM." &
	: PRINT &
	: PRINT "DO NOT PROCEED!  CALL CMC IMMEDIATELY."; &
	: V$=SYS(CHR$(5%)) &
	: STOP &

21900	  IF ERL=21040% THEN Q9%=ERR &
		: Q%(C%,0%)=0% &
		: CLOSE C% &
		: RESUME 21060 &

21910	  IF ERR=10% AND ERL=21800% THEN Q9%=10% &
		: RESUME 21810 &

21920	  IF ERR=11% AND ERL=21810% THEN Q9%=11% &
		: Q%(C%,6%)=Q7% &
		: RESUME 21820 &

21930	  IF (ERR=71% OR ERR=5%) AND ERL=21710% THEN PRINT &
		  "ERROR";ERR;"IN MENU CHAIN.  STATEMENT, FILE OR ACCOUNT NOT FOUND." &
		: V$=SYS(CHR$(5%)) &
		: STOP &

21940	  PRINT "UNTRAPPED ERROR";ERR;"AT LINE";ERL &
	: Q9%=ERR &
	: PRINT &
	: PRINT "DO NOT PROCEED!  CALL CMC IMMEDIATELY."; &
	: V$=SYS(CHR$(5%)) &
	: STOP &

21999	  ON ERROR GOTO 0 &
	: RETURN &

32767 END
