1	  EXTEND
10	  INPUT "File to build from <xxxxxx.BDR>"; IN$; &
		"File to output to  <xxxxxx.CMD>"; OUT$ &

20	  PRINT	"Output dev and acct  <NL:[*,*]>"; &
	\ INPUT LINE OUT.DEV$ &
	\ OUT.DEV$ = CVT$$(OUT.DEV$, 1% + 4% + 8%) &
	\ PRINT	"Output switches          <none>"; &
	\ INPUT LINE OUT.SWITCHES$ &
	\ OUT.SWITCHES$ = CVT$$(OUT.SWITCHES$, 1% + 4% + 8%) &
	\ PRINT	"Input  switches          <none>"; &
	\ INPUT LINE IN.SWITCHES$  &
	\ IN.SWITCHES$ = CVT$$(IN.SWITCHES$, 1% + 4% + 8%) &
	\ OUT.DEV$ = "NL:[*,*]" UNLESS LEN(OUT.DEV$) &

100	  OPEN IN$  FOR INPUT  AS FILE 1% &
	\ OPEN OUT$ FOR OUTPUT AS FILE 2% &

200	  ON ERROR GOTO 19000 &

300	  INPUT LINE #1%, INN$ &
	\ GOTO 300 IF ASCII(INN$)=32% &
		\ INN$ = CVT$$(INN$, 1%+4%+32%+128%) &
		\ GOTO 300 UNLESS LEN(INN$) &
			\ III% = INSTR(1%, INN$, ":") &
			\ IF III% &
			  THEN	  ACCT$ = RIGHT(INN$, III%+1%) &
				\ DEV$  = LEFT (INN$, III%   ) &
				\ PRINT #2% &
				\ GOTO 300 &

400	  PRINT #2%, OUT.DEV$; OUT.SWITCHES$; "="; &
		     DEV$; ACCT$; INN$; IN.SWITCHES$ &
	\ GOTO 300 &

19000	  CLOSE 1%, 2% &
	\ PRINT ERR,ERL &

32767	  END
