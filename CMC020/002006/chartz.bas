1	! Zero numbers in chart of accounts. &
	! &
	! No recovery from this program except for restoring &
	! the file from backup or re-generating the chart of &
	! account information. &
	! &

10	OPEN "NL:" FOR INPUT AS FILE 12%, RECORDSIZE 256% &
\	FIELD #12%, 54% AS A$, &
		22%*8% AS B$, &
		26% AS C$ &
\	FIELD #12%, 256% AS CH$ &

100	PRINT "Account to zero (SY:) ?"; &
\	INPUT LINE CHART.DEV$ &
\	CHART.DEV$ = CVT$$(CHART.DEV$, -1%) &

110	INPUT "Conform ZEROING chart of accounts?"; YN$ &
\	YN$ = LEFT(CVT$$(YN$,-1%),1%) &
\	GOTO 32767 IF YN$<>"Y" &

200	IF FNO%(2%, CHART.DEV$ + "CHART.DAT", "/RW", "") &
	THEN &
		PRINT "Unable to open chart - "; FNS% &
\		GOTO 32767 &

300	IF FNG%(2%, "") &
	THEN &
		PRINT "Chart of accounts is empty!" &
\		GOTO 32767 &

310	LSET CH$ = FNL$ &
\	LSET B$ = STRING$(22%*8%, 0%) &
\	STOP IF FNU%(2%, CH$) &

315	PRINT "."; &
\	PRINT IF CCPOS(0%) >= 50% &

320	GOTO 310 UNLESS FNN%(2%) &

400	V% = FNC%(2%) &
\	GOTO 32767
32767	END
