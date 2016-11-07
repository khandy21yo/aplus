10	! &
	! Set up a VT100 to use an LA100 or LA50 printer &
	! &
	! &
	READ HORZ.PITCH%(I%), HORZ.PITCH$(I%) FOR I%=0% TO 7% &
\	READ VERT.PITCH%(I%), VERT.PITCH$(I%) FOR I%=0% TO 6% &
\	DATA	0,  0,  5,  5,  6,  6,  8,  8, 10,  0, 12,  2, 13,  3, 16,  4, &
		0,  0,  6,  0,  8,  2, 12,  3,  2,  4,  3,  5,  4,  6  &

20	ESC$ = CHR$(155%) &
\	SEQ$ = ESC$ + "[" &
\	PRINT SEQ$; "?4i" &

30	PRINT ESC$; "<"; SEQ$; "H"; SEQ$; "J"; &
\	PRINT "PRINTER SETUP"; &
\	PRINT &
\	INPUT "KB number (return for printer port)"; K$ &
\	K$ = CVT$$(K$, -1%) &
\	PRINT &
\	INPUT "Print Quality:  (N)ormal or (E)nhanced"; PQ$ &
\	PQ$ = "N" IF PQ$ = "" &
\	PQ$ = LEFT(CVT$$(PQ$, -1%), 1%) &
\	GOTO 30 IF PQ$ <> "N" AND PQ$ <> "E" &
\	DEN$ = "0" &
\	DEN$ = "2" IF PQ$ = "E" &

40	PRINT SEQ$; "8;0H"; SEQ$; "J"; &
\	INPUT "Pitch: 5, 6, 8, 10, 12, 13, 16 (cpi)", HORZ.PITCH% &
\	HORZ.PITCH$ = "10" IF HORZ.PITCH$ = "" &
\	FOR I% = 0% TO 7% &
\		HORZ.PITCH$ = HORZ.PITCH$(I%) &
			IF HORZ.PITCH% = HORZ.PITCH%(I%) &
\		GOTO 50 IF HORZ.PITCH% = HORZ.PITCH%(I%) &
\	NEXT I% &
\	GOTO 40 &

50	PRINT SEQ$; "10;0H"; SEQ$; "J"; &
\	INPUT "Vertical pitch: 2, 3, 4, 6, 8, 12 (cpi)", VERT.PITCH% &
\	VERT.PITCH$ = "6" IF VERT.PITCH$ = "" &
\	FOR I% = 0% TO 6% &
\		VERT.PITCH$ = VERT.PITCH$(I%) &
			IF VERT.PITCH% = VERT.PITCH%(I%) &
\		GOTO 60 IF VERT.PITCH% = VERT.PITCH%(I%) &
\	NEXT I% &
\	GOTO 50 &

60	PRINT SEQ$; "12;0H"; SEQ$; "J"; &
\	INPUT "Printer on (Y/N)"; PRINTER.ON$ &
\	PRINTER.ON$ = "N" IF PRINTER.ON$ = "" &
\	PRINTER.ON% = 0% &
\	PRINTER.ON% = -1% IF LEFT(CVT$$(PRINTER.ON$, -1%), 1) = "Y" &

70	FORM$ = "" &
\	FORM$ = FORM$ + SEQ$ + "5i" ! Set pass through mode on VT100 &
\	FORM$ = FORM$ + SEQ$ + DEN$ + '"z' ! Set density &
\	FORM$ = FORM$ + SEQ$ + HORZ.PITCH$ + 'w' ! Set horizontal pitch &
\	FORM$ = FORM$ + SEQ$ + VERT.PITCH$ + 'z' ! Set vertical pitch&

80	FORM$ = FORM$ + SEQ$ + "4i" ! Get out of pass through mode &
\	FORM$ = FORM$ + SEQ$ + "?5i" IF PRINTER.ON% ! Leave in copy mode &

900	OPEN 'KB'+K$+':' AS FILE 1%, RECORDSIZE LEN(FORM$), MODE 1% &
\	FIELD #1%, LEN(FORM$) AS SETUP$ &
\	LSET SETUP$ = FORM$ &
\	PUT #1%, RECORD 4096%, COUNT LEN(FORM$) &
\	CLOSE #1% &

8000	PRINT "SETUP COMPLETE. . ."; CHR$(12%) &

8100	S$=SYS(CHR$(7%)) &
\	I%=INSTR(1%,S$,CHR$(255%)) &
\	CHAIN "[1,3]MENU" LINE 8100 IF I% &

32767	END
