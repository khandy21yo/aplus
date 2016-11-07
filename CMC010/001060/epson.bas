10	! &
	! Set up a VT100 to use an LA100 or LA50 printer &
	! &
	! &
	  READ HORZ.PITCH%(I%), HORZ.PITCH$(I%) FOR I%=0% TO 7% &
	\ READ VERT.PITCH%(I%), VERT.PITCH$(I%) FOR I%=0% TO 6% &
	\ DATA	0,  0,  5,  5,  6,  6,  8,  8, 10,  0, 12,  2, 13,  3, 16,  4, &
		0,  0,  6,  0,  8,  2, 12,  3,  2,  4,  3,  5,  4,  6 &

20	  ESC$ = CHR$(155%) &
	\ SEQ$ = ESC$ + "[" &
	\ PRINT SEQ$; "?4i" &

30	  PRINT ESC$; "<"; SEQ$; "H"; SEQ$; "J"; &
	\ PRINT "PRINTER SETUP"; &
	\ PRINT &
	\ INPUT "Print Quality:  (N)ormal or (E)nhanced"; PQ$ &
	\ PQ$ = "N" IF PQ$ = "" &
	\ PQ$ = LEFT(CVT$$(PQ$, -1%), 1%) &
	\ GOTO 30 IF PQ$ <> "N" AND PQ$ <> "E" &
	\ DEN$ = ESC$ + "H" &
	\ DEN$ = ESC$ + "G" IF PQ$ = "E" &

40	  PRINT SEQ$; "8;0H"; SEQ$; "J"; &
	\ INPUT "Pitch: 10, 12, 17 (cpi)", HORZ.PITCH% &
	\ HORZ.PITCH% = 10% IF HORZ.PITCH% = 0% &
	\ HORZ.PITCH$ = ESC$ + "P" IF HORZ.PITCH% = 10% &
	\ HORZ.PITCH$ = ESC$ + "M" IF HORZ.PITCH% = 12% &
	\ HORZ.PITCH$ = ESC$ + CHR$(143%) IF HORZ.PITCH% = 17% &

50	  PRINT SEQ$; "10;0H"; SEQ$; "J"; &
	\ INPUT "Vertical pitch: 6, 8, 10 (cpi)", VERT.PITCH% &
	\ VERT.PITCH% = 6% IF VERT.PITCH% = 0% &
	\ VERT.PITCH$ = ESC$ + "1" IF VERT.PITCH% = 10% &
	\ VERT.PITCH$ = ESC$ + "0" IF VERT.PITCH% = 8% &
	\ VERT.PITCH$ = ESC$ + "2" IF VERT.PITCH% = 6% &

60	  PRINT SEQ$; "12;0H"; SEQ$; "J"; &
	\ INPUT "Printer on (Y/N)"; PRINTER.ON$ &
	\ PRINTER.ON$ = "N" IF PRINTER.ON$ = "" &
	\ PRINTER.ON% = 0% &
	\ PRINTER.ON% = -1% IF LEFT(CVT$$(PRINTER.ON$, -1%), 1) = "Y" &

70	  FORM$ = "" &
	\ FORM$ = FORM$ + SEQ$ + "5i" ! Set pass through mode on VT100 &
	\ FORM$ = FORM$ + DEN$ ! Set density &
	\ FORM$ = FORM$ + HORZ.PITCH$ ! Set horizontal pitch &
	\ FORM$ = FORM$ + VERT.PITCH$ ! Set vertical pitch &

80	  FORM$ = FORM$ + SEQ$ + "4i" ! Get out of pass through mode &
	\ FORM$ = FORM$ + SEQ$ + "?5i" IF PRINTER.ON% ! Leave in copy mode &

900	PRINT FORM$; &
 !	  OPEN 'KB:' AS FILE 1%, RECORDSIZE LEN(FORM$), MODE 1% &
 !	\ FIELD #1%, LEN(FORM$) AS SETUP$ &
 !	\ LSET SETUP$ = FORM$ &
 !	\ PUT #1%, RECORD 4096%, COUNT LEN(FORM$) &
 !	\ CLOSE #1% &

8000	  PRINT "SETUP COMPLETE. . ."; CHR$(12%) &

32767	  NO EXTEND &
	\ END
