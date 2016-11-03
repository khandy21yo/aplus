10	! &
	! Program name: [1,11]PORTRE		Compiled with SCALE 0 on V07.0 &
	! Decompiled on 06-Jan-86 at 06:48 PM by UNBAC Version 1 &

50	DIM X1%(128%), X2%(128%) &

55	DIM F1$(32%) &

60	ON ERROR GOTO 19000 &

100	! &

110	PRINT "Please transfer the data now using the file" &
\	PRINT "option in port I/O, while this is running on" &
\	PRINT "the port." &
\	PRINT &

120	OPEN "KB:" AS FILE 1% &

500	! &

510	X0$="0123456789ABCDEF" &
\	X1$,X2$="" &
\	FOR I%=0% TO 127% &
\		X1%=INSTR(1%,X0$,CHR$(I%))-1% &
\		X2%=X1%*16% &
\		X2%=-1% IF X1%=-1% &
\		X1%=128% IF X1%=0% &
\		X2%=1% IF X2%=0% &
\		X1$=X1$+CHR$(X1%) &
\		X2$=X2$+CHR$(X2%) &
\	NEXT I% &

1000	! &

1010	PRINT "*"; &
\	INPUT LINE #1%, F$ &
\	F$=CVT$$(F$,4%) &
\	I%=INSTR(1%,F$,"^") &
\	F1$=RIGHT(F$,I%+1%) IF I% &
\	F$=LEFT(F$,I%-1%) IF I% &

1015	F$=CVT$$(F$,-1%) &
\	OPEN F$ FOR OUTPUT AS FILE 2% &

1020	R0%=0% &

1030	GOTO 2000 IF F1$="" &
\	F1%=0% &

1040	I%=INSTR(1%,F1$," ") &
\	IF I% THEN &
		F1%=F1%+1% &
\		F1$(F1%)=LEFT(F1$,I%-1%) &
\		F1$=RIGHT(F1$,I%+1%) &
\		GOTO 1040 &

1050	IF F1$<>"" THEN &
		F1%=F1%+1% &
\		F1$(F1%)=F1$ &

1060	X2%(I%)=VAL(F1$(I%)) FOR I%=1% TO F1% &
\	X1%(I%)=X2%(I%) FOR I%=5% TO F1% &
\	X1%(I%)=0% FOR I%=F1%+1% TO 26% &
\	X1%(0%)=30% &
\	X1%(1%)=6% &
\	X1%(2%)=-25% &
\	X1%(3%)=2% &
\	X1%(4%)=11% &
\	CHANGE X1% TO V$ &
\	V$=SYS(V$) &

1070	V$=SYS(CHR$(6%)+CHR$(-17%)+CHR$(2%)+CHR$(X2%(1%))+CHR$(X2%(2%))+CHR$( &
		X2%(3%))+CHR$(X2%(4%))) &

2000	! &

2010	PRINT ">"; &

2020	INPUT LINE #1%, L$ &
\	L$=CVT$$(L$,5%) &
\	GOTO 3000 IF L$="QUIT" OR L$="MORE" &
\	I%=INSTR(1%,L$,"X") &
\	IF I%<>65% THEN &
		GOTO 2800 &
\		I1%=INSTR(1%,L$,"Y") &
\		IF I1%<>69% THEN &
			GOTO 2800 &

2030	CHANGE L$ TO X1% &
\	X%=0% &
\	X%=X%+(X1%(I%) AND 127%) FOR I%=1% TO 64% &
\	X%=X% AND 511% &
\	GOTO 2800 IF X%<>VAL(MID(L$,66%,3%)) &

2035	CHANGE XLATE(L$,X1$) TO X1% &

2040	PRINT "#"; &

2050	INPUT LINE #1%, L$ &
\	L$=CVT$$(L$,5%) &
\	GOTO 3000 IF L$="QUIT" &
\	I%=INSTR(1%,L$,"X") &
\	IF I%<>65% THEN &
		GOTO 2810 &

2060	CHANGE L$ TO X2% &
\	X%=0% &
\	X%=X%+(X2%(I%) AND 127%) FOR I%=1% TO 64% &
\	X%=X% AND 511% &
\	GOTO 2810 IF X%<>VAL(MID(L$,66%,3%)) &

2065	CHANGE XLATE(L$,X2$) TO X2% &

2100	X1%(I%)=(X1%(I%) AND 15%)+(X2%(I%) AND (NOT 15%)) FOR I%=1% TO 64% &
\	CHANGE X1% TO D$ &

2110	FIELD #2%, R0%*64% AS E$,64% AS E$ &
\	LSET E$=D$ &
\	R0%=R0%+1% &
\	IF R0%=8% THEN &
		PUT #2% &
\		R0%=0% &

2190	GOTO 2000 &

2800	SLEEP 1% &
\	V$=SYS(CHR$(4%)) &
\	GET #1%, RECORD 8192% &
\	GOTO 2800 &

2805	PRINT "?"; &
\	GOTO 2020 &

2810	SLEEP 1% &
\	V$=SYS(CHR$(4%)) &
\	GET #1%, RECORD 8192% &
\	GOTO 2810 &

2815	PRINT "?"; &
\	GOTO 2050 &

3000	! &

3005	PUT #2% IF R0% &

3010	CLOSE 2% &
\	GOTO 1000 IF L$="MORE" &
\	GOTO 32767 &

19000	! &

19010	IF ERR=13% AND ERL=2800% THEN &
		RESUME 2805 &

19020	IF ERR=13% AND ERL=2810% THEN &
		RESUME 2815 &

19999	ON ERROR GOTO 0 &

32767	END
