1	! Try to unroll a batch number into the proper date/time

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	EXTERNAL STRING FUNCTION DATE_TODAY
	EXTERNAL STRING FUNCTION TIME_NOW
	EXTERNAL STRING FUNCTION ASSG_MAKEBATCH

100	THIS_DATE$ = DATE_TODAY
	THIS_TIME$ = TIME_NOW

	THIS_BATCH$ = ASSG_MAKEBATCH(THIS_DATE$, THIS_TIME$)

	PRINT "Current Batch: "; THIS_BATCH$
	PRINT "         Date: "; THIS_DATE$
	PRINT "         Time: "; THIS_TIME$

110	PRINT

	PRINT " Batch Number: "; THIS_BATCH$

	CALL ASSG_UNMAKEBATCH(THIS_BATCH$, THIS_DATE$, THIS_TIME$)

	PRINT "        Date: "; THIS_DATE$
	PRINT "        Time: "; THIS_TIME$
	PRINT

120	PRINT "For Batch Number: ";
	LINPUT THIS_BATCH$

	GOTO 110 IF THIS_BATCH$ <> ""

32767	END
