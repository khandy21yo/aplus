1	!
	! Special program for LL to regenerate labor file
	!
	! History:
	!
	!	05/25/2004 - Kevin Handy
	!
	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.HB"
	MAP (BM_PRODOPER) BM_PRODOPER_CDD BM_PRODOPER

100	%INCLUDE "SOURCE:[BM.OPEN]BM_PRODOPER.MOD"

120	OPEN "LABORHOURSBYPART.CSV" FOR INPUT AS FILE 1%

200	LINPUT #1%, LIN$

	I1% = INSTR(1%, LIN$, "	")
	I2% = INSTR(I1% + 1%, LIN$, "	")

	GOTO 200 IF (I1% = 0%) OR (I2% = 0%)

	PART$ = TRM$(LEFT(LIN$, I1% - 1%))
	WHEN ERROR IN
		HRS = VAL(RIGHT(LIN$, I2% + 1%))
	USE
		PRINT "-";
		PRINT IF CCPOS(0%) >= 50%
		CONTINUE 200
	END WHEN

300	BM_PRODOPER::PRODUCT	= PART$
	BM_PRODOPER::ITEMNUM	= "0001"
	BM_PRODOPER::OPERATION	= "ALL"
	BM_PRODOPER::HOURS	= HRS
	BM_PRODOPER::EFFDATE	= "20040101"
	BM_PRODOPER::STAT	= "A"
	BM_PRODOPER::THISHOURS	= HRS

	WHEN ERROR IN
		PUT #BM_PRODOPER.CH%
		PRINT "*";
		PRINT IF CCPOS(0%) >= 50%
	USE
		PRINT "?";
		PRINT IF CCPOS(0%) >= 50%
		CONTINUE 200
	END WHEN

	GOTO 200

32767	END
