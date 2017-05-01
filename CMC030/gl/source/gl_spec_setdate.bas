1	! Special program to Date in GL period, given a batch number
	!
	OPTION SIZE = (INTEGER LONG, REAL DOUBLE)

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP) GL_YYYY_PP_CDD GL_YYYY_PP

110	!
	! Open period file
	!
	LINPUT "Period to work on"; YYYY_PP$
	YYYY_PP$ = EDIT$(YYYY_PP$, -1%)

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.MOD"

200	!
	! Get batch number
	!
	LINPUT "Batch Number"; BATCHNO$
	BATCHNO$ = EDIT$(BATCHNO$, -1%)

300	!
	! Get old date
	!
	LINPUT "Old Date YYYYMMDD "; OLDDATE$
	OLDDATE$ = EDIT$(OLDDATE$, -1%)


400	!
	! Get old date
	!
	LINPUT "New Date YYYYMMDD "; NEWDATE$
	NEWDATE$ = EDIT$(NEWDATE$, -1%)

1000	!
	! Startup loop
	!
	FIND #GL_YYYY_PP.CH%, KEY #4% EQ BATCHNO$

1100	WHEN ERROR IN
		GET #GL_YYYY_PP.CH%
	USE
		CONTINUE 2000 IF ERR = 11%
	END WHEN

1110	IF (GL_YYYY_PP::BTHNUM = BATCHNO$)
	THEN
		IF GL_YYYY_PP::TRANDAT = OLDDATE$
		THEN
			GL_YYYY_PP::TRANDAT = NEWDATE$

			! Key not directly changable
			DELETE #GL_YYYY_PP.CH%
			PUT #GL_YYYY_PP.CH%

			PRINT GL_YYYY_PP::ACCT; " "; &
				GL_YYYY_PP::xrefno; " "; &
				GL_YYYY_PP::SUBACC

			GOTO 1000
		END IF

		GOTO 1100
	END IF

2000	CLOSE GL_YYYY_PP.CH%

32767	END
