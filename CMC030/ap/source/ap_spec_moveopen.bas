1	!++
	! Special program to move AP_OPEN data from one user to another
	!
	! Description:
	!
	!	This program is set up to move AP_OPEN records from one
	!	user to another.
	!
	!	Note that the second account and the batch number are
	!	hardcoded into this program, since it is only a one-time
	!	program that I hope I never need again.
	!
	! History:
	!
	!	08/29/2001 - Kevin Handy
	!		Original program (jet)
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP (AP_OPEN) AP_OPEN_CDD AP_OPEN

110	!
	! Open old AP_OPEN file
	!
	AP_OPEN.DEV$ = "[fj.temp]"
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.MOD"
	USE
		PRINT "CAN'T OPEN ORIGINAL AP_OPEN FILE"
		STOP
	END WHEN

	AP_OPEN.CH_OLD% = AP_OPEN.CH%
	AP_OPEN.CH% = 0%

120	!
	! Open new AP_OPEN file
	!
	AP_OPEN.DEV$ = "[fj]"

	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.MOD"
	USE
		PRINT "CAN'T OPEN FINAL AP_OPEN FILE"
		STOP
	END WHEN

	AP_OPEN.CH_NEW% = AP_OPEN.CH%
	AP_OPEN.CH% = 0%

200	BATCH$ = "EF69EM"

	WHEN ERROR IN
		FIND #AP_OPEN.CH_OLD%, &
			KEY #2% EQ BATCH$
	USE
		PRINT "CAN'T FIND BATCH"
		STOP
	END WHEN

210	WHEN ERROR IN
		GET #AP_OPEN.CH_OLD%
	USE
		CONTINUE 900 IF ERR = 11%
	END WHEN

	IF (AP_OPEN::BATCH = BATCH$)
	THEN
		PUT #AP_OPEN.CH_NEW%
		DELETE #AP_OPEN.CH_OLD%
		PRINT ".";
		PRINT IF CCPOS(0%) >= 50%

		GOTO 210
	END IF

900	PRINT
	PRINT "DONE"

	CLOSE AP_OPEN.CH_OLD%
	CLOSE AP_OPEN.CH_NEW%

32767	END
