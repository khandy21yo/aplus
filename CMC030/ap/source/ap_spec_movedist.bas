1	!++
	! Special program to move AP_OPEN_DIST data from one user to another
	!
	! Description:
	!
	!	This program is set up to move AP_OPEN_DIST records from one
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

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.HB"
	MAP (AP_OPEN_DIST) AP_OPEN_DIST_CDD AP_OPEN_DIST

110	!
	! Open old AP_OPEN_DIST file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.MOD"
	USE
		PRINT "CAN'T OPEN ORIGINAL AP_OPEN_DIST FILE"
		STOP
	END WHEN

	AP_OPEN_DIST.CH_OLD% = AP_OPEN_DIST.CH%
	AP_OPEN_DIST.CH% = 0%

120	!
	! Open new AP_OPEN_DIST file
	!
	AP_OPEN_DIST.DEV$ = "[KINGMAN]"

	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN_DIST.MOD"
	USE
		PRINT "CAN'T OPEN FINAL AP_OPEN_DIST FILE"
		STOP
	END WHEN

	AP_OPEN_DIST.CH_NEW% = AP_OPEN_DIST.CH%
	AP_OPEN_DIST.CH% = 0%

200	BATCH$ = "C6DZEE"

	WHEN ERROR IN
		FIND #AP_OPEN_DIST.CH_OLD%, &
			KEY #1% EQ BATCH$
	USE
		PRINT "CAN'T FIND BATCH"
		STOP
	END WHEN

210	WHEN ERROR IN
		GET #AP_OPEN_DIST.CH_OLD%
	USE
		CONTINUE 900 IF ERR = 11%
	END WHEN

	IF (AP_OPEN_DIST::BTHNUM = BATCH$)
	THEN
		PUT #AP_OPEN_DIST.CH_NEW%
		DELETE #AP_OPEN_DIST.CH_OLD%
		PRINT ".";
		PRINT IF CCPOS(0%) >= 50%

		GOTO 210
	END IF

900	PRINT
	PRINT "DONE"

	CLOSE AP_OPEN_DIST.CH_OLD%
	CLOSE AP_OPEN_DIST.CH_NEW%

32767	END
