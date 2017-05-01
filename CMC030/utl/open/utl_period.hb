	!
	! File Layout for: UTL.UTL_PERIOD on 21-May-01
	!
	! Period Timekeeper
	!

	RECORD UTL_PERIOD_CDD
		! Element = ERA
		!   Description = Era code
		STRING ERA = 2
		! Element = YEAR
		!   Description = Physical year (YYYY)
		STRING YEAR = 4
		! Element = CYCLE
		!   Description = Period in the physical year
		STRING CYCLE = 2
		! Element =
		!   Description = Period description
		STRING DESCRIPTION = 20
		! Element =
		!   Description = Period status flag
		STRING PERIOD_STATUS = 1
		! Element = DATE
		!   Description = Beginning date (YYYYMMDD)
		STRING BEG_DATE = 8
		! Element = DATE
		!   Description = End date (YYYYMMDD)
		STRING END_DATE = 8
		! Element =
		!   Description = Period sequential number
		STRING AGE = 4
	END RECORD
